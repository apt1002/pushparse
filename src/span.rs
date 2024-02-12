use super::{Parser, E, Push, Wrapper, MaybePush, EscapeSequence};

/// Represents a comment.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Comment;

/// Represents a character literal.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CharLiteral(pub char);

/// Represents a string.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral(pub String);

pub const UNTERMINATED_BLOCK_COMMENT: E = "Unterminated block comment";
pub const UNTERMINATED_STRING: E = "Unterminated string";
pub const MISSING_CHAR: E = "Missing character literal";
pub const UNTERMINATED_CHAR: E = "Unterminated character literal";

// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum State {
    /// The initial state.
    Home,

    /// After reading a slash.
    Slash,

    /// After reading a double slash.
    LineComment,

    /// After reading `"/*"`. The boolean indicates whether we have just read a
    /// `'*'` other than the opening one.
    BlockComment(bool),

    /// After reading `'"'`.
    StringSpan(String),

    /// After reading `'\''` and maybe another char.
    CharSpan(Option<char>),
}

use State::*;

pub trait PushSpan: Push<Comment> + Push<CharLiteral> + Push<StringLiteral> {}

impl<I: Push<Comment> + Push<CharLiteral> + Push<StringLiteral>> PushSpan for I {}

/// A [`Parser`] that recognizes comments, and character and string literals.
#[derive(Debug, Clone)]
pub struct SpanParser<I: PushSpan + Push<char>> {
    /// The output stream.
    inner: I,

    /// The parser state.
    state: State,
}

impl<I: PushSpan + Push<char>> SpanParser<I> {
    /// Construct a `SpanParser` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { SpanParser {inner, state: Home} }
}

impl<I: PushSpan + Push<char>> Wrapper for SpanParser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        match self.state {
            Home => {},
            Slash => {
                self.inner.push('/');
            },
            LineComment => {
                self.inner.push(Comment);
            },
            BlockComment(_) => {
                self.error(UNTERMINATED_BLOCK_COMMENT);
            },
            StringSpan(_) => {
                self.error(UNTERMINATED_STRING);
            },
            CharSpan(None) => {
                self.error(MISSING_CHAR);
            },
            CharSpan(_) => {
                self.error(UNTERMINATED_CHAR);
            },
        }
        self.state = Home;
    }

    fn partial_reset(&mut self) {
        match self.state {
            Slash => {
                self.inner.push('/');
            },
            LineComment => {
                self.inner.push(Comment);
            },
            _ => {},
        }
        self.state = Home;
    }

    const MISSING: E = "span: Should not happen";
}

impl<I: PushSpan + Push<char>> MaybePush<char> for SpanParser<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        match &mut self.state {
            Home => {
                match token {
                    '/' => { self.state = Slash; },
                    '\'' => { self.state = CharSpan(None); },
                    '"' => { self.state = StringSpan(String::new()); },
                    _ => { self.inner.push(token); },
                }
                return None;
            },
            Slash => {
                match token {
                    '/' => {
                        self.state = LineComment;
                        return None;
                    },
                    '*' => {
                        self.state = BlockComment(false);
                        return None;
                    },
                    _ => {
                        return Some(token);
                    },
                }
            },
            LineComment => {
                if token == '\n' { return Some(token); }
                return None;
            },
            BlockComment(star) => {
                if token == '/' && *star {
                    self.inner.push(Comment);
                    self.state = Home;
                    return None;
                }
                *star = token == '*';
                return None;
            },
            StringSpan(s) => {
                if token == '"' {
                    let mut s_ = String::new();
                    std::mem::swap(s, &mut s_);
                    s_.shrink_to_fit();
                    self.inner.push(StringLiteral(s_));
                    self.state = Home;
                } else {
                    s.push(token);
                }
                return None;
            },
            CharSpan(None) => {
                if token == '\'' {
                    self.error(MISSING_CHAR);
                    return None;
                }
                self.state = CharSpan(Some(token));
                return None;
            },
            CharSpan(Some(c)) => {
                if token == '\'' {
                    self.inner.push(CharLiteral(*c));
                    self.state = Home;
                    return None;
                }
                return Some(token);
            },
        }
    }
}

impl<I: PushSpan + Push<char>> MaybePush<EscapeSequence> for SpanParser<I> {
    fn maybe_push(&mut self, token: EscapeSequence) -> Option<EscapeSequence> {
        match &mut self.state {
            LineComment => {
                self.state = LineComment;
                return None;
            },
            BlockComment(_) => {
                self.state = BlockComment(false);
                return None;
            },
            StringSpan(s) => {
                let mut s_ = String::new();
                std::mem::swap(s, &mut s_);
                s_.push(token.0);
                self.state = StringSpan(s_);
                return None;
            },
            CharSpan(None) => {
                self.state = CharSpan(Some(token.0));
                return None;
            },
            _ => { return Some(token); },
        }
    }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush, escape, EscapeParser};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Co, CL(CharLiteral), SL(StringLiteral), Char(char)}
    use Token::*;

    /// A [`Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl Parser for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl Push<Comment> for Buffer {
        fn push(&mut self, _token: Comment) { self.0.push(Co); }
    }

    impl Push<CharLiteral> for Buffer {
        fn push(&mut self, token: CharLiteral) { self.0.push(CL(token)); }
    }

    impl Push<StringLiteral> for Buffer {
        fn push(&mut self, token: StringLiteral) { self.0.push(SL(token)); }
    }

    impl Push<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = EscapeParser::new(SpanParser::new(Buffer::default()));
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check("Hello", &[Char('H'), Char('e'), Char('l'), Char('l'), Char('o')]);
    }

    #[test]
    fn comment() {
        check("<// Comment!\n>", &[Char('<'), Co, Char('\n'), Char('>')]);
        check("<//* Comment!\n>", &[Char('<'), Co, Char('\n'), Char('>')]);
        check("<// \"Comment!\" >", &[Char('<'), Co]);
        check("</* Comment! \n*/>", &[Char('<'), Co, Char('>')]);
        check("</*// Comment!\n*/>", &[Char('<'), Co, Char('>')]);
        check("</* \"Comment!\" */>", &[Char('<'), Co, Char('>')]);
    }

    #[test]
    fn char_literal() {
        check("<'", &[Char('<'), Error(MISSING_CHAR)]);
        check("<''>", &[Char('<'), Error(MISSING_CHAR), Char('>')]);
        check("<'A>", &[Char('<'), Error(UNTERMINATED_CHAR), Char('>')]);
        check("<'A'>", &[Char('<'), CL(CharLiteral('A')), Char('>')]);
        check("<'\\x4'>", &[Char('<'), Error(escape::MISSING_HEX), Error(UNTERMINATED_CHAR)]);
        check("<'\\x41'>", &[Char('<'), CL(CharLiteral('A')), Char('>')]);
        check("<'AA>", &[Char('<'), Error(UNTERMINATED_CHAR), Char('A'), Char('>')]);
        check("<'AA'>", &[Char('<'), Error(UNTERMINATED_CHAR), Char('A'), Error(UNTERMINATED_CHAR)]);
    }

    #[test]
    fn string_literal() {
        check("<\"", &[Char('<'), Error(UNTERMINATED_STRING)]);
        check("<\"\">", &[Char('<'), SL(StringLiteral("".into())), Char('>')]);
        check("<\"A>", &[Char('<'), Error(UNTERMINATED_STRING)]);
        check("<\"A\">", &[Char('<'), SL(StringLiteral("A".into())), Char('>')]);
        check("<\"\\x4\">", &[Char('<'), Error(escape::MISSING_HEX), Error(UNTERMINATED_STRING)]);
        check("<\"\\x41\">", &[Char('<'), SL(StringLiteral("A".into())), Char('>')]);
        check("<\"AA>", &[Char('<'), Error(UNTERMINATED_STRING)]);
        check("<\"AA\">", &[Char('<'), SL(StringLiteral("AA".into())), Char('>')]);
        check("<\"A\x41\">", &[Char('<'), SL(StringLiteral("AA".into())), Char('>')]);
        check("<\"\x41A\">", &[Char('<'), SL(StringLiteral("AA".into())), Char('>')]);
        check("<\"//\">", &[Char('<'), SL(StringLiteral("//".into())), Char('>')]);
        check("<\"/*\">", &[Char('<'), SL(StringLiteral("/*".into())), Char('>')]);
    }
}

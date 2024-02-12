use super::{E, Push, Wrapper, MaybePush, NeverPush, Comment, CharLiteral, StringLiteral};

/// Represents some whitespace.
#[derive(Debug, Clone, PartialEq)]
pub struct Whitespace(pub String);

/// Represents an alphanumeric word.
#[derive(Debug, Clone, PartialEq)]
pub struct Alphanumeric(pub String);

/// Represents an operator word.
#[derive(Debug, Clone, PartialEq)]
pub struct Operator(pub String);

// ----------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum State {
    /// The initial state.
    Home,

    /// After reading whitespace characters.
    Whitespace,

    /// After reading alpha-numeric characters.
    Alphanumeric,

    /// After reading operator characters.
    Operator,
}

pub trait Output: Push<Whitespace> + Push<Alphanumeric> + Push<Operator> +
    Push<Comment> + Push<CharLiteral> + Push<StringLiteral> + Push<char> {}

impl<I: Push<Whitespace> + Push<Alphanumeric> + Push<Operator> +
    Push<Comment> + Push<CharLiteral> + Push<StringLiteral> + Push<char>
> Output for I {}

/// A [`Parser`] that recognizes comments, and character and string literals.
#[derive(Debug, Clone)]
pub struct WordParser<I: Output> {
    /// The output stream.
    inner: I,

    /// The parser state.
    state: State,

    /// Buffered characters.
    buffer: String,
}

impl<I: Output> WordParser<I> {
    /// Construct a `WordParser` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self {
        WordParser {inner, state: State::Home, buffer: String::new()}
    }
}

impl<I: Output> Wrapper for WordParser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        let mut s = String::new();
        std::mem::swap(&mut s, &mut self.buffer);
        match self.state {
            State::Home => {},
            State::Whitespace => {
                self.inner.push(Whitespace(s));
            },
            State::Alphanumeric => {
                self.inner.push(Alphanumeric(s));
            },
            State::Operator => {
                self.inner.push(Operator(s));
            },
        }
        self.state = State::Home;
    }

    fn partial_reset(&mut self) { self.partial_flush() }

    const MISSING: E = "word: Should not happen";
}

impl<I: Output> NeverPush<Comment> for WordParser<I> {}
impl<I: Output> NeverPush<CharLiteral> for WordParser<I> {}
impl<I: Output> NeverPush<StringLiteral> for WordParser<I> {}

impl<I: Output> MaybePush<char> for WordParser<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        assert_eq!(self.state == State::Home, self.buffer.len() == 0);
        let new_state = match token {
            ' ' | '\t' | '\n' | '\r' => State::Whitespace,
            '_' | '0'..='9' | 'A'..='Z' | 'a'..='z' => State::Alphanumeric,
            '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => State::Operator,
            _ => State::Home,
        };
        match (self.state == State::Home, new_state == self.state) {
            (false, false) => {
                // We cannot continue the word.
                return Some(token);
            },
            (true, true) => {
                // It's a solitary character.
                self.inner.push(token);
                return None;
            },
            _ => {
                // Begin or continue a word.
                self.state = new_state;
                self.buffer.push(token);
                return None;
            },
        }
    }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Parser, Flush, EscapeParser, span, SpanParser};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ws(String), An(String), Op(String), Co, CL(char), SL(String), Char(char)}
    use Token::*;

    /// A [`Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl Parser for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl Push<Whitespace> for Buffer {
        fn push(&mut self, token: Whitespace) { self.0.push(Ws(token.0)); }
    }

    impl Push<Alphanumeric> for Buffer {
        fn push(&mut self, token: Alphanumeric) { self.0.push(An(token.0)); }
    }

    impl Push<Operator> for Buffer {
        fn push(&mut self, token: Operator) { self.0.push(Op(token.0)); }
    }

    impl Push<Comment> for Buffer {
        fn push(&mut self, _token: Comment) { self.0.push(Co); }
    }

    impl Push<CharLiteral> for Buffer {
        fn push(&mut self, token: CharLiteral) { self.0.push(CL(token.0)); }
    }

    impl Push<StringLiteral> for Buffer {
        fn push(&mut self, token: StringLiteral) { self.0.push(SL(token.0)); }
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
        let mut parser = EscapeParser::new(SpanParser::new(WordParser::new(Buffer::default())));
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check("#(),.;[]`{}", &[
            Char('#'), Char('('), Char(')'), Char(','), Char('.'), Char(';'),
            Char('['), Char(']'), Char('`'), Char('{'), Char('}'),
        ]);
    }

    #[test]
    fn whitespace() {
        check("[ \n\r\t]", &[Char('['), Ws(" \n\r\t".into()), Char(']')]);
        check("[  ; ]", &[Char('['), Ws("  ".into()), Char(';'), Ws(" ".into()), Char(']')]);
    }

    #[test]
    fn alphanumeric() {
        check("[0123456789]", &[Char('['), An("0123456789".into()), Char(']')]);
        check("[0_9;A_Z;a_z]", &[
            Char('['), An("0_9".into()), Char(';'), An("A_Z".into()), Char(';'), An("a_z".into()), Char(']'),
        ]);
    }

    #[test]
    fn operator() {
        check("[!$%&*+-/:<=>?@^|~]", &[Char('['), Op("!$%&*+-/:<=>?@^|~".into()), Char(']')]);
        check("[+=;<>;*/]", &[
            Char('['), Op("+=".into()), Char(';'), Op("<>".into()), Char(';'), Op("*/".into()), Char(']'),
        ]);
    }

    #[test]
    fn span() {
        check("[ //\n]", &[Char('['), Ws(" ".into()), Co, Ws("\n".into()), Char(']')]);
        check("[\"A\"+'A']", &[Char('['), SL("A".into()), Op("+".into()), CL('A'), Char(']')]);
    }
}
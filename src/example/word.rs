//! Recognise whitespace, alphanumeric and punctuation words, and keywords.
//!
//! - [`Parser`] - a [`crate::Parse`] implementation that accepts:
//!   - [`char`]
//! - [`Push`] - a trait that specifies the output token types:
//!   - [`char`]
//!   - [`Whitespace`]
//!   - [`Alphanumeric`]
//!   - [`Symbolic`]
//!   - [`Keyword`]
//! - In addition, any type that implements [`Spectate`] is accepted and
//!   passed on unchanged.

use std::collections::{HashMap};
use crate::{E, Push as P, Wrap, Spectate};
use super::{escape, span, keyword};
use keyword::{Keyword};

/// Represents a string of whitespace characters.
#[derive(Debug, Clone, PartialEq)]
pub struct Whitespace(pub String);

/// Represents a string of alphanumeric characters.
#[derive(Debug, Clone, PartialEq)]
pub struct Alphanumeric(pub String);

/// Represents a string of operator characters (`!$%&*+-/:<=>?@^|~`).
#[derive(Debug, Clone, PartialEq)]
pub struct Symbolic(pub String);

// ----------------------------------------------------------------------------

/// An abbreviation for [`crate::Push<T>`] for all `T` generated by [`Parser`].
pub trait Push: P<char> + P<Whitespace> + P<Alphanumeric> + P<Symbolic> + P<Keyword> {}

impl<I: P<char> + P<Whitespace> + P<Alphanumeric> + P<Symbolic> + P<Keyword>> Push for I {}

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
    Symbolic,
}

/// A parser that recognizes [`Whitespace`]s, [`Alphanumeric`]s and
/// [`Symbolic`]s. Reserved words (alphanumeric or operators) are replaced with
/// [`Keyword`]s.
#[derive(Debug, Clone)]
pub struct Parser<I: Push> {
    /// All keywords of the language.
    keywords: HashMap<&'static str, usize>,

    /// The output stream.
    inner: I,

    /// The parser state.
    state: State,

    /// Buffered characters.
    buffer: String,
}

impl<I: Push> Parser<I> {
    /// Construct a `Parser` that feeds its output to `inner`.
    pub fn new(keywords: impl IntoIterator<Item=&'static str>, inner: I) -> Self {
        let keywords = keywords.into_iter().enumerate().map(|(index, name)| (name, index)).collect();
        Self {keywords, inner, state: State::Home, buffer: String::new()}
    }

    /// If `s` is in [`keywords`], push a [`Keyword`], otherwise a `W`.
    fn push_word<W>(&mut self, s: String, to_w: impl Fn(String) -> W) where I: P<W> {
        if let Some(&index) = self.keywords.get(s.as_str()) {
            self.inner.push(Keyword(index));
        } else {
            self.inner.push(to_w(s));
        }
    }

    fn classify(token: char) -> State {
        match token {
            ' ' | '\t' | '\n' | '\r' => State::Whitespace,
            '_' | '0'..='9' | 'A'..='Z' | 'a'..='z' => State::Alphanumeric,
            '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => State::Symbolic,
            _ => State::Home,
        }
    }
}

impl<I: Push> Wrap for Parser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        let s = std::mem::replace(&mut self.buffer, String::new());
        match self.state {
            State::Home => {},
            State::Whitespace => {
                self.inner.push(Whitespace(s));
            },
            State::Alphanumeric => {
                self.push_word(s, Alphanumeric);
            },
            State::Symbolic => {
                self.push_word(s, Symbolic);
            },
        }
        self.state = State::Home;
    }

    fn partial_reset(&mut self) { self.partial_flush() }

    const MISSING: E = "word: Should not happen";
}

impl<I: Push> crate::MaybePush<char> for Parser<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        assert_eq!(self.state == State::Home, self.buffer.len() == 0);
        let new_state = Self::classify(token);
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

impl<I: Push> Spectate<Parser<I>> for escape::Sequence {}
impl<I: Push> Spectate<Parser<I>> for span::Comment {}
impl<I: Push> Spectate<Parser<I>> for span::CharLiteral {}
impl<I: Push> Spectate<Parser<I>> for span::StringLiteral {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ws(String), An(String), Op(String), Kw(usize), Co, CL(char), SL(String), Char(char)}
    use Token::*;

    /// A parser that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl crate::Parse for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl P<Whitespace> for Buffer {
        fn push(&mut self, token: Whitespace) { self.0.push(Ws(token.0)); }
    }

    impl P<Alphanumeric> for Buffer {
        fn push(&mut self, token: Alphanumeric) { self.0.push(An(token.0)); }
    }

    impl P<Symbolic> for Buffer {
        fn push(&mut self, token: Symbolic) { self.0.push(Op(token.0)); }
    }

    impl P<Keyword> for Buffer {
        fn push(&mut self, token: Keyword) { self.0.push(Kw(token.0)); }
    }

    impl P<span::Comment> for Buffer {
        fn push(&mut self, _token: span::Comment) { self.0.push(Co); }
    }

    impl P<span::CharLiteral> for Buffer {
        fn push(&mut self, token: span::CharLiteral) { self.0.push(CL(token.0)); }
    }

    impl P<span::StringLiteral> for Buffer {
        fn push(&mut self, token: span::StringLiteral) { self.0.push(SL(token.0)); }
    }

    impl P<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = escape::Parser::new(span::Parser::new(Parser::new(["return"], Buffer::default())));
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
    fn keyword() {
        check("retur", &[An("retur".into())]);
        check("return", &[Kw(0)]);
        check("returns", &[An("returns".into())]);
    }

    #[test]
    fn span() {
        check("[ //\n]", &[Char('['), Ws(" ".into()), Co, Ws("\n".into()), Char(']')]);
        check("[\"A\"+'A']", &[Char('['), SL("A".into()), Op("+".into()), CL('A'), Char(']')]);
    }
}

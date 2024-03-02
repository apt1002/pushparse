//! Recognise whitespace, alphanumeric and punctuation words, and keywords.
//!
//! - [`Parser`] - a [`crate::Parser`] implementation that accepts:
//!   - [`char`]
//! - [`Push`] - a trait that specifies the output token types:
//!   - [`char`]
//!   - [`Whitespace`]
//!   - [`Alphanumeric`]
//!   - [`Symbolic`]
//!   - [`Keyword`]
//! - In addition, any type that implements [`Spectator`] is accepted and
//!   passed on unchanged.

use std::collections::{HashMap};
use crate::{E, Push as P};
use super::{escape, span};

/// Represents a string of whitespace characters.
#[derive(Debug, Clone, PartialEq)]
pub struct Whitespace(pub String);

/// Represents a string of alphanumeric characters.
#[derive(Debug, Clone, PartialEq)]
pub struct Alphanumeric(pub String);

/// Represents a string of operator characters (`!$%&*+-/:<=>?@^|~`).
#[derive(Debug, Clone, PartialEq)]
pub struct Symbolic(pub String);

/// Represents a keyword of the language.
///
/// Keywords must be entirely alphanumeric or entirely operator characters.
/// The `usize` is an index into the [`Iterator`] passed to [`Parser::new()`].
#[derive(Debug, Clone, PartialEq)]
pub struct Keyword(pub usize);

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

/// An abbreviation for [`crate::Push<T>`] for all `T` generated by [`Parser`].
pub trait Push: P<char> + P<Whitespace> + P<Alphanumeric> + P<Symbolic> + P<Keyword> {}

impl<I: P<char> + P<Whitespace> + P<Alphanumeric> + P<Symbolic> + P<Keyword>> Push for I {}

/// A [`Parser`] that recognizes [`Whitespace`]s, [`Alphanumeric`]s and [`Symbolic`]s.
/// Reserved words (alphanumeric or operators) are replaced with [`Keyword`]s.
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
}

impl<I: Push> crate::Wrapper for Parser<I> {
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
        let new_state = match token {
            ' ' | '\t' | '\n' | '\r' => State::Whitespace,
            '_' | '0'..='9' | 'A'..='Z' | 'a'..='z' => State::Alphanumeric,
            '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' => State::Symbolic,
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

/// A token type ignored by [`Parser`].
pub trait Spectator {}

impl<T: Spectator, I: Push + P<T>> crate::NeverPush<T> for Parser<I> {}

impl Spectator for escape::Sequence {}
impl Spectator for span::Comment {}
impl Spectator for span::CharLiteral {}
impl Spectator for span::StringLiteral {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ws(String), An(String), Op(String), Kw(usize), Co, CL(char), SL(String), Char(char)}
    use Token::*;

    /// A [`Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl crate::Parser for Buffer {
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
//! Match brackets.
//!
//! The token type that represent something inside brackets must implement
//! [`Bracket`] and provide a [`crate::Parser`] implementation for their
//! contents.
//!
//! Each [`Parser`] recognises just one kind of [`Bracket`]. To recognise
//! multiple kinds of brackets (e.g. round, square, ...) use several nested
//! [`Parser`]s.
//!
//! - [`Parser<_, B>`] - a [`crate::Parser`] implementation that accepts:
//!   - [`char`]
//! - The output token types are:
//!   - [`char`]
//!   - `B`
//! - In addition, any type that implements [`Spectator`] is accepted and
//!   passed on unchanged.

use crate::{E, Push as P, Parser as _, Flush as _};
use super::{escape, span, word};

pub const MISSING_OPEN: E = "Missing open bracket";
pub const MISSING_CLOSE: E = "Missing close bracket";

// ----------------------------------------------------------------------------

/// Represents something inside brackets.
pub trait Bracket: Sized {
    /// The opening bracket character.
    const OPEN: char;

    /// The closing bracket character.
    const CLOSE: char;

    /// The return type of [`Self::new_parser()`].
    type Parser: P<char> + P<Self> + crate::Flush;

    /// Constructs an inner [`crate::Parser`] that generates tokens of type
    /// `Self`.
    fn new_parser() -> Self::Parser;

    /// Constructs `Self`.
    fn new(contents: <Self::Parser as crate::Flush>::Output) -> Self;
}

// ----------------------------------------------------------------------------

/// A [`crate::Parser`] that generates nested brackets of type `B`.
#[derive(Debug, Clone)]
pub struct Parser<I: P<B>, B: Bracket> {
    /// The output stream for the top level, i.e. outside all `B`s.
    top_inner: I,

    /// The output streams which collect the contents of `B`s, ordered from
    /// outside to inside.
    inners: Vec<B::Parser>,
}

impl<I: P<B>, B: Bracket> Parser<I, B> {
    pub fn new(top_inner: I) -> Self {
        Self {top_inner, inners: Vec::new()}
    }

    /// Push a token to the `crate::Parser` for the innermost `B` that has been
    /// opened but not yet closed, if any, otherwise to the top level
    /// `crate::Parser`.
    pub fn inner_push<T>(&mut self, token: T) where I: P<T>, B::Parser: P<T> {
        if let Some(inner) = self.inners.last_mut() {
            inner.push(token);
        } else {
            self.top_inner.push(token);
        }
    }

    /// Open a nested `B`.
    pub fn open(&mut self) {
        self.inners.push(B::new_parser());
    }

    /// Close the innermost `B`.
    pub fn close(&mut self) {
        if let Some(mut inner) = self.inners.pop() {
            self.inner_push(B::new(inner.flush()));
        } else {
            self.top_inner.error(MISSING_OPEN);
        }
    }
}

impl<I: P<B>, B: Bracket> crate::Parser for Parser<I, B> {
    fn error(&mut self, error: E) {
        if let Some(inner) = self.inners.last_mut() {
            inner.error(error);
        } else {
            self.top_inner.error(error);
        }
    }
}

impl<I: P<char> + P<B>, B: Bracket> P<char> for Parser<I, B> {
    fn push(&mut self, token: char) {
        if token == B::OPEN {
            self.open();
        } else if token == B::CLOSE {
            self.close();
        } else {
            self.inner_push(token);
        }
    }
}

impl<T: Spectator, I: P<B>, B: Bracket> P<T> for Parser<I, B> where
    I: P<T>,
    B::Parser: P<T>
{
    fn push(&mut self, token: T) { self.inner_push(token); }
}

impl<I: P<B> + crate::Flush, B: Bracket> crate::Flush for Parser<I, B> {
    type Output = I::Output;

    fn flush(&mut self) -> Self::Output {
        while let Some(inner) = self.inners.last_mut() {
            inner.error(MISSING_CLOSE);
            self.close();
        }
        self.top_inner.flush()
    }
}

// ----------------------------------------------------------------------------

/// A token type ignored by [`Parser`].
pub trait Spectator {}

impl Spectator for escape::Sequence {}
impl Spectator for span::Comment {}
impl Spectator for span::CharLiteral {}
impl Spectator for span::StringLiteral {}
impl Spectator for word::Whitespace {}
impl Spectator for word::Alphanumeric {}
impl Spectator for word::Symbolic {}
impl<T: Bracket> Spectator for T {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ro(Box<[Token]>), Sequence(char), Char(char)}
    use Token::*;

    /// A [`crate::Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl crate::Parser for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl P<Round> for Buffer {
        fn push(&mut self, token: Round) { self.0.push(Ro(token.0)); }
    }

    impl P<escape::Sequence> for Buffer {
        fn push(&mut self, token: escape::Sequence) { self.0.push(Sequence(token.0)); }
    }

    impl P<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    /// Round brackets containing a list of [`Token`]s
    #[derive(Debug, Clone)]
    struct Round(pub Box<[Token]>);

    impl Bracket for Round {
        type Parser = Buffer;
        fn new_parser() -> Self::Parser { Self::Parser::default() }
        fn new(contents: <Self::Parser as Flush>::Output) -> Self { Round(contents) }
        const OPEN: char = '(';
        const CLOSE: char = ')';
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = escape::Parser::new(
            Parser::<_, Round>::new(Buffer::default())
        );
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check("Hello", &[Char('H'), Char('e'), Char('l'), Char('l'), Char('o')]);
    }

    #[test]
    fn nested() {
        check("[(]", &[Char('['), Ro([Char(']'), Error(MISSING_CLOSE)].into())]);
        check("[(A]", &[Char('['), Ro([Char('A'), Char(']'), Error(MISSING_CLOSE)].into())]);
        check("[(A)]", &[Char('['), Ro([Char('A')].into()), Char(']')]);
        check("[A)]", &[Char('['), Char('A'), Error(MISSING_OPEN), Char(']')]);
        check("[)]", &[Char('['), Error(MISSING_OPEN), Char(']')]);
        check("[(A(B()).C)]", &[
            Char('['), Ro([
                Char('A'), Ro([
                    Char('B'), Ro([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
    }

    #[test]
    fn unmatched() {
        check("[(A(B)).C)]", &[
            Char('['), Ro([
                Char('A'), Ro([
                    Char('B'),
                ].into()),
            ].into()), Char('.'), Char('C'), Error(MISSING_OPEN), Char(']'),
        ]);
        check("[(A(B().C)]", &[
            Char('['), Ro([
                Char('A'), Ro([
                    Char('B'), Ro([].into()), Char('.'), Char('C'),
                ].into()), Char(']'), Error(MISSING_CLOSE),
            ].into()),
        ]);
    }

    #[test]
    fn errors() {
        check("[(\\A(B()).C)]", &[
            Char('['), Ro([
                Error(escape::MISSING_SEQUENCE), Char('A'), Ro([
                    Char('B'), Ro([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
        check("[(A(\\B()).C)]", &[
            Char('['), Ro([
                Char('A'), Ro([
                    Error(escape::MISSING_SEQUENCE), Char('B'), Ro([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
        check("[(A(B()).\\C)]", &[
            Char('['), Ro([
                Char('A'), Ro([
                    Char('B'), Ro([].into()),
                ].into()), Char('.'), Error(escape::MISSING_SEQUENCE), Char('C'),
            ].into()), Char(']')
        ]);
    }
}

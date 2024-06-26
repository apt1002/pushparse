//! Match brackets.
//!
//! Each [`Parser`] recognises just one kind of bracket. To recognise
//! multiple kinds of brackets (e.g. round, square, ...) use several nested
//! [`Parser`]s.
//!
//! - [`Parser<B, _>`] - a [`Parse`] implementation that accepts:
//!   - [`char`] (to find opening and closing brackets)
//! - [`Push<B>`] - a trait that defines the syntax of bracket token `B` and
//!   specifies the output token types:
//!   - [`char`] (all others)
//!   - `B`
//! - In addition, any type that implements [`Spectator`] is accepted and
//!   passed on unchanged, including `B` itself. Such tokens form the contents
//!   of the brackets.

use crate::{E, Push as P, Parse, Flush};
use super::{escape, span, word};

pub const MISSING_OPEN: E = "Missing open bracket";
pub const MISSING_CLOSE: E = "Missing close bracket";

// ----------------------------------------------------------------------------

/// Wrap parser output as a single token.
///
/// Implies `Spectator`.
pub trait Bracket: Spectator {
    /// The opening bracket character.
    const OPEN: char;

    /// The closing bracket character.
    const CLOSE: char;
}

impl<B: Bracket> Spectator for B {}

// ----------------------------------------------------------------------------

/// Parse bracket tokens of type `B`.
///
/// Implies `crate::Push<T>` for every type `T` generated by [`Parser<B>`], and
/// Also defines the syntax and parser for `B`.
pub trait Push<B: Bracket>: P<char> + P<B> {
    /// The type of a parser for the contents of the brackets.
    type Parser: Flush<Output=B> + P<char> + P<B>;

    /// Construct a [`Self::Parser`].
    fn new_parser(&self) -> Self::Parser;
}

// ----------------------------------------------------------------------------

/// A parser that generates nested brackets of type `B`.
///
/// Each of the following traits will be implemented by `Self` if both `I` and
/// `I::Parser` provide compatible implementations of it:
/// - [`crate::Push<T>`] for any `T` that implements [`Spectator`].
/// - [`Push<T>`] for any `T` that implements [`Bracket`].
#[derive(Debug, Clone)]
pub struct Parser<B: Bracket, I: Push<B>> {
    /// The output stream for the top level, i.e. outside all `B`s.
    top_inner: I,

    /// The output streams which collect the contents of `B`s, ordered from
    /// outside to inside.
    inners: Vec<I::Parser>,
}

impl<B: Bracket, I: Push<B>> Parser<B, I> {
    pub fn new(top_inner: I) -> Self {
        Self {top_inner, inners: Vec::new()}
    }

    /// Push a token to the parser for the innermost `B` that has been opened
    /// but not yet closed, if any, otherwise to the top level parser.
    pub fn inner_push<T>(&mut self, token: T) where I: P<T>, I::Parser: P<T> {
        if let Some(inner) = self.inners.last_mut() {
            inner.push(token);
        } else {
            self.top_inner.push(token);
        }
    }

    /// Open a nested `B`.
    pub fn open(&mut self) {
        self.inners.push(self.top_inner.new_parser());
    }

    /// Close the innermost `B`.
    pub fn close(&mut self) {
        if let Some(mut inner) = self.inners.pop() {
            self.inner_push(inner.flush());
        } else {
            self.top_inner.error(MISSING_OPEN);
        }
    }
}

impl<B: Bracket, I: Push<B>> crate::Parse for Parser<B, I> {
    fn error(&mut self, error: E) {
        if let Some(inner) = self.inners.last_mut() {
            inner.error(error);
        } else {
            self.top_inner.error(error);
        }
    }
}

impl<B: Bracket, I: Push<B>> P<char> for Parser<B, I> {
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

impl<B: Bracket, I: Push<B> + Flush> Flush for Parser<B, I> {
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
impl Spectator for word::Keyword {}

impl<
    T: Spectator,
    B: Bracket,
    I: Push<B> + P<T>,
> P<T> for Parser<B, I> where
    I::Parser: P<T>,
{
    fn push(&mut self, token: T) { self.inner_push(token); }
}

impl<
    T: Bracket,
    B: Bracket,
    I: Push<B> + Push<T>,
> Push<T> for Parser<B, I> where
    <I as Push<B>>::Parser: P<T>,
    <I as Push<T>>::Parser: Push<B, Parser=<I as Push<B>>::Parser>,
{
    type Parser = Parser<B, <I as Push<T>>::Parser>;
    fn new_parser(&self) -> Self::Parser { Parser::new(<I as Push<T>>::new_parser(&self.top_inner)) }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Round(Box<[Token]>), Sequence(char), Char(char)}
    use Token::*;

    /// A parser that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl Parse for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl P<Box<[Token]>> for Buffer {
        fn push(&mut self, token: Box<[Token]>) { self.0.push(Round(token)); }
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

    impl Push<Box<[Token]>> for Buffer {
        type Parser = Self;
        fn new_parser(&self) -> Self::Parser { Self::Parser::default() }
    }

    impl Bracket for Box<[Token]> {
        const OPEN: char = '(';
        const CLOSE: char = ')';
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = escape::Parser::new(
            Parser::new(Buffer::default())
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
        check("[(]", &[Char('['), Round([Char(']'), Error(MISSING_CLOSE)].into())]);
        check("[(A]", &[Char('['), Round([Char('A'), Char(']'), Error(MISSING_CLOSE)].into())]);
        check("[(A)]", &[Char('['), Round([Char('A')].into()), Char(']')]);
        check("[A)]", &[Char('['), Char('A'), Error(MISSING_OPEN), Char(']')]);
        check("[)]", &[Char('['), Error(MISSING_OPEN), Char(']')]);
        check("[(A(B()).C)]", &[
            Char('['), Round([
                Char('A'), Round([
                    Char('B'), Round([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
    }

    #[test]
    fn unmatched() {
        check("[(A(B)).C)]", &[
            Char('['), Round([
                Char('A'), Round([
                    Char('B'),
                ].into()),
            ].into()), Char('.'), Char('C'), Error(MISSING_OPEN), Char(']'),
        ]);
        check("[(A(B().C)]", &[
            Char('['), Round([
                Char('A'), Round([
                    Char('B'), Round([].into()), Char('.'), Char('C'),
                ].into()), Char(']'), Error(MISSING_CLOSE),
            ].into()),
        ]);
    }

    #[test]
    fn errors() {
        check("[(\\A(B()).C)]", &[
            Char('['), Round([
                Error(escape::MISSING_SEQUENCE), Char('A'), Round([
                    Char('B'), Round([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
        check("[(A(\\B()).C)]", &[
            Char('['), Round([
                Char('A'), Round([
                    Error(escape::MISSING_SEQUENCE), Char('B'), Round([].into()),
                ].into()), Char('.'), Char('C'),
            ].into()), Char(']')
        ]);
        check("[(A(B()).\\C)]", &[
            Char('['), Round([
                Char('A'), Round([
                    Char('B'), Round([].into()),
                ].into()), Char('.'), Error(escape::MISSING_SEQUENCE), Char('C'),
            ].into()), Char(']')
        ]);
    }
}

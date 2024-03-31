//! Recognise fragments of mathematical espressions.
//!
//! - [`Parser`] - a [`crate::Parse`] implementation that accepts:
//!   - [`char`]
//!   - [`Alphanumeric`]
//! - [`Push`] - a trait that specifies the output token types:
//!   - [`char`]
//!   - [`Alphanumeric`]
//!   - [`Field`]
//!   - [`Dots`]
//! - In addition, any type that implements [`Spectate`] is accepted and
//!   passed on unchanged.

use crate::{E, Push as P, Spectate};
use super::{escape, span, keyword, word, bracket};
use word::{Alphanumeric};
use bracket::{Bracket};

/// Represents a field projection operator: the `.field` part of `x.field`.
pub struct Field(pub String);

/// Represents two or more `.`s.
pub struct Dots(pub usize);

// TODO: Function types and function literals.

// ----------------------------------------------------------------------------

pub trait Push: P<char> + P<Alphanumeric> + P<Field> + P<Dots> {}

impl<I: P<char> + P<Alphanumeric> + P<Field> + P<Dots>> Push for I {}

// ----------------------------------------------------------------------------

/// A parser that recognises [`Field`]s.
#[derive(Debug, Clone)]
pub struct Parser<I: Push> {
    /// The output stream.
    inner: I,

    /// The number of `'.'` characters in a row.
    num_dots: usize,
}

impl<I: Push> Parser<I> {
    pub fn new(inner: I) -> Self { Self {inner, num_dots: 0} }
}

impl<I: Push> crate::Wrap for Parser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        match self.num_dots {
            0 => {},
            1 => { self.inner.push('.'); }
            n => { self.inner.push(Dots(n)); }
        }
        self.num_dots = 0;
    }

    fn partial_reset(&mut self) { self.partial_flush(); }

    const MISSING: E = "atom: Should not happen";
}

impl<I: Push> crate::MaybePush<char> for Parser<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        if token == '.' {
            self.num_dots += 1;
            return None;
        }
        if self.num_dots > 0 {
            return Some(token);
        }
        self.inner.push(token);
        return None;
    }
}

impl<I: Push> crate::MaybePush<Alphanumeric> for Parser<I> {
    fn maybe_push(&mut self, token: Alphanumeric) -> Option<Alphanumeric> {
        match self.num_dots {
            0 => { self.inner.push(token); },
            1 => { self.inner.push(Field(token.0)); self.num_dots = 0; },
            _ => { return Some(token); },
        }
        return None;
    }
}

impl<I: Push + keyword::Push> keyword::NoExtra for Parser<I> {}

// ----------------------------------------------------------------------------

impl<
    I: Push + bracket::Push<B>,
    B: Bracket + Spectate<Parser<I>> + Spectate<Parser<I::Parser>>,
> bracket::Push<B> for Parser<I> where
    I::Parser: Push,
{
    type Parser = Parser<I::Parser>;
    fn new_parser(&self) -> Self::Parser { Parser::new(self.inner.new_parser()) }
}

impl<I: Push> Spectate<Parser<I>> for escape::Sequence {}
impl<I: Push> Spectate<Parser<I>> for span::Comment {}
impl<I: Push> Spectate<Parser<I>> for span::CharLiteral {}
impl<I: Push> Spectate<Parser<I>> for span::StringLiteral {}
impl<I: Push> Spectate<Parser<I>> for word::Whitespace {}
impl<I: Push> Spectate<Parser<I>> for word::Symbolic {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ws, An(String), Sy, Kw, F(String), D(usize), Char(char)}
    use Token::*;

    /// A parser that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl crate::Parse for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl P<word::Whitespace> for Buffer {
        fn push(&mut self, _: word::Whitespace) { self.0.push(Ws); }
    }

    impl P<Alphanumeric> for Buffer {
        fn push(&mut self, token: Alphanumeric) { self.0.push(An(token.0)); }
    }

    impl P<word::Symbolic> for Buffer {
        fn push(&mut self, _: word::Symbolic) { self.0.push(Sy); }
    }

    impl P<keyword::Keyword> for Buffer {
        fn push(&mut self, _: keyword::Keyword) { self.0.push(Kw); }
    }

    impl P<Field> for Buffer {
        fn push(&mut self, token: Field) { self.0.push(F(token.0)); }
    }

    impl P<Dots> for Buffer {
        fn push(&mut self, token: Dots) { self.0.push(D(token.0)); }
    }

    impl P<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    impl keyword::List for Buffer {
        const NUM_KEYWORDS: usize = 1;
        fn name(_: keyword::Keyword) -> &'static str { &"return" }
    }

    impl keyword::Push for Buffer {
        type List = Self;
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = word::Parser::new(Parser::new(Buffer::default()));
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check(",.;", &[Char(','), Char('.'), Char(';')]);
    }

    #[test]
    fn words() {
        check("return a: Int;", &[Kw, Ws, An("a".into()), Sy, Ws, An("Int".into()), Char(';')]);
    }

    #[test]
    fn dots() {
        check("", &[]);
        check(".", &[Char('.')]);
        check("..", &[D(2)]);
        check("...", &[D(3)]);
        check(". .", &[Char('.'), Ws, Char('.')]);
        check(".. .", &[D(2), Ws, Char('.')]);
        check(". ..", &[Char('.'), Ws, D(2)]);
        check(".. ..", &[D(2), Ws, D(2)]);
    }

    #[test]
    fn field() {
        check("A", &[An("A".into())]);
        check(".A", &[F("A".into())]);
        check(". A", &[Char('.'), Ws, An("A".into())]);
        check("..A", &[D(2), An("A".into())]);
        check(". .A", &[Char('.'), Ws, F("A".into())]);
        check(".. A", &[D(2), Ws, An("A".into())]);
        check("A.B", &[An("A".into()), F("B".into())]);
        check("A .B", &[An("A".into()), Ws, F("B".into())]);
        check("A. B", &[An("A".into()), Char('.'), Ws, An("B".into())]);
        check("A..B", &[An("A".into()), D(2), An("B".into())]);
    }
}

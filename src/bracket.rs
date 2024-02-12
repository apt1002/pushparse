use super::{escape, span, word, Parser, E, Push, Flush};

pub const MISSING_OPEN: E = "Missing open bracket";
pub const MISSING_CLOSE: E = "Missing close bracket";

// ----------------------------------------------------------------------------

/// Represents something inside brackets.
pub trait Nested: Sized {
    /// The opening bracket character.
    const OPEN: char;

    /// The closing bracket character.
    const CLOSE: char;

    /// The return type of [`new_parser()`].
    type Parser: Push<char> + Push<Self> + Flush;

    /// Constructs an inner [`Parser`] that generates tokens of type `Self`.
    fn new_parser() -> Self::Parser;

    /// Constructs `Self`.
    fn new(contents: <Self::Parser as Flush>::Output) -> Self;
}

// ----------------------------------------------------------------------------

/// A token that represents a bracket whose contents are parsed by `P`.
///
/// You might prefer to define your own type and implement [`Nested`] for it.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Bracket<P: Flush, const OPEN: char, const CLOSE: char>(P::Output);

impl<
    P: Default + Push<char> + Push<Self> + Flush,
    const OPEN: char,
    const CLOSE: char,
> Nested for Bracket<P, OPEN, CLOSE> {
    type Parser = P;
    fn new_parser() -> P { P::default() }
    fn new(contents: P::Output) -> Self { Bracket(contents) }
    const OPEN: char = OPEN;
    const CLOSE: char = CLOSE;
}

// ----------------------------------------------------------------------------

/// A token type ignored by [`BracketParser`].
pub trait Spectator {}

impl Spectator for escape::EscapeSequence {}
impl Spectator for span::Comment {}
impl Spectator for span::CharLiteral {}
impl Spectator for span::StringLiteral {}
impl Spectator for word::Whitespace {}
impl Spectator for word::Alphanumeric {}
impl Spectator for word::Operator {}
impl<T: Nested> Spectator for T {}

// ----------------------------------------------------------------------------

/// A [`Parser`] that generates nested brackets of type `B`.
///
/// The `BracketParser` will ignore (i.e. pass on) tokens of any type that
/// implements [`Spectator`].
#[derive(Debug, Clone)]
pub struct BracketParser<I: Push<B>, B: Nested> {
    /// The output stream for the top level, i.e. outside all `B`s.
    top_inner: I,

    /// The output streams which collect the contents of `B`s, ordered from
    /// outside to inside.
    inners: Vec<B::Parser>,
}

impl<I: Push<B>, B: Nested> BracketParser<I, B> {
    pub fn new(top_inner: I) -> Self {
        BracketParser {top_inner, inners: Vec::new()}
    }

    /// Push a token to the `Parser` for the innermost `Bracket` that has been
    /// opened but not yet closed, if any, otherwise to the top level `Parser`.
    pub fn inner_push<T>(&mut self, token: T) where I: Push<T>, B::Parser: Push<T> {
        if let Some(inner) = self.inners.last_mut() {
            inner.push(token);
        } else {
            self.top_inner.push(token);
        }
    }

    /// Open a nested bracket.
    pub fn open(&mut self) {
        self.inners.push(B::new_parser());
    }

    /// Close the innermost bracket.
    pub fn close(&mut self) {
        if let Some(mut inner) = self.inners.pop() {
            self.inner_push(B::new(inner.flush()));
        } else {
            self.top_inner.error(MISSING_OPEN);
        }
    }
}

impl<I: Push<B>, B: Nested> Parser for BracketParser<I, B> {
    fn error(&mut self, error: E) {
        if let Some(inner) = self.inners.last_mut() {
            inner.error(error);
        } else {
            self.top_inner.error(error);
        }
    }
}

impl<I: Push<char> + Push<B>, B: Nested> Push<char> for BracketParser<I, B> {
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

impl<T: Spectator, I: Push<B>, B: Nested> Push<T> for BracketParser<I, B> where
    I: Push<T>,
    B::Parser: Push<T>
{
    fn push(&mut self, token: T) { self.inner_push(token); }
}

impl<I: Push<B> + Flush, B: Nested> Flush for BracketParser<I, B> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ro(Box<[Token]>), Escape(char), Char(char)}
    use Token::*;

    /// A [`Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl Parser for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl Push<Round> for Buffer {
        fn push(&mut self, token: Round) { self.0.push(Ro(token.0)); }
    }

    impl Push<escape::EscapeSequence> for Buffer {
        fn push(&mut self, token: escape::EscapeSequence) { self.0.push(Escape(token.0)); }
    }

    impl Push<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    /// Round brackets containing a list of [`Token`]s
    #[derive(Debug, Clone)]
    struct Round(pub Box<[Token]>);

    impl Nested for Round {
        type Parser = Buffer;
        fn new_parser() -> Self::Parser { Self::Parser::default() }
        fn new(contents: <Self::Parser as Flush>::Output) -> Self { Round(contents) }
        const OPEN: char = '(';
        const CLOSE: char = ')';
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = escape::EscapeParser::new(
            BracketParser::<_, Round>::new(Buffer::default())
        );
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }
}

use std::fmt::{Debug};

/// Parse tokens.
pub trait Token: Debug + Clone {}

// ----------------------------------------------------------------------------

/// Parsers that can accept tokens of type `T`.
///
/// One parser may implement `Push<T>` for several types `T`.
pub trait Push<T: Token> {
    /// Feed `token` to this parser, and perform any resulting actions.
    fn push(&mut self, token: T);
}

// ----------------------------------------------------------------------------

/// Parsers that produce an output at the end of the input.
pub trait Finish {
    type Output;

    /// Feed "end of file" to this parser, and retrieve the output.
    fn finish(self) -> Self::Output;
}

// ----------------------------------------------------------------------------

/// Parsers that feed their output to another parser.
pub trait Wrap {
    type Inner;

    /// Returns the wrapped parser.
    fn inner(&mut self) -> &mut Self::Inner;
}

// ----------------------------------------------------------------------------

/// A trivial parser that merely collects tokens.
#[derive(Default, Debug, Clone)]
pub struct Buffer<T> {
    tokens: Vec<T>,
}

impl<T: Token> Push<T> for Buffer<T> {
    fn push(&mut self, token: T) { self.tokens.push(token); }
}

impl<T: Token> Finish for Buffer<T> {
    type Output = Box<[T]>;

    fn finish(self) -> Self::Output { self.tokens.into() }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
}

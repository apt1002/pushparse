use std::fmt::{Debug};

mod utf8;
pub use utf8::{Decoder};

type E = &'static str;

pub trait Parser {
    /// Insert an error token into the output, and reset this `Parser` to its
    /// initial state.
    fn push_error(&mut self, error: E);
}

// ----------------------------------------------------------------------------

/// [`Parser`]s that can accept tokens of type `T`.
///
/// One `Parser` may implement `Push<T>` for several types `T`.
pub trait Push<T>: Parser {
    /// Feed `token` to this `Parser`, and perform any resulting actions.
    fn push(&mut self, token: T);
}

// ----------------------------------------------------------------------------

/// [`Parser`]s that produce an output at the end of the input.
pub trait Finish {
    type Output;

    /// Feed "end of file" to this `Parser`, and retrieve the output.
    fn finish(self) -> Self::Output;
}

// ----------------------------------------------------------------------------

/// A trivial [`Parser`] that merely collects tokens.
#[derive(Default, Debug, Clone)]
pub struct Buffer<T> {
    tokens: Vec<Result<T, E>>,
}

impl<T> Parser for Buffer<T> {
    fn push_error(&mut self, error: E) { self.tokens.push(Err(error)); }
}

impl<T> Push<T> for Buffer<T> {
    fn push(&mut self, token: T) { self.tokens.push(Ok(token)); }
}

impl<T> Finish for Buffer<T> {
    type Output = Box<[Result<T, E>]>;

    fn finish(self) -> Self::Output { self.tokens.into() }
}

// ----------------------------------------------------------------------------

/// [`Parser`]s that feed their output to another `Parser`.
pub trait Wrap {
    type Inner: Parser;

    /// Returns the wrapped `Parser`.
    fn inner(&mut self) -> &mut Self::Inner;
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
}

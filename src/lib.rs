use std::fmt::{Debug};

pub mod example;

/// The type of parse errors.
pub type E = &'static str;

/// The push parser protocol.
///
/// To be useful, a type that implements `Parse` should also implement
/// [`Push`] for at least one type of input token (other than [`E`]).
///
/// A `Parse` for which the concept of "end of file" makes sense should also
/// implement [`Flush`].
pub trait Parse {
    /// Process `error`, possibly discarding some of the buffered input.
    fn error(&mut self, error: E);
}

// ----------------------------------------------------------------------------

/// Produce an output at the end of the input.
pub trait Flush: Parse {
    /// A completely parsed file (which may contain errors).
    type Output;

    /// Feed "end of file" to this parse, forcing it to process any buffered
    /// input, and retrieve the [`Self::Output`].
    ///
    /// `Self` is reset to its initial state, and can be used again.
    fn flush(&mut self) -> Self::Output;
}

// ----------------------------------------------------------------------------

/// Parsers that can accept tokens of type `T`.
///
/// One parse may implement `Push<T>` for several types `T`.
pub trait Push<T>: Parse {
    /// Feed `token` to this parser, and perform any resulting actions.
    fn push(&mut self, token: T);
}

// ----------------------------------------------------------------------------

/// A trivial parser that merely collects tokens.
#[derive(Default, Debug, Clone)]
pub struct Buffer<T> {
    tokens: Vec<Result<T, E>>,
}

impl<T> Parse for Buffer<T> {
    fn error(&mut self, error: E) { self.tokens.push(Err(error)); }
}

impl<T> Push<T> for Buffer<T> {
    fn push(&mut self, token: T) { self.tokens.push(Ok(token)); }
}

impl<T> Flush for Buffer<T> {
    type Output = Box<[Result<T, E>]>;

    fn flush(&mut self) -> Self::Output { self.tokens.drain(..).collect() }
}

// ----------------------------------------------------------------------------

/// Parsers that feed their output to another parser.
///
/// Parsers that implement `Wrap`s get a blanket implementations of several
/// traits:
/// - [`Parse`] itself.
/// - [`Flush`], if [`Self::Inner`] implements `Flush`.
/// - [`Push`] if `Self` implements [`MaybePush`].
pub trait Wrap: Parse {
    /// The parser that this wrapper feeds.
    type Inner: Parse;

    /// Returns the wrapped parser.
    fn inner(&mut self) -> &mut Self::Inner;

    /// Process any input buffered by this wrapper and return it to its
    /// initial state, but do not flush `self.inner()`.
    fn partial_flush(&mut self);

    /// Discard any input buffered by this wrapper, and return it to its
    /// initial state. `self.inner()` is unaffected.
    fn partial_reset(&mut self);

    /// An error message to use when this parser is in its initial state and
    /// receives an input token that it cannot accept.
    ///
    /// Typically the message will be "Syntax error: missing {noun}".
    const MISSING: E;
}

impl<P: Wrap> Parse for P {
    fn error(&mut self, error: E) {
        self.partial_reset();
        self.inner().error(error);
    }
}

impl<P: Wrap> Flush for P where P::Inner: Flush {
    type Output = <P::Inner as Flush>::Output;

    fn flush(&mut self) -> Self::Output {
        self.partial_flush();
        self.inner().flush()
    }
}

// ----------------------------------------------------------------------------

/// A simpler way to implement [`Push<T>`] for a wrapper.
///
/// The implementation of `push(token)` is equivalent to the following:
/// ```text
/// if let Some(token) = self.maybe_push(token) {
///     self.partial_flush();
///     if let Some(token) = self.maybe_push(token) {
///         self.error(Self::MISSING);
///     }
/// }
/// ```
pub trait MaybePush<T>: Wrap {
    /// Try to feed `token` to this parser.
    ///
    /// If `token` is accepted in the current state, perform any resulting
    /// actions and return `None`, otherwise return `Some(token)`.
    fn maybe_push(&mut self, token: T) -> Option<T>;
}

impl<T, P: Wrap + MaybePush<T>> Push<T> for P {
    fn push(&mut self, token: T) {
        if let Some(token) = self.maybe_push(token) {
            self.partial_flush();
            if let Some(_) = self.maybe_push(token) {
                self.error(Self::MISSING);
            }
        }
    }
}

// ----------------------------------------------------------------------------

/// A trivial way to implement [`Push<T>`] for a wrapper.
pub trait NeverPush<T>: Wrap {}

// Ideally we'd implement `Push<T>` directly but Rust won't let us.
// Instead we implement `MaybePush<T>` to achieve the desired effect.
impl<T, P: Wrap + NeverPush<T>> MaybePush<T> for P where P::Inner: Push<T> {
    fn maybe_push(&mut self, token: T) -> Option<T> {
        self.partial_flush();
        self.inner().push(token);
        return None;
    }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
}

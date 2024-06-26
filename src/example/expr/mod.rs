//! Recognise mathematical espressions.
//!
//! - [`Parser`] - a [`crate::Parse`] implementation that accepts:
//!   - [`Keyword`]
//!   - [`Alphanumeric`]
//!   - [`Round`]
//! - [`Push`] - a trait that specified the output token types:
//!   - [`Keyword`]
//!   - [`Expr`]
//! - In addition, any type that implements [`Spectate`] is accepted and
//!   passed on unchanged.

use crate::{E, Push as P, Wrap, MaybePush, Spectate};
use super::{escape, span, word, bracket, precedence};
use word::{Keyword};
use precedence::{Precedence};

pub mod round;
pub use round::{Round};

pub mod atom;

pub fn all_keywords() -> impl Iterator<Item=Keyword> {
    atom::ALL_INFOS.iter().map(|info| Keyword(info))
}

// ----------------------------------------------------------------------------

/// An arithmetic operator.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Cast,
    Exclusive, Inclusive,
    BoolOr, BoolAnd, BoolNot,
    BitOr, BitXor, BitAnd, BitNot,
    EQ, NE,
    LT, GT, LE, GE, LG,
    SL, ASR, LSR,
    Add, Sub,
    Mul, Div, Rem,
    Pow,
    Plus, Minus,
    Query,
}

// ----------------------------------------------------------------------------

/// Represents an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// An identifier or literal number value.
    Name(String),

    /// A literal string value.
    String(String),

    /// A literal character value.
    Char(char),

    /// Comma-separated [`Expr`]s in round brackets.
    Round(Round),

    /// A keyword operator applied to zero, one or two operands.
    Op(Option<Box<Expr>>, Op, Option<Box<Expr>>),

    /// Field access.
    Field(Option<Box<Expr>>, String),

    /// Function or macro call.
    Call(Option<Box<Expr>>, Round),
}

/// Represents an [`Expr`] that is missing a right operand.
#[derive(Debug, Clone)]
pub struct Waiting {
    left: Option<Box<Expr>>,
    op: Op,
    right: Precedence,
}

impl precedence::Expr for Expr {
    type Waiting = Waiting;
}

impl precedence::Waiting<Expr> for Waiting {
    fn right(&self) -> Precedence { self.right }

    /// Supply the operand that `self` was waiting for.
    fn apply(self, right: Option<Expr>) -> Expr {
        Expr::Op(self.left, self.op, right.map(Box::new))
    }
}

// ----------------------------------------------------------------------------

/// An abbreviation for `crate::Push<T>` for all `T` generated by [`Parser`].
pub trait Push: P<Expr> + P<char> + P<Keyword> {}

impl<T: P<Expr> + P<char> + P<Keyword>> Push for T {}

// ----------------------------------------------------------------------------

/// A parser that recognises [`Expr`]s.
pub struct Parser<I: Push> (atom::Parser<I>);

impl<I: Push> Parser<I> {
    /// Construct a `Parser` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { Self(atom::Parser::new(precedence::Parser::new(inner))) }
}

impl<I: Push> Wrap for Parser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { self.0.inner().inner() }

    fn partial_flush(&mut self) {
        self.0.partial_flush();
        self.0.inner().partial_flush();
    }

    fn partial_reset(&mut self) {
        self.0.partial_reset();
        self.0.inner().partial_reset();
    }

    const MISSING: E = "Syntax error: missing value or operator";
}

impl<I: Push> bracket::Push<Round> for Parser<I> {
    type Parser = Parser<round::Buffer>;
    fn new_parser(&self) -> Self::Parser { Parser::new(Default::default()) }
}

// ----------------------------------------------------------------------------

macro_rules! pass_to_inner {
    ($t: ty) => {
        impl<I: Push> MaybePush<$t> for Parser<I> {
            fn maybe_push(&mut self, token: $t) -> Option<$t> { self.0.maybe_push(token) }
        }
    }
}

pass_to_inner!(Keyword);
pass_to_inner!(word::Alphanumeric);
pass_to_inner!(Round);
// TODO: Etc.

// ----------------------------------------------------------------------------

impl<I: Push> Spectate<Parser<I>> for char {}
impl<I: Push> Spectate<Parser<I>> for escape::Sequence {}
impl<I: Push> Spectate<Parser<I>> for span::Comment {}
impl<I: Push> Spectate<Parser<I>> for span::CharLiteral {}
impl<I: Push> Spectate<Parser<I>> for span::StringLiteral {}
impl<I: Push> Spectate<Parser<I>> for word::Symbolic {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
}

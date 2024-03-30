//! A precedence parser for mathematical expressions.
//!
//! - [`Parser<X>`] - a [`crate::Parse`] implementation that accepts:
//!   - [`Atom`]
//!   - [`Prefix`]
//!   - [`Postfix`]
//!   - [`Infix`]
//! - The output token types are:
//!   - `X`
//! - In addition, any type that implements [`Spectator`] is accepted and
//!   passed on unchanged.

use std::fmt::{Debug};
use crate::{E, Push as P, Parse, Flush};
use super::{escape, span, keyword, word, bracket, atom};
use bracket::{Bracket};

// ----------------------------------------------------------------------------

/// Represents the binding precedence of an operator.
/// No left-`Precedence` is ever equal to a right-`Precedence`.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(u8);

/// Represents an expression of type `X` that is missing a right operand.
pub trait Waiting<X>: Debug + Clone {
    /// The [`Precedence`] with which `self` binds to its right operand.
    fn right(&self) -> Precedence;

    /// Supply the right operand of `self`, or `None` if it is missing.
    fn apply(self, right: Option<X>) -> X;
}

pub trait Expr: Debug + Sized {
    /// The type of a `Self` that is missing a right operand.
    type Waiting: Waiting<Self>;
}

/// A token that is an `X` on its own.
pub struct Atom<X: Expr>(pub X);

/// A token that makes an `X` if followed by an `X`.
pub struct Prefix<X: Expr>(pub X::Waiting);

/// A token that makes an `X` if preceded by an `X`.
///
/// `F` must implement `FnOnce(Option<X>) -> X` for some `X` that
/// implements [`Expr`].
pub struct Postfix<F> {
    /// The [`Precedence`] with which `self` binds to its left operand.
    pub left: Precedence,

    /// Supply the left operand of `self`, or `None` if it is missing.
    pub apply: F,
}

/// A token that makes an `X` if preceded and followed by an `X`.
///
/// `F` must implement `FnOnce(Option<X>) -> X::Waiting` for some `X` that
/// implements [`Expr`].
pub struct Infix<F> {
    /// The [`Precedence`] with which `self` binds to its left operand.
    pub left: Precedence,

    /// Supply the left operand of `self`, or `None` if it is missing.
    pub apply: F,
}

// ----------------------------------------------------------------------------

/// A parser that resolves the binding of operators.
#[derive(Debug, Clone)]
pub struct Parser<X: Expr, I: P<X>> {
    /// The output stream.
    inner: I,

    /// A stack of partial `X`s that are waiting for a right operand.
    stack: Vec<X::Waiting>,

    /// An `X` that is waiting for infix and postfix operators.
    expr: Option<X>,
}

impl<X: Expr, I: P<X>> Parser<X, I> {
    pub fn new(inner: I) -> Self {
        Self {inner, stack: Vec::new(), expr: None}
    }

    /// Returns `true` if the last token was the end of an expression.
    pub fn has_expr(&self) -> bool { self.expr.is_some() }

    /// If the last [`X::Waiting`] exists and has a right [`Precedence`] larger
    /// than `left`, returns it.
    fn pop_if_higher(&mut self, left: Precedence) -> Option<X::Waiting> {
        if let Some(waiting) = self.stack.last() {
            if waiting.right() > left { return self.stack.pop(); }
        }
        None
    }

    /// If any [`X::Waiting`]s on [`self.stack`] have a higher [`Precedence`]
    /// than `left`, apply them to `expr`.
    fn flush_up_to(&mut self, left: Precedence) -> Option<X> {
        let mut expr = self.expr.take();
        while let Some(waiting) = self.pop_if_higher(left) {
            expr = Some(waiting.apply(expr));
        }
        expr
    }

    fn partial_flush(&mut self) {
        if let Some(expr) = self.flush_up_to(Precedence(u8::MAX)) {
            self.inner.push(expr);
        }
    }

    pub fn spectate<T>(&mut self, token: T) where I: P<T> {
        self.partial_flush();
        self.inner.push(token);
    }
}

// TODO: Implement `Wrap` instead.
impl<X: Expr, I: P<X>> Parse for Parser<X, I> {
    fn error(&mut self, error: E) {
        self.partial_flush();
        self.inner.error(error);
    }
}

impl<X: Expr, I: P<X> + Flush> Flush for Parser<X, I> {
    type Output = I::Output;
    fn flush(&mut self) -> Self::Output {
        self.partial_flush();
        self.inner.flush()
    }
}

impl<X: Expr, I: P<X>> P<Atom<X>> for Parser<X, I> {
    fn push(&mut self, token: Atom<X>) {
        if self.has_expr() { self.partial_flush(); }
        self.expr = Some(token.0);
    }
    
}

impl<X: Expr, I: P<X>> P<Prefix<X>> for Parser<X, I> {
    fn push(&mut self, token: Prefix<X>) {
        if self.has_expr() { self.partial_flush(); }
        self.stack.push(token.0);
    }
    
}

impl<
    X: Expr,
    I: P<X>,
    F: FnOnce(Option<X>) -> X,
> P<Postfix<F>> for Parser<X, I> {
    fn push(&mut self, token: Postfix<F>) {
        let expr = self.flush_up_to(token.left);
        self.expr = Some((token.apply)(expr));
    }
}

impl<
    X: Expr,
    I: P<X>,
    F: FnOnce(Option<X>) -> X::Waiting,
> P<Infix<F>> for Parser<X, I> {
    fn push(&mut self, token: Infix<F>) {
        let expr = self.flush_up_to(token.left);
        self.stack.push((token.apply)(expr));
    }
}

impl<
    B: Bracket,
    X: Expr,
    I: P<X> + bracket::Push<B>,
> bracket::Push<B> for Parser<X, I> where
    <I as bracket::Push<B>>::Parser: P<X>,
{
    type Parser = Parser<X, I::Parser>;
    fn new_parser(&self) -> Self::Parser { Parser::new(self.inner.new_parser()) }
}

// ----------------------------------------------------------------------------

/// A token type ignored by [`Parser`].
// TODO: Replace with `crate::Spectate`.
pub trait Spectator {}

impl Spectator for char {}
impl Spectator for escape::Sequence {}
impl Spectator for span::Comment {}
impl Spectator for span::CharLiteral {}
impl Spectator for span::StringLiteral {}
impl Spectator for keyword::Keyword {}
impl Spectator for word::Alphanumeric {}
impl Spectator for word::Symbolic {}
impl Spectator for atom::Field {}
impl Spectator for atom::Dots {}
impl<B: Bracket> Spectator for B {}

impl<T: Spectator, X: Expr, I: P<X> + P<T>> P<T> for Parser<X, I> {
    fn push(&mut self, token: T) { self.spectate(token); }
}

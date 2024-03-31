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
use crate::{E, Push as P, Wrap, MaybePush, Spectate};
use super::{escape, span, keyword, word, bracket};
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
}

impl<X: Expr, I: P<X>> Wrap for Parser<X, I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        if let Some(expr) = self.flush_up_to(Precedence(u8::MAX)) {
            self.inner.push(expr);
        }
    }

    fn partial_reset(&mut self) { self.partial_flush(); }

    const MISSING: E = "Precedence: should not happen";
}

impl<X: Expr, I: P<X>> MaybePush<Atom<X>> for Parser<X, I> {
    fn maybe_push(&mut self, token: Atom<X>) -> Option<Atom<X>> {
        if self.has_expr() { self.partial_flush(); }
        self.expr = Some(token.0);
        None
    }
    
}

impl<X: Expr, I: P<X>> MaybePush<Prefix<X>> for Parser<X, I> {
    fn maybe_push(&mut self, token: Prefix<X>) -> Option<Prefix<X>> {
        if self.has_expr() { self.partial_flush(); }
        self.stack.push(token.0);
        None
    }
    
}

impl<
    X: Expr,
    I: P<X>,
    F: FnOnce(Option<X>) -> X,
> MaybePush<Postfix<F>> for Parser<X, I> {
    fn maybe_push(&mut self, token: Postfix<F>) -> Option<Postfix<F>> {
        let expr = self.flush_up_to(token.left);
        self.expr = Some((token.apply)(expr));
        None
    }
}

impl<
    X: Expr,
    I: P<X>,
    F: FnOnce(Option<X>) -> X::Waiting,
> MaybePush<Infix<F>> for Parser<X, I> {
    fn maybe_push(&mut self, token: Infix<F>) -> Option<Infix<F>> {
        let expr = self.flush_up_to(token.left);
        self.stack.push((token.apply)(expr));
        None
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

impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for char {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for escape::Sequence {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for span::Comment {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for span::CharLiteral {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for span::StringLiteral {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for keyword::Keyword {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for word::Alphanumeric {}
impl<X: Expr, I: P<X>> Spectate<Parser<X, I>> for word::Symbolic {}
impl<B: Bracket, X: Expr, I: P<X>> Spectate<Parser<X, I>> for B {}

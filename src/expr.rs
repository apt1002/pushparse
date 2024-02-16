use super::{span, word, E, Parser, Push, Flush};
use word::{Keyword, Alphanumeric};

pub const MISSING_EXPR: E = "Missing expression";

/// Represents an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A syntax error.
    Error(E),

    /// An identifier or literal number value.
    Name(String),

    /// A literal string value.
    String(String),

    /// A literal character value.
    Char(char),

    /// Comma-separated [`Expr`]s in round brackets.
    Round(Box<[Expr]>),

    /// A keyword operator applied to zero, one or two operands.
    Op(Option<Box<Expr>>, Keyword, Option<Box<Expr>>),

    /// Field access.
    Field(Box<Expr>, Alphanumeric),

    /// Function or macro call.
    Call(Box<Expr>, Box<[Expr]>),
}

// ----------------------------------------------------------------------------

/// A type that is never constructed but that Rust wants to know anyway.
type DUMMY = &'static dyn Fn(Box<Expr>) -> MaybeExpr;

/// A token type ignored by [`ExprParser`].
pub trait Spectator: Sized {}

impl Spectator for span::Comment {}
impl Spectator for char {}

impl<T: Spectator> Part for T {
    fn with_left(self) -> Result<(Precedence, DUMMY), Self> { Err(self) }
    fn without_left(self) -> Result<MaybeExpr, Self> { Err(self) }
    type Alternative = Self;
    fn alternative(self) -> Self::Alternative { self }
}

// ----------------------------------------------------------------------------

/* TODO: impl `Part` for `Keyword` using this:
    ('right', ['?', ':']),
    ('left', ['||']),
    ('left', ['&&']),
    ('left', ['|']),
    ('left', ['^']),
    ('left', ['&']),
    ('left', ['==', '!=']),
    ('left', ['<', '>', '<=', '>=', '<>']),
    ('right', ['<<', '>>', '>>>']),
    ('left', ['+', '-']),
    ('left', ['*', '/', '%']),
    ('right', ['**']),
*/

/// Represents the binding precedence of an operator.
/// No left-`Precedence` is ever equal to a right-`Precedence`.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence(u8);

/// Represents an [`Expr`] that is missing a right operand.
#[derive(Debug, Clone)]
struct Waiting {left: Option<Box<Expr>>, op: Keyword, right: Precedence}

impl Waiting {
    /// Supply the operand that `self` was waiting for.
    fn apply(self, right: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Op(self.left, self.op, Some(right)))
    }
}

/// Either a complete [`Expr`] or one [`Waiting`] for its right operand.
enum MaybeExpr {Complete(Box<Expr>), Incomplete(Waiting)}
use MaybeExpr::*;

/// Implemented by tokens that can form part of an [`Expr`].
trait Part: Sized {
    /// If `self` is an infix or postfix operator, return its binding strength.
    fn with_left(self) -> Result<(
        Precedence,
        impl Fn(Box<Expr>) -> MaybeExpr,
    ), Self>;

    /// If `self` is a prefix or nonfix operator, return its binding strength.
    fn without_left(self) -> Result<MaybeExpr, Self>;

    /// The return type of [`alternative()`].
    type Alternative;

    /// If [`with_left()`] and [`without_left()`] both return [`Err`], 
    /// convert `self` into an [`Alternative`].
    ///
    /// If one of `with_left()` and `with_right()` always returns [`Ok`], this
    /// method will not be called, but you have to implement it anyway. I
    /// suggest you implement it as `ExprParser::missing()`.
    fn alternative(self) -> Self::Alternative;
}

// ----------------------------------------------------------------------------

/// A stack of partial [`Expr`]s that are waiting for a right operand.
#[derive(Default, Debug, Clone)]
struct Stack(Vec<Waiting>);

impl Stack {
    /// Tests if there are any [`Waiting`]s.
    fn is_empty(&self) -> bool { self.0.len() == 0 }

    /// Forget all the [`Waiting`]s. Useful in case of an error.
    fn clear(&mut self) { self.0.clear() }

    /// Append `waiting`.
    fn push(&mut self, waiting: Waiting) { self.0.push(waiting); }

    /// If the last [`Waiting`] exists and has a right [`Precedence`] larger
    /// than `left`, returns it.
    fn pop_if_higher(&mut self, left: Precedence) -> Option<Waiting> {
        if let Some(waiting) = self.0.last() {
            if waiting.right > left { return self.0.pop(); }
        }
        None
    }

    /// If any [`Waiting`]s on [`self.stack`] have a higher [`Precedence`] than
    /// `left`, apply them to `expr`.
    fn flush_up_to(&mut self, mut expr: Box<Expr>, left: Precedence) -> Box<Expr> {
        while let Some(waiting) = self.pop_if_higher(left) {
            expr = waiting.apply(expr);
        }
        expr
    }

    /// Apply all [`Waiting`]s to `expr`.
    fn flush(&mut self, expr: Box<Expr>) -> Box<Expr> {
        self.flush_up_to(expr, Precedence(u8::MAX))
    }
}

// ----------------------------------------------------------------------------

/// A [`Parser`] the recognises [`Expr`]s.
#[derive(Debug, Clone)]
pub struct ExprParser<I: Push<Box<Expr>>> {
    /// The output stream.
    inner: I,

    /// Partial expressions that are waiting for an operand.
    stack: Stack,

    /// An [`Expr`] that is waiting for infix and postfix operators.
    expr: Option<Box<Expr>>,
}

impl<I: Push<Box<Expr>>> ExprParser<I> {
    pub fn new(inner: I) -> Self {
        ExprParser {inner, stack: Default::default(), expr: None}
    }

    /// Returns `MISSING_EXPR` suitably wrapped.
    pub fn missing() -> Box<Expr> { Box::new(Expr::Error(MISSING_EXPR)) }

    /// Try to interpret `token` as an infix or postfix operator.
    fn with_left<T: Part>(&mut self, expr: Box<Expr>, token: T) -> Result<MaybeExpr, T> {
        match token.with_left() {
            Ok((left, apply)) => {
                let expr = self.stack.flush_up_to(expr, left);
                Ok(apply(expr))
            },
            Err(token) => {
                let expr = self.stack.flush(expr);
                self.inner.push(expr);
                Err(token)
            },
        }
    }

    /// Try to interpret `token` as a nonfix or prefix operator.
    fn without_left<T: Part>(&mut self, token: T) -> Result<MaybeExpr, T> {
        token.without_left()
    }

    /// Called by [`Push::push()`] to decide how to interpret `token`.
    ///
    /// If `token` can be part of an [`Expr`], process any [`Waiting`]s that
    /// have a high enough [`Precedence`] not to contain it, and return `Ok()`.
    /// Otherwise, process all [`Waiting`]s and return `Err()`.
    fn push_helper<T: Part>(&mut self, token: T) -> Result<MaybeExpr, T> {
        if let Some(expr) = self.expr.take() {
            self.with_left(expr, token).or_else(
                |token| self.without_left(token)
            )
        } else {
            self.without_left(token).or_else(
                |token| self.with_left(Self::missing(), token)
            )
        }
    }
}

impl<I: Push<Box<Expr>>> Parser for ExprParser<I> {
    fn error(&mut self, error: E) {
        if let Some(expr) = self.expr.take() {
            let expr = self.stack.flush(expr);
            self.inner.push(expr);
        } else {
            // `error` supercedes errors about any unsatisfied [`Waiting`]s.
            self.stack.clear();
        }
        self.inner.error(error);
    }
}

impl<T: Part, I: Push<Box<Expr>> + Push<T::Alternative>> Push<T> for ExprParser<I> {
    fn push(&mut self, token: T) {
        match self.push_helper(token) {
            Ok(Complete(expr)) => { self.expr = Some(expr); },
            Ok(Incomplete(waiting)) => { self.stack.push(waiting); },
            Err(token) => { self.inner.push(token.alternative()); },
        }
    }
}

impl<I: Push<Box<Expr>> + Push<Keyword> + Flush> Flush for ExprParser<I> {
    type Output = I::Output;
    fn flush(&mut self) -> Self::Output {
        if !self.stack.is_empty() || self.expr.is_some() {
            let expr = self.expr.take().unwrap_or_else(Self::missing);
            let expr = self.stack.flush(expr);
            self.inner.push(expr);
        }
        self.inner.flush()
    }
}

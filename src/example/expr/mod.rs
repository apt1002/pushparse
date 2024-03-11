use std::mem::{replace};
use crate::{E, Parse, Push as P, Flush};
use super::{escape, span, word, bracket, atom, precedence};
use precedence::{Precedence};

pub const INVALID: E = "Expected a comma-separated list of expressions";
pub const MISSING_COMMA: E = "Expressions must be comma-separated";
pub const TWO_COMMAS: E = "Two commas in a row";

/* TODO: impl `Part` for common operators:
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

// ----------------------------------------------------------------------------

/// Represents an expression.
#[derive(Debug, Clone)]
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
    Op(Option<Box<Expr>>, word::Keyword, Option<Box<Expr>>),

    /// Field access.
    Field(Box<Expr>, word::Alphanumeric),

    /// Function or macro call.
    Call(Box<Expr>, Box<[Expr]>),
}

/// Represents an [`Expr`] that is missing a right operand.
#[derive(Debug, Clone)]
pub struct Waiting {
    left: Option<Box<Expr>>,
    op: word::Keyword,
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

/// Comma-separated [`Expr`]s in round brackets.
///
/// The first error ([`E`]) in each `Round` truncates the contents,
/// except that `expecting_comma` correctly describes the final token.
#[derive(Default, Debug, Clone)]
pub struct Round {
    /// The [`Expr`] tokens up to the first error, if any.
    pub exprs: Box<[Expr]>,

    /// The first error, if any.
    pub error: Option<E>,

    /// `true` if the last token inside the brackets is a comma.
    pub has_trailing_comma: bool,
}

impl bracket::Bracket for Round {
    const OPEN: char = '(';
    const CLOSE: char = ')';
}

// ----------------------------------------------------------------------------

/// A Parser that collects tokens into a `Round`.
/// [`word::Whitespace`] and [`span::Comment`] are ignored.
#[derive(Default, Debug, Clone)]
pub struct RoundBuffer {
    /// The [`Expr`] tokens up to the first error, if any.
    exprs: Vec<Expr>,

    /// The first error, if any.
    error: Option<E>,

    /// `true` unless the last token inside the brackets is a comma.
    expecting_comma: bool
}

impl Parse for RoundBuffer {
    fn error(&mut self, error: E) { self.error = self.error.or(Some(error)); }
}

impl Flush for RoundBuffer {
    type Output = Round;
    fn flush(&mut self) -> Self::Output {
        Round {
            exprs: self.exprs.drain(..).collect(),
            error: self.error.take(),
            has_trailing_comma: !replace(&mut self.expecting_comma, false),
        }
    }
}

impl P<Expr> for RoundBuffer {
    fn push(&mut self, token: Expr) {
        if replace(&mut self.expecting_comma, true) { return self.error(MISSING_COMMA); }
        if self.error.is_some() { return; }
        self.exprs.push(token);
    }
}

impl P<char> for RoundBuffer {
    fn push(&mut self, token: char) {
        if token != ',' { return self.error(INVALID); }
        if !replace(&mut self.expecting_comma, false) { return self.error(TWO_COMMAS); }
    }
}

impl P<Round> for RoundBuffer {
    fn push(&mut self, token: Round) {
        // This won't happen if `Self` is wrapped in an `expr::Parser`,
        // but it could be useful for testing?
        self.push(Expr::Round(token));
    }
}

impl P<word::Whitespace> for RoundBuffer {
    fn push(&mut self, _: word::Whitespace) {}
}

impl P<span::Comment> for RoundBuffer {
    fn push(&mut self, _: span::Comment) {}
}

impl bracket::Push<Round> for RoundBuffer {
    type Parser = Self;
    fn new_parser(&self) -> Self::Parser { Self::default() }
}

// ----------------------------------------------------------------------------

/// A token type rejected by `RoundBuffer`.
pub trait Spectator {}

impl Spectator for escape::Sequence {}
impl Spectator for span::CharLiteral {}
impl Spectator for span::StringLiteral {}
impl Spectator for word::Alphanumeric {}
impl Spectator for word::Symbolic {}
impl Spectator for word::Keyword {}
impl Spectator for atom::Field {}
impl Spectator for atom::Dots {}

impl<T: Spectator> P<T> for RoundBuffer {
    fn push(&mut self, _: T) { self.error(INVALID); }
}

use super::{escape, span, word, bracket, atom, precedence};
use precedence::{Precedence};

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

pub mod round;
pub use round::{Round};

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

//! Collect comma-separated [`Expr`]s in [`Round`] brackets.
//!
//! - [`Buffer`] - a [`crate::Parse`] implementation that accepts:
//!   - [`char`] (looking for commas)
//!   - [`span::Comment`] (discarded)
//!   - [`word::Whitespace`] (discarded)
//!   - [`Expr`]
//! - [`char`]s other than ',' are turned into an error, as is any type that
//!   implements [`Reject`].

use std::mem::{replace};
use crate::{E, Parse, Push as P, Flush};
use super::{escape, span, keyword, word, bracket, atom, Expr};

pub const INVALID: E = "Expected a comma-separated list of expressions";
pub const MISSING_COMMA: E = "Expressions must be comma-separated";
pub const TWO_COMMAS: E = "Two commas in a row";

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
pub struct Buffer {
    /// The [`Expr`] tokens up to the first error, if any.
    exprs: Vec<Expr>,

    /// The first error, if any.
    error: Option<E>,

    /// `true` unless the last token inside the brackets is a comma.
    expecting_comma: bool
}

impl Parse for Buffer {
    fn error(&mut self, error: E) { self.error = self.error.or(Some(error)); }
}

impl Flush for Buffer {
    type Output = Round;
    fn flush(&mut self) -> Self::Output {
        Round {
            exprs: self.exprs.drain(..).collect(),
            error: self.error.take(),
            has_trailing_comma: !replace(&mut self.expecting_comma, false),
        }
    }
}

impl P<Expr> for Buffer {
    fn push(&mut self, token: Expr) {
        if replace(&mut self.expecting_comma, true) { return self.error(MISSING_COMMA); }
        if self.error.is_some() { return; }
        self.exprs.push(token);
    }
}

impl P<char> for Buffer {
    fn push(&mut self, token: char) {
        if token != ',' { return self.error(INVALID); }
        if !replace(&mut self.expecting_comma, false) { return self.error(TWO_COMMAS); }
    }
}

impl P<Round> for Buffer {
    fn push(&mut self, token: Round) {
        // This won't happen if `Self` is wrapped in an `expr::Parser`,
        // but it could be useful for testing?
        self.push(Expr::Round(token));
    }
}

impl P<word::Whitespace> for Buffer {
    fn push(&mut self, _: word::Whitespace) {}
}

impl P<span::Comment> for Buffer {
    fn push(&mut self, _: span::Comment) {}
}

impl bracket::Push<Round> for Buffer {
    type Parser = Self;
    fn new_parser(&self) -> Self::Parser { Self::default() }
}

// ----------------------------------------------------------------------------

/// A token type rejected by `Buffer`.
pub trait Reject {}

impl Reject for escape::Sequence {}
impl Reject for span::CharLiteral {}
impl Reject for span::StringLiteral {}
impl Reject for keyword::Keyword {}
impl Reject for word::Alphanumeric {}
impl Reject for word::Symbolic {}
impl Reject for atom::Field {}
impl Reject for atom::Dots {}

impl<T: Reject> P<T> for Buffer {
    fn push(&mut self, _: T) { self.error(INVALID); }
}

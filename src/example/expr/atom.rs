//! Recognise fragments of mathematical espressions.
//!
//! - [`Parser`] - a [`crate::Parse`] implementation that accepts:
//!   - [`Keyword`]
//!   - [`Alphanumeric`]
//!   - [`Round`]
//! - the output is fed to a [`precedence::Parser`] which in turn feeds a
//!   parser that implements [`super::Push`].

use crate::{E, Parse as _, Push as P, Wrap, MaybePush, Spectate};
use super::{escape, span, word, bracket, precedence, round, Op, Expr, Waiting, Push};
use word::{Alphanumeric, Keyword};
use bracket::{Bracket};
use precedence::{Precedence, Atom, Prefix, Postfix, Infix};
use round::{Round};

/// Error message for a dot appearing on its own.
pub const SPURIOUS_DOT: E = "Syntax error: '.' must not appear on its own";

/// Error message for an unknown non-operator keyword.
pub const SPURIOUS_KEYWORD: E = "Syntax error: the keyword is out of context";

// TODO: Function types and function literals.

// ----------------------------------------------------------------------------

/// The associativity of an operator depends only on its precedence level.
const IS_LEFT: [bool; 14] = [
    false, // Cast
    true, // Exclusive, INCLUSIVE
    true, // BoolOr
    true, // BoolAnd, BoolNot
    true, // BitOr
    true, // BitXor
    true, // BitAnd
    true, // EQ, NE
    true, // LT, GT, LE, GE, LG
    false, // SL, ASR, LSR
    true, // Add, Sub
    true, // Mul, Div, Rem
    false, // Pow
    true, // BitNot, Minus, Query, FIELD, CALL
];

/// Returns the left [`Precedence`] of an operator at `level`.
const fn lp(level: usize) -> Precedence {
    Precedence((level << 1) as u8 | (IS_LEFT[level] as u8))
}

/// Returns the right [`Precedence`] of an operator at `level`.
const fn rp(level: usize) -> Precedence {
    Precedence((level << 1) as u8 | (!IS_LEFT[level] as u8))
}

/// Describes one of the built-in operators.
pub struct Info {
    /// The textual representation of the operator.
    name: &'static str,

    /// The precedence and meaning of the operator if has a left operand.
    with_left: Option<(Precedence, Op, Option<Precedence>)>,

    /// The precedence and meaning of the operator if has no left operand.
    without_left: Option<(Op, Option<Precedence>)>,
}

impl AsRef<str> for Info {
    fn as_ref(&self) -> &str { &self.name }
}

impl Info {
    /// Make an Info describing an operator that is infix, prefix, or both.
    pub const fn new(
        name: &'static str,
        infix: Option<(usize, Op)>,
        prefix: Option<(usize, Op)>,
    ) -> Self {
        let with_left = if let Some((level, op)) = infix { Some((lp(level), op, Some(rp(level)))) } else { None };
        let without_left = if let Some((level, op)) = prefix { Some((op, Some(rp(level)))) } else { None };
        Info {name, with_left, without_left}
    }

    /// Make an Info describing an operator that is postfix, prefix, or both.
    pub const fn new_postfix(
        name: &'static str,
        postfix: Option<(usize, Op)>,
        prefix: Option<(usize, Op)>,
    ) -> Self {
        let with_left = if let Some((level, op)) = postfix { Some((lp(level), op, None)) } else { None };
        let without_left = if let Some((level, op)) = prefix { Some((op, Some(rp(level)))) } else { None };
        Info {name, with_left, without_left}
    }
}

pub const ALL_INFOS: &'static [Info] = &[
    // Special keywords.
    Info::new(".", None, None),
    Info::new("fn", None, None),
    // Operators.
    Info::new(":", Some((0, Op::Cast)), None),
    Info::new("..", Some((1, Op::Exclusive)), None),
    Info::new("...", Some((1, Op::Inclusive)), None),
    Info::new("or", Some((2, Op::BoolOr)), None),
    Info::new("and", Some((3, Op::BoolAnd)), None),
    Info::new("not", Some((3, Op::BoolNot)), None),
    Info::new("|", Some((4, Op::BitOr)), None),
    Info::new("^", Some((5, Op::BitXor)), None),
    Info::new("&", Some((6, Op::BitAnd)), None),
    Info::new("==", Some((7, Op::EQ)), None),
    Info::new("!=", Some((7, Op::NE)), None),
    Info::new("<", Some((8, Op::LT)), None),
    Info::new(">", Some((8, Op::GT)), None),
    Info::new("<=", Some((8, Op::LE)), None),
    Info::new(">=", Some((8, Op::GE)), None),
    Info::new("<>", Some((8, Op::LG)), None),
    Info::new("<<", Some((9, Op::SL)), None),
    Info::new(">>", Some((9, Op::ASR)), None),
    Info::new(">>>", Some((9, Op::LSR)), None),
    Info::new("+", Some((10, Op::Add)), Some((13, Op::Plus))),
    Info::new("-", Some((10, Op::Sub)), Some((13, Op::Minus))),
    Info::new("*", Some((11, Op::Mul)), None),
    Info::new("/", Some((11, Op::Div)), None),
    Info::new("%", Some((11, Op::Rem)), None),
    Info::new("**", Some((12, Op::Pow)), None),
    Info::new("~", None, Some((13, Op::BitNot))),
    Info::new_postfix("?", Some((13, Op::Query)), None),
];

/// The [`Precedence`] of the member operator `expr.name`.
const FIELD: Precedence = rp(13);

/// The [`Precedence`] of the call operator `expr(...)`.
const CALL: Precedence = rp(13);

// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum State {
    /// The initial state.
    Home,

    /// After a `'.'`.
    Dot,
}

/// A parser that recognises [`Field`]s.
#[derive(Debug, Clone)]
pub struct Parser<I: Push> {
    /// The output stream.
    inner: precedence::Parser<Expr, I>,

    /// The parser state.
    state: State,
}

impl<I: Push> Parser<I> {
    pub fn new(inner: precedence::Parser<Expr, I>) -> Self { Self {inner, state: State::Home} }

    /// Feed an [`Atom`] to `inner`.
    fn atom(&mut self, e: Expr) { self.inner.push(Atom(e)); }

    /// Feed a [`Prefix`] to `inner`.
    fn prefix(&mut self, op: Op, right: Precedence) {
        self.inner.push(Prefix(Waiting {left: None, op, right}));
    }

    /// Feed a [`Postfix`] to `inner`.
    fn postfix(&mut self, left: Precedence, apply: impl FnOnce(Option<Box<Expr>>) -> Expr) {
        self.inner.push(Postfix {left, apply: |e: Option<Expr>| apply(e.map(Box::new))});
    }

    /// Feed an [`Infix`] to `inner`.
    fn infix(&mut self, left: Precedence, op: Op, right: Precedence) {
        self.inner.push(Infix {
            left,
            apply: |e: Option<Expr>| Waiting {left: e.map(Box::new), op, right},
        });
    }
}

impl<I: Push> Wrap for Parser<I> {
    type Inner = precedence::Parser<Expr, I>;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        match std::mem::replace(&mut self.state, State::Home) {
            State::Home => {},
            State::Dot => { self.inner.error(SPURIOUS_DOT); }
        }
    }

    fn partial_reset(&mut self) { self.partial_flush(); }

    const MISSING: E = "Syntax error: expected a value or an operator";
}

impl<I: Push> MaybePush<&'static Info> for Parser<I> {
    fn maybe_push(&mut self, token: &'static Info) -> Option<&'static Info> {
        match self.state {
            State::Home => {
                let has_left = match token {
                    Info {with_left: None, without_left: None, ..} => {
                        // The keyword is not an operator. Special cases...
                        match token.name {
                            "." => { self.state = State::Dot; },
                            "fn" => { todo!(); },
                            _ => { self.inner.error(SPURIOUS_KEYWORD); },
                        }
                        return None;
                    }
                    Info {with_left: None, ..} => false,
                    Info {without_left: None, ..} => true,
                    _ => self.inner.has_expr(),
                };
                if !has_left {
                    // Interpret it as an atom or a prefix operator.
                    match token.without_left.unwrap() {
                        (op, None) => self.atom(Expr::Op(None, op, None)),
                        (op, Some(right)) => self.prefix(op, right),
                    }
                } else {
                    // Interpret it as a postfix or infix operator.
                    match token.with_left.unwrap() {
                        (left, op, None) => self.postfix(left, |e| Expr::Op(e, op, None)),
                        (left, op, Some(right)) => self.infix(left, op, right),
                    }
                }
                return None;
            },
            State::Dot => { return Some(token); },
        }
    }
}

impl<I: Push> MaybePush<Keyword> for Parser<I> {
    /// As recommended by [`Extra`].
    fn maybe_push(&mut self, token: Keyword) -> Option<Keyword> {
        if let Some(info) = token.try_borrow::<Info>() {
            self.push(info);
        } else {
            self.partial_flush();
            self.inner().push(token);
        }
        None
    }
}

impl<I: Push> MaybePush<Alphanumeric> for Parser<I> {
    fn maybe_push(&mut self, token: Alphanumeric) -> Option<Alphanumeric> {
        match self.state {
            State::Home => { self.atom(Expr::Name(token.0)); },
            State::Dot => {
                self.postfix(FIELD, |e| Expr::Field(e, token.0));
                self.state = State::Home;
            },
        }
        None
    }
}

impl<I: Push> MaybePush<Round> for Parser<I> {
    fn maybe_push(&mut self, token: Round) -> Option<Round> {
        match self.state {
            State::Home => {
                if self.inner.has_expr() {
                    self.postfix(CALL, |e| Expr::Call(e, token));
                } else {
                    self.atom(Expr::Round(token));
                }
            },
            _ => { return Some(token); },
        }
        None
    }
}

impl<I: Push> bracket::Push<Round> for Parser<I> {
    type Parser = Parser<round::Buffer>;
    fn new_parser(&self) -> Self::Parser { Parser::new(precedence::Parser::new(Default::default())) }
}

// ----------------------------------------------------------------------------

impl<
    I: Push + bracket::Push<B>,
    B: Bracket + Spectate<Parser<I>> + Spectate<Parser<I::Parser>>,
> bracket::Push<B> for Parser<I> where
    I::Parser: Push,
{
    type Parser = Parser<I::Parser>;
    fn new_parser(&self) -> Self::Parser { Parser::new(self.inner.new_parser()) }
}

impl<I: Push> Spectate<Parser<I>> for char {}
impl<I: Push> Spectate<Parser<I>> for escape::Sequence {}
impl<I: Push> Spectate<Parser<I>> for span::Comment {}
impl<I: Push> Spectate<Parser<I>> for span::CharLiteral {}
impl<I: Push> Spectate<Parser<I>> for span::StringLiteral {}
impl<I: Push> Spectate<Parser<I>> for word::Whitespace {}
impl<I: Push> Spectate<Parser<I>> for word::Symbolic {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Ws, Sy(String), Kw(&'static str), Ex(Expr), Char(char)}
    use Token::*;

    const E_DOT: Token = Error(SPURIOUS_DOT);

    /// A parser that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl crate::Parse for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl P<word::Whitespace> for Buffer {
        fn push(&mut self, _: word::Whitespace) { self.0.push(Ws); }
    }

    impl P<word::Symbolic> for Buffer {
        fn push(&mut self, token: word::Symbolic) { self.0.push(Sy(token.0)); }
    }

    impl P<word::Keyword> for Buffer {
        fn push(&mut self, token: word::Keyword) { self.0.push(Kw(token.name())); }
    }

    impl P<Expr> for Buffer {
        fn push(&mut self, token: Expr) { self.0.push(Ex(token)); }
    }

    impl P<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let parser = bracket::Parser::new(Parser::new(precedence::Parser::new(Buffer::default())));
        let mut parser = word::Parser::new(
            parser,
            [Keyword(&"return")].into_iter().chain(
                ALL_INFOS.iter().map(|info| Keyword(info))
            ),
        );
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    /// Construct an [`Expr`] representing the identifer `s`.
    fn name(s: impl Into<String>) -> Expr { Expr::Name(s.into()) }

    /// Construct an [`Expr`] representing `op` with operands.
    fn op(
        left: impl Into<Option<Expr>>,
        op: Op,
        right: impl Into<Option<Expr>>,
    ) -> Expr {
        let left = left.into().map(Box::new);
        let right = right.into().map(Box::new);
        Expr::Op(left, op, right)
    }

    /// Construct an [`Expr`] representing a field access.
    fn field(
        e: impl Into<Option<Expr>>,
        f: impl Into<String>,
    ) -> Expr {
        let e = e.into().map(Box::new);
        Expr::Field(e, f.into())
    }

    /// Construct an [`Expr`] representing `exprs` in round brackets.
    fn group(exprs: impl IntoIterator<Item=Expr>) -> Expr {
        Expr::Round(Round::new(exprs))
    }

    /// Construct an [`Expr`] representing `op` with the specified operands.
    fn bare(bare_op: Op) -> Expr { op(None, bare_op, None) }

    #[test]
    fn ascii() {
        check(",.;", &[Char(','), E_DOT, Char(';')]);
    }

    #[test]
    fn words() {
        check("return a: Int;", &[Kw("return"), Ws, Ex(op(name("a"), Op::Cast, name("Int"))), Char(';')]);
    }

    #[test]
    fn dots() {
        check("", &[]);
        check(".", &[E_DOT]);
        check("..", &[Ex(bare(Op::Exclusive))]);
        check("...", &[Ex(op(None, Op::Inclusive, None))]);
        check("....", &[Sy("....".into())]);
        check(". .", &[E_DOT, Ws, E_DOT]);
        check(".. .", &[Ex(bare(Op::Exclusive)), E_DOT]);
        check(". ..", &[E_DOT, Ws, Ex(bare(Op::Exclusive))]);
        check(".. ..", &[Ex(op(None, Op::Exclusive, bare(Op::Exclusive)))]);
    }

    #[test]
    fn fields() {
        check("A", &[Ex(name("A"))]);
        check(".A", &[Ex(field(None, "A"))]);
        check(". A", &[E_DOT, Ws, Ex(name("A"))]);
        check("..A", &[Ex(op(None, Op::Exclusive, name("A")))]);
        check(". .A", &[E_DOT, Ws, Ex(field(None, "A"))]);
        check(".. A", &[Ex(op(None, Op::Exclusive, name("A")))]);
        check("A.B", &[Ex(field(name("A"), "B"))]);
        check("A .B", &[Ex(field(name("A"), "B"))]);
        check("A. B", &[Ex(name("A")), E_DOT, Ws, Ex(name("B"))]);
        check("A..B", &[Ex(op(name("A"), Op::Exclusive, name("B")))]);
    }

    #[test]
    fn rounds() {
        check("(A)", &[Ex(group([name("A")]))]);
        check("(a: Int, b)", &[Ex(group([
            op(name("a"), Op::Cast, name("Int")),
            name("b"),
        ]))]);
    }
}

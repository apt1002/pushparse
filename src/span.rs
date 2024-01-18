use super::{Parser, E, Push, MaybePush, Wrapper, EscapeSequence};

/// Represents some whitespace.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Whitespace;

/// Represents a comment.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Comment;

/// Represents a character literal.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CharLiteral(char);

/// Represents a string.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral(String);

pub const UNTERMINATED_BLOCK_COMMENT: E = "Unterminated block comment";
pub const UNTERMINATED_STRING: E = "Unterminated string";
pub const UNTERMINATED_CHAR: E = "Unterminated character literal";

// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum State {
    /// The initial state.
    Home,

    /// After reading whitespace characters.
    WhitespaceSpan,

    /// After reading a slash.
    Slash,

    /// After reading a double slash.
    LineComment,

    /// After reading `"/*"`. The boolean indicates whether we have just read a
    /// `'*'` other than the opening one.
    BlockComment(bool),

    /// After reading `'"'`.
    StringSpan(String),

    /// After reading `'\''` and maybe another char.
    CharSpan(Option<char>),
}

use State::*;

pub trait Output: Push<Whitespace> + Push<Comment> + Push<CharLiteral> + Push<StringLiteral> + Push<char> {}

impl<I: Push<Whitespace> + Push<Comment> + Push<CharLiteral> + Push<StringLiteral> + Push<char>> Output for I {}

/// A [`Parser`] that recognizes comments, and character and string literals.
#[derive(Debug, Clone)]
pub struct Spanner<I: Output> {
    /// The output stream.
    inner: I,

    /// The parser state.
    state: State,
}

impl<I: Output> Spanner<I> {
    /// Construct a `Spanner` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { Spanner {inner, state: Home} }
}

impl<I: Output> Wrapper for Spanner<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        match self.state {
            Home => {},
            WhitespaceSpan => {
                self.inner.push(Whitespace);
            },
            Slash => {
                self.inner.push('/');
            },
            LineComment => {
                self.inner.push(Comment);
            },
            BlockComment(_) => {
                self.error(UNTERMINATED_BLOCK_COMMENT);
            },
            StringSpan(_) => {
                self.error(UNTERMINATED_STRING);
            },
            CharSpan(_) => {
                self.error(UNTERMINATED_CHAR);
            },
        }
        self.state = Home;
    }

    fn partial_reset(&mut self) {
        match self.state {
            WhitespaceSpan => {
                self.inner.push(Whitespace);
            },
            Slash => {
                self.inner.push('/');
            },
            _ => {},
        }
        self.state = Home;
    }

    const MISSING: E = "span: Should not happen";
}

impl<I: Output> MaybePush<char> for Spanner<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        match &mut self.state {
            Home => {
                match token {
                    ' ' | '\t' | '\n' | '\r' => { self.state = WhitespaceSpan; },
                    '/' => { self.state = Slash; },
                    '\'' => { self.state = CharSpan(None); },
                    '"' => { self.state = StringSpan(String::new()); },
                    _ => { self.inner.push(token); },
                }
                return None;
            },
            WhitespaceSpan => {
                match token {
                    ' ' | '\t' | '\n' | '\r' => {
                        self.state = WhitespaceSpan;
                        return None;
                    },
                    _ => {
                        return Some(token);
                    },
                }
            },
            Slash => {
                match token {
                    '/' => {
                        self.state = LineComment;
                        return None;
                    },
                    '*' => {
                        self.state = BlockComment(false);
                        return None;
                    },
                    _ => {
                        return Some(token);
                    },
                }
            },
            LineComment => {
                if token == '\n' { return Some(token); }
                self.state = LineComment;
                return None;
            },
            BlockComment(star) => {
                if token == '/' && *star {
                    self.inner.push(Comment);
                    self.state = Home;
                    return None;
                }
                self.state = BlockComment(token == '*');
                return None;
            },
            StringSpan(s) => {
                if token == '"' {
                    let mut s_ = String::new();
                    std::mem::swap(s, &mut s_);
                    s_.shrink_to_fit();
                    self.inner.push(StringLiteral(s_));
                    self.state = Home;
                } else {
                    s.push(token);
                }
                return None;
            },
            CharSpan(None) => {
                if token == '\'' { return Some(token); }
                self.state = CharSpan(Some(token));
                return None;
            },
            CharSpan(Some(c)) => {
                if token == '\'' {
                    self.inner.push(CharLiteral(*c));
                    self.state = Home;
                    return None;
                }
                return Some(token);
            },
        }
    }
}

impl<I: Output> MaybePush<EscapeSequence> for Spanner<I> {
    fn maybe_push(&mut self, token: EscapeSequence) -> Option<EscapeSequence> {
        match &mut self.state {
            LineComment => {
                self.state = LineComment;
                return None;
            },
            BlockComment(_) => {
                self.state = BlockComment(false);
                return None;
            },
            StringSpan(s) => {
                let mut s_ = String::new();
                std::mem::swap(s, &mut s_);
                s_.push(token.0);
                self.state = StringSpan(s_);
                return None;
            },
            CharSpan(None) => {
                self.state = CharSpan(Some(token.0));
                return None;
            },
            _ => { return Some(token); },
        }
    }
}

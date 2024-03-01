//! A collection of reusable and replaceable [`crate::Parser`] implementations.
//! Used together, these implement a parser for a language similar to
//! Javascript. You can use them, or copy and modify them, as you see fit.
//!
//! The provided parser components include:
//! - [`utf8`] - Turn UTF-8 byte sequences into characters.
//! - [`escape`] - Recognise escape sequences such as `\n` and `\u000A`.
//! - [`span`] - Recognise C-style comments and string and character literals.
//! - [`word`] - Recognise whitespace, alphanumeric and punctuation words, and
//!   keywords.
//! - [`bracket`] - Match brackets.
//! - [`expr`] - A precedence parser for mathematical expressions.
//!
//! Each module typically defines some subset of the following:
//! - `Parser` the `crate::Parser` implementation.
//! - `Push` a trait which needs to be implemented by whatever `Parser` feeds.
//! - `Spectator` a trait which can be implemented by any token type that you
//!   want to pass through `Parser` unchanged.
//! - Some output token types.
//! - Some error message constants.

pub mod utf8;

pub mod escape;

pub mod span;

pub mod word;

pub mod bracket;

pub mod expr;

//! Turn UTF-8 byte sequences into characters.
//!
//! - [`Parser`] - a [`crate::Parse`] implementation that accepts:
//!   - [`u8`]
//! - The output token types are:
//!   - [`char`]
//! - In addition, any type that implements [`crate::Spectate`] is accepted and
//!   passed on unchanged.

use crate::{E, Push as P, Parse};

pub const MISSING: E = "Invalid UTF-8: missing continuation byte";
pub const SPURIOUS: E = "Invalid UTF-8: spurious continuation byte";
pub const RESERVED: E = "Invalid UTF-8: undefined start byte";
pub const INVALID: E = "Invalid unicode scalar value";

// ----------------------------------------------------------------------------

/// A [`Parser`] that accepts `u8`s and generates `char`s.
#[derive(Default, Debug, Clone)]
pub struct Parser<I: P<char>> {
    /// The output stream.
    inner: I,

    /// A partial scalar value read from previous bytes.
    /// Must be zero if [`count`] is zero.
    bits: u32,

    /// The number of continuation bytes needed to complete the character.
    count: u32,
}

impl<I: P<char>> Parser<I> {
    /// Construct a `Parser` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { Parser {inner, bits: 0, count: 0} }
}

impl<I: P<char>> crate::Wrap for Parser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        if self.count > 0 { self.error(MISSING); }
    }

    fn partial_reset(&mut self) { self.bits = 0; self.count = 0; }

    /// Any byte string is valid input.
    const MISSING: E = "utf8: Should not happen";
}

impl<I: P<char>> crate::MaybePush<u8> for Parser<I> {
    fn maybe_push(&mut self, token: u8) -> Option<u8> {
        let ones = (token & 0xf8).leading_ones();
        self.bits <<= 6;
        self.bits |= token as u32 & (0x7f >> ones);
        match (ones, self.count) {
            (0, 0) => { self.count = 0; }, // ASCII.
            (1, 0) => { self.error(SPURIOUS); return None; },
            (5, 0) => { self.error(RESERVED); return None; },
            (_, 0) => { self.count = ones - 1; }, // Start of multi-byte char.
            (1, _) => { self.count -= 1; }, // Continuation of multi-byte char.
            (_, _) => { self.error(MISSING); return Some(token); },
        }
        if self.count == 0 {
            if let Some(c) = char::from_u32(self.bits) {
                self.inner.push(c);
            } else {
                self.error(INVALID);
            }
            self.bits = 0;
        }
        None
    }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Buffer, Flush};

    fn check(input: &[u8], expected: &[Result<char, E>]) {
        let mut parser = Parser::new(Buffer::default());
        for &byte in input { parser.push(byte); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check(b"Hello", &[Ok('H'), Ok('e'), Ok('l'), Ok('l'), Ok('o')]);
    }

    #[test]
    fn sparkle_heart() {
        check(&[0x3c, 0xf0, 0x9f, 0x92, 0x96, 0x3e], &[Ok('<'), Ok('💖'), Ok('>')]);
    }

    #[test]
    fn missing() {
        check(&[0x3c, 0xf0, 0x9f, 0x92, 0x3e], &[Ok('<'), Err(MISSING), Ok('>')]);
    }

    #[test]
    fn spurious() {
        check(&[0x3c, 0xf0, 0x9f, 0x92, 0x96, 0x80, 0x3e], &[Ok('<'), Ok('💖'), Err(SPURIOUS), Ok('>')]);
    }

    #[test]
    fn reserved() {
        check(&[0x3c, 0xff, 0x3e], &[Ok('<'), Err(RESERVED), Ok('>')]);
    }
}

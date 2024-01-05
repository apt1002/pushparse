use super::{Parser, E, Push, MaybePush, Wrapper};

/// The meaning of a UTF-8 byte.
#[derive(Debug, Copy, Clone)]
struct Meaning {
    /// A mask indicating which bits of the byte encode the scalar value.
    mask: u8,
    
    /// The number of continuation bytes which should follow this byte.
    count: u8,
}

const ASCII: Meaning = Meaning {mask: 0x7f, count: 0};
const DUMMY: Meaning = Meaning {mask: 0x00, count: 0};
const NEED1: Meaning = Meaning {mask: 0x1f, count: 1};
const NEED2: Meaning = Meaning {mask: 0x0f, count: 2};
const NEED3: Meaning = Meaning {mask: 0x07, count: 3};

/// The [`Meaning`]s of all non-continuation bytes, indexed by the top 4 bits.
const MEANINGS: [Meaning; 0x10] = [
    ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,
    DUMMY, DUMMY, DUMMY, DUMMY, NEED1, NEED1, NEED2, NEED3,
];

const MISSING: E = "Invalid UTF-8: missing continuation byte";
const SPURIOUS: E = "Invalid UTF-8: spurious continuation byte";
const INVALID: E = "Invalid unicode scalar value";

/// A [`Parser`] that accepts `u8`s and generates `char`s.
#[derive(Default, Debug, Clone)]
pub struct Decoder<I: Push<char>> {
    /// The output stream.
    inner: I,

    /// A partial scalar value read from previous bytes.
    /// Unused if [`count`] is zero.
    bits: u32,

    /// The number of continuation bytes needed to complete the character.
    count: u8,
}

impl<I: Push<char>> Decoder<I> {
    /// Construct a `Decoder` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { Decoder {inner, bits: 0, count: 0} }
}

impl<I: Push<char>> Wrapper for Decoder<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        if self.count > 0 { self.error(MISSING); }
    }

    fn partial_reset(&mut self) { self.count = 0; }

    /// Any byte string is valid input.
    const MISSING: E = "Should not happen";
}

impl<I: Push<char>> MaybePush<u8> for Decoder<I> {
    fn maybe_push(&mut self, token: u8) -> Option<u8> {
        if token & 0xc0 == 0x80 {
            // It's a continuation byte.
            if self.count == 0 {
                self.error(SPURIOUS);
                return None;
            }
            let low_bits = token & 0x3f;
            self.bits = (self.bits << 6) | (low_bits as u32);
            self.count -= 1;
        } else {
            // It's not a continuation byte.
            if self.count > 0 {
                self.error(MISSING);
                return Some(token); // Try again.
            }
            let meaning = MEANINGS[token as usize >> 4];
            self.bits = (token & meaning.mask) as u32;
            self.count = meaning.count;
        }
        if self.count == 0 {
            if let Some(c) = char::from_u32(self.bits) {
                self.inner().push(c);
            } else {
                self.error(INVALID);
            }
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
        let mut parser = Decoder::new(Buffer::default());
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
}

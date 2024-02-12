use super::{Parser, E, Push, MaybePush, Wrapper};

/// Represents an escape sequence that represents `c`.
pub struct EscapeSequence(pub char);

pub const MISSING_SEQUENCE: E = "Missing escape sequence";
pub const MISSING_HEX: E = "Expected a hex digit";
pub const MISSING_OPEN_BRACE: E = "Expected {";
pub const MISSING_HEX_OR_CLOSE_BRACE: E = "Expected a hex digit or }";
pub const EXCESSIVE_HEX: E = "Too many hex digits";
pub const INVALID: E = "Invalid unicode scalar value";

// ----------------------------------------------------------------------------

fn parse_hex_digit(c: char) -> Option<u32> {
    if '0' <= c && c <= '9' { return Some(c as u32 - '0' as u32); }
    if 'A' <= c && c <= 'F' { return Some(c as u32 - 'A' as u32 + 10); }
    if 'a' <= c && c <= 'f' { return Some(c as u32 - 'a' as u32 + 10); }
    return None;
}

// ----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum State {
    /// The initial state.
    Home,

    /// After reading a backslash.
    Backslash,

    /// After reading backslash followed by `'x'`.
    BackslashX {
        /// A partial scalar value read from previous digits.
        bits: u32,

        /// The number of digits read so far.
        count: u32,
    },

    /// After reading backslash followed by `'u'`.
    BackslashU,

    /// After reading backslash followed by `'u'` followed by `'{'`.
    Unicode {
        /// A partial scalar value read from previous digits.
        bits: u32,

        /// The number of digits read so far.
        count: u32,
    },
}

use State::*;

/// A [`Parser`] that accepts `char`s and generates [`EscapeSequence`]s,
/// passing on any other `char`s.
#[derive(Debug, Clone)]
pub struct EscapeParser<I: Push<char> + Push<EscapeSequence>> {
    /// The output stream.
    inner: I,

    /// The parser state.
    state: State,
}

impl<I: Push<char> + Push<EscapeSequence>> EscapeParser<I> {
    /// Construct an `EscapeParser` that feeds its output to `inner`.
    pub fn new(inner: I) -> Self { EscapeParser {inner, state: Home} }

    /// Output an [`EscapeSequence`] and return to the initial [`State`].
    fn output(&mut self, c: char) {
        self.inner.push(EscapeSequence(c));
        self.state = Home;
    }

    /// Output an [`EscapeSequence`] or `INVALID` and return to the initial
    /// [`State`].
    fn output_u32(&mut self, bits: u32) {
        if let Some(c) = char::from_u32(bits) {
            self.output(c);
        } else {
            self.error(INVALID);
        }
    }
}

impl<I: Push<char> + Push<EscapeSequence>> Wrapper for EscapeParser<I> {
    type Inner = I;

    fn inner(&mut self) -> &mut Self::Inner { &mut self.inner }

    fn partial_flush(&mut self) {
        match self.state {
            Home => {},
            Backslash => { self.error(MISSING_SEQUENCE); },
            BackslashX {..} => { self.error(MISSING_HEX); },
            BackslashU {..} => { self.error(MISSING_OPEN_BRACE); },
            Unicode {count, ..} => {
                self.error(
                    if count == 0 { MISSING_HEX } else { MISSING_HEX_OR_CLOSE_BRACE }
                );
            },
        }
        self.state = Home;
    }

    fn partial_reset(&mut self) { self.state = Home; }

    const MISSING: E = "escape: Should not happen";
}

impl<I: Push<char> + Push<EscapeSequence>> MaybePush<char> for EscapeParser<I> {
    fn maybe_push(&mut self, token: char) -> Option<char> {
        match self.state {
            Home => {
                if token == '\\' {
                    self.state = Backslash;
                } else {
                    self.inner.push(token);
                }
                return None;
            },
            Backslash => {
                match token {
                    '0' => { self.output('\0'); self.state = Home; },
                    't' => { self.output('\t'); self.state = Home; },
                    'n' => { self.output('\n'); self.state = Home; },
                    'r' => { self.output('\r'); self.state = Home; },
                    'x' => { self.state = BackslashX {bits: 0, count: 0}; },
                    'u' => { self.state = BackslashU; },
                    '\\' | '"' => { self.output(token); self.state = Home; },
                    _ => { return Some(token); },
                }
                return None;
            },
            BackslashX {mut bits, mut count} => {
                if let Some(x) = parse_hex_digit(token) {
                    bits <<= 4; bits |= x;
                    count += 1;
                    if count < 2 {
                        self.state = BackslashX {bits, count};
                    } else {
                        self.output_u32(bits);
                    }
                    return None;
                } else {
                    return Some(token);
                }
            },
            BackslashU => {
                if token == '{' {
                    self.state = Unicode {bits: 0, count: 0};
                    return None;
                } else {
                    return Some(token);
                }
            },
            Unicode {mut bits, mut count} => {
                if let Some(x) = parse_hex_digit(token) {
                    bits <<= 4; bits |= x;
                    count += 1;
                    self.state = Unicode {bits, count};
                    return None;
                } else if token == '}' {
                    if count == 0 {
                        self.error(MISSING_HEX);
                    } else if count > 6 {
                        self.error(EXCESSIVE_HEX);
                    } else {
                        self.output_u32(bits);
                    }
                    return None;
                } else {
                    return Some(token);
                }
            },
        }
    }
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flush};

    #[derive(Debug, Clone, PartialEq)]
    enum Token {Error(E), Char(char), Escape(char)}
    use Token::*;

    /// A [`Parser`] that converts everything to a [`Token`].
    #[derive(Debug, Default, Clone, PartialEq)]
    struct Buffer(Vec<Token>);

    impl Parser for Buffer {
        fn error(&mut self, error: E) { self.0.push(Error(error)); }
    }

    impl Push<char> for Buffer {
        fn push(&mut self, token: char) { self.0.push(Char(token)); }
    }

    impl Push<EscapeSequence> for Buffer {
        fn push(&mut self, token: EscapeSequence) { self.0.push(Escape(token.0)); }
    }

    impl Flush for Buffer {
        type Output = Box<[Token]>;
        fn flush(&mut self) -> Self::Output { self.0.drain(..).collect() }
    }

    fn check(input: &str, expected: &[Token]) {
        println!("input = {:}", input);
        let mut parser = EscapeParser::new(Buffer::default());
        for c in input.chars() { parser.push(c); println!("parser = {:x?}", parser); }
        let observed = parser.flush();
        assert_eq!(expected, &*observed);
    }

    #[test]
    fn ascii() {
        check("Hello", &[Char('H'), Char('e'), Char('l'), Char('l'), Char('o')]);
    }

    #[test]
    fn bad_escape() {
        check(r"<\>", &[Char('<'), Error(MISSING_SEQUENCE), Char('>')]);
    }

    #[test]
    fn escapes() {
        check(r"<\0>", &[Char('<'), Escape('\0'), Char('>')]);
        check(r"<\t>", &[Char('<'), Escape('\t'), Char('>')]);
        check(r"<\n>", &[Char('<'), Escape('\n'), Char('>')]);
        check(r"<\r>", &[Char('<'), Escape('\r'), Char('>')]);
        check("<\\\\>", &[Char('<'), Escape('\\'), Char('>')]);
        check("<\\\">", &[Char('<'), Escape('\"'), Char('>')]);
    }

    #[test]
    fn hex() {
        check(r"<\x>", &[Char('<'), Error(MISSING_HEX), Char('>')]);
        check(r"<\x4>", &[Char('<'), Error(MISSING_HEX), Char('>')]);
        check(r"<\x44>", &[Char('<'), Escape('\x44'), Char('>')]);
        check(r"<\x444>", &[Char('<'), Escape('\x44'), Char('4'), Char('>')]);
    }

    #[test]
    fn unicode() {
        check(r"<\u>", &[Char('<'), Error(MISSING_OPEN_BRACE), Char('>')]);
        check(r"<\u4>", &[Char('<'), Error(MISSING_OPEN_BRACE), Char('4'), Char('>')]);
        check(r"<\u}>", &[Char('<'), Error(MISSING_OPEN_BRACE), Char('}'), Char('>')]);
        check(r"<\u{>", &[Char('<'), Error(MISSING_HEX), Char('>')]);
        check(r"<\u{}>", &[Char('<'), Error(MISSING_HEX), Char('>')]);
        check(r"<\u{4>", &[Char('<'), Error(MISSING_HEX_OR_CLOSE_BRACE), Char('>')]);
        check(r"<\u{0}>", &[Char('<'), Escape('\u{0}'), Char('>')]);
        check(r"<\u{4x}>", &[Char('<'), Error(MISSING_HEX_OR_CLOSE_BRACE), Char('x'), Char('}'), Char('>')]);
        check(r"<\u{00}>", &[Char('<'), Escape('\u{00}'), Char('>')]);
        check(r"<\u{000}>", &[Char('<'), Escape('\u{000}'), Char('>')]);
        check(r"<\u{0000}>", &[Char('<'), Escape('\u{0000}'), Char('>')]);
        check(r"<\u{00000}>", &[Char('<'), Escape('\u{00000}'), Char('>')]);
        check(r"<\u{000000}>", &[Char('<'), Escape('\u{000000}'), Char('>')]);
        check(r"<\u{444444}>", &[Char('<'), Error(INVALID), Char('>')]);
        check(r"<\u{0000000}>", &[Char('<'), Error(EXCESSIVE_HEX), Char('>')]);
        check(r"<\u{00000000}>", &[Char('<'), Error(EXCESSIVE_HEX), Char('>')]);
    }

    #[test]
    fn digits() {
        check(r"<\u{0}>", &[Char('<'), Escape('\u{0}'), Char('>')]);
        check(r"<\u{9}>", &[Char('<'), Escape('\u{9}'), Char('>')]);
        check(r"<\u{A}>", &[Char('<'), Escape('\u{A}'), Char('>')]);
        check(r"<\u{F}>", &[Char('<'), Escape('\u{F}'), Char('>')]);
        check(r"<\u{a}>", &[Char('<'), Escape('\u{a}'), Char('>')]);
        check(r"<\u{f}>", &[Char('<'), Escape('\u{f}'), Char('>')]);
    }
}

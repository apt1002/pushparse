//! Traits that allow [`word::Parser`] to enumerate all keywords recognised
//! by the parser it feeds.
//!
//! A parser defines its keywords by implementing [`Push`].
//! In the case that the parser implements [`crate::Wrap`], it should not
//! implement [`Push`] directly but should instead implement [`Extra`].
//! In the case that the parser does not define any additional keywords it need
//! not implement [`Extra`] directly but can instead implement [`NoExtra`].
//!
//! [`word::Parser`]: super::word::Parser

use crate::{Push as P, Wrap, MaybePush};

/// A token that compactly represents a keyword.
///
/// Keywords must be entirely alphanumeric or entirely operator characters,
/// as defined by [`word::Parser`].
///
/// [`word::Parser`]: super::word::Parser
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Keyword(pub usize);

// ----------------------------------------------------------------------------

/// Statically enumerate all keywords needed by a parser.
///
/// `()` represents the empty list.
pub trait List {
    /// The number of distinct keywords.
    const NUM_KEYWORDS: usize;

    /// Returns the [`str`] representation of the specified keyword.
    ///
    /// - token - `token.0` must be less than `NUM_KEYWORDS`.
    fn name(token: Keyword) -> &'static str;
}

impl List for () {
    const NUM_KEYWORDS: usize = 0;
    fn name(_: Keyword) -> &'static str { panic!("Parser uses no keywords") }
}

// ----------------------------------------------------------------------------

/// A parser that recognises some keywords.
pub trait Push: P<Keyword> {
    /// The static list of keywords recognised by this parser.
    ///
    /// It is common for `List` to be `Self`.
    type List: List;
}

// ----------------------------------------------------------------------------

/// A parser that looks for keywords in addition to those recognised by
/// [`Wrap::Inner`].
///
/// [`<Self as MaybePush<Keyword>>::maybe_push()`] should call
/// [`Self::push_keyword()`]. Unfortunately this cannot be provided as a
/// blanket implementation.
///
/// [`<Self as MaybePush<Keyword>>::maybe_push()`]: MaybePush::maybe_push()
pub trait Extra: Wrap where Self::Inner: Push {
    /// The return type of [`Self::keyword_info()`]
    type Info: 'static;

    /// Enumerates all relevant keywords, i.e. those recognised by this parser
    /// but not by [`Wrap::Inner`].
    const EXTRA: &'static [Self::Info];

    /// If `token` is relevant to `Self`, returns its [`Self::Info`].
    fn keyword_info(token: Keyword) -> Option<&'static <Self as Extra>::Info> {
        token.0.checked_sub(<Self::Inner as Push>::List::NUM_KEYWORDS).map(|index| &Self::EXTRA[index])
    }

    /// Returns the [`str`] representation of a relevant keyword.
    fn name(info: &Self::Info) -> &str;

    /// If `token` is a relevant keyword, calls [`MaybePush::maybe_push()`]
    /// passing the corresponding [`Self::Info`], otherwise calls
    /// [`Wrap::partial_flush()`] and passes `token` to [`Wrap::inner()`].
    fn push_keyword(&mut self, token: Keyword) -> Option<Keyword> where
        Self: MaybePush<&'static Self::Info>,
    {
        if let Some(info) = Self::keyword_info(token) {
            self.maybe_push(info).map(|_| token)
        } else {
            self.partial_flush();
            self.inner().push(token);
            None
        }
    }
}

impl<T: Extra> List for T where T::Inner: Push {
    const NUM_KEYWORDS: usize = <<Self as Wrap>::Inner as Push>::List::NUM_KEYWORDS + Self::EXTRA.len();

    fn name(token: Keyword) -> &'static str {
        if let Some(info) = Self::keyword_info(token) {
            <Self as Extra>::name(info)
        } else {
            <<Self as Wrap>::Inner as Push>::List::name(token)
        }
    }
}

impl<T: Extra + MaybePush<Keyword>> Push for T where T::Inner: Push {
    type List = Self;
}

// ----------------------------------------------------------------------------

/// A parser that passes on all [`Keyword`]s.
pub trait NoExtra: Wrap where Self::Inner: Push {}

impl<T: NoExtra> Extra for T where Self::Inner: Push {
    type Info = ();
    const EXTRA: &'static [()] = &[];
    fn name(_: &()) -> &str { "" }
}

impl<T: NoExtra> crate::Spectate<T> for Keyword where T::Inner: Push {}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::E;

    struct Inner;

    impl List for Inner {
        const NUM_KEYWORDS: usize = 2;
        fn name(token: Keyword) -> &'static str { ["foo", "bar"][token.0] }
    }

    impl crate::Parse for Inner {
        fn error(&mut self, _: E) {}
    }

    impl P<Keyword> for Inner {
        fn push(&mut self, _: Keyword) {}
    }

    impl Push for Inner {
        type List = Self;
    }

    struct Outer(Inner);

    impl Wrap for Outer {
        type Inner = Inner;
        const MISSING: E = "Missing";
        fn inner(&mut self) -> &mut self::Inner { &mut self.0 }
        fn partial_flush(&mut self) {}
        fn partial_reset(&mut self) {}
    }

    impl MaybePush<&'static &'static str> for Outer {
        fn maybe_push(&mut self, _: &'static &'static str) -> Option<&'static &'static str> { None }
    }

    impl MaybePush<Keyword> for Outer {
        fn maybe_push(&mut self, token: Keyword) -> Option<Keyword> { self.push_keyword(token) }
    }

    impl Extra for Outer {
        type Info = &'static str;
        const EXTRA: &'static [Self::Info] = &["baz", "quux"];
        fn name(info: &Self::Info) -> &str { *info }
    }

    #[test]
    fn list() {
        assert_eq!(<Outer as Push>::List::NUM_KEYWORDS, 4);
        fn name(index: usize) -> &'static str { <<Outer as Push>::List as List>::name(Keyword(index)) }
        assert_eq!(name(0), "foo");
        assert_eq!(name(1), "bar");
        assert_eq!(name(2), "baz");
        assert_eq!(name(3), "quux");
    }

    #[test]
    fn info() {
        fn info(index: usize) -> Option<&'static &'static str> { Outer::keyword_info(Keyword(index)) }
        assert_eq!(info(0), None);
        assert_eq!(info(1), None);
        assert_eq!(info(2), Some(&"baz"));
        assert_eq!(info(3), Some(&"quux"));
    }
}

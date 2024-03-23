/// A token that compactly represents a keyword.
///
/// Keywords must be entirely alphanumeric or entirely operator characters,
/// as defined by [`word::Parser`].
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Keyword(pub usize);

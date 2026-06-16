//! [`VerseId`] — the stable address every layer links to.
//!
//! A verse carries two canonical keys:
//! - the `OSIS` string, `John.3.16` — the human / interchange form, and
//! - the `BBCCCVVV` integer, `43003016` — `book*1_000_000 +
//!   chapter*1000 + verse`, which sorts in canonical order.
//!
//! [`VerseId::parse`] accepts both the human form (`John 3:16`,
//! `1 John 2:3`, `Song of Solomon 1:1`) and the dotted `OSIS` form
//! (`John.3.16`).

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::book::Book;

/// Why a reference string failed to parse.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum RefError {
    #[error("empty reference")]
    Empty,
    #[error("unknown book: {0:?}")]
    UnknownBook(String),
    #[error("malformed chapter:verse in {0:?}")]
    BadNumbers(String),
}

/// A single verse: book + chapter + verse.
///
/// Field order is `(book, chapter, verse)` so the derived ordering is
/// canonical reading order, matching [`VerseId::numeric`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VerseId {
    pub book: Book,
    pub chapter: u16,
    pub verse: u16,
}

impl VerseId {
    /// Construct directly. No range validation against the canon's
    /// chapter/verse counts — that's the ingest layer's job.
    #[must_use]
    pub const fn new(book: Book, chapter: u16, verse: u16) -> Self {
        Self {
            book,
            chapter,
            verse,
        }
    }

    /// The sortable `BBCCCVVV` integer key, e.g. `John 3:16` →
    /// `43_003_016`.
    #[must_use]
    pub fn numeric(self) -> u32 {
        u32::from(self.book.ordinal()) * 1_000_000
            + u32::from(self.chapter) * 1_000
            + u32::from(self.verse)
    }

    /// Rebuild a [`VerseId`] from its `BBCCCVVV` integer key.
    #[must_use]
    pub fn from_numeric(n: u32) -> Option<Self> {
        let book = Book::from_ordinal((n / 1_000_000) as u8)?;
        let chapter = ((n / 1_000) % 1_000) as u16;
        let verse = (n % 1_000) as u16;
        Some(Self {
            book,
            chapter,
            verse,
        })
    }

    /// The `OSIS` reference string, e.g. `John.3.16`.
    #[must_use]
    pub fn osis(self) -> String {
        format!("{}.{}.{}", self.book.osis(), self.chapter, self.verse)
    }

    /// Parse a human (`John 3:16`) or `OSIS` (`John.3.16`) reference.
    pub fn parse(s: &str) -> Result<Self, RefError> {
        let s = s.trim();
        if s.is_empty() {
            return Err(RefError::Empty);
        }

        // Split the book name from the trailing chapter:verse numbers.
        let (book_part, ch, v) = if let Some((book_part, tail)) = s.rsplit_once(' ') {
            // Human form: "<book> <chapter>:<verse>".
            let (ch, v) =
                parse_chapter_verse(tail).ok_or_else(|| RefError::BadNumbers(s.into()))?;
            (book_part.to_string(), ch, v)
        } else {
            // No space: dotted OSIS form "<book>.<chapter>.<verse>".
            let mut parts = s.rsplitn(3, '.');
            let v = parts.next();
            let ch = parts.next();
            let book_part = parts.next();
            match (book_part, ch, v) {
                (Some(b), Some(c), Some(vv)) => {
                    let ch = c.parse().map_err(|_| RefError::BadNumbers(s.into()))?;
                    let v = vv.parse().map_err(|_| RefError::BadNumbers(s.into()))?;
                    (b.to_string(), ch, v)
                }
                _ => return Err(RefError::BadNumbers(s.into())),
            }
        };

        let book = Book::lookup(book_part.trim()).ok_or(RefError::UnknownBook(book_part))?;
        Ok(Self {
            book,
            chapter: ch,
            verse: v,
        })
    }
}

impl std::fmt::Display for VerseId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}:{}", self.book.name(), self.chapter, self.verse)
    }
}

/// Parse a `chapter:verse` or `chapter.verse` tail into `(chapter,
/// verse)`.
fn parse_chapter_verse(tail: &str) -> Option<(u16, u16)> {
    let (c, v) = tail.split_once(':').or_else(|| tail.split_once('.'))?;
    Some((c.trim().parse().ok()?, v.trim().parse().ok()?))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn john_3_16() -> VerseId {
        VerseId::new(Book::from_ordinal(43).unwrap(), 3, 16)
    }

    #[test]
    fn numeric_key_matches_spec() {
        assert_eq!(john_3_16().numeric(), 43_003_016);
        assert_eq!(
            VerseId::new(Book::from_ordinal(1).unwrap(), 1, 1).numeric(),
            1_001_001
        );
    }

    #[test]
    fn numeric_round_trips() {
        let v = john_3_16();
        assert_eq!(VerseId::from_numeric(v.numeric()), Some(v));
    }

    #[test]
    fn osis_string() {
        assert_eq!(john_3_16().osis(), "John.3.16");
    }

    #[test]
    fn parse_human_and_osis_forms() {
        for s in ["John 3:16", "john 3:16", "Jn 3:16", "John.3.16"] {
            assert_eq!(VerseId::parse(s).unwrap(), john_3_16(), "{s}");
        }
        // Multi-word and numbered books.
        let first_cor = VerseId::parse("1 Corinthians 13:4").unwrap();
        assert_eq!(first_cor.book.name(), "1 Corinthians");
        assert_eq!((first_cor.chapter, first_cor.verse), (13, 4));
        let song = VerseId::parse("Song of Solomon 1:1").unwrap();
        assert_eq!(song.book.name(), "Song of Solomon");
        assert_eq!(VerseId::parse("1John.2.3").unwrap().book.name(), "1 John");
    }

    #[test]
    fn parse_rejects_garbage() {
        assert_eq!(VerseId::parse(""), Err(RefError::Empty));
        assert!(matches!(
            VerseId::parse("Nope 1:1"),
            Err(RefError::UnknownBook(_))
        ));
        assert!(matches!(
            VerseId::parse("John x:y"),
            Err(RefError::BadNumbers(_))
        ));
    }

    #[test]
    fn ordering_is_canonical() {
        let mut v = [
            VerseId::parse("John 3:16").unwrap(),
            VerseId::parse("Genesis 1:1").unwrap(),
            VerseId::parse("John 3:1").unwrap(),
            VerseId::parse("Revelation 22:21").unwrap(),
        ];
        v.sort();
        assert_eq!(v[0], VerseId::parse("Genesis 1:1").unwrap());
        assert_eq!(v[1], VerseId::parse("John 3:1").unwrap());
        assert_eq!(v[2], VerseId::parse("John 3:16").unwrap());
        assert_eq!(v[3], VerseId::parse("Revelation 22:21").unwrap());
    }
}

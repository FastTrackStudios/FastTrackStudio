//! [`Bible`] — an in-memory, ordered store of one translation's verses.
//!
//! Keyed by [`VerseId`] in a `BTreeMap`, so iteration and chapter slices
//! come out in canonical order for free. Read-only after ingest: insert
//! books, then look verses up. This is the slice-1 backbone the reader
//! (slice 2) will serve over the `scripture-proto` RPC surface.

use std::collections::BTreeMap;

use scripture_proto::{Book, VerseId};

use crate::usfm::{UsfmError, parse_book};

/// One translation's verses, addressable by [`VerseId`].
#[derive(Debug, Clone, Default)]
pub struct Bible {
    /// Translation id, e.g. `WEB`.
    pub translation: String,
    verses: BTreeMap<VerseId, String>,
}

impl Bible {
    /// An empty store for the given translation id.
    #[must_use]
    pub fn new(translation: impl Into<String>) -> Self {
        Self {
            translation: translation.into(),
            verses: BTreeMap::new(),
        }
    }

    /// Parse a USFM book and merge its verses in. Returns the number of
    /// verses added.
    pub fn insert_usfm_book(&mut self, src: &str) -> Result<usize, UsfmError> {
        let parsed = parse_book(src)?;
        let n = parsed.len();
        for v in parsed {
            self.verses.insert(v.id, v.text);
        }
        Ok(n)
    }

    /// Clean reading text for one verse, if present.
    #[must_use]
    pub fn get(&self, id: VerseId) -> Option<&str> {
        self.verses.get(&id).map(String::as_str)
    }

    /// All verses of a chapter, in order, as `(verse_number, text)`.
    #[must_use]
    pub fn chapter(&self, book: Book, chapter: u16) -> Vec<(u16, &str)> {
        let lo = VerseId::new(book, chapter, 0);
        let hi = VerseId::new(book, chapter, u16::MAX);
        self.verses
            .range(lo..=hi)
            .map(|(id, text)| (id.verse, text.as_str()))
            .collect()
    }

    /// Total verses loaded.
    #[must_use]
    pub fn len(&self) -> usize {
        self.verses.len()
    }

    /// Whether any verses are loaded.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.verses.is_empty()
    }

    /// The bundled WEB Gospel of John (public domain) — slice-1's
    /// end-to-end proof. Later slices bundle the full WEB + BSB corpus
    /// from the same `assets/<translation>/<BOOK>.usfm` layout.
    ///
    /// # Panics
    /// Never in practice: the fixture is a valid USFM book checked in
    /// with the crate.
    #[must_use]
    pub fn web_sample() -> Self {
        let mut bible = Self::new("WEB");
        bible
            .insert_usfm_book(include_str!("../assets/web/JHN.usfm"))
            .expect("bundled WEB John fixture parses");
        bible
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_a_verse_end_to_end() {
        let bible = Bible::web_sample();
        let id = VerseId::parse("John 3:16").unwrap();
        let text = bible.get(id).expect("John 3:16 resolves");
        assert!(text.starts_with("For God so loved the world"));
        assert_eq!(bible.translation, "WEB");
    }

    #[test]
    fn chapter_slice_is_ordered_and_contiguous() {
        let bible = Bible::web_sample();
        let john = Book::lookup("John").unwrap();
        let ch3 = bible.chapter(john, 3);
        assert!(!ch3.is_empty());
        // First verse is 1; verse numbers ascend.
        assert_eq!(ch3[0].0, 1);
        assert!(ch3.windows(2).all(|w| w[0].0 < w[1].0));
    }

    #[test]
    fn unknown_verse_is_none() {
        let bible = Bible::web_sample();
        // Genesis isn't loaded in the sample.
        assert!(bible.get(VerseId::parse("Genesis 1:1").unwrap()).is_none());
    }
}

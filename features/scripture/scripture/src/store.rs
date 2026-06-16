//! [`Store`] — the in-process [`ScriptureService`] backend.
//!
//! Holds every installed translation's [`Bible`] in memory, keyed by id,
//! and answers the read-only reader queries (translations / chapter /
//! verse). Loaded once from the resource library
//! (`<org>/resources/bible/<TX>/`) at startup; immutable thereafter, so
//! no lock is needed — the scripture spine is read-only.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use scripture_proto::{
    Book, ChapterView, ScriptureError, ScriptureService, Translation, TranslationInfo, VerseId,
    VerseLine,
};

use crate::bible::{Bible, LoadError};

/// Read-only scripture backend: translation id → [`Bible`].
#[derive(Clone, architect::HasDispatcher)]
pub struct Store {
    bibles: Arc<BTreeMap<String, Bible>>,
}

impl Store {
    /// Build from already-loaded bibles (tests, custom wiring).
    #[must_use]
    pub fn from_bibles(bibles: impl IntoIterator<Item = Bible>) -> Self {
        let map = bibles
            .into_iter()
            .map(|b| (b.translation.clone(), b))
            .collect();
        Self {
            bibles: Arc::new(map),
        }
    }

    /// Load every translation subdirectory of a Bible resource root
    /// (e.g. `<org>/resources/bible/`). Each immediate subdirectory is a
    /// translation whose folder name is its id (`WEB`, `BSB`). A missing
    /// root yields an empty store rather than an error — the reader just
    /// shows no translations until a corpus is installed.
    pub fn load_resource_root(bible_root: &Path) -> Result<Self, LoadError> {
        let mut bibles = Vec::new();
        let entries = match std::fs::read_dir(bible_root) {
            Ok(e) => e,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Self::from_bibles([])),
            Err(source) => {
                return Err(LoadError::Io {
                    path: bible_root.display().to_string(),
                    source,
                });
            }
        };
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let id = entry.file_name().to_string_lossy().into_owned();
            bibles.push(Bible::load_dir(&path, id)?);
        }
        Ok(Self::from_bibles(bibles))
    }

    fn bible(&self, translation: &str) -> Result<&Bible, ScriptureError> {
        self.bibles
            .get(translation)
            .ok_or_else(|| ScriptureError::NotFound(format!("translation {translation:?}")))
    }
}

impl ScriptureService for Store {
    fn translations(&self) -> Result<Vec<TranslationInfo>, ScriptureError> {
        let mut out: Vec<TranslationInfo> = self
            .bibles
            .keys()
            .map(|id| {
                // Enrich from the licensing registry when known.
                Translation::lookup(id).map_or_else(
                    || TranslationInfo {
                        id: id.clone(),
                        name: id.clone(),
                        license: String::new(),
                        bundled: true,
                    },
                    |t| TranslationInfo {
                        id: id.clone(),
                        name: t.name.to_string(),
                        license: t.license.to_string(),
                        bundled: t.is_bundled(),
                    },
                )
            })
            .collect();
        // Bundled first, then alphabetical by id.
        out.sort_by(|a, b| b.bundled.cmp(&a.bundled).then_with(|| a.id.cmp(&b.id)));
        Ok(out)
    }

    fn chapter(
        &self,
        translation: &str,
        book: &str,
        chapter: u16,
    ) -> Result<ChapterView, ScriptureError> {
        let bible = self.bible(translation)?;
        let book = Book::lookup(book)
            .ok_or_else(|| ScriptureError::BadRequest(format!("unknown book {book:?}")))?;
        let verses: Vec<VerseLine> = bible
            .chapter(book, chapter)
            .into_iter()
            .map(|(verse, text)| VerseLine {
                verse,
                osis: VerseId::new(book, chapter, verse).osis(),
                text: text.to_string(),
            })
            .collect();
        if verses.is_empty() {
            return Err(ScriptureError::NotFound(format!(
                "{} {} {chapter}",
                bible.translation,
                book.name()
            )));
        }
        Ok(ChapterView {
            translation: bible.translation.clone(),
            book_osis: book.osis().to_string(),
            book_name: book.name().to_string(),
            book_ordinal: book.ordinal(),
            chapter,
            chapter_count: book.chapters(),
            verses,
        })
    }

    fn verse(&self, translation: &str, reference: &str) -> Result<VerseLine, ScriptureError> {
        let bible = self.bible(translation)?;
        let id = VerseId::parse(reference)
            .map_err(|e| ScriptureError::BadRequest(format!("{reference:?}: {e}")))?;
        let text = bible
            .get(id)
            .ok_or_else(|| ScriptureError::NotFound(format!("{reference} in {translation}")))?;
        Ok(VerseLine {
            verse: id.verse,
            osis: id.osis(),
            text: text.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn store() -> Store {
        let mut web = Bible::new("WEB");
        web.insert_usfm_book(crate::usfm::tests::SAMPLE).unwrap();
        Store::from_bibles([web])
    }

    #[test]
    fn lists_translations() {
        let infos = store().translations().unwrap();
        assert_eq!(infos.len(), 1);
        assert_eq!(infos[0].id, "WEB");
        assert_eq!(infos[0].name, "World English Bible");
        assert!(infos[0].bundled);
    }

    #[test]
    fn serves_a_chapter() {
        let ch = store().chapter("WEB", "John", 3).unwrap();
        assert_eq!(ch.book_name, "John");
        assert_eq!(ch.book_ordinal, 43);
        assert_eq!(ch.chapter_count, 21);
        assert_eq!(ch.verses[0].verse, 16);
        assert_eq!(ch.verses[0].osis, "John.3.16");
        assert!(ch.verses[0].text.starts_with("For God so loved"));
    }

    #[test]
    fn serves_a_verse_and_reports_errors() {
        let s = store();
        assert!(
            s.verse("WEB", "John 3:16")
                .unwrap()
                .text
                .starts_with("For God")
        );
        assert!(matches!(
            s.verse("NIV", "John 3:16"),
            Err(ScriptureError::NotFound(_))
        ));
        assert!(matches!(
            s.chapter("WEB", "Nope", 1),
            Err(ScriptureError::BadRequest(_))
        ));
        assert!(matches!(
            s.chapter("WEB", "Genesis", 1),
            Err(ScriptureError::NotFound(_))
        ));
    }
}

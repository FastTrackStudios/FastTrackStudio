//! [`Store`] — the in-process [`ScriptureService`] backend.
//!
//! Holds every installed translation's [`Bible`] in memory, keyed by id,
//! and answers the read-only reader queries (translations / chapter /
//! verse). Loaded once from the resource library
//! (`<org>/resources/bible/<TX>/`) at startup; immutable thereafter, so
//! no lock is needed — the scripture spine is read-only.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use scripture_proto::{
    Book, ChapterView, ScriptureError, ScriptureService, Translation, TranslationInfo,
    VerseBacklink, VerseBacklinks, VerseId, VerseLine,
};

use crate::bible::{Bible, LoadError};

/// Read-only scripture backend: translation id → [`Bible`], plus an
/// optional vault root for verse backlinks.
#[derive(Clone, architect::HasDispatcher)]
pub struct Store {
    bibles: Arc<BTreeMap<String, Bible>>,
    /// Vault to scan for `[[John 3:16]]` backlinks. `None` ⇒ the reader
    /// just shows no backlinks.
    vault_root: Option<PathBuf>,
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
            vault_root: None,
        }
    }

    /// Point the store at a vault root so [`ScriptureService::chapter_backlinks`]
    /// can surface notes that link verses.
    #[must_use]
    pub fn with_vault(mut self, vault_root: impl Into<PathBuf>) -> Self {
        self.vault_root = Some(vault_root.into());
        self
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

    fn chapter_backlinks(
        &self,
        book: &str,
        chapter: u16,
    ) -> Result<Vec<VerseBacklinks>, ScriptureError> {
        let book = Book::lookup(book)
            .ok_or_else(|| ScriptureError::BadRequest(format!("unknown book {book:?}")))?;
        let Some(vault_root) = self.vault_root.as_deref() else {
            return Ok(Vec::new());
        };
        // Numeric base for this chapter; a verse `v` is `base + v`.
        let base = u32::from(book.ordinal()) * 1_000_000 + u32::from(chapter) * 1_000;

        // verse number → (notes, note paths already seen for dedup).
        let mut per_verse: BTreeMap<u16, (Vec<VerseBacklink>, BTreeSet<String>)> = BTreeMap::new();
        for rb in crate::backlinks::scan_vault(vault_root) {
            // Intersect the referenced range with this chapter.
            let lo = rb.range.start.numeric().max(base + 1);
            let hi = rb.range.end.numeric().min(base + 999);
            for n in lo..=hi {
                let verse = (n - base) as u16;
                let entry = per_verse.entry(verse).or_default();
                if entry.1.insert(rb.link.note_path.clone()) {
                    entry.0.push(rb.link.clone());
                }
            }
        }

        Ok(per_verse
            .into_iter()
            .map(|(verse, (notes, _))| VerseBacklinks {
                verse,
                osis: VerseId::new(book, chapter, verse).osis(),
                notes,
            })
            .collect())
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
    fn chapter_backlinks_from_a_vault() {
        let vault = tempfile::tempdir().unwrap();
        std::fs::write(
            vault.path().join("note.md"),
            "# Grace\nSee [[John 3:16]] and [[John 3:17]].\n",
        )
        .unwrap();
        let mut web = Bible::new("WEB");
        web.insert_usfm_book(crate::usfm::tests::SAMPLE).unwrap();
        let s = Store::from_bibles([web]).with_vault(vault.path().to_path_buf());

        let bl = s.chapter_backlinks("John", 3).unwrap();
        let verses: Vec<u16> = bl.iter().map(|b| b.verse).collect();
        assert_eq!(verses, vec![16, 17]);
        assert_eq!(bl[0].osis, "John.3.16");
        assert_eq!(bl[0].notes.len(), 1);
        assert_eq!(bl[0].notes[0].note_title, "Grace");
        // A chapter nobody links to comes back empty.
        assert!(s.chapter_backlinks("John", 4).unwrap().is_empty());
    }

    #[test]
    fn range_links_backlink_every_covered_verse() {
        let vault = tempfile::tempdir().unwrap();
        std::fs::write(
            vault.path().join("structure.md"),
            "# Discourse\nStructural unit: [[John 3:16-18]].\n",
        )
        .unwrap();
        let mut web = Bible::new("WEB");
        web.insert_usfm_book(crate::usfm::tests::SAMPLE).unwrap();
        let s = Store::from_bibles([web]).with_vault(vault.path().to_path_buf());

        let bl = s.chapter_backlinks("John", 3).unwrap();
        // The span [[John 3:16-18]] surfaces on 16, 17, and 18.
        assert_eq!(
            bl.iter().map(|b| b.verse).collect::<Vec<_>>(),
            vec![16, 17, 18]
        );
        assert!(
            bl.iter()
                .all(|b| b.notes.len() == 1 && b.notes[0].note_title == "Discourse")
        );
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

//! [`Store`] — the [`ScriptureService`] backend.
//!
//! Two sources of text: **bundled** editions held in memory (loaded from
//! the resource library, `<org>/resources/bible/<TX>/`) and **API**
//! editions (ESV / NIV) fetched live over HTTP with the user's key (see
//! [`crate::api`]). Reads route to whichever owns the translation;
//! `compare` happily mixes the two. Bundled text is immutable, so the
//! in-memory side needs no lock.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use scripture_proto::{
    Book, ChapterView, ComparisonRow, ComparisonView, ScriptureError, ScriptureService,
    Translation, TranslationInfo, VerseBacklink, VerseBacklinks, VerseId, VerseLine, VerseRange,
};

use crate::api::{ApiTranslation, fetch_chapter};
use crate::bible::{Bible, LoadError};

/// Read-only scripture backend.
#[derive(Clone, architect::HasDispatcher)]
pub struct Store {
    bibles: Arc<BTreeMap<String, Bible>>,
    /// Copyright-restricted editions fetched over HTTP.
    api: Arc<Vec<ApiTranslation>>,
    http: reqwest::Client,
    /// Vault to scan for `[[John 3:16]]` backlinks. `None` ⇒ no backlinks.
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
            api: Arc::new(Vec::new()),
            http: reqwest::Client::new(),
            vault_root: None,
        }
    }

    /// Register API-backed editions (ESV / NIV).
    #[must_use]
    pub fn with_api(mut self, api: impl IntoIterator<Item = ApiTranslation>) -> Self {
        self.api = Arc::new(api.into_iter().collect());
        self
    }

    /// Point the store at a vault root so [`ScriptureService::chapter_backlinks`]
    /// can surface notes that link verses.
    #[must_use]
    pub fn with_vault(mut self, vault_root: impl Into<PathBuf>) -> Self {
        self.vault_root = Some(vault_root.into());
        self
    }

    /// Load every translation subdirectory of a Bible resource root
    /// (e.g. `<org>/resources/bible/`). A missing root yields an empty
    /// store rather than an error.
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

    fn api_translation(&self, id: &str) -> Option<&ApiTranslation> {
        self.api.iter().find(|t| t.id.eq_ignore_ascii_case(id))
    }

    /// Bundled chapter, synchronously (no network).
    fn chapter_local(&self, bible: &Bible, book: Book, chapter: u16) -> Vec<VerseLine> {
        bible
            .chapter(book, chapter)
            .into_iter()
            .map(|(verse, text)| VerseLine {
                verse,
                osis: VerseId::new(book, chapter, verse).osis(),
                text: text.to_string(),
            })
            .collect()
    }

    /// Verses of `tx` within `start..=end`, from whichever source owns it.
    async fn verses_for(
        &self,
        tx: &str,
        start: VerseId,
        end: VerseId,
    ) -> Result<Vec<(VerseId, String)>, ScriptureError> {
        if let Some(bible) = self.bibles.get(tx) {
            return Ok(bible
                .verses_in_range(start, end)
                .into_iter()
                .map(|(id, t)| (id, t.to_string()))
                .collect());
        }
        if let Some(api) = self.api_translation(tx) {
            let mut out = Vec::new();
            for (book, chapter) in spanned_chapters(start, end) {
                for line in fetch_chapter(&self.http, api, book, chapter).await? {
                    let id = VerseId::new(book, chapter, line.verse);
                    if start.numeric() <= id.numeric() && id.numeric() <= end.numeric() {
                        out.push((id, line.text));
                    }
                }
            }
            return Ok(out);
        }
        Err(ScriptureError::NotFound(format!("translation {tx:?}")))
    }
}

impl ScriptureService for Store {
    fn translations(&self) -> Result<Vec<TranslationInfo>, ScriptureError> {
        let mut out: Vec<TranslationInfo> = self
            .bibles
            .keys()
            .map(|id| {
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
        // API editions are never bundled.
        out.extend(self.api.iter().map(|t| TranslationInfo {
            id: t.id.clone(),
            name: t.name.clone(),
            license: t.license.clone(),
            bundled: false,
        }));
        // Bundled first, then alphabetical by id.
        out.sort_by(|a, b| b.bundled.cmp(&a.bundled).then_with(|| a.id.cmp(&b.id)));
        out.dedup_by(|a, b| a.id.eq_ignore_ascii_case(&b.id));
        Ok(out)
    }

    async fn chapter(
        &self,
        translation: &str,
        book: &str,
        chapter: u16,
    ) -> Result<ChapterView, ScriptureError> {
        let book = Book::lookup(book)
            .ok_or_else(|| ScriptureError::BadRequest(format!("unknown book {book:?}")))?;

        let (tx_id, verses) = if let Some(bible) = self.bibles.get(translation) {
            (
                bible.translation.clone(),
                self.chapter_local(bible, book, chapter),
            )
        } else if let Some(api) = self.api_translation(translation) {
            (
                api.id.clone(),
                fetch_chapter(&self.http, api, book, chapter).await?,
            )
        } else {
            return Err(ScriptureError::NotFound(format!(
                "translation {translation:?}"
            )));
        };

        if verses.is_empty() {
            return Err(ScriptureError::NotFound(format!(
                "{tx_id} {} {chapter}",
                book.name()
            )));
        }
        Ok(ChapterView {
            translation: tx_id,
            book_osis: book.osis().to_string(),
            book_name: book.name().to_string(),
            book_ordinal: book.ordinal(),
            chapter,
            chapter_count: book.chapters(),
            verses,
        })
    }

    async fn verse(&self, translation: &str, reference: &str) -> Result<VerseLine, ScriptureError> {
        let id = VerseId::parse(reference)
            .map_err(|e| ScriptureError::BadRequest(format!("{reference:?}: {e}")))?;
        self.verses_for(translation, id, id)
            .await?
            .into_iter()
            .next()
            .map(|(vid, text)| VerseLine {
                verse: vid.verse,
                osis: vid.osis(),
                text,
            })
            .ok_or_else(|| ScriptureError::NotFound(format!("{reference} in {translation}")))
    }

    async fn compare(
        &self,
        reference: &str,
        translations: Vec<String>,
    ) -> Result<ComparisonView, ScriptureError> {
        let range = VerseRange::parse(reference)
            .map_err(|e| ScriptureError::BadRequest(format!("{reference:?}: {e}")))?;

        // Columns: requested-and-installed (in request order), else every
        // edition (bundled-first).
        let cols: Vec<String> = if translations.is_empty() {
            self.translations()?.into_iter().map(|t| t.id).collect()
        } else {
            translations
                .into_iter()
                .map(|t| t.to_ascii_uppercase())
                .filter(|id| self.bibles.contains_key(id) || self.api_translation(id).is_some())
                .collect()
        };
        if cols.is_empty() {
            return Err(ScriptureError::NotFound(
                "no matching translations installed".into(),
            ));
        }

        // Per-column verse map, then union the verse ids.
        let mut col_maps: Vec<BTreeMap<VerseId, String>> = Vec::with_capacity(cols.len());
        let mut ids = BTreeSet::new();
        for tx in &cols {
            let map: BTreeMap<VerseId, String> = self
                .verses_for(tx, range.start, range.end)
                .await?
                .into_iter()
                .collect();
            ids.extend(map.keys().copied());
            col_maps.push(map);
        }

        let rows = ids
            .into_iter()
            .map(|id| ComparisonRow {
                reference: id.to_string(),
                osis: id.osis(),
                cells: col_maps
                    .iter()
                    .map(|m| m.get(&id).cloned().unwrap_or_default())
                    .collect(),
            })
            .collect();

        Ok(ComparisonView {
            reference: range.to_string(),
            osis: range.osis(),
            translations: cols,
            rows,
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
        let base = u32::from(book.ordinal()) * 1_000_000 + u32::from(chapter) * 1_000;

        let mut per_verse: BTreeMap<u16, (Vec<VerseBacklink>, BTreeSet<String>)> = BTreeMap::new();
        for rb in crate::backlinks::scan_vault(vault_root) {
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

/// Every `(book, chapter)` whose verses intersect `start..=end`,
/// in canonical order — used to fan range fetches over an API edition.
fn spanned_chapters(start: VerseId, end: VerseId) -> Vec<(Book, u16)> {
    let mut out = Vec::new();
    for ord in start.book.ordinal()..=end.book.ordinal() {
        let Some(book) = Book::from_ordinal(ord) else {
            continue;
        };
        let first = if ord == start.book.ordinal() {
            start.chapter
        } else {
            1
        };
        let last = if ord == end.book.ordinal() {
            end.chapter
        } else {
            u16::from(book.chapters())
        };
        for ch in first..=last {
            out.push((book, ch));
        }
    }
    out
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
    fn api_editions_listed_unbundled() {
        let s = store().with_api([ApiTranslation::esv("k")]);
        let infos = s.translations().unwrap();
        let esv = infos.iter().find(|t| t.id == "ESV").unwrap();
        assert!(!esv.bundled);
        // Bundled WEB sorts before API ESV.
        assert_eq!(infos[0].id, "WEB");
    }

    #[tokio::test]
    async fn serves_a_chapter() {
        let ch = store().chapter("WEB", "John", 3).await.unwrap();
        assert_eq!(ch.book_name, "John");
        assert_eq!(ch.book_ordinal, 43);
        assert_eq!(ch.chapter_count, 21);
        assert_eq!(ch.verses[0].verse, 16);
        assert_eq!(ch.verses[0].osis, "John.3.16");
        assert!(ch.verses[0].text.starts_with("For God so loved"));
    }

    #[tokio::test]
    async fn serves_a_verse_and_reports_errors() {
        let s = store();
        assert!(
            s.verse("WEB", "John 3:16")
                .await
                .unwrap()
                .text
                .starts_with("For God")
        );
        assert!(matches!(
            s.verse("NIV", "John 3:16").await,
            Err(ScriptureError::NotFound(_))
        ));
        assert!(matches!(
            s.chapter("WEB", "Nope", 1).await,
            Err(ScriptureError::BadRequest(_))
        ));
        assert!(matches!(
            s.chapter("WEB", "Genesis", 1).await,
            Err(ScriptureError::NotFound(_))
        ));
    }

    #[tokio::test]
    async fn compare_across_translations() {
        let mut web = Bible::new("WEB");
        web.insert_usfm_book("\\id JHN\n\\c 3\n\\v 16 web sixteen\n\\v 17 web seventeen\n")
            .unwrap();
        let mut bsb = Bible::new("BSB");
        bsb.insert_usfm_book("\\id JHN\n\\c 3\n\\v 16 bsb sixteen\n")
            .unwrap();
        let s = Store::from_bibles([web, bsb]);

        let view = s
            .compare("John 3:16-17", vec!["WEB".into(), "BSB".into()])
            .await
            .unwrap();
        assert_eq!(view.translations, ["WEB", "BSB"]);
        assert_eq!(view.rows.len(), 2);
        assert_eq!(view.rows[0].reference, "John 3:16");
        assert_eq!(view.rows[0].cells, ["web sixteen", "bsb sixteen"]);
        assert_eq!(view.rows[1].cells, ["web seventeen", ""]);

        assert_eq!(
            s.compare("John 3:16", vec![])
                .await
                .unwrap()
                .translations
                .len(),
            2
        );
        assert!(matches!(
            s.compare("John 3:16", vec!["NIV".into()]).await,
            Err(ScriptureError::NotFound(_))
        ));
    }

    #[test]
    fn spanned_chapters_cover_cross_book() {
        let chapters = spanned_chapters(
            VerseId::parse("Genesis 50:1").unwrap(),
            VerseId::parse("Exodus 2:3").unwrap(),
        );
        // Genesis 50, then Exodus 1 and 2.
        assert_eq!(chapters.len(), 3);
        assert_eq!(chapters[0], (Book::lookup("Genesis").unwrap(), 50));
        assert_eq!(chapters[2], (Book::lookup("Exodus").unwrap(), 2));
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
        assert_eq!(bl.iter().map(|b| b.verse).collect::<Vec<_>>(), vec![16, 17]);
        assert_eq!(bl[0].osis, "John.3.16");
        assert_eq!(bl[0].notes[0].note_title, "Grace");
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
        assert_eq!(
            bl.iter().map(|b| b.verse).collect::<Vec<_>>(),
            vec![16, 17, 18]
        );
        assert!(
            bl.iter()
                .all(|b| b.notes.len() == 1 && b.notes[0].note_title == "Discourse")
        );
    }
}

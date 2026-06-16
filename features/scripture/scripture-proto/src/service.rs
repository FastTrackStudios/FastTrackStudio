//! `ScriptureService` — the read-only wire surface the reader binds to.
//!
//! Decorated with `#[architect::rpc]`: the backend (`scripture::Store`)
//! implements the sync trait directly for zero-cost in-process calls, and
//! the web reader reaches the same surface via the auto-emitted vox
//! client. All wire types are plain `Facet` DTOs with primitive fields —
//! the pure [`crate::VerseId`] / [`crate::Book`] logic types stay
//! server-side; the wire carries strings and numbers.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum ScriptureError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("bad request: {0}")]
    BadRequest(String),
}

/// One installed translation, for the picker.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct TranslationInfo {
    /// Short id, e.g. `WEB`.
    pub id: String,
    /// Full name, e.g. `World English Bible`.
    pub name: String,
    /// Short license summary.
    pub license: String,
    /// Bundled offline (vs API-fetched).
    pub bundled: bool,
}

/// One verse line in a rendered chapter.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct VerseLine {
    pub verse: u16,
    /// Stable OSIS id, e.g. `John.3.16` — the link/permalink key.
    pub osis: String,
    /// Clean reading text.
    pub text: String,
}

/// One chapter of one translation, ready to render.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct ChapterView {
    pub translation: String,
    /// OSIS book abbreviation, e.g. `John`.
    pub book_osis: String,
    /// Display name, e.g. `John`.
    pub book_name: String,
    /// Canonical book ordinal, `1..=66`.
    pub book_ordinal: u8,
    pub chapter: u16,
    /// Total chapters in this book (for prev/next nav).
    pub chapter_count: u8,
    pub verses: Vec<VerseLine>,
}

/// One vault note that links to a verse, with context.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct VerseBacklink {
    /// Vault-relative path of the linking note.
    pub note_path: String,
    /// Display title (first heading, else file stem).
    pub note_title: String,
    /// The line the link appears on, trimmed — the "inline thesis".
    pub excerpt: String,
}

/// Backlinks for one verse — every vault note that references it.
///
/// Translation-independent: keyed by the verse address, so a note that
/// links `John 3:16` shows up no matter which edition you're reading.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct VerseBacklinks {
    pub verse: u16,
    pub osis: String,
    pub notes: Vec<VerseBacklink>,
}

#[architect::rpc]
pub trait ScriptureService {
    /// Installed translations, bundled editions first.
    fn translations(&self) -> Result<Vec<TranslationInfo>, ScriptureError>;

    /// One chapter. `book` accepts any spelling (name / OSIS / USFM id /
    /// common abbreviation); `BadRequest` if it doesn't resolve to a
    /// canonical book, `NotFound` if the translation or chapter is absent.
    fn chapter(
        &self,
        translation: &str,
        book: &str,
        chapter: u16,
    ) -> Result<ChapterView, ScriptureError>;

    /// A single verse. `reference` is a human or OSIS reference such as
    /// `John 3:16` or `John.3.16`.
    fn verse(&self, translation: &str, reference: &str) -> Result<VerseLine, ScriptureError>;

    /// Per-verse backlinks for a whole chapter: every vault note that
    /// links a verse in `book` `chapter` via a `[[John 3:16]]`-style
    /// wikilink. Only verses with at least one backlink are returned.
    /// Translation-independent (keyed by verse address).
    fn chapter_backlinks(
        &self,
        book: &str,
        chapter: u16,
    ) -> Result<Vec<VerseBacklinks>, ScriptureError>;
}

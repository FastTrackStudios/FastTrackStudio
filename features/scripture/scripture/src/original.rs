//! Original-language editions — the Hebrew/Greek text with per-word
//! lemma, Strong's, morphology and gloss.
//!
//! Every source (STEPBible TAGNT/TAHOT, SBLGNT, OSHB) normalizes into one
//! schema — [`OrigWord`] — so nothing downstream cares where a word came
//! from. On disk an edition is a directory in the resource library,
//! `<org>/resources/original/<EDITION>/`, holding `text.jsonl` (one verse
//! per line) plus `meta.json`. JSONL keeps the data human-inspectable and
//! quick to load.

use std::collections::BTreeMap;
use std::path::Path;

use scripture_proto::VerseId;
use serde::{Deserialize, Serialize};

use crate::bible::LoadError;

/// One original-language word, fully normalized.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrigWord {
    /// Surface form as printed — Greek, or pointed Hebrew.
    pub word: String,
    /// Transliteration.
    #[serde(default)]
    pub translit: String,
    /// Dictionary headword (lemma).
    #[serde(default)]
    pub lemma: String,
    /// Strong's code, source-faithful (`G0976`, `H7225`, possibly with a
    /// disambiguating suffix). Normalize at lexicon-lookup time.
    #[serde(default)]
    pub strong: String,
    /// Morphology / parsing code (source-specific scheme).
    #[serde(default)]
    pub morph: String,
    /// Short English gloss.
    #[serde(default)]
    pub gloss: String,
}

/// One verse's worth of original-language words.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrigVerse {
    /// OSIS id, e.g. `John.3.16`.
    pub osis: String,
    pub words: Vec<OrigWord>,
}

/// Edition metadata (`meta.json`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrigMeta {
    pub id: String,
    pub name: String,
    pub language: String,
    pub license: String,
}

/// One loaded original-language edition, addressable by [`VerseId`].
#[derive(Debug, Clone, Default)]
pub struct OrigText {
    pub meta: Option<OrigMeta>,
    verses: BTreeMap<VerseId, Vec<OrigWord>>,
}

impl OrigText {
    /// Build from verses in memory (install / tests).
    #[must_use]
    pub fn from_verses(verses: impl IntoIterator<Item = (VerseId, Vec<OrigWord>)>) -> Self {
        Self {
            meta: None,
            verses: verses.into_iter().collect(),
        }
    }

    /// Read just an edition's `meta.json` (cheap — no `text.jsonl`).
    /// Used to list editions without loading their words.
    #[must_use]
    pub fn read_meta(dir: &Path) -> Option<OrigMeta> {
        std::fs::read_to_string(dir.join("meta.json"))
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
    }

    /// Load an edition directory (`text.jsonl` + optional `meta.json`).
    pub fn load_dir(dir: &Path) -> Result<Self, LoadError> {
        let jsonl = dir.join("text.jsonl");
        let text = std::fs::read_to_string(&jsonl).map_err(|source| LoadError::Io {
            path: jsonl.display().to_string(),
            source,
        })?;
        let mut verses = BTreeMap::new();
        for line in text.lines().filter(|l| !l.trim().is_empty()) {
            let v: OrigVerse = serde_json::from_str(line).map_err(|e| LoadError::Io {
                path: jsonl.display().to_string(),
                source: std::io::Error::new(std::io::ErrorKind::InvalidData, e),
            })?;
            if let Ok(id) = VerseId::parse(&v.osis) {
                verses.insert(id, v.words);
            }
        }
        let meta = std::fs::read_to_string(dir.join("meta.json"))
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok());
        Ok(Self { meta, verses })
    }

    /// The words of a verse (empty if absent).
    #[must_use]
    pub fn words_of(&self, id: VerseId) -> &[OrigWord] {
        self.verses.get(&id).map_or(&[], Vec::as_slice)
    }

    /// Number of verses loaded.
    #[must_use]
    pub fn len(&self) -> usize {
        self.verses.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.verses.is_empty()
    }

    /// Serialize to `text.jsonl` form (one verse per line), in canonical
    /// verse order. Used by the installer.
    #[must_use]
    pub fn to_jsonl(&self) -> String {
        let mut out = String::new();
        for (id, words) in &self.verses {
            let v = OrigVerse {
                osis: id.osis(),
                words: words.clone(),
            };
            if let Ok(line) = serde_json::to_string(&v) {
                out.push_str(&line);
                out.push('\n');
            }
        }
        out
    }
}

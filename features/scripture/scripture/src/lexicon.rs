//! Strong's lexicon — what each Strong's number means.
//!
//! Loaded from the resource library
//! (`<org>/resources/lexicon/strongs/{greek,hebrew}.json`), in the
//! public-domain OpenScriptures shape: a JSON object keyed by Strong's
//! code (`G25`) whose values carry `lemma` / `translit` / `strongs_def`
//! / `kjv_def` / `derivation`.
//!
//! OpenScriptures ships the data as a `.js` module; [`js_to_json`]
//! extracts the embedded object so the installer can write plain JSON.

use std::collections::BTreeMap;
use std::path::Path;

use scripture_proto::LexiconEntry;
use serde::Deserialize;

use crate::bible::LoadError;

/// Raw OpenScriptures entry (every field optional / defaulted).
#[derive(Debug, Default, Deserialize)]
struct RawEntry {
    #[serde(default)]
    lemma: String,
    #[serde(default)]
    translit: String,
    #[serde(default)]
    strongs_def: String,
    #[serde(default)]
    kjv_def: String,
    #[serde(default)]
    derivation: String,
}

/// Strong's code → [`LexiconEntry`].
#[derive(Debug, Clone, Default)]
pub struct Lexicon {
    entries: BTreeMap<String, LexiconEntry>,
}

impl Lexicon {
    /// Load `greek.json` + `hebrew.json` from a Strong's lexicon
    /// directory. Missing files are skipped; a missing directory yields
    /// an empty lexicon.
    pub fn load_dir(dir: &Path) -> Result<Self, LoadError> {
        let mut entries = BTreeMap::new();
        for name in ["greek.json", "hebrew.json"] {
            let path = dir.join(name);
            match std::fs::read_to_string(&path) {
                Ok(text) => load_json(&text, &mut entries)?,
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(source) => {
                    return Err(LoadError::Io {
                        path: path.display().to_string(),
                        source,
                    });
                }
            }
        }
        Ok(Self { entries })
    }

    /// Build directly from already-parsed JSON text (tests / custom).
    pub fn from_json(text: &str) -> Result<Self, LoadError> {
        let mut entries = BTreeMap::new();
        load_json(text, &mut entries)?;
        Ok(Self { entries })
    }

    /// Look up one code (case-insensitive on the `G`/`H` prefix isn't
    /// needed — codes are stored as upper-case `G25`).
    #[must_use]
    pub fn get(&self, strongs: &str) -> Option<&LexiconEntry> {
        self.entries.get(strongs.trim())
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

fn load_json(text: &str, out: &mut BTreeMap<String, LexiconEntry>) -> Result<(), LoadError> {
    let raw: BTreeMap<String, RawEntry> =
        serde_json::from_str(text).map_err(|e| LoadError::Io {
            path: "<lexicon json>".into(),
            source: std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        })?;
    for (code, e) in raw {
        out.insert(
            code.clone(),
            LexiconEntry {
                strongs: code,
                lemma: e.lemma,
                translit: e.translit,
                definition: e.strongs_def.trim().to_string(),
                kjv_def: e.kjv_def,
                derivation: e.derivation,
            },
        );
    }
    Ok(())
}

/// Extract the embedded JSON object from an OpenScriptures `.js` module
/// (`… var x = { … }; module.exports = x;`). Returns the `{ … }` slice.
#[must_use]
pub fn js_to_json(js: &str) -> Option<&str> {
    let start = js.find('{')?;
    let end = js.rfind('}')?;
    (end >= start).then(|| &js[start..=end])
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"{
        "G25": {"derivation":"of uncertain affinity;","strongs_def":" to love","kjv_def":"(be-)love","lemma":"ἀγαπάω","translit":"agapáō"},
        "G26": {"strongs_def":" love","derivation":"from G25;","translit":"agápē","lemma":"ἀγάπη","kjv_def":"love"}
    }"#;

    #[test]
    fn loads_and_looks_up() {
        let lex = Lexicon::from_json(SAMPLE).unwrap();
        assert_eq!(lex.len(), 2);
        let g26 = lex.get("G26").unwrap();
        assert_eq!(g26.lemma, "ἀγάπη");
        assert_eq!(g26.translit, "agápē");
        assert_eq!(g26.definition, "love");
        assert!(lex.get("G999").is_none());
    }

    #[test]
    fn strips_js_wrapper() {
        let js = "/* header */ var d = {\"G25\":{\"lemma\":\"x\"}}; module.exports = d;";
        let json = js_to_json(js).unwrap();
        assert!(json.starts_with('{') && json.ends_with('}'));
        assert!(Lexicon::from_json(json).is_ok());
    }
}

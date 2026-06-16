//! Minimal USFM ingest — enough to lift clean verse text out of
//! eBible-style USFM (the format WEB/BSB ship in).
//!
//! USFM is line-oriented backslash markup. We track the current chapter
//! (`\c N`), open a verse at each `\v N`, accumulate its raw span, then
//! strip the inline markup down to plain reading text. The markup we
//! handle, in the order it's removed:
//!
//! - footnotes `\f … \f*` and cross-references `\x … \x*` — dropped whole;
//! - word tags `\w word|strong="G…"\w*` (and nested `\+w …\+w*`) — the
//!   word is kept, the Strong's attribute dropped *(slice 1 keeps reading
//!   text; the lemma/Strong's data is the slice-4 word-study hook —
//!   see `// FUTURE` below)*;
//! - remaining character/paragraph markers (`\wj`, `\nd`, `\p`, `\q1`, …)
//!   — markers removed, inner text kept.
//!
//! FUTURE: a richer `parse_words` that retains each `\w` token's
//! `strong="…"` lemma so word-level study (Strong's → lexicon →
//! every-occurrence concordance) can be built on the same ingest. The
//! data is already in the WEB USFM; slice 1 just doesn't surface it.

use std::sync::LazyLock;

use regex::Regex;
use scripture_proto::{Book, VerseId};

/// One parsed verse: its stable id and clean reading text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Verse {
    pub id: VerseId,
    pub text: String,
}

/// Why USFM ingest failed.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum UsfmError {
    #[error("no \\id line found")]
    NoId,
    #[error("unknown book id: {0:?}")]
    UnknownBook(String),
}

/// Parse one USFM book into its verses, in canonical order.
///
/// The book is taken from the `\id` line (e.g. `\id JHN …`). Verses with
/// no chapter context (front matter before the first `\c`) are skipped.
pub fn parse_book(src: &str) -> Result<Vec<Verse>, UsfmError> {
    let book = detect_book(src)?;

    let mut verses = Vec::new();
    let mut chapter: u16 = 0;
    // (verse number, raw accumulated span)
    let mut current: Option<(u16, String)> = None;

    for line in src.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("\\c ") {
            flush(&mut verses, book, chapter, current.take());
            if let Some(n) = leading_u16(rest.trim()) {
                chapter = n;
            }
        } else if let Some(rest) = line.strip_prefix("\\v ") {
            flush(&mut verses, book, chapter, current.take());
            let (num, text) = rest.split_once(' ').unwrap_or((rest, ""));
            if let Some(n) = leading_u16(num) {
                current = Some((n, text.to_string()));
            }
        } else if let Some((_, buf)) = current.as_mut() {
            // Continuation: poetry / paragraph-marker lines inside a
            // verse. Keep them; the marker stripper cleans them out.
            buf.push(' ');
            buf.push_str(line);
        }
        // Header lines before the first verse (\id, \h, \toc, \mt, …)
        // fall through and are ignored.
    }
    flush(&mut verses, book, chapter, current.take());

    Ok(verses)
}

/// Push a finished verse if it has a chapter and non-empty text.
fn flush(verses: &mut Vec<Verse>, book: Book, chapter: u16, current: Option<(u16, String)>) {
    if let Some((num, raw)) = current {
        if chapter == 0 {
            return;
        }
        let text = clean_verse_text(&raw);
        if !text.is_empty() {
            verses.push(Verse {
                id: VerseId::new(book, chapter, num),
                text,
            });
        }
    }
}

/// Read the `\id` line and resolve its 3-letter book code.
pub(crate) fn detect_book(src: &str) -> Result<Book, UsfmError> {
    let id_line = src
        .lines()
        .map(str::trim)
        .find_map(|l| l.strip_prefix("\\id "))
        .ok_or(UsfmError::NoId)?;
    let code = id_line.split_whitespace().next().unwrap_or("");
    Book::from_usfm(code).ok_or_else(|| UsfmError::UnknownBook(code.to_string()))
}

/// Leading run of ASCII digits as a `u16` (handles `\v 1-2` → `1`).
fn leading_u16(s: &str) -> Option<u16> {
    let digits: String = s.trim().chars().take_while(char::is_ascii_digit).collect();
    digits.parse().ok()
}

/// Strip USFM inline markup to plain reading text. See the module docs
/// for the order and rationale.
fn clean_verse_text(raw: &str) -> String {
    static NOTE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?s)\\f .*?\\f\*").unwrap());
    static XREF: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?s)\\x .*?\\x\*").unwrap());
    // `\w word|strong="…"\w*` or `\+w …\+w*` → the word.
    static WORD: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"\\\+?w ([^|\\]+?)(?:\|[^\\]*)?\\\+?w\*").unwrap());
    // End markers (`\w*`, `\wj*`, `\nd*`, …). Must run after WORD.
    static END: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\\\+?[a-z]+\*").unwrap());
    // Start / standalone markers (`\wj `, `\p`, `\q1`, …).
    static START: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\\\+?[a-z]+\d* ?").unwrap());
    static WS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\s+").unwrap());

    let s = NOTE.replace_all(raw, "");
    let s = XREF.replace_all(&s, "");
    let s = WORD.replace_all(&s, "$1");
    let s = END.replace_all(&s, "");
    let s = START.replace_all(&s, "");
    WS.replace_all(&s, " ").trim().to_string()
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// A few real WEB verses (public domain) carrying the full markup
    /// menagerie — words of Jesus (`\wj`), nested word tags with
    /// `strong=`, a mid-verse footnote, and a chapter break — so the
    /// parser is exercised against actual USFM without bundling a whole
    /// book. The corpus itself lives in the resource library, not the
    /// repo (see `plans/bible-study.md`).
    pub(crate) const SAMPLE: &str = r#"\id JHN test fixture
\h John
\mt1 John
\c 3
\p
\v 16 \wj \+w For|strong="G1063"\+w* \+w God|strong="G2316"\+w* \+w so|strong="G3779"\+w* loved \+w the|strong="G1519"\+w* \+w world|strong="G2889"\+w*, \+w that|strong="G2443"\+w* \+w he|strong="G3588"\+w* \+w gave|strong="G1325"\+w* \+w his|strong="G3956"\+w* \+w only|strong="G3439"\+w* born\wj*\f + \fr 3:16 \ft The phrase "only born" is from the Greek word.\f* \wj \+w Son|strong="G5207"\+w*, \+w that|strong="G2443"\+w* \+w whoever|strong="G3956"\+w* \+w believes|strong="G4100"\+w* \+w in|strong="G1519"\+w* \+w him|strong="G3588"\+w* \+w should|strong="G2316"\+w* \+w not|strong="G3361"\+w* perish, \+w but|strong="G3361"\+w* \+w have|strong="G2192"\+w* eternal \+w life|strong="G2222"\+w*. \wj*
\v 17 \w For|strong="G1063"\w* God didn't send his Son into the world to judge the world.
\c 4
\p
\v 1 Therefore when the Lord knew that the Pharisees had heard.
"#;

    #[test]
    fn parses_chapters_and_orders_verses() {
        let verses = parse_book(SAMPLE).unwrap();
        assert_eq!(verses.len(), 3);
        assert!(
            verses.windows(2).all(|w| w[0].id < w[1].id),
            "verses sorted"
        );
        let chapters: std::collections::BTreeSet<u16> =
            verses.iter().map(|v| v.id.chapter).collect();
        assert_eq!(chapters, [3, 4].into_iter().collect());
    }

    #[test]
    fn john_3_16_resolves_to_clean_text() {
        let verses = parse_book(SAMPLE).unwrap();
        let target = VerseId::parse("John 3:16").unwrap();
        let v = verses
            .iter()
            .find(|v| v.id == target)
            .expect("John 3:16 present");
        assert_eq!(
            v.text,
            "For God so loved the world, that he gave his only born Son, \
             that whoever believes in him should not perish, but have eternal life."
        );
    }

    #[test]
    fn markup_is_fully_stripped() {
        for v in parse_book(SAMPLE).unwrap() {
            assert!(
                !v.text.contains('\\'),
                "{} still has markup: {}",
                v.id,
                v.text
            );
            assert!(!v.text.contains("strong="), "{} leaked a strong tag", v.id);
        }
    }

    #[test]
    fn missing_id_is_an_error() {
        assert_eq!(parse_book("\\c 1\n\\v 1 hi\n"), Err(UsfmError::NoId));
    }
}

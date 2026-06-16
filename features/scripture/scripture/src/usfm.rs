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
fn detect_book(src: &str) -> Result<Book, UsfmError> {
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
mod tests {
    use super::*;

    const JOHN: &str = include_str!("../assets/web/JHN.usfm");

    #[test]
    fn parses_the_whole_book() {
        let verses = parse_book(JOHN).unwrap();
        // John has 21 chapters; spot-check the count is in the right
        // ballpark (879 verses in WEB John) and ordering holds.
        assert!(verses.len() > 800, "got {} verses", verses.len());
        assert!(
            verses.windows(2).all(|w| w[0].id < w[1].id),
            "verses are sorted"
        );
        let chapters: std::collections::BTreeSet<u16> =
            verses.iter().map(|v| v.id.chapter).collect();
        assert_eq!(*chapters.iter().max().unwrap(), 21);
    }

    #[test]
    fn john_3_16_resolves_to_clean_text() {
        let verses = parse_book(JOHN).unwrap();
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
        let verses = parse_book(JOHN).unwrap();
        // No residual backslash markers or strong tags anywhere.
        for v in &verses {
            assert!(
                !v.text.contains('\\'),
                "{} still has markup: {}",
                v.id,
                v.text
            );
            assert!(!v.text.contains("strong="), "{} leaked a strong tag", v.id);
        }
    }
}

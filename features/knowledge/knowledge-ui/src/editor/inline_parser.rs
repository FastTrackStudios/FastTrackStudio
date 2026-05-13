//! Inline-span tokenizer for the live-preview editor.
//!
//! Hand-rolled single-pass scanner over bytes. The output is a flat
//! `Vec<InlineSpan>` covering every byte of the source — the caret
//! tracker maps DOM offsets back to these byte ranges to know which
//! span the cursor is editing.
//!
//! Priorities follow Obsidian/CommonMark conventions:
//! 1. `***text***` (bold-italic) before `**` (bold) before `*` (italic)
//! 2. `![[…]]` (embed) before `[[…]]` (wikilink) before `[…](…)` (md link)
//! 3. `[[entity://kind/uuid|alias]]` is detected as a sub-shape of `[[…]]`
//! 4. `((uuid))` only if inner is a 36-char UUID
//! 5. `#tag` only after whitespace/start-of-line, must start with a letter
//!
//! Anything that doesn't match a markup pattern becomes `SpanKind::Plain`.
//!
//! NOTE: this scanner deliberately does NOT call into `knowledge_proto::obsidian`
//! because we need byte-range fidelity per span, not just a `Vec<Ref>`.

use std::ops::Range;
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq)]
pub struct InlineSpan {
    pub byte_range: Range<usize>,
    pub kind: SpanKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SpanKind {
    Plain(String),
    Bold(String),
    Italic(String),
    BoldItalic(String),
    Strike(String),
    Highlight(String),
    InlineCode(String),
    Wikilink {
        target: String,
        heading: Option<String>,
        block_id: Option<String>,
        alias: Option<String>,
        raw: String,
    },
    Embed {
        target: String,
        heading: Option<String>,
        block_id: Option<String>,
        alias: Option<String>,
        raw: String,
    },
    BlockRef {
        target_id: Uuid,
        raw: String,
    },
    Tag {
        path: Vec<String>,
        raw: String,
    },
    EntityLink {
        kind: String,
        id: Uuid,
        alias: Option<String>,
        raw: String,
    },
    MdLink {
        text: String,
        url: String,
        raw: String,
    },
    Footnote {
        id: String,
        raw: String,
    },
    Math {
        raw: String,
    },
}

/// Tokenize block content into spans. Byte-range fidelity is critical:
/// the caret tracker maps DOM positions to these ranges.
pub fn parse_inline(content: &str) -> Vec<InlineSpan> {
    let bytes = content.as_bytes();
    let mut out: Vec<InlineSpan> = Vec::new();
    let mut plain_start: Option<usize> = None;
    let mut i = 0usize;

    let flush_plain = |out: &mut Vec<InlineSpan>, plain_start: &mut Option<usize>, end: usize| {
        if let Some(s) = plain_start.take() {
            if s < end {
                out.push(InlineSpan {
                    byte_range: s..end,
                    kind: SpanKind::Plain(content[s..end].to_string()),
                });
            }
        }
    };

    while i < bytes.len() {
        let c = bytes[i];
        // Track whether we are at start-of-line or after whitespace —
        // used for tag detection.
        let prev_is_ws_or_start = i == 0 || matches!(bytes[i - 1], b' ' | b'\t' | b'\n' | b'\r');

        // ── Embed `![[…]]` ────────────────────────────────────────
        if c == b'!'
            && i + 1 < bytes.len()
            && bytes[i + 1] == b'['
            && i + 2 < bytes.len()
            && bytes[i + 2] == b'['
        {
            if let Some(end) = find_close_brackets(bytes, i + 3) {
                flush_plain(&mut out, &mut plain_start, i);
                let raw = &content[i..end];
                let inner = &content[i + 3..end - 2];
                let (target, heading, block_id, alias) = parse_wikilink_inner(inner);
                out.push(InlineSpan {
                    byte_range: i..end,
                    kind: SpanKind::Embed {
                        target,
                        heading,
                        block_id,
                        alias,
                        raw: raw.to_string(),
                    },
                });
                i = end;
                continue;
            }
        }

        // ── Wikilink `[[…]]` (entity-link sub-shape too) ──────────
        if c == b'[' && i + 1 < bytes.len() && bytes[i + 1] == b'[' {
            if let Some(end) = find_close_brackets(bytes, i + 2) {
                flush_plain(&mut out, &mut plain_start, i);
                let raw = &content[i..end];
                let inner = &content[i + 2..end - 2];
                // entity://kind/uuid|alias
                if let Some(rest) = inner.strip_prefix("entity://") {
                    let (kind_part, rest2) = match rest.find('/') {
                        Some(idx) => (&rest[..idx], &rest[idx + 1..]),
                        None => ("", rest),
                    };
                    let (id_part, alias) = match rest2.find('|') {
                        Some(idx) => (&rest2[..idx], Some(rest2[idx + 1..].to_string())),
                        None => (rest2, None),
                    };
                    if let Ok(id) = Uuid::parse_str(id_part) {
                        out.push(InlineSpan {
                            byte_range: i..end,
                            kind: SpanKind::EntityLink {
                                kind: kind_part.to_string(),
                                id,
                                alias,
                                raw: raw.to_string(),
                            },
                        });
                        i = end;
                        continue;
                    }
                }
                let (target, heading, block_id, alias) = parse_wikilink_inner(inner);
                out.push(InlineSpan {
                    byte_range: i..end,
                    kind: SpanKind::Wikilink {
                        target,
                        heading,
                        block_id,
                        alias,
                        raw: raw.to_string(),
                    },
                });
                i = end;
                continue;
            }
        }

        // ── Block-ref `((uuid))` ──────────────────────────────────
        if c == b'(' && i + 1 < bytes.len() && bytes[i + 1] == b'(' {
            // need exactly 36 hex chars then `))`
            if i + 2 + 36 + 2 <= bytes.len()
                && bytes[i + 2 + 36] == b')'
                && bytes[i + 2 + 37] == b')'
            {
                let inner = &content[i + 2..i + 2 + 36];
                if let Ok(target_id) = Uuid::parse_str(inner) {
                    flush_plain(&mut out, &mut plain_start, i);
                    let end = i + 2 + 36 + 2;
                    let raw = &content[i..end];
                    out.push(InlineSpan {
                        byte_range: i..end,
                        kind: SpanKind::BlockRef {
                            target_id,
                            raw: raw.to_string(),
                        },
                    });
                    i = end;
                    continue;
                }
            }
        }

        // ── Markdown link `[text](url)` (skip if `[[` — handled above)
        if c == b'[' && (i + 1 >= bytes.len() || bytes[i + 1] != b'[') {
            if let Some((text_end, url_end)) = find_md_link(bytes, i) {
                flush_plain(&mut out, &mut plain_start, i);
                let text = content[i + 1..text_end].to_string();
                let url = content[text_end + 2..url_end].to_string();
                let raw = content[i..url_end + 1].to_string();
                out.push(InlineSpan {
                    byte_range: i..url_end + 1,
                    kind: SpanKind::MdLink { text, url, raw },
                });
                i = url_end + 1;
                continue;
            }
            // Footnote `[^id]`
            if i + 1 < bytes.len() && bytes[i + 1] == b'^' {
                if let Some(close) = content[i + 2..].find(']') {
                    let end = i + 2 + close + 1;
                    let id = content[i + 2..end - 1].to_string();
                    if !id.is_empty()
                        && id
                            .chars()
                            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
                    {
                        flush_plain(&mut out, &mut plain_start, i);
                        let raw = content[i..end].to_string();
                        out.push(InlineSpan {
                            byte_range: i..end,
                            kind: SpanKind::Footnote { id, raw },
                        });
                        i = end;
                        continue;
                    }
                }
            }
        }

        // ── Inline code `` ` ``
        if c == b'`' {
            // skip triple-backtick fences (those are block-level)
            if i + 2 < bytes.len() && bytes[i + 1] == b'`' && bytes[i + 2] == b'`' {
                // emit as plain — block parser handles fences
            } else if let Some(close) = content[i + 1..].find('`') {
                let end = i + 1 + close + 1;
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 1..end - 1].to_string();
                out.push(InlineSpan {
                    byte_range: i..end,
                    kind: SpanKind::InlineCode(inner),
                });
                i = end;
                continue;
            }
        }

        // ── Bold-italic `***text***`
        if i + 2 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b'*' && bytes[i + 2] == b'*' {
            if let Some(end) = find_emphasis_close(bytes, i + 3, b"***") {
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 3..end].to_string();
                out.push(InlineSpan {
                    byte_range: i..end + 3,
                    kind: SpanKind::BoldItalic(inner),
                });
                i = end + 3;
                continue;
            }
        }

        // ── Bold `**text**` or `__text__`
        if i + 1 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b'*' {
            if let Some(end) = find_emphasis_close(bytes, i + 2, b"**") {
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 2..end].to_string();
                out.push(InlineSpan {
                    byte_range: i..end + 2,
                    kind: SpanKind::Bold(inner),
                });
                i = end + 2;
                continue;
            }
        }
        if i + 1 < bytes.len() && bytes[i] == b'_' && bytes[i + 1] == b'_' {
            if let Some(end) = find_emphasis_close(bytes, i + 2, b"__") {
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 2..end].to_string();
                out.push(InlineSpan {
                    byte_range: i..end + 2,
                    kind: SpanKind::Bold(inner),
                });
                i = end + 2;
                continue;
            }
        }

        // ── Strike `~~text~~`
        if i + 1 < bytes.len() && bytes[i] == b'~' && bytes[i + 1] == b'~' {
            if let Some(end) = find_emphasis_close(bytes, i + 2, b"~~") {
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 2..end].to_string();
                out.push(InlineSpan {
                    byte_range: i..end + 2,
                    kind: SpanKind::Strike(inner),
                });
                i = end + 2;
                continue;
            }
        }

        // ── Highlight `==text==`
        if i + 1 < bytes.len() && bytes[i] == b'=' && bytes[i + 1] == b'=' {
            if let Some(end) = find_emphasis_close(bytes, i + 2, b"==") {
                flush_plain(&mut out, &mut plain_start, i);
                let inner = content[i + 2..end].to_string();
                out.push(InlineSpan {
                    byte_range: i..end + 2,
                    kind: SpanKind::Highlight(inner),
                });
                i = end + 2;
                continue;
            }
        }

        // ── Italic single `*text*` or `_text_`
        if c == b'*' && (i == 0 || bytes[i - 1] != b'*') {
            if let Some(end) = find_emphasis_close(bytes, i + 1, b"*") {
                if end > i + 1 {
                    flush_plain(&mut out, &mut plain_start, i);
                    let inner = content[i + 1..end].to_string();
                    out.push(InlineSpan {
                        byte_range: i..end + 1,
                        kind: SpanKind::Italic(inner),
                    });
                    i = end + 1;
                    continue;
                }
            }
        }
        if c == b'_' && (i == 0 || bytes[i - 1] != b'_') && prev_is_ws_or_start_or_punct(bytes, i) {
            if let Some(end) = find_emphasis_close(bytes, i + 1, b"_") {
                if end > i + 1 {
                    flush_plain(&mut out, &mut plain_start, i);
                    let inner = content[i + 1..end].to_string();
                    out.push(InlineSpan {
                        byte_range: i..end + 1,
                        kind: SpanKind::Italic(inner),
                    });
                    i = end + 1;
                    continue;
                }
            }
        }

        // ── Tag `#tag/path`
        if c == b'#' && prev_is_ws_or_start {
            let start = i + 1;
            // First char must be a letter
            if let Some(&first) = bytes.get(start) {
                if (first as char).is_alphabetic() {
                    let mut end = start;
                    while end < bytes.len() {
                        let ch = bytes[end] as char;
                        if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == '/' {
                            end += 1;
                        } else {
                            break;
                        }
                    }
                    flush_plain(&mut out, &mut plain_start, i);
                    let raw = content[i..end].to_string();
                    let path = content[start..end]
                        .split('/')
                        .map(|s| s.to_string())
                        .collect();
                    out.push(InlineSpan {
                        byte_range: i..end,
                        kind: SpanKind::Tag { path, raw },
                    });
                    i = end;
                    continue;
                }
            }
        }

        // ── Math `$inline$` (passthrough)
        if c == b'$' {
            if let Some(close) = content[i + 1..].find('$') {
                let end = i + 1 + close + 1;
                let inner = &content[i + 1..end - 1];
                if !inner.is_empty()
                    && !inner.contains('\n')
                    && !inner.starts_with(' ')
                    && !inner.ends_with(' ')
                {
                    flush_plain(&mut out, &mut plain_start, i);
                    out.push(InlineSpan {
                        byte_range: i..end,
                        kind: SpanKind::Math {
                            raw: content[i..end].to_string(),
                        },
                    });
                    i = end;
                    continue;
                }
            }
        }

        // Default — extend plain
        if plain_start.is_none() {
            plain_start = Some(i);
        }
        i += 1;
    }
    flush_plain(&mut out, &mut plain_start, bytes.len());
    out
}

fn prev_is_ws_or_start_or_punct(bytes: &[u8], i: usize) -> bool {
    if i == 0 {
        return true;
    }
    matches!(
        bytes[i - 1] as char,
        ' ' | '\t' | '\n' | '\r' | '(' | '[' | '{' | ',' | '.' | ';' | ':' | '!' | '?'
    )
}

fn find_close_brackets(bytes: &[u8], from: usize) -> Option<usize> {
    let mut i = from;
    while i + 1 < bytes.len() {
        if bytes[i] == b']' && bytes[i + 1] == b']' {
            return Some(i + 2);
        }
        if bytes[i] == b'\n' {
            return None;
        }
        i += 1;
    }
    None
}

fn find_md_link(bytes: &[u8], i: usize) -> Option<(usize, usize)> {
    // i points at `[`. Find matching `]` (no `\n`, no `]]` since handled above)
    let mut j = i + 1;
    let mut depth = 1usize;
    while j < bytes.len() {
        let b = bytes[j];
        if b == b'\n' {
            return None;
        }
        if b == b'[' {
            depth += 1;
        } else if b == b']' {
            depth -= 1;
            if depth == 0 {
                break;
            }
        }
        j += 1;
    }
    if j >= bytes.len() || depth != 0 {
        return None;
    }
    let text_end = j;
    // need `(` immediately after
    if text_end + 1 >= bytes.len() || bytes[text_end + 1] != b'(' {
        return None;
    }
    let mut k = text_end + 2;
    let mut pdepth = 1usize;
    while k < bytes.len() {
        let b = bytes[k];
        if b == b'\n' {
            return None;
        }
        if b == b'(' {
            pdepth += 1;
        } else if b == b')' {
            pdepth -= 1;
            if pdepth == 0 {
                break;
            }
        }
        k += 1;
    }
    if k >= bytes.len() || pdepth != 0 {
        return None;
    }
    Some((text_end, k))
}

fn find_emphasis_close(bytes: &[u8], from: usize, marker: &[u8]) -> Option<usize> {
    let mut i = from;
    while i + marker.len() <= bytes.len() {
        if bytes[i] == b'\n' {
            return None;
        }
        if &bytes[i..i + marker.len()] == marker {
            // For single-char markers, ensure the preceding inner char
            // isn't whitespace (CommonMark right-flank rule, simplified).
            if marker.len() == 1 {
                if i > from && matches!(bytes[i - 1], b' ' | b'\t') {
                    i += 1;
                    continue;
                }
            }
            return Some(i);
        }
        i += 1;
    }
    None
}

fn parse_wikilink_inner(inner: &str) -> (String, Option<String>, Option<String>, Option<String>) {
    // target[#^block | #heading][|alias]
    let (left, alias) = match inner.find('|') {
        Some(idx) => (&inner[..idx], Some(inner[idx + 1..].to_string())),
        None => (inner, None),
    };
    let (target, heading, block_id) = if let Some(idx) = left.find("#^") {
        (
            left[..idx].to_string(),
            None,
            Some(left[idx + 2..].to_string()),
        )
    } else if let Some(idx) = left.find('#') {
        (
            left[..idx].to_string(),
            Some(left[idx + 1..].to_string()),
            None,
        )
    } else {
        (left.to_string(), None, None)
    };
    (target, heading, block_id, alias)
}

// ── Tests ─────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(spans: &[InlineSpan]) -> Vec<&SpanKind> {
        spans.iter().map(|s| &s.kind).collect()
    }

    #[test]
    fn plain_text() {
        let s = parse_inline("hello world");
        assert_eq!(s.len(), 1);
        assert!(matches!(s[0].kind, SpanKind::Plain(ref t) if t == "hello world"));
        assert_eq!(s[0].byte_range, 0..11);
    }

    #[test]
    fn bold() {
        let s = parse_inline("a **bold** b");
        assert_eq!(s.len(), 3);
        assert!(matches!(s[1].kind, SpanKind::Bold(ref t) if t == "bold"));
    }

    #[test]
    fn italic_star() {
        let s = parse_inline("an *italic* word");
        assert!(
            s.iter()
                .any(|sp| matches!(sp.kind, SpanKind::Italic(ref t) if t == "italic"))
        );
    }

    #[test]
    fn italic_underscore() {
        let s = parse_inline("an _italic_ word");
        assert!(
            s.iter()
                .any(|sp| matches!(sp.kind, SpanKind::Italic(ref t) if t == "italic"))
        );
    }

    #[test]
    fn bold_italic_triple() {
        let s = parse_inline("***wow***");
        assert_eq!(s.len(), 1);
        assert!(matches!(s[0].kind, SpanKind::BoldItalic(ref t) if t == "wow"));
    }

    #[test]
    fn strike() {
        let s = parse_inline("~~no~~");
        assert!(matches!(s[0].kind, SpanKind::Strike(ref t) if t == "no"));
    }

    #[test]
    fn highlight() {
        let s = parse_inline("==hey==");
        assert!(matches!(s[0].kind, SpanKind::Highlight(ref t) if t == "hey"));
    }

    #[test]
    fn inline_code() {
        let s = parse_inline("see `foo()` here");
        assert!(
            s.iter()
                .any(|sp| matches!(sp.kind, SpanKind::InlineCode(ref t) if t == "foo()"))
        );
    }

    #[test]
    fn wikilink_simple() {
        let s = parse_inline("see [[Hello]]");
        let last = &s.last().unwrap().kind;
        match last {
            SpanKind::Wikilink {
                target,
                alias,
                heading,
                block_id,
                ..
            } => {
                assert_eq!(target, "Hello");
                assert!(alias.is_none());
                assert!(heading.is_none());
                assert!(block_id.is_none());
            }
            other => panic!("expected wikilink got {other:?}"),
        }
    }

    #[test]
    fn wikilink_alias_heading_block() {
        let s = parse_inline("[[Page#Section|the page]]");
        match &s[0].kind {
            SpanKind::Wikilink {
                target,
                heading,
                alias,
                ..
            } => {
                assert_eq!(target, "Page");
                assert_eq!(heading.as_deref(), Some("Section"));
                assert_eq!(alias.as_deref(), Some("the page"));
            }
            o => panic!("{o:?}"),
        }
        let s2 = parse_inline("[[Page#^abc-1]]");
        match &s2[0].kind {
            SpanKind::Wikilink { block_id, .. } => {
                assert_eq!(block_id.as_deref(), Some("abc-1"));
            }
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn embed() {
        let s = parse_inline("![[Image.png]]");
        match &s[0].kind {
            SpanKind::Embed { target, .. } => assert_eq!(target, "Image.png"),
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn entity_link() {
        let s = parse_inline("[[entity://task/00000000-0000-0000-0000-000000000001|the task]]");
        match &s[0].kind {
            SpanKind::EntityLink {
                kind, id, alias, ..
            } => {
                assert_eq!(kind, "task");
                assert_eq!(alias.as_deref(), Some("the task"));
                assert_eq!(id.to_string(), "00000000-0000-0000-0000-000000000001");
            }
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn block_ref_uuid() {
        let s = parse_inline("see ((00000000-0000-0000-0000-000000000abc))");
        assert!(
            s.iter()
                .any(|sp| matches!(sp.kind, SpanKind::BlockRef { .. }))
        );
    }

    #[test]
    fn block_ref_non_uuid_is_plain() {
        let s = parse_inline("call ((not-a-uuid))");
        assert!(
            s.iter()
                .all(|sp| !matches!(sp.kind, SpanKind::BlockRef { .. }))
        );
    }

    #[test]
    fn tag_simple_and_nested() {
        let s = parse_inline("hello #work and #proj/alpha");
        let tags: Vec<&SpanKind> = s
            .iter()
            .filter_map(|sp| match &sp.kind {
                k @ SpanKind::Tag { .. } => Some(k),
                _ => None,
            })
            .collect();
        assert_eq!(tags.len(), 2);
        match tags[1] {
            SpanKind::Tag { path, .. } => {
                assert_eq!(path, &vec!["proj".to_string(), "alpha".to_string()])
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn tag_digit_first_rejected() {
        let s = parse_inline("nope #1bad");
        assert!(s.iter().all(|sp| !matches!(sp.kind, SpanKind::Tag { .. })));
    }

    #[test]
    fn md_link() {
        let s = parse_inline("a [click](https://x.example) here");
        let mid = &s[1].kind;
        match mid {
            SpanKind::MdLink { text, url, .. } => {
                assert_eq!(text, "click");
                assert_eq!(url, "https://x.example");
            }
            o => panic!("{o:?}"),
        }
    }

    #[test]
    fn footnote() {
        let s = parse_inline("a claim[^1] needs evidence");
        assert!(
            s.iter()
                .any(|sp| matches!(sp.kind, SpanKind::Footnote { ref id, .. } if id == "1"))
        );
    }

    #[test]
    fn math_passthrough() {
        let s = parse_inline("solve $x^2$ now");
        assert!(s.iter().any(|sp| matches!(sp.kind, SpanKind::Math { .. })));
    }

    #[test]
    fn byte_range_covers_full_input() {
        let src = "a **b** [[c]] ((00000000-0000-0000-0000-000000000abc)) #t";
        let s = parse_inline(src);
        let mut cursor = 0usize;
        for sp in &s {
            assert_eq!(sp.byte_range.start, cursor);
            cursor = sp.byte_range.end;
        }
        assert_eq!(cursor, src.len());
    }

    #[test]
    fn unclosed_emphasis_falls_back_to_plain() {
        let s = parse_inline("a **bold but no close");
        assert!(s.iter().all(|sp| matches!(sp.kind, SpanKind::Plain(_))));
    }
}

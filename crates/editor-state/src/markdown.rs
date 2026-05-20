//! Markdown live-preview decoration source.
//!
//! Scans the doc for `**…**` (bold), `*…*` (italic), and
//! `` `…` `` (inline code) spans and emits decorations:
//!
//! - The body of the span gets a `MarkDecoration` with the
//!   corresponding class (`md-bold`, `md-italic`, `md-code`).
//! - The opening + closing markers are `Replace`d **only when
//!   the primary cursor is outside the span**. While the cursor
//!   is on the span, markers stay visible so the user sees
//!   the raw markdown and can edit it directly.
//!
//! This is exactly Obsidian's "Live Preview" mode in spirit —
//! it's a renderer trick, not a document-model change.
//!
//! The parser is single-pass and intentionally tiny. Not a real
//! CommonMark implementation; just enough to demo the
//! decoration pipeline. A future commit can swap in a proper
//! markdown parser (pulldown-cmark or a port of CM6's
//! lang-markdown) without touching the decoration shape.

use crate::decoration::{Decoration, DecoratedRange};
use crate::selection::Range;
use crate::state::EditorState;

/// The full live-preview decoration source. Suitable to register
/// as `editor_view::DecorationSource`.
pub fn live_preview(state: &EditorState) -> Vec<DecoratedRange> {
    let text = state.doc.to_string();
    let primary = state.selection.primary();
    let mut out = Vec::new();
    for span in find_spans(&text) {
        out.push(Decoration::mark(span.body.clone(), span.class));
        // Reveal markers when the caret touches the span.
        if !cursor_touches(primary, span.outer.clone()) {
            out.push(Decoration::replace(span.outer.start..span.body.start));
            out.push(Decoration::replace(span.body.end..span.outer.end));
        }
    }
    out
}

struct Span {
    /// Includes the opening + closing markers.
    outer: std::ops::Range<usize>,
    /// Just the inner content.
    body: std::ops::Range<usize>,
    /// CSS class to apply to the body. Static for now; later
    /// callers may want to inject their own class names.
    class: &'static str,
}

/// Did the primary selection touch any byte in `range`? A caret
/// *adjacent* to the span counts as touching — so cursors at
/// either edge keep the markers visible (matches Obsidian).
fn cursor_touches(primary: Range, range: std::ops::Range<usize>) -> bool {
    let (sel_from, sel_to) = (primary.from(), primary.to());
    sel_to >= range.start && sel_from <= range.end
}

/// Single-pass scanner. Walks bytes, recognizing three flavors
/// of paired marker. Doesn't cross newlines (a stray `*` on one
/// line shouldn't pair with one on the next).
fn find_spans(text: &str) -> Vec<Span> {
    let mut out = Vec::new();
    let b = text.as_bytes();
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'\n' {
            i += 1;
            continue;
        }
        // **bold**
        if i + 4 <= b.len() && &b[i..i + 2] == b"**" {
            if let Some(end) = find_close(b, i + 2, b"**") {
                out.push(Span {
                    outer: i..end + 2,
                    body: i + 2..end,
                    class: "md-bold",
                });
                i = end + 2;
                continue;
            }
        }
        // `inline code`
        if b[i] == b'`' {
            if let Some(end) = find_close(b, i + 1, b"`") {
                out.push(Span {
                    outer: i..end + 1,
                    body: i + 1..end,
                    class: "md-code",
                });
                i = end + 1;
                continue;
            }
        }
        // *italic* — must not be `**` (handled above) and must
        // start a word (not surrounded by other `*`s).
        if b[i] == b'*' {
            if let Some(end) = find_close(b, i + 1, b"*") {
                if end > i + 1 && b[end + 1..].first() != Some(&b'*') {
                    out.push(Span {
                        outer: i..end + 1,
                        body: i + 1..end,
                        class: "md-italic",
                    });
                    i = end + 1;
                    continue;
                }
            }
        }
        i += 1;
    }
    out
}

/// Find the next occurrence of `needle` in `b` starting at
/// `from`, returning the start byte offset. Stops at newlines
/// (a span can't cross a line boundary).
fn find_close(b: &[u8], from: usize, needle: &[u8]) -> Option<usize> {
    let mut i = from;
    while i + needle.len() <= b.len() {
        if b[i] == b'\n' {
            return None;
        }
        if &b[i..i + needle.len()] == needle {
            return Some(i);
        }
        i += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc::Doc;
    use crate::selection::{Range, Selection};

    fn state(text: &str, caret: usize) -> EditorState {
        EditorState {
            doc: Doc::from_str(text),
            selection: Selection::caret(caret),
        }
    }

    #[test]
    fn bold_with_caret_outside_hides_markers() {
        // "**hi**" at offset 0..6. Body is 2..4 ("hi").
        // Caret at 7 (past the span) — markers should be hidden.
        let s = state("**hi** there", 7);
        let decs = live_preview(&s);
        // Expect: mark(2..4 bold), replace(0..2), replace(4..6).
        assert!(decs.iter().any(|d| d.from == 0 && d.to == 2));
        assert!(decs.iter().any(|d| d.from == 4 && d.to == 6));
        assert!(decs.iter().any(|d| d.from == 2 && d.to == 4));
    }

    #[test]
    fn bold_with_caret_inside_keeps_markers() {
        // Caret at 3 — inside "hi". Markers should NOT be hidden.
        let s = state("**hi** there", 3);
        let decs = live_preview(&s);
        let replace_count = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Replace))
            .count();
        assert_eq!(replace_count, 0, "caret inside span should keep markers");
        // But the body mark is still there.
        assert!(decs.iter().any(|d| d.from == 2 && d.to == 4));
    }

    #[test]
    fn caret_adjacent_to_span_counts_as_touching() {
        // Caret right after the closing `**` — adjacent.
        let s = state("**hi**", 6);
        let decs = live_preview(&s);
        let replace_count = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Replace))
            .count();
        assert_eq!(replace_count, 0);
    }

    #[test]
    fn italic_recognized() {
        let s = state("hello *world*", 0);
        let decs = live_preview(&s);
        assert!(decs.iter().any(|d| matches!(
            &d.kind,
            crate::decoration::DecorationKind::Mark { class } if class == "md-italic"
        )));
    }

    #[test]
    fn inline_code_recognized() {
        let s = state("see `let x = 1`", 0);
        let decs = live_preview(&s);
        assert!(decs.iter().any(|d| matches!(
            &d.kind,
            crate::decoration::DecorationKind::Mark { class } if class == "md-code"
        )));
    }

    #[test]
    fn span_does_not_cross_newline() {
        let s = state("**a\nb**", 0);
        let decs = live_preview(&s);
        // No span — the opening `**` doesn't pair across the \n.
        let marks: Vec<_> = decs
            .iter()
            .filter(|d| matches!(d.kind, crate::decoration::DecorationKind::Mark { .. }))
            .collect();
        assert!(marks.is_empty());
    }
}

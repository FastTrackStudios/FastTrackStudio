//! Per-span rendered ↔ raw mode dispatch given a caret position.
//!
//! Block-level constructs (headings, code fences, callouts) flip
//! block-wide on focus — that decision lives in `block.rs`, not here.
//! This module only sees one block's inline spans.

use super::inline_parser::InlineSpan;

/// Render decision for one span.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SpanDecoration {
    /// Show the rendered (formatted) view — `<strong>`, `<a>`, etc.
    Rendered,
    /// Show the raw source text. Used when caret is inside or
    /// immediately adjacent to the span's byte range.
    Raw,
}

/// Decorate each span based on caret position. A `None` caret means
/// the block is not focused and everything renders normally.
pub fn decorate(spans: &[InlineSpan], caret: Option<usize>) -> Vec<SpanDecoration> {
    let Some(c) = caret else {
        return vec![SpanDecoration::Rendered; spans.len()];
    };
    spans
        .iter()
        .map(|s| {
            let r = &s.byte_range;
            // Caret inside [start, end] OR exactly at start/end boundary.
            if c >= r.start && c <= r.end {
                SpanDecoration::Raw
            } else {
                SpanDecoration::Rendered
            }
        })
        .collect()
}

// ── Tests ─────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor::inline_parser::{SpanKind, parse_inline};

    #[test]
    fn no_caret_all_rendered() {
        let spans = parse_inline("a **b** c");
        let d = decorate(&spans, None);
        assert!(d.iter().all(|x| *x == SpanDecoration::Rendered));
    }

    #[test]
    fn caret_inside_bold_marks_only_that_span_raw() {
        let src = "a **bold** c";
        let spans = parse_inline(src);
        // Find the Bold span and put caret in the middle
        let bold = spans
            .iter()
            .find(|s| matches!(s.kind, SpanKind::Bold(_)))
            .unwrap();
        let mid = (bold.byte_range.start + bold.byte_range.end) / 2;
        let d = decorate(&spans, Some(mid));
        for (s, dec) in spans.iter().zip(d.iter()) {
            if std::ptr::eq(s, bold) {
                assert_eq!(*dec, SpanDecoration::Raw);
            } else {
                assert_eq!(*dec, SpanDecoration::Rendered);
            }
        }
    }

    #[test]
    fn caret_at_boundary_treats_span_as_raw() {
        let src = "**b**";
        let spans = parse_inline(src);
        let d = decorate(&spans, Some(0));
        assert_eq!(d[0], SpanDecoration::Raw);
        let d2 = decorate(&spans, Some(src.len()));
        assert_eq!(d2[0], SpanDecoration::Raw);
    }

    #[test]
    fn caret_far_outside_keeps_rendered() {
        let src = "hi **b** there";
        let spans = parse_inline(src);
        let d = decorate(&spans, Some(0));
        // Caret at 0 → first plain span Raw; bold span Rendered
        let bold_idx = spans
            .iter()
            .position(|s| matches!(s.kind, SpanKind::Bold(_)))
            .unwrap();
        assert_eq!(d[bold_idx], SpanDecoration::Rendered);
    }
}

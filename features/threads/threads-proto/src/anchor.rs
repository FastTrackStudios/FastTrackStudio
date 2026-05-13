//! Thread anchors — where a thread is attached.
//!
//! Variant names follow the W3C Web Annotation Data Model selectors so the
//! schema interops with existing annotation tools and a decade of accumulated
//! vocabulary. See <https://www.w3.org/TR/annotation-model/>.
//!
//! `TextPositionSelector` carries Loro `Cursor` bytes — position resolution
//! is the caller's job (it needs the live LoroDoc). This module only
//! resolves `TextQuoteSelector`, which is content-only and works offline.

use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
}

/// Where a thread is anchored. Serialized JSON uses `{"type": "<Variant>", ...}`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Anchor {
    /// Whole entity — the default. Use the legacy `(entity_type, entity_id)`
    /// columns on the Comment row to identify the target.
    Entity,

    /// W3C TextQuoteSelector — anchor on quoted text with optional context.
    /// Survives edits when the exact substring (or its context) persists.
    TextQuoteSelector {
        block_id: Uuid,
        exact: String,
        #[serde(skip_serializing_if = "Option::is_none", default)]
        prefix: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none", default)]
        suffix: Option<String>,
    },

    /// W3C TextPositionSelector — anchor on stable CRDT positions. We store
    /// Loro `Cursor` opaque bytes; resolution happens at the call site with
    /// access to the live LoroDoc.
    TextPositionSelector {
        block_id: Uuid,
        start_cursor_bytes: Vec<u8>,
        end_cursor_bytes: Vec<u8>,
    },

    /// W3C FragmentSelector for time ranges (media fragment URI `t=…`).
    FragmentSelector {
        asset_id: Uuid,
        time_start_ms: i64,
        time_end_ms: i64,
    },

    /// W3C-style region selector for images / PDF pages / canvases.
    /// Shape follows Logseq's PDF highlight schema: a bounding box plus
    /// one rectangle per selection line, with an optional pinned quote
    /// (the selected text, for re-anchoring on reflow).
    RegionSelector {
        asset_id: Uuid,
        #[serde(skip_serializing_if = "Option::is_none", default)]
        page: Option<u32>,
        bounding: Rect,
        rects: Vec<Rect>,
        #[serde(skip_serializing_if = "Option::is_none", default)]
        quote: Option<String>,
    },

    /// Whiteboard / canvas node anchor — `block_id` is the canvas Block's UUID.
    CanvasNodeSelector { block_id: Uuid },

    /// Single cell in a table / base view.
    CellSelector {
        entity_type: String,
        entity_id: Uuid,
        column: String,
        row_index: u32,
    },
}

/// Resolve a `TextQuoteSelector` against a current content string.
///
/// Returns `Some((start_byte, end_byte))` on hit, `None` on orphan (the quote
/// no longer locatable). Resolution order:
///   1. Exact substring match (cheapest, most common).
///   2. Context-anchored match: search for `prefix + exact + suffix`, then
///      `prefix + ... + suffix` of approximately the right length.
///   3. Bitap-lite windowed Hamming match against the exact text.
///
/// For other anchor variants this returns `None` — position-based anchors
/// resolve via Loro `Cursor` at the call site.
pub fn resolve_text_quote(anchor: &Anchor, current_content: &str) -> Option<(usize, usize)> {
    let Anchor::TextQuoteSelector {
        exact,
        prefix,
        suffix,
        ..
    } = anchor
    else {
        return None;
    };
    if exact.is_empty() {
        return None;
    }

    // 1. Exact substring.
    if let Some(idx) = current_content.find(exact.as_str()) {
        return Some((idx, idx + exact.len()));
    }

    // 2. Context-anchored: locate prefix, then check whether the bytes after
    //    it look like `exact` (allowing some drift), and confirm with suffix.
    if let Some(pre) = prefix.as_deref() {
        if !pre.is_empty() {
            if let Some(pre_idx) = current_content.find(pre) {
                let start = pre_idx + pre.len();
                let end = start + exact.len();
                if end <= current_content.len() && current_content.is_char_boundary(start) {
                    let mut e = end.min(current_content.len());
                    while !current_content.is_char_boundary(e) {
                        e -= 1;
                    }
                    let suf_match = suffix
                        .as_deref()
                        .filter(|s| !s.is_empty())
                        .map(|s| current_content[e..].starts_with(s))
                        .unwrap_or(false);
                    if suf_match {
                        return Some((start, e));
                    }
                    let window = &current_content[start..e];
                    if hamming_le(window, exact, threshold(exact.len())) {
                        return Some((start, e));
                    }
                }
            }
        }
    }

    // 3. Bitap-lite — slide a window of len(exact) bytes across the content
    //    and pick the lowest-Hamming-distance hit, if any beat the threshold.
    bitap_lite(current_content, exact)
}

fn threshold(len: usize) -> usize {
    // Allow up to ~25% byte drift, min 1 for non-empty input.
    (len / 4).max(1)
}

fn hamming_le(a: &str, b: &str, max: usize) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff = 0usize;
    for (x, y) in a.bytes().zip(b.bytes()) {
        if x != y {
            diff += 1;
            if diff > max {
                return false;
            }
        }
    }
    true
}

fn bitap_lite(haystack: &str, needle: &str) -> Option<(usize, usize)> {
    let nlen = needle.len();
    let hlen = haystack.len();
    if nlen == 0 || hlen < nlen {
        return None;
    }
    let thr = threshold(nlen);
    let nbytes = needle.as_bytes();
    let hbytes = haystack.as_bytes();
    let mut best: Option<(usize, usize)> = None; // (start, diff)
    for start in 0..=hlen - nlen {
        if !haystack.is_char_boundary(start) || !haystack.is_char_boundary(start + nlen) {
            continue;
        }
        let mut diff = 0usize;
        for i in 0..nlen {
            if hbytes[start + i] != nbytes[i] {
                diff += 1;
                if diff > thr {
                    break;
                }
            }
        }
        if diff <= thr {
            match best {
                Some((_, bd)) if bd <= diff => {}
                _ => best = Some((start, diff)),
            }
            if diff == 0 {
                break;
            }
        }
    }
    best.map(|(s, _)| (s, s + nlen))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_anchors() -> Vec<Anchor> {
        let bid = Uuid::nil();
        let aid = Uuid::from_u128(0x1234);
        vec![
            Anchor::Entity,
            Anchor::TextQuoteSelector {
                block_id: bid,
                exact: "the quick brown fox".into(),
                prefix: Some("once upon a time, ".into()),
                suffix: Some(" jumped over".into()),
            },
            Anchor::TextPositionSelector {
                block_id: bid,
                start_cursor_bytes: vec![1, 2, 3, 4],
                end_cursor_bytes: vec![5, 6, 7, 8],
            },
            Anchor::FragmentSelector {
                asset_id: aid,
                time_start_ms: 0,
                time_end_ms: 12_500,
            },
            Anchor::RegionSelector {
                asset_id: aid,
                page: Some(3),
                bounding: Rect {
                    x: 0.1,
                    y: 0.2,
                    w: 0.5,
                    h: 0.05,
                },
                rects: vec![Rect {
                    x: 0.1,
                    y: 0.2,
                    w: 0.5,
                    h: 0.05,
                }],
                quote: Some("highlighted line".into()),
            },
            Anchor::CanvasNodeSelector { block_id: bid },
            Anchor::CellSelector {
                entity_type: "task".into(),
                entity_id: bid,
                column: "status".into(),
                row_index: 7,
            },
        ]
    }

    #[test]
    fn serde_roundtrip_every_variant() {
        for a in sample_anchors() {
            let json = serde_json::to_string(&a).unwrap();
            assert!(json.contains("\"type\""), "missing tag in {json}");
            let back: Anchor = serde_json::from_str(&json).unwrap();
            assert_eq!(a, back, "round-trip mismatch for {json}");
        }
    }

    #[test]
    fn resolve_exact_hit() {
        let anchor = Anchor::TextQuoteSelector {
            block_id: Uuid::nil(),
            exact: "Loro CRDT".into(),
            prefix: None,
            suffix: None,
        };
        let content = "We're using Loro CRDT for everything.";
        let r = resolve_text_quote(&anchor, content).expect("exact hit");
        assert_eq!(&content[r.0..r.1], "Loro CRDT");
    }

    #[test]
    fn resolve_fuzzy_hit_after_edit() {
        // Original anchor on "the quick brown fox"; content has one byte changed.
        let anchor = Anchor::TextQuoteSelector {
            block_id: Uuid::nil(),
            exact: "the quick brown fox".into(),
            prefix: Some("watch ".into()),
            suffix: Some(" leap".into()),
        };
        // Single-letter swap: brown -> brawn
        let content = "watch the quick brawn fox leap the fence";
        let r = resolve_text_quote(&anchor, content).expect("fuzzy hit");
        // The matched span should be the original "exact" length, located
        // where the typo'd text actually sits.
        assert_eq!(r.1 - r.0, "the quick brown fox".len());
        assert!(content[r.0..r.1].contains("fox"));
    }

    #[test]
    fn resolve_context_anchored_hit() {
        // The exact text shifted: edits inside the quote, but prefix+suffix
        // sandwich it. We should recover via the prefix+suffix path.
        let anchor = Anchor::TextQuoteSelector {
            block_id: Uuid::nil(),
            exact: "needs citation".into(),
            prefix: Some("[".into()),
            suffix: Some("]".into()),
        };
        let content = "Statement here [needs citatxon] more text.";
        let r = resolve_text_quote(&anchor, content).expect("context hit");
        // start should be just past '[', end just before ']'
        assert_eq!(&content[r.0..r.1], "needs citatxon");
    }

    #[test]
    fn resolve_orphan_returns_none() {
        let anchor = Anchor::TextQuoteSelector {
            block_id: Uuid::nil(),
            exact: "completely different text never here".into(),
            prefix: None,
            suffix: None,
        };
        let content = "Nothing remotely similar lives in this paragraph.";
        assert!(resolve_text_quote(&anchor, content).is_none());
    }

    #[test]
    fn resolve_non_text_quote_returns_none() {
        for a in sample_anchors() {
            if matches!(a, Anchor::TextQuoteSelector { .. }) {
                continue;
            }
            assert!(resolve_text_quote(&a, "anything").is_none());
        }
    }
}

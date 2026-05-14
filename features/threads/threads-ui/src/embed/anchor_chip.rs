//! Anchor breadcrumb — a tiny pill that tells the reader where this
//! thread is attached. Rendered at the top of every thread card.

use dioxus::prelude::*;
use threads_proto::Anchor;
use uuid::Uuid;

/// Pure helper: render an Anchor as its short breadcrumb label.
/// Empty string for `Entity` (caller should hide the chip in that case).
pub fn breadcrumb_label(anchor: &Anchor) -> String {
    match anchor {
        Anchor::Entity => String::new(),
        Anchor::TextQuoteSelector { exact, .. } => {
            let truncated = truncate(exact, 32);
            format!("\u{201C}{}\u{201D}", truncated)
        }
        Anchor::TextPositionSelector { block_id, .. } => {
            format!("Block #{}", short_id(block_id))
        }
        Anchor::FragmentSelector {
            time_start_ms,
            time_end_ms,
            ..
        } => format!(
            "{}\u{2013}{}",
            ms_to_mmss(*time_start_ms),
            ms_to_mmss(*time_end_ms)
        ),
        Anchor::RegionSelector { page, .. } => match page {
            Some(p) => format!("Region p{}", p),
            None => "Region".into(),
        },
        Anchor::CanvasNodeSelector { .. } => "Canvas node".into(),
        Anchor::CellSelector {
            entity_type,
            column,
            row_index,
            ..
        } => format!("{entity_type}.{column}[row {row_index}]"),
    }
}

fn truncate(s: &str, max_chars: usize) -> String {
    let mut out = String::new();
    let mut count = 0;
    for ch in s.chars() {
        if count >= max_chars {
            out.push('\u{2026}');
            break;
        }
        out.push(ch);
        count += 1;
    }
    out
}

fn short_id(id: &Uuid) -> String {
    id.to_string().chars().take(8).collect()
}

fn ms_to_mmss(ms: i64) -> String {
    let total_s = (ms / 1000).max(0);
    let m = total_s / 60;
    let s = total_s % 60;
    format!("{:02}:{:02}", m, s)
}

#[component]
pub fn AnchorChip(anchor: Anchor) -> Element {
    let label = breadcrumb_label(&anchor);
    if label.is_empty() {
        return rsx! {};
    }
    rsx! {
        span {
            class: "inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs bg-muted text-muted-foreground",
            "{label}"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use threads_proto::Rect;

    fn rect() -> Rect {
        Rect {
            x: 0.0,
            y: 0.0,
            w: 0.0,
            h: 0.0,
        }
    }

    #[test]
    fn entity_is_blank() {
        assert_eq!(breadcrumb_label(&Anchor::Entity), "");
    }

    #[test]
    fn text_quote_wraps_in_curly_quotes_and_truncates() {
        let a = Anchor::TextQuoteSelector {
            block_id: Uuid::nil(),
            exact: "the quick brown fox jumped over the lazy dog and then some".into(),
            prefix: None,
            suffix: None,
        };
        let label = breadcrumb_label(&a);
        assert!(label.starts_with('\u{201C}'));
        assert!(label.ends_with('\u{201D}'));
        assert!(label.contains('\u{2026}'));
    }

    #[test]
    fn text_position_uses_short_block_id() {
        let id = Uuid::parse_str("12345678-90ab-cdef-1234-567890abcdef").unwrap();
        let a = Anchor::TextPositionSelector {
            block_id: id,
            start_cursor_bytes: vec![],
            end_cursor_bytes: vec![],
        };
        assert_eq!(breadcrumb_label(&a), "Block #12345678");
    }

    #[test]
    fn fragment_formats_as_mmss_range() {
        let a = Anchor::FragmentSelector {
            asset_id: Uuid::nil(),
            time_start_ms: 32_500,
            time_end_ms: 75_000,
        };
        assert_eq!(breadcrumb_label(&a), "00:32\u{2013}01:15");
    }

    #[test]
    fn region_with_page() {
        let a = Anchor::RegionSelector {
            asset_id: Uuid::nil(),
            page: Some(3),
            bounding: rect(),
            rects: vec![],
            quote: None,
        };
        assert_eq!(breadcrumb_label(&a), "Region p3");
    }

    #[test]
    fn region_without_page() {
        let a = Anchor::RegionSelector {
            asset_id: Uuid::nil(),
            page: None,
            bounding: rect(),
            rects: vec![],
            quote: None,
        };
        assert_eq!(breadcrumb_label(&a), "Region");
    }

    #[test]
    fn canvas_node_label() {
        let a = Anchor::CanvasNodeSelector {
            block_id: Uuid::nil(),
        };
        assert_eq!(breadcrumb_label(&a), "Canvas node");
    }

    #[test]
    fn cell_label() {
        let a = Anchor::CellSelector {
            entity_type: "projects".into(),
            entity_id: Uuid::nil(),
            column: "status".into(),
            row_index: 7,
        };
        assert_eq!(breadcrumb_label(&a), "projects.status[row 7]");
    }
}

//! Render the doc + decorations to a stream of typed segments
//! the view emits as Dioxus elements.
//!
//! The input is `(doc_text, sorted_decorations)`; the output is
//! a flat `Vec<Segment>` covering the doc end-to-end. Each
//! segment is either a styled run of text or a widget injected
//! at a point.
//!
//! ## Why a flat segment list, not a tree
//!
//! CM6's render uses a flat list of "pieces" too — overlapping
//! decorations are layered into adjacent non-overlapping pieces.
//! This keeps the cursor model simple: a single linear text
//! stream the DOM Selection can walk byte-by-byte.
//!
//! For v1 we support overlap by sorting decorations and emitting
//! one segment per *boundary* between decoration starts/ends.
//! A character covered by both Bold and Italic becomes one
//! segment with class "bold italic" (space-joined).

use editor_state::{DecoratedRange, DecorationKind};

/// One renderable piece. The view turns each into a Dioxus
/// element: `Text` → `<span class="...">…</span>`, `Widget`
/// → injected raw HTML at a point.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Segment {
    /// A run of text that should render as visible characters.
    /// `classes` is a space-joined class string; empty for
    /// undecorated text.
    Text {
        from: usize,
        to: usize,
        text: String,
        classes: String,
    },
    /// A range that's been `Replace`d — hidden from the rendered
    /// output entirely. The text still exists in `Doc`; we just
    /// don't emit it. Useful for hiding markdown markers in
    /// live-preview when the cursor isn't on the span.
    ///
    /// The view emits nothing for `Hidden`, but the segment is
    /// kept in the stream so byte-offset translation between
    /// DOM nodes and the doc stays correct.
    Hidden { from: usize, to: usize },
    /// A widget placed at a point in the doc.
    Widget { at: usize, html: String },
}

/// Build the rendered segment list from doc text + sorted
/// decorations.
///
/// Decorations must be sorted by `from` ascending. Within ties
/// it doesn't matter; the function handles overlap by walking
/// boundaries.
pub fn render(text: &str, decorations: &[DecoratedRange]) -> Vec<Segment> {
    let mut out: Vec<Segment> = Vec::new();
    if text.is_empty() && decorations.is_empty() {
        return out;
    }
    // Collect all boundary offsets — each starts or ends some
    // decoration. We emit one Text/Hidden segment per gap.
    let mut bounds: Vec<usize> = vec![0, text.len()];
    for d in decorations {
        match d.kind {
            DecorationKind::Mark { .. } | DecorationKind::Replace => {
                bounds.push(d.from);
                bounds.push(d.to);
            }
            // Line decorations also slice on `from`. v1 doesn't
            // render them but the boundary keeps everything
            // consistent.
            DecorationKind::Line { .. } => {
                bounds.push(d.from);
            }
            // Widgets are zero-width but their insertion point
            // still has to slice the surrounding text — otherwise
            // a widget at position 3 inside a single span "abcdef"
            // would land *outside* the text (before or after the
            // whole run) instead of between c and d.
            DecorationKind::Widget { .. } => {
                bounds.push(d.from);
            }
        }
    }
    bounds.sort_unstable();
    bounds.dedup();

    // Walk each text gap [bounds[i] .. bounds[i+1]) and emit a
    // segment. The list of decorations whose range overlaps the
    // gap gives us the classes / replace state.
    for w in bounds.windows(2) {
        let (from, to) = (w[0], w[1]);
        if from == to {
            continue;
        }
        let mut replace = false;
        let mut classes: Vec<&str> = Vec::new();
        for d in decorations {
            if d.from >= to || d.to <= from {
                continue;
            }
            match &d.kind {
                DecorationKind::Mark { class } => classes.push(class),
                DecorationKind::Replace => replace = true,
                _ => {}
            }
        }
        if replace {
            out.push(Segment::Hidden { from, to });
        } else {
            out.push(Segment::Text {
                from,
                to,
                text: text[from..to].to_string(),
                classes: classes.join(" "),
            });
        }
    }

    // Now interleave widgets. They sit at a point — between
    // segments — so we just find the index where the widget's
    // `at` lands and insert.
    let mut widgets: Vec<(usize, &str)> = decorations
        .iter()
        .filter_map(|d| match &d.kind {
            DecorationKind::Widget { html } => Some((d.from, html.as_str())),
            _ => None,
        })
        .collect();
    widgets.sort_by_key(|(at, _)| *at);
    for (at, html) in widgets.into_iter().rev() {
        // Find first segment whose `from >= at` and insert
        // before. (Working in reverse so indices we've already
        // chosen stay valid.)
        let idx = out
            .iter()
            .position(|s| segment_start(s).map_or(false, |f| f >= at))
            .unwrap_or(out.len());
        out.insert(
            idx,
            Segment::Widget {
                at,
                html: html.to_string(),
            },
        );
    }

    out
}

fn segment_start(s: &Segment) -> Option<usize> {
    match s {
        Segment::Text { from, .. } | Segment::Hidden { from, .. } => Some(*from),
        Segment::Widget { at, .. } => Some(*at),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use editor_state::Decoration;

    #[test]
    fn empty_text_no_decorations_yields_empty() {
        assert_eq!(render("", &[]), vec![]);
    }

    #[test]
    fn plain_text_one_segment() {
        let r = render("hello", &[]);
        assert_eq!(
            r,
            vec![Segment::Text {
                from: 0,
                to: 5,
                text: "hello".into(),
                classes: String::new()
            }]
        );
    }

    #[test]
    fn one_mark_splits_text() {
        let decs = vec![Decoration::mark(2..4, "bold")];
        let r = render("hello", &decs);
        assert_eq!(
            r,
            vec![
                Segment::Text {
                    from: 0,
                    to: 2,
                    text: "he".into(),
                    classes: String::new()
                },
                Segment::Text {
                    from: 2,
                    to: 4,
                    text: "ll".into(),
                    classes: "bold".into()
                },
                Segment::Text {
                    from: 4,
                    to: 5,
                    text: "o".into(),
                    classes: String::new()
                },
            ]
        );
    }

    #[test]
    fn overlapping_marks_combine_classes() {
        let decs = vec![
            Decoration::mark(0..4, "bold"),
            Decoration::mark(2..6, "italic"),
        ];
        let r = render("hello!", &decs);
        // boundaries at 0, 2, 4, 6
        assert_eq!(r.len(), 3);
        assert!(matches!(
            &r[1],
            Segment::Text { classes, .. } if classes == "bold italic"
        ));
    }

    #[test]
    fn replace_hides_range() {
        let decs = vec![Decoration::replace(0..2)];
        let r = render("**bold**", &decs);
        // Expect: Hidden(0..2), Text(2..8 "bold**", classes="")
        assert_eq!(r[0], Segment::Hidden { from: 0, to: 2 });
        if let Segment::Text { text, .. } = &r[1] {
            assert_eq!(text, "bold**");
        } else {
            panic!("expected Text segment after Hidden");
        }
    }

    #[test]
    fn widget_inserts_at_point() {
        let decs = vec![Decoration::widget(3, "<i class=icon/>")];
        let r = render("abcdef", &decs);
        // segments: Text(0..3), Widget(3), Text(3..6)
        assert_eq!(r.len(), 3);
        assert!(matches!(r[1], Segment::Widget { at: 3, .. }));
    }
}

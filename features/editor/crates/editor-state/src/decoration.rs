//! Decorations — visual overlays on the document. The document
//! itself is plain text; decorations say "wrap these characters
//! in a `<strong>`", "hide these two characters from the
//! rendered view", or "insert a widget here".
//!
//! Mirrors `@codemirror/view`'s `Decoration` (defined in
//! `~/Development/research/codemirror/view/src/decoration.ts`).
//! We keep four variants — the four CM6 ships — but only `Mark`
//! and `Replace` are wired into the v1 view.

/// What a decoration *does* visually.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DecorationKind {
    /// Wrap the covered range in an element with the given CSS
    /// class. Bold, italic, link, etc. `attrs` are extra HTML
    /// attributes copied verbatim onto the span (e.g.
    /// `data-href` for clickable links).
    Mark {
        class: String,
        attrs: Vec<(String, String)>,
    },
    /// Remove the covered range from the rendered output (the
    /// document still contains it). Used to hide markdown
    /// markers like `**` once the cursor leaves the span.
    Replace,
    /// Inject content at a point that isn't in the document.
    /// Used for inline widgets like a checkbox in front of a
    /// task. v1 stores the HTML as a string; later we can swap
    /// to a typed widget enum or Dioxus VNode.
    Widget { html: String },
    /// Add a CSS class to the line element containing the
    /// position. Used for the active-line highlight.
    Line { class: String },
    /// Treat the range as a single indivisible unit for cursor
    /// purposes. A caret landing inside snaps to the nearer
    /// edge; a non-empty selection that overlaps extends to
    /// cover the whole range. Behavior-only — does not change
    /// rendering. Ports CM6's `atomicRanges` facet
    /// (`view/src/extension.ts:295`, applied by
    /// `view/src/cursor.ts:skipAtomicRanges`).
    Atomic,
}

/// A decoration is just an inclusive-start, exclusive-end byte
/// range with a [`DecorationKind`]. Widgets and line decorations
/// are zero-width (`from == to`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecoratedRange {
    pub from: usize,
    pub to: usize,
    pub kind: DecorationKind,
}

impl DecoratedRange {
    pub fn mark(range: std::ops::Range<usize>, class: impl Into<String>) -> Self {
        Self {
            from: range.start,
            to: range.end,
            kind: DecorationKind::Mark {
                class: class.into(),
                attrs: Vec::new(),
            },
        }
    }
    pub fn mark_with_attrs(
        range: std::ops::Range<usize>,
        class: impl Into<String>,
        attrs: Vec<(String, String)>,
    ) -> Self {
        Self {
            from: range.start,
            to: range.end,
            kind: DecorationKind::Mark {
                class: class.into(),
                attrs,
            },
        }
    }

    pub fn replace(range: std::ops::Range<usize>) -> Self {
        Self {
            from: range.start,
            to: range.end,
            kind: DecorationKind::Replace,
        }
    }

    pub fn widget(at: usize, html: impl Into<String>) -> Self {
        Self {
            from: at,
            to: at,
            kind: DecorationKind::Widget { html: html.into() },
        }
    }

    pub fn line(at: usize, class: impl Into<String>) -> Self {
        Self {
            from: at,
            to: at,
            kind: DecorationKind::Line {
                class: class.into(),
            },
        }
    }
    pub fn atomic(range: std::ops::Range<usize>) -> Self {
        Self {
            from: range.start,
            to: range.end,
            kind: DecorationKind::Atomic,
        }
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.from..self.to
    }
}

/// Snap `pos` out of any atomic range it lands strictly inside,
/// preferring the nearer edge (CM6 picks the nearer edge when
/// `bias == 0`). Used to keep callers' selections from landing
/// in the middle of an atomic region. No-op when `pos` is at a
/// range boundary or no atomic range contains it.
///
/// Ports CM6's `skipAtomicRanges` (`view/src/cursor.ts`).
pub fn skip_atomic(decs: &[DecoratedRange], pos: usize) -> usize {
    let mut p = pos;
    loop {
        let mut moved = false;
        for d in decs {
            if !matches!(d.kind, DecorationKind::Atomic) {
                continue;
            }
            if p > d.from && p < d.to {
                p = if p - d.from <= d.to - p { d.from } else { d.to };
                moved = true;
            }
        }
        if !moved {
            return p;
        }
    }
}

/// Type alias used by extensions that produce many decorations
/// per render. v1 is a flat sorted `Vec`; later we'll back this
/// with a range tree like CM6's `RangeSet` for O(log n) lookup.
pub type DecorationSet = Vec<DecoratedRange>;

/// Marker type re-exported so callers can `use editor_state::Decoration;`
/// and reach the constructors without ceremony.
pub struct Decoration;

impl Decoration {
    pub fn mark(range: std::ops::Range<usize>, class: impl Into<String>) -> DecoratedRange {
        DecoratedRange::mark(range, class)
    }
    pub fn mark_with_attrs(
        range: std::ops::Range<usize>,
        class: impl Into<String>,
        attrs: Vec<(String, String)>,
    ) -> DecoratedRange {
        DecoratedRange::mark_with_attrs(range, class, attrs)
    }
    pub fn replace(range: std::ops::Range<usize>) -> DecoratedRange {
        DecoratedRange::replace(range)
    }
    pub fn widget(at: usize, html: impl Into<String>) -> DecoratedRange {
        DecoratedRange::widget(at, html)
    }
    pub fn line(at: usize, class: impl Into<String>) -> DecoratedRange {
        DecoratedRange::line(at, class)
    }
    pub fn atomic(range: std::ops::Range<usize>) -> DecoratedRange {
        DecoratedRange::atomic(range)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mark_constructor() {
        let d = Decoration::mark(2..5, "bold");
        assert_eq!(d.from, 2);
        assert_eq!(d.to, 5);
        assert_eq!(
            d.kind,
            DecorationKind::Mark {
                class: "bold".into(),
                attrs: Vec::new(),
            }
        );
    }

    #[test]
    fn replace_constructor() {
        let d = Decoration::replace(0..2);
        assert!(matches!(d.kind, DecorationKind::Replace));
    }

    #[test]
    fn widget_is_zero_width() {
        let d = Decoration::widget(7, "<span/>");
        assert_eq!(d.from, d.to);
    }
}

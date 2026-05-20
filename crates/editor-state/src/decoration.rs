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
    /// class. Bold, italic, link, etc.
    Mark { class: String },
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

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.from..self.to
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
    pub fn replace(range: std::ops::Range<usize>) -> DecoratedRange {
        DecoratedRange::replace(range)
    }
    pub fn widget(at: usize, html: impl Into<String>) -> DecoratedRange {
        DecoratedRange::widget(at, html)
    }
    pub fn line(at: usize, class: impl Into<String>) -> DecoratedRange {
        DecoratedRange::line(at, class)
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
                class: "bold".into()
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

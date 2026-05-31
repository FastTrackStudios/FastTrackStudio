//! Keyflow language support for the Editor.
//!
//! Two pure functions bridge keyflow's text tooling into the editor's
//! decoration model so a `.kf` document edited in the [`editor`] view gets the
//! same syntax colors and live-diagnostic squiggles the standalone keyflow
//! editor shows:
//!
//! - [`keyflow_decorations`] is a [`DecorationSource`]-shaped
//!   `fn(&EditorState) -> Vec<DecoratedRange>`. It runs keyflow's line
//!   highlighter over every line and `ide::analyze` over the whole document,
//!   emitting `Mark` decorations carrying the keyflow CSS class for each token
//!   and a wavy-underline class for each diagnostic.
//! - [`highlight_css`] returns the stylesheet that colors those classes — the
//!   keyflow theme's `.kf-*` rules plus the `.kf-diag-*` squiggle rules.
//!
//! No Dioxus dependency: callers wire the function into `<Editor decorations>`
//! and inject the CSS however they mount styles.
//!
//! [`DecorationSource`]: https://docs.rs/editor-view (editor_view::DecorationSource)

use editor_state::{DecoratedRange, Decoration, EditorState};
use keyflow::text::highlighting::{Highlighter, Renderer, Theme};
use keyflow::text::ide::{self, Severity};

/// Build the keyflow decoration set for the current document.
///
/// Coerces to `editor`'s `DecorationSource = fn(&EditorState) ->
/// Vec<DecoratedRange>` — pass it straight into the `<Editor>` `decorations`
/// prop.
///
/// Highlight spans come from [`Highlighter::highlight_line`], which works per
/// line with line-relative byte offsets; we add each line's start offset to
/// get document-absolute ranges. Diagnostics come from [`ide::analyze`], whose
/// ranges are already absolute. The returned vec is sorted by start offset.
#[must_use]
pub fn keyflow_decorations(state: &EditorState) -> Vec<DecoratedRange> {
    let text = state.doc.to_string();
    let len = text.len();
    let mut out: Vec<DecoratedRange> = Vec::new();

    // Syntax highlighting — one pass per line, offsets shifted to absolute.
    // `split_inclusive` keeps the trailing '\n' so `line_start` advances by the
    // full line length; we strip it (and a stray '\r') before highlighting.
    let mut line_start = 0usize;
    for line in text.split_inclusive('\n') {
        let content = line.strip_suffix('\n').unwrap_or(line);
        let content = content.strip_suffix('\r').unwrap_or(content);
        for span in Highlighter::highlight_line(content) {
            let start = line_start + span.span.start;
            let end = line_start + span.span.end();
            if start < end && end <= len {
                out.push(Decoration::mark(start..end, span.kind.css_class()));
            }
        }
        line_start += line.len();
    }

    // Live diagnostics — wavy underline + the message as a `title` tooltip.
    for d in ide::analyze(&text).diagnostics {
        let start = d.range.start;
        let end = d.range.end();
        if start < end && end <= len {
            out.push(Decoration::mark_with_attrs(
                start..end,
                diagnostic_class(d.severity),
                vec![("title".to_string(), d.message.clone())],
            ));
        }
    }

    out.sort_by_key(|d| d.from);
    out
}

/// CSS for the classes [`keyflow_decorations`] emits: the keyflow theme's
/// per-token `.kf-*` color rules (from [`Renderer::generate_css`]) plus the
/// `.kf-diag-*` wavy-underline rules for diagnostics.
///
/// The squiggle colors reference `fts-ui` theme custom properties with literal
/// fallbacks, so they track light/dark themes when those tokens are defined and
/// still render standalone when they aren't.
#[must_use]
pub fn highlight_css(theme: &Theme) -> String {
    let mut css = Renderer::generate_css(theme);
    css.push_str(DIAGNOSTIC_CSS);
    css
}

/// Wavy-underline class for a diagnostic severity. Two classes per range: the
/// shared `kf-diag` (offset/skip-ink) plus the severity color class.
const fn diagnostic_class(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "kf-diag kf-diag-error",
        Severity::Warning => "kf-diag kf-diag-warning",
        Severity::Info => "kf-diag kf-diag-info",
        Severity::Hint => "kf-diag kf-diag-hint",
    }
}

const DIAGNOSTIC_CSS: &str = "\
.kf-diag { text-decoration-skip-ink: none; text-underline-offset: 3px; }
.kf-diag-error { text-decoration: underline wavy var(--destructive, #e5484d); }
.kf-diag-warning { text-decoration: underline wavy var(--warning, #f5a623); }
.kf-diag-info { text-decoration: underline wavy var(--info, #4a9eff); }
.kf-diag-hint { text-decoration: underline wavy var(--muted-foreground, #888888); }
";

#[cfg(test)]
mod tests {
    use super::*;
    use editor_state::EditorState;

    #[test]
    fn empty_doc_has_no_decorations() {
        let state = EditorState::new(String::new());
        assert!(keyflow_decorations(&state).is_empty());
    }

    #[test]
    fn highlights_a_chord_line() {
        let state = EditorState::new("Cmaj7 | G7".to_string());
        let decs = keyflow_decorations(&state);
        // At least the two chord roots should be marked.
        assert!(!decs.is_empty());
        // Sorted by start offset.
        assert!(decs.windows(2).all(|w| w[0].from <= w[1].from));
        // Every range is within bounds.
        let len = state.doc.to_string().len();
        assert!(decs.iter().all(|d| d.to <= len && d.from < d.to));
    }

    #[test]
    fn absolute_offsets_across_lines() {
        // A token on the second line must be marked at a doc-absolute offset
        // (past the first line + newline), never at a line-relative one.
        let state = EditorState::new("Cmaj7\nDm7".to_string());
        let decs = keyflow_decorations(&state);
        assert!(decs.iter().any(|d| d.from >= 6), "second-line token should be at absolute offset >= 6");
    }

    #[test]
    fn css_includes_diagnostic_rules() {
        let css = highlight_css(&Theme::default_dark());
        assert!(css.contains(".kf-diag-error"));
    }
}

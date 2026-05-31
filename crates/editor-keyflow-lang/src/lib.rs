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

use std::sync::atomic::{AtomicBool, Ordering};

use editor_state::{DecoratedRange, Decoration, EditorState, HoverTooltip};
use keyflow_text::highlighting::{Highlighter, Renderer, Theme};
use keyflow_text::ide::{self, Severity};
use keyflow_text::key::Key;
use keyflow_text::primitives::RootNotation;

/// Re-export so callers can pick a palette for [`highlight_css`] without taking
/// a direct `keyflow-text` dependency:
/// `editor_keyflow_lang::HighlightTheme::default_dark()`.
pub use keyflow_text::highlighting::Theme as HighlightTheme;

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

    // One analysis pass feeds both diagnostics and the resolved-chord overlays.
    let analysis = ide::analyze(&text);

    // Live diagnostics — wavy underline + the message as a `title` tooltip.
    for d in &analysis.diagnostics {
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

    // Resolved-chord type overlays — IDE-style inline inlay badges. For each
    // chord written in a key-relative system (Nashville number / Roman) that
    // resolves against the chart's key, drop a dim badge right after the symbol
    // showing the absolute letter-name chord (e.g. `5` → `G`, `V7` → `G7`).
    // Gated by `SHOW_OVERLAYS` so the app can toggle it (the `fn` source can't
    // carry the flag itself).
    if overlays_enabled() {
        if let Some(key) = analysis
            .chart
            .current_key
            .as_ref()
            .or(analysis.chart.initial_key.as_ref())
        {
            for section in &analysis.chart.sections {
                for measure in section.measures() {
                    for ci in &measure.chords {
                        let Some(span) = ci.source_span else { continue };
                        let at = span.end().min(len);
                        if let Some(label) = resolved_chord_label(ci, key) {
                            out.push(Decoration::widget(
                                at,
                                format!("<span class=\"kf-inlay\">{}</span>", escape_html(&label)),
                            ));
                        }
                    }
                }
            }
        }
    }

    out.sort_by_key(|d| d.from);
    out
}

/// Resolve a chord instance written in a key-relative system to its absolute
/// letter-name display. Returns `None` for chords already written as note
/// names (nothing to add) or that don't resolve against `key`.
///
/// Reuses keyflow's own resolution: [`RootNotation::resolve`] gives the
/// absolute root note; swapping the chord's root for that note and rendering
/// via `Chord`'s `Display` keeps the original quality/extensions/bass intact
/// (e.g. `5maj7` → `Gmaj7`).
fn resolved_chord_label(ci: &keyflow_text::chart::ChordInstance, key: &Key) -> Option<String> {
    if !ci.root.is_key_relative() {
        return None;
    }
    let resolved_root = ci.parsed.root.resolve(Some(key))?;
    let mut absolute = ci.parsed.clone();
    absolute.root = RootNotation::from_note_name(resolved_root);
    Some(absolute.to_string())
}

/// Process-global toggle for the resolved-chord overlays. Default on.
static SHOW_OVERLAYS: AtomicBool = AtomicBool::new(true);

/// Whether resolved-chord type overlays are currently shown.
#[must_use]
pub fn overlays_enabled() -> bool {
    SHOW_OVERLAYS.load(Ordering::Relaxed)
}

/// Turn resolved-chord type overlays on or off. The app should bump a signal
/// after calling this so the editor recomputes decorations.
pub fn set_overlays_enabled(on: bool) {
    SHOW_OVERLAYS.store(on, Ordering::Relaxed);
}

/// Flip the resolved-chord overlay toggle, returning the new state.
pub fn toggle_overlays() -> bool {
    let next = !overlays_enabled();
    set_overlays_enabled(next);
    next
}

/// Hover source for the keyflow editor — a [`HoverSource`]-shaped
/// `fn(&EditorState, usize) -> Option<HoverTooltip>`. Pass it straight into the
/// `<Editor>` `hover` prop.
///
/// Combines, for the byte offset under the pointer:
/// - any diagnostic whose range covers it (rendered first, colored by
///   severity), so hovering a squiggle shows the keyflow error, and
/// - keyflow's [`ide::hover`] token info (chord / scale degree / command /
///   melody-variable), rendered from its markdown.
///
/// [`HoverSource`]: editor_state::HoverSource
#[must_use]
pub fn keyflow_hover(state: &EditorState, pos: usize) -> Option<HoverTooltip> {
    let text = state.doc.to_string();
    let pos = pos.min(text.len());
    let analysis = ide::analyze(&text);

    let mut html = String::new();
    let mut from = pos;
    let mut to = pos;

    let mut covered = false;
    for d in &analysis.diagnostics {
        let (s, e) = (d.range.start, d.range.end());
        if pos >= s && pos <= e {
            html.push_str(&format!(
                "<div class=\"kf-hover-diag kf-hover-{}\"><strong>{}</strong> {}</div>",
                severity_slug(d.severity),
                severity_label(d.severity),
                escape_html(&d.message),
            ));
            if !covered {
                from = s;
                to = e;
                covered = true;
            }
        }
    }

    if let Some(info) = ide::hover(&text, pos, &analysis.chart) {
        html.push_str(&format!(
            "<div class=\"kf-hover-info\">{}</div>",
            md_to_html(&info.markdown)
        ));
        if !covered {
            from = info.range.start;
            to = info.range.end();
        }
    }

    if html.is_empty() {
        return None;
    }
    Some(HoverTooltip { html, from, to })
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
    css.push_str(OVERLAY_CSS);
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

const fn severity_slug(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Hint => "hint",
    }
}

const fn severity_label(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "Error",
        Severity::Warning => "Warning",
        Severity::Info => "Info",
        Severity::Hint => "Hint",
    }
}

/// Escape the five HTML-significant characters for safe injection into the
/// widget / tooltip `dangerous_inner_html`.
fn escape_html(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

/// Tiny markdown → HTML for the short snippets `ide::hover` emits: fenced code
/// blocks, inline `` `code` ``, `**bold**`, and newlines. Input is escaped
/// first, so the only HTML produced is from these recognized tokens.
fn md_to_html(md: &str) -> String {
    let mut out = String::new();
    // Split on fenced code blocks first; even segments are prose, odd are code.
    for (i, seg) in md.split("```").enumerate() {
        if i % 2 == 1 {
            out.push_str("<pre class=\"kf-hover-code\">");
            out.push_str(&escape_html(seg.trim_matches('\n')));
            out.push_str("</pre>");
        } else {
            out.push_str(&inline_md(seg));
        }
    }
    out
}

/// Inline markdown for a prose segment: `**bold**`, `` `code` ``, and newlines.
fn inline_md(seg: &str) -> String {
    let esc = escape_html(seg);
    // `**bold**` → <strong>
    let mut s = String::with_capacity(esc.len());
    let mut rest = esc.as_str();
    let mut bold_open = false;
    while let Some(idx) = rest.find("**") {
        s.push_str(&rest[..idx]);
        s.push_str(if bold_open { "</strong>" } else { "<strong>" });
        bold_open = !bold_open;
        rest = &rest[idx + 2..];
    }
    s.push_str(rest);
    if bold_open {
        s.push_str("</strong>");
    }
    // `` `code` `` → <code>
    let mut t = String::with_capacity(s.len());
    let mut rest = s.as_str();
    let mut code_open = false;
    while let Some(idx) = rest.find('`') {
        t.push_str(&rest[..idx]);
        t.push_str(if code_open { "</code>" } else { "<code>" });
        code_open = !code_open;
        rest = &rest[idx + 1..];
    }
    t.push_str(rest);
    if code_open {
        t.push_str("</code>");
    }
    t.replace('\n', "<br>")
}

const DIAGNOSTIC_CSS: &str = "\
.kf-diag { text-decoration-skip-ink: none; text-underline-offset: 3px; }
.kf-diag-error { text-decoration: underline wavy var(--destructive, #e5484d); }
.kf-diag-warning { text-decoration: underline wavy var(--warning, #f5a623); }
.kf-diag-info { text-decoration: underline wavy var(--info, #4a9eff); }
.kf-diag-hint { text-decoration: underline wavy var(--muted-foreground, #888888); }
";

/// Styles for the resolved-chord inlay badges and the keyflow hover content.
/// `.editor-hover-tooltip` (the floating container) is styled by the editor
/// view itself; these rules cover the keyflow-specific bits inside it plus the
/// inline `.kf-inlay` badge.
const OVERLAY_CSS: &str = "\
.kf-inlay {
  display: inline-block;
  margin: 0 0.15em 0 0.3em;
  padding: 0 0.35em;
  border-radius: 0.35em;
  font-size: 0.82em;
  line-height: 1.45;
  vertical-align: baseline;
  color: var(--muted-foreground, #9a9aa2);
  background: color-mix(in srgb, var(--muted-foreground, #9a9aa2) 14%, transparent);
  user-select: none;
}
.kf-hover-info { white-space: normal; }
.kf-hover-info code { font-family: ui-monospace, Menlo, monospace; }
.kf-hover-code {
  margin: 0.35em 0 0;
  padding: 0.35em 0.5em;
  border-radius: 0.3em;
  background: color-mix(in srgb, var(--muted-foreground, #9a9aa2) 12%, transparent);
  font-family: ui-monospace, Menlo, monospace;
  white-space: pre-wrap;
}
.kf-hover-diag { margin-bottom: 0.35em; }
.kf-hover-error strong { color: var(--destructive, #e5484d); }
.kf-hover-warning strong { color: var(--warning, #f5a623); }
.kf-hover-info strong, .kf-hover-hint strong { color: var(--info, #4a9eff); }
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
    fn css_includes_diagnostic_and_overlay_rules() {
        let css = highlight_css(&Theme::default_dark());
        assert!(css.contains(".kf-diag-error"));
        assert!(css.contains(".kf-inlay"));
    }

    #[test]
    fn escape_html_handles_specials() {
        assert_eq!(escape_html("<a>&\"'"), "&lt;a&gt;&amp;&quot;&#39;");
    }

    #[test]
    fn md_to_html_basic() {
        let h = md_to_html("**bold** and `code`\nline2");
        assert!(h.contains("<strong>bold</strong>"));
        assert!(h.contains("<code>code</code>"));
        assert!(h.contains("<br>"));
    }

    #[test]
    fn md_to_html_fenced_code() {
        let h = md_to_html("text\n```\nC | G\n```");
        assert!(h.contains("<pre class=\"kf-hover-code\">C | G</pre>"));
    }

    #[test]
    fn hover_returns_content_on_a_chord() {
        let state = EditorState::new("Cmaj7 | G7".to_string());
        let h = keyflow_hover(&state, 1).expect("hover over a chord token");
        assert!(!h.html.is_empty());
        assert!(h.from <= 1 && 1 <= h.to);
    }

    #[test]
    fn hover_none_on_blank() {
        let state = EditorState::new("   ".to_string());
        assert!(keyflow_hover(&state, 1).is_none());
    }

    fn widget_count(decs: &[DecoratedRange]) -> usize {
        decs.iter()
            .filter(|d| matches!(d.kind, editor_state::DecorationKind::Widget { .. }))
            .count()
    }

    // One self-contained test owns the process-global overlay toggle so parallel
    // tests don't race on it.
    #[test]
    fn overlays_resolve_number_system_and_toggle() {
        // `#C` sets the key; `5` and `1` are scale-degree chords that resolve to
        // G and C in C major.
        let state = EditorState::new("4/4 120bpm #C\n\n5 | 1\n".to_string());

        set_overlays_enabled(true);
        let on = keyflow_decorations(&state);
        let widgets: Vec<_> = on
            .iter()
            .filter_map(|d| match &d.kind {
                editor_state::DecorationKind::Widget { html } => Some(html.clone()),
                _ => None,
            })
            .collect();
        assert!(
            !widgets.is_empty(),
            "expected resolved-chord overlays for a #C number-system chart, got none"
        );
        // The `5` should resolve to a G-rooted badge.
        assert!(
            widgets.iter().any(|h| h.contains("kf-inlay") && h.contains('G')),
            "expected a 'G' inlay badge for degree 5 in C; widgets = {widgets:?}"
        );

        set_overlays_enabled(false);
        assert_eq!(
            widget_count(&keyflow_decorations(&state)),
            0,
            "overlays disabled should emit no widget decorations"
        );

        set_overlays_enabled(true); // restore default
    }
}

//! In-note section tabs for `type: event` notes — the Order | Teams |
//! Times facets from `plans/event-planner.md`.
//!
//! Pure decoration layer: every top-level `# Section` AFTER the title H1
//! becomes a tab; the tab bar renders as a widget where the first section
//! starts, and every non-active section (heading + body) is hidden with
//! `Replace` decorations. Tab clicks arrive as `event-tab:<name>` hrefs
//! through the editor's link channel; the active tab lives in a global
//! signal (one focused event note at a time). The caret escapes the
//! system: if the selection sits inside a hidden section, that section
//! becomes active automatically, so editing never fights the tabs.

use dioxus::prelude::*;
use editor::state::EditorState;
use editor::{Decoration, DecoratedRange};

/// The active event tab (section name). Empty = first section.
pub static EVENT_ACTIVE_TAB: GlobalSignal<String> = Signal::global(String::new);

/// Section spans: `(heading_line_start, body_end, name)` for every
/// top-level `#` after the first (the title).
fn sections(text: &str) -> Vec<(usize, usize, String)> {
    let mut heads: Vec<(usize, String)> = Vec::new();
    let mut pos = 0;
    let mut seen_title = false;
    let mut in_fence = false;
    for line in text.split_inclusive('\n') {
        let content = line.strip_suffix('\n').unwrap_or(line);
        let t = content.trim_start();
        if t.starts_with("```") {
            in_fence = !in_fence;
        }
        if !in_fence && content.starts_with("# ") {
            if seen_title {
                heads.push((pos, content[2..].trim().to_owned()));
            } else {
                seen_title = true;
            }
        }
        pos += line.len();
    }
    let mut out = Vec::new();
    for (i, (start, name)) in heads.iter().enumerate() {
        let end = heads.get(i + 1).map_or(text.len(), |(s, _)| *s);
        out.push((*start, end, name.clone()));
    }
    out
}

/// Decorations for the event tab system (empty when the note isn't an
/// event or has fewer than two sections).
pub fn event_tab_decorations(state: &EditorState) -> Vec<DecoratedRange> {
    let text = state.doc.to_string();
    let is_event = text.strip_prefix("---").is_some_and(|rest| {
        rest.split_once("\n---").is_some_and(|(front, _)| {
            front.lines().any(|l| {
                l.trim_start()
                    .strip_prefix("type:")
                    .is_some_and(|v| v.trim().trim_matches(['"', '\'']) == "event")
            })
        })
    });
    if !is_event {
        return Vec::new();
    }
    let secs = sections(&text);
    if secs.len() < 2 {
        return Vec::new();
    }

    // Resolve the active section: caret-inside wins, then the signal,
    // then the first section.
    let caret = state.selection.primary().head;
    let caret_section = secs
        .iter()
        .find(|(s, e, _)| (*s..*e).contains(&caret))
        .map(|(_, _, n)| n.clone());
    let wanted = EVENT_ACTIVE_TAB.read().clone();
    let active = caret_section
        .or_else(|| secs.iter().find(|(_, _, n)| *n == wanted).map(|(_, _, n)| n.clone()))
        .unwrap_or_else(|| secs[0].2.clone());

    let mut out = Vec::new();
    // Tab bar widget at the first section's start.
    let tabs: String = secs
        .iter()
        .map(|(_, _, name)| {
            let cls = if *name == active {
                "md-note-tab md-note-tab--active"
            } else {
                "md-note-tab"
            };
            format!(
                r#"<span class="{cls}" data-href="event-tab:{name}">{name}</span>"#,
                name = name,
            )
        })
        .collect();
    out.push(Decoration::widget(
        secs[0].0,
        format!(r#"<span class="md-note-tabs">{tabs}</span>"#),
    ));
    for (start, end, name) in &secs {
        if *name == active {
            // Hide the active section's own `# Heading` line (the tab is
            // the label) — body stays.
            let head_end = text[*start..].find('\n').map_or(*end, |i| start + i + 1);
            out.push(Decoration::replace(*start..head_end));
        } else {
            out.push(Decoration::replace(*start..*end));
        }
    }
    out
}

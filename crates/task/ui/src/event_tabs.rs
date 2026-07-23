//! In-note section tabs.
//!
//! Two flavors, both a pure decoration layer:
//!
//! 1. **Event sections** — for `type: event` notes, every top-level
//!    `# Section` AFTER the title H1 becomes a tab (the Order | Teams |
//!    Times facets from `plans/event-planner.md`).
//! 2. **`tabs` fences** — a GENERAL note syntax usable in ANY note: a
//!    ```` ```tabs ```` fenced block whose body is split into tabs by
//!    `=== Tab Name` delimiter lines. Multiple such blocks per note are
//!    supported; each keeps its own active tab.
//!
//! The tab bar renders as a widget where the section/block starts, and
//! every non-active tab body (plus the fence/delimiter markers) is hidden
//! with `Replace` decorations. Tab clicks arrive as `event-tab:<name>`
//! (event sections) or `tab:<block>:<name>` (fences) hrefs through the
//! editor's link channel; the active tab lives in a global signal. The
//! caret escapes the system: if the selection sits inside a hidden
//! section/tab, that one becomes active automatically, so editing never
//! fights the tabs.

use std::collections::HashMap;

use dioxus::prelude::*;
use editor::state::EditorState;
use editor::{Decoration, DecoratedRange};

/// The active event-section tab (section name). Empty = first section.
pub static EVENT_ACTIVE_TAB: GlobalSignal<String> = Signal::global(String::new);

/// Active tab per ```` ```tabs ```` block, keyed by the block's index in
/// document order (stable across text edits within a block).
pub static FENCE_ACTIVE_TABS: GlobalSignal<HashMap<usize, String>> =
    Signal::global(HashMap::new);

/// Handle a tab-selection href from the editor's link channel. Returns
/// `true` if it was a tab href (event section or fence tab) and was
/// applied. Callers should treat a `true` return as "consumed".
pub fn handle_tab_href(href: &str) -> bool {
    if let Some(name) = href.strip_prefix("event-tab:") {
        *EVENT_ACTIVE_TAB.write() = name.to_string();
        return true;
    }
    if let Some(rest) = href.strip_prefix("tab:") {
        if let Some((idx, name)) = rest.split_once(':') {
            if let Ok(block) = idx.parse::<usize>() {
                FENCE_ACTIVE_TABS.write().insert(block, name.to_string());
                return true;
            }
        }
    }
    false
}

/// Decorations for both tab systems. Empty when the note has neither
/// event sections nor `tabs` fences.
pub fn event_tab_decorations(state: &EditorState) -> Vec<DecoratedRange> {
    let text = state.doc.to_string();
    let mut out = event_section_decorations(&text, state);
    out.extend(fence_tab_decorations(&text, state));
    out
}

// ── event `# Section` tabs (type: event) ────────────────────────────

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

fn event_section_decorations(text: &str, state: &EditorState) -> Vec<DecoratedRange> {
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
    let secs = sections(text);
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
        .or_else(|| {
            secs.iter()
                .find(|(_, _, n)| *n == wanted)
                .map(|(_, _, n)| n.clone())
        })
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
            format!(r#"<span class="{cls}" data-href="event-tab:{name}">{name}</span>"#, name = name)
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

// ── general ```tabs fences (any note) ───────────────────────────────

/// One tab inside a fence: the `=== Name` delimiter line, the tab name,
/// and the body span `[start, end)`.
struct FenceTab {
    delim: (usize, usize),
    name: String,
    body: (usize, usize),
}

/// One ```` ```tabs ```` block: the opening + closing fence line spans
/// and its tabs.
struct TabsBlock {
    open: (usize, usize),
    close: (usize, usize),
    tabs: Vec<FenceTab>,
}

/// Parse every ```` ```tabs ```` block. Nested code fences inside a tab
/// body are tracked so their ``` markers and `===` lines aren't mistaken
/// for tab structure.
fn tabs_blocks(text: &str) -> Vec<TabsBlock> {
    // (byte_start, line_with_newline) per line.
    let mut lines: Vec<(usize, &str)> = Vec::new();
    let mut pos = 0;
    for line in text.split_inclusive('\n') {
        lines.push((pos, line));
        pos += line.len();
    }

    let mut blocks = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let (lstart, line) = lines[i];
        let content = line.strip_suffix('\n').unwrap_or(line);
        if content.trim_start().starts_with("```tabs") {
            let open = (lstart, lstart + line.len());
            let mut tabs: Vec<FenceTab> = Vec::new();
            let mut nested = false;
            let mut close = (text.len(), text.len());
            let mut j = i + 1;
            while j < lines.len() {
                let (js, jline) = lines[j];
                let jcontent = jline.strip_suffix('\n').unwrap_or(jline);
                let t = jcontent.trim_start();
                if t.starts_with("```") {
                    let rest = t.trim_start_matches('`');
                    if !nested && rest.trim().is_empty() {
                        close = (js, js + jline.len());
                        break; // closing fence of the tabs block
                    } else if !nested {
                        nested = true; // opening a nested code fence
                    } else {
                        nested = false; // closing the nested code fence
                    }
                } else if !nested && t.starts_with("===") {
                    let name = t.trim_start_matches('=').trim().to_string();
                    if let Some(prev) = tabs.last_mut() {
                        prev.body.1 = js;
                    }
                    tabs.push(FenceTab {
                        delim: (js, js + jline.len()),
                        name,
                        body: (js + jline.len(), text.len()),
                    });
                }
                j += 1;
            }
            if let Some(prev) = tabs.last_mut() {
                prev.body.1 = close.0;
            }
            if !tabs.is_empty() {
                blocks.push(TabsBlock { open, close, tabs });
            }
            i = j + 1;
        } else {
            i += 1;
        }
    }
    blocks
}

fn fence_tab_decorations(text: &str, state: &EditorState) -> Vec<DecoratedRange> {
    let blocks = tabs_blocks(text);
    if blocks.is_empty() {
        return Vec::new();
    }
    let caret = state.selection.primary().head;
    let active_map = FENCE_ACTIVE_TABS.read();
    let mut out = Vec::new();
    for (bi, block) in blocks.iter().enumerate() {
        // Active tab: caret-inside wins, then the per-block signal, then
        // the first tab.
        let caret_tab = block
            .tabs
            .iter()
            .find(|t| (t.delim.0..t.body.1).contains(&caret))
            .map(|t| t.name.clone());
        let wanted = active_map.get(&bi).cloned();
        let active = caret_tab
            .or_else(|| wanted.filter(|w| block.tabs.iter().any(|t| &t.name == w)))
            .unwrap_or_else(|| block.tabs[0].name.clone());

        // Tab bar widget where the opening fence was.
        let tabs_html: String = block
            .tabs
            .iter()
            .map(|t| {
                let cls = if t.name == active {
                    "md-note-tab md-note-tab--active"
                } else {
                    "md-note-tab"
                };
                format!(
                    r#"<span class="{cls}" data-href="tab:{bi}:{name}">{name}</span>"#,
                    name = t.name,
                )
            })
            .collect();
        out.push(Decoration::widget(
            block.open.0,
            format!(r#"<span class="md-note-tabs">{tabs_html}</span>"#),
        ));
        // Hide the fence markers.
        out.push(Decoration::replace(block.open.0..block.open.1));
        if block.close.0 < block.close.1 {
            out.push(Decoration::replace(block.close.0..block.close.1));
        }
        // Each tab: hide its `=== Name` delimiter; hide the body unless
        // it's the active tab.
        for t in &block.tabs {
            out.push(Decoration::replace(t.delim.0..t.delim.1));
            if t.name != active {
                out.push(Decoration::replace(t.body.0..t.body.1));
            }
        }
    }
    out
}

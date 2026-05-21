//! Slash-command catalog + runner. Mirrors Logseq's
//! `frontend.commands` namespace — each command is a record
//! `{label, group, desc, action}` and `run_command` interprets
//! the action against the editor state.
//!
//! Logseq encodes its commands as `[label, [[:editor/input
//! "txt"] [:editor/search-page]], desc, :icon/foo, group]`
//! tuples. We collapse that to a simple Rust enum because we
//! only need a fraction of the dispatch surface; if we end up
//! wanting first-class steps we can swap the enum for a
//! `Vec<Action>` later without touching callers.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::handler::block::{split_block, update_block_content};
use crate::state::AppState;

/// One menu entry. Title is what shows in the row, group is the
/// header above it ("Basic", "Format", "Task", ...).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommandEntry {
    pub label: &'static str,
    pub group: &'static str,
    pub desc: &'static str,
    pub kind: CommandKind,
}

/// What the command does. Each variant carries the data the
/// runner needs — text to insert, marker to apply, etc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommandKind {
    /// Replace the block's leading task marker (TODO / DOING / …).
    SetTask(TaskMarker),
    /// Promote / demote a heading. `Some(n)` for `# …` through
    /// `###### …`; `None` to remove a heading marker.
    SetHeading(Option<u8>),
    /// Insert a literal snippet at the caret. `caret_back` is how
    /// many characters to move the caret back from the end of the
    /// insert (`"[[]]"`, 2 → caret between the brackets).
    InsertSnippet(&'static str, usize),
    /// Append/insert a journal date reference. The runner resolves
    /// the date.
    InsertDate(DateRef),
    /// Insert the current wall-clock time (`HH:MM`).
    InsertCurrentTime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskMarker {
    Todo,
    Doing,
    Done,
    Later,
    Now,
    Waiting,
    Cancelled,
}

impl TaskMarker {
    pub fn label(self) -> &'static str {
        match self {
            TaskMarker::Todo => "TODO",
            TaskMarker::Doing => "DOING",
            TaskMarker::Done => "DONE",
            TaskMarker::Later => "LATER",
            TaskMarker::Now => "NOW",
            TaskMarker::Waiting => "WAITING",
            TaskMarker::Cancelled => "CANCELLED",
        }
    }
    pub fn all() -> &'static [TaskMarker] {
        &[
            TaskMarker::Todo,
            TaskMarker::Doing,
            TaskMarker::Done,
            TaskMarker::Later,
            TaskMarker::Now,
            TaskMarker::Waiting,
            TaskMarker::Cancelled,
        ]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DateRef {
    Today,
    Tomorrow,
    Yesterday,
}

/// The full catalog. Static-friendly — every command is
/// const-foldable, and the menu component filters this list at
/// render time. New commands go here.
pub fn all_commands() -> Vec<CommandEntry> {
    let mut out: Vec<CommandEntry> = Vec::new();

    // Group: Basic
    out.extend([
        CommandEntry {
            label: "Page reference",
            group: "Basic",
            desc: "Insert a [[Page]] link",
            kind: CommandKind::InsertSnippet("[[]]", 2),
        },
        CommandEntry {
            label: "Block reference",
            group: "Basic",
            desc: "Insert a ((block-id)) reference",
            kind: CommandKind::InsertSnippet("(())", 2),
        },
        CommandEntry {
            label: "Embed",
            group: "Basic",
            desc: "Embed a page or block — {{embed [[Page]]}}",
            kind: CommandKind::InsertSnippet("{{embed [[]]}}", 4),
        },
    ]);

    // Group: Format
    // Bold (Cmd-B), Italic (Cmd-I), Inline code (Cmd-E), and
    // Link (Cmd-K) live in keyboard shortcuts — see
    // `components::EditableBlock::on_keydown`. Mirrors Logseq's
    // split between the slash menu and its toolbar shortcuts.
    out.extend([
        CommandEntry {
            label: "Highlight",
            group: "Format",
            desc: "Wrap selection with ==highlight==",
            kind: CommandKind::InsertSnippet("====", 2),
        },
        CommandEntry {
            label: "Strikethrough",
            group: "Format",
            desc: "Wrap selection with ~~strike~~",
            kind: CommandKind::InsertSnippet("~~~~", 2),
        },
        CommandEntry {
            label: "Image",
            group: "Format",
            desc: "![alt](url)",
            kind: CommandKind::InsertSnippet("![]()", 3),
        },
        CommandEntry {
            label: "Math (inline)",
            group: "Format",
            desc: "$expr$",
            kind: CommandKind::InsertSnippet("$$", 1),
        },
    ]);

    // Group: Heading
    for level in 1u8..=6 {
        let label = match level {
            1 => "Heading 1",
            2 => "Heading 2",
            3 => "Heading 3",
            4 => "Heading 4",
            5 => "Heading 5",
            6 => "Heading 6",
            _ => unreachable!(),
        };
        out.push(CommandEntry {
            label,
            group: "Heading",
            desc: "Convert block to heading",
            kind: CommandKind::SetHeading(Some(level)),
        });
    }
    out.push(CommandEntry {
        label: "Plain paragraph",
        group: "Heading",
        desc: "Remove heading marker",
        kind: CommandKind::SetHeading(None),
    });

    // Group: Task
    for marker in TaskMarker::all() {
        let (label, desc) = match marker {
            TaskMarker::Todo => ("TODO", "Mark as TODO"),
            TaskMarker::Doing => ("DOING", "Mark as DOING"),
            TaskMarker::Done => ("DONE", "Mark as DONE"),
            TaskMarker::Later => ("LATER", "Mark as LATER"),
            TaskMarker::Now => ("NOW", "Mark as NOW"),
            TaskMarker::Waiting => ("WAITING", "Mark as WAITING"),
            TaskMarker::Cancelled => ("CANCELLED", "Mark as CANCELLED"),
        };
        out.push(CommandEntry {
            label,
            group: "Task",
            desc,
            kind: CommandKind::SetTask(*marker),
        });
    }

    // Group: Date
    out.extend([
        CommandEntry {
            label: "Today",
            group: "Date",
            desc: "Insert today's journal page reference",
            kind: CommandKind::InsertDate(DateRef::Today),
        },
        CommandEntry {
            label: "Tomorrow",
            group: "Date",
            desc: "Insert tomorrow's journal page reference",
            kind: CommandKind::InsertDate(DateRef::Tomorrow),
        },
        CommandEntry {
            label: "Yesterday",
            group: "Date",
            desc: "Insert yesterday's journal page reference",
            kind: CommandKind::InsertDate(DateRef::Yesterday),
        },
        CommandEntry {
            label: "Current time",
            group: "Date",
            desc: "Insert HH:MM",
            kind: CommandKind::InsertCurrentTime,
        },
    ]);

    // Group: Macro
    out.extend([
        CommandEntry {
            label: "Query",
            group: "Macro",
            desc: "Embed a live {{query #tag}} result list",
            kind: CommandKind::InsertSnippet("{{query }}", 2),
        },
        CommandEntry {
            label: "Video",
            group: "Macro",
            desc: "{{video <url>}}",
            kind: CommandKind::InsertSnippet("{{video }}", 2),
        },
        CommandEntry {
            label: "YouTube",
            group: "Macro",
            desc: "{{youtube <id-or-url>}}",
            kind: CommandKind::InsertSnippet("{{youtube }}", 2),
        },
        CommandEntry {
            label: "Tweet",
            group: "Macro",
            desc: "{{tweet <id-or-url>}}",
            kind: CommandKind::InsertSnippet("{{tweet }}", 2),
        },
        CommandEntry {
            label: "Code block",
            group: "Macro",
            desc: "```lang\\n…\\n```",
            kind: CommandKind::InsertSnippet("```\n\n```", 4),
        },
        CommandEntry {
            label: "Quote block",
            group: "Macro",
            desc: "> quote",
            kind: CommandKind::InsertSnippet("> ", 0),
        },
        CommandEntry {
            label: "Math block",
            group: "Macro",
            desc: "$$\\nexpr\\n$$",
            kind: CommandKind::InsertSnippet("$$\n\n$$", 3),
        },
    ]);

    out
}

/// Filter the catalog by the user's `/query` substring (case
/// insensitive, matches label or group prefix). An empty query
/// returns the full list.
pub fn filter(query: &str) -> Vec<CommandEntry> {
    let q = query.trim().to_lowercase();
    if q.is_empty() {
        return all_commands();
    }
    all_commands()
        .into_iter()
        .filter(|c| {
            c.label.to_lowercase().contains(&q)
                || c.group.to_lowercase().contains(&q)
                || c.desc.to_lowercase().contains(&q)
        })
        .collect()
}

/// Run a command against the current block. The caller passes
/// the slash-prefix range to be removed (start..end byte offset
/// of `/query` inside the block content); the runner replaces
/// that range with the command's effect.
///
/// `SetTask` and `SetHeading` are special: they're block-level
/// transforms (strip existing prefix, prepend the new one) — not
/// inline splices. The slash trigger is removed first, then the
/// transform runs on the cleaned content. Mirrors Logseq's
/// `set-marker`/`set-heading!` which always operate on the whole
/// block title regardless of caret position.
pub fn run_command(
    state: AppState,
    block_id: Uuid,
    slash_range: std::ops::Range<usize>,
    cmd: CommandKind,
) -> Option<CommandResult> {
    let current = state
        .vault
        .read()
        .blocks
        .iter()
        .find(|b| b.id == block_id)
        .map(|b| b.content.clone())?;
    let before = current.get(..slash_range.start)?;
    let after = current.get(slash_range.end..).unwrap_or("");
    let cleaned = format!("{before}{after}");
    let _ = split_block; // keep import live for future Enter-style commands

    match cmd {
        CommandKind::SetTask(m) => {
            let new_content = set_task_marker(&cleaned, Some(m));
            let caret = new_content.len();
            update_block_content(state, block_id, new_content);
            Some(CommandResult { caret })
        }
        CommandKind::SetHeading(level) => {
            let new_content = set_heading_level(&cleaned, level);
            let caret = new_content.len();
            update_block_content(state, block_id, new_content);
            Some(CommandResult { caret })
        }
        _ => {
            let (replacement, caret_back) = render_kind(cmd);
            let new_content = format!("{before}{replacement}{after}");
            let caret = before.len() + replacement.len() - caret_back;
            update_block_content(state, block_id, new_content);
            Some(CommandResult { caret })
        }
    }
}

/// Strip any leading task marker (TODO / DOING / …) and a
/// trailing space from `content`. If none is present, returns
/// the input unchanged.
pub fn strip_task_marker(content: &str) -> &str {
    for m in TaskMarker::all() {
        let label = m.label();
        if let Some(rest) = content.strip_prefix(label) {
            // Marker must be followed by a space (or end of content)
            // — otherwise "TODOIST" would get its prefix stripped.
            if rest.is_empty() {
                return rest;
            }
            if let Some(rest_after_space) = rest.strip_prefix(' ') {
                return rest_after_space;
            }
        }
    }
    content
}

/// Strip a leading `#…#` heading prefix (1-6 hashes followed by
/// a space) from `content`. Returns the input unchanged when no
/// heading marker is present.
pub fn strip_heading_marker(content: &str) -> &str {
    let hashes = content.chars().take_while(|c| *c == '#').count();
    if (1..=6).contains(&hashes) {
        let after = &content[hashes..];
        if after.is_empty() {
            return after;
        }
        if let Some(rest_after_space) = after.strip_prefix(' ') {
            return rest_after_space;
        }
    }
    content
}

/// Set (or clear) the leading task marker on the block content.
/// Preserves any existing heading prefix — `## DOING foo` with
/// `SetTask(Done)` becomes `## DONE foo`, matching Logseq's
/// `set-marker` behavior of treating heading + marker as a
/// composite prefix.
pub fn set_task_marker(content: &str, marker: Option<TaskMarker>) -> String {
    // Split out the heading prefix (if any), keep the rest as the
    // body where the task marker lives.
    let (heading_prefix, body) = match split_heading_prefix(content) {
        Some((h, b)) => (h, b),
        None => ("", content),
    };
    let body_no_marker = strip_task_marker(body);
    match marker {
        Some(m) => format!("{heading_prefix}{} {body_no_marker}", m.label()),
        None => format!("{heading_prefix}{body_no_marker}"),
    }
}

/// Set (or clear) the heading level on the block. Preserves any
/// existing task marker.
pub fn set_heading_level(content: &str, level: Option<u8>) -> String {
    // Drop existing heading prefix to find the body.
    let body = strip_heading_marker(content);
    match level {
        Some(n) => format!("{} {body}", "#".repeat(n as usize)),
        None => body.to_string(),
    }
}

/// Cycle the block's task marker. Mirrors Logseq's
/// `db-based-cycle-todo!`:
/// none → TODO → DOING → DONE → none.
pub fn cycle_task_marker(state: AppState, block_id: Uuid) {
    let current = match state
        .vault
        .read()
        .blocks
        .iter()
        .find(|b| b.id == block_id)
        .map(|b| b.content.clone())
    {
        Some(c) => c,
        None => return,
    };
    let body = match split_heading_prefix(&current) {
        Some((_, b)) => b,
        None => current.as_str(),
    };
    let next = match leading_task_marker(body) {
        None => Some(TaskMarker::Todo),
        Some(TaskMarker::Todo) => Some(TaskMarker::Doing),
        Some(TaskMarker::Doing) => Some(TaskMarker::Done),
        Some(TaskMarker::Done) => None,
        // Other markers (LATER/NOW/WAITING/CANCELLED) reset to
        // unmarked then re-enter the cycle on the next press.
        Some(_) => None,
    };
    let new_content = set_task_marker(&current, next);
    update_block_content(state, block_id, new_content);
}

/// Return the leading task marker if the body starts with one,
/// followed by a space or end-of-input. Used by `cycle_task_marker`
/// to know what's currently set without re-parsing.
pub fn leading_task_marker(body: &str) -> Option<TaskMarker> {
    for m in TaskMarker::all() {
        let label = m.label();
        if let Some(rest) = body.strip_prefix(label) {
            if rest.is_empty() || rest.starts_with(' ') {
                return Some(*m);
            }
        }
    }
    None
}

/// Split `content` at the heading prefix boundary. Returns
/// `Some((prefix_with_trailing_space, body))` when content
/// begins with `#…# `; `None` otherwise. The prefix keeps its
/// trailing space so callers can reattach it verbatim.
fn split_heading_prefix(content: &str) -> Option<(&str, &str)> {
    let hashes = content.chars().take_while(|c| *c == '#').count();
    if !(1..=6).contains(&hashes) {
        return None;
    }
    let after = &content[hashes..];
    let rest = after.strip_prefix(' ')?;
    let prefix_len = hashes + 1; // hashes + the space
    Some((&content[..prefix_len], rest))
}

/// What `run_command` reports back: the caret offset the caller
/// should restore in the textarea after Dioxus re-renders.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CommandResult {
    pub caret: usize,
}

/// Map a [`CommandKind`] to the literal text to splice in + how
/// many chars to move the caret back from the end of the splice.
fn render_kind(kind: CommandKind) -> (String, usize) {
    match kind {
        CommandKind::InsertSnippet(text, back) => (text.to_string(), back),
        CommandKind::SetTask(m) => (format!("{} ", m.label()), 0),
        CommandKind::SetHeading(Some(n)) => (format!("{} ", "#".repeat(n as usize)), 0),
        CommandKind::SetHeading(None) => (String::new(), 0),
        CommandKind::InsertDate(d) => {
            let date = match d {
                DateRef::Today => chrono::Local::now().date_naive(),
                DateRef::Tomorrow => chrono::Local::now().date_naive() + chrono::Duration::days(1),
                DateRef::Yesterday => chrono::Local::now().date_naive() - chrono::Duration::days(1),
            };
            (format!("[[{}]]", date.format("%Y-%m-%d")), 0)
        }
        CommandKind::InsertCurrentTime => (chrono::Local::now().format("%H:%M").to_string(), 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filter_matches_label() {
        let hits = filter("todo");
        assert!(hits.iter().any(|c| c.label == "TODO"));
    }

    #[test]
    fn filter_matches_group() {
        let hits = filter("heading");
        assert!(hits.iter().any(|c| c.label == "Heading 1"));
        assert!(hits.iter().any(|c| c.label == "Heading 6"));
    }

    #[test]
    fn empty_query_returns_full_catalog() {
        let all = filter("");
        // Sanity: at least a couple dozen commands across groups.
        assert!(all.len() >= 20);
    }

    #[test]
    fn render_kind_wikilink_caret_between_brackets() {
        let (txt, back) = render_kind(CommandKind::InsertSnippet("[[]]", 2));
        assert_eq!(txt, "[[]]");
        assert_eq!(back, 2);
    }

    #[test]
    fn strip_task_marker_removes_todo() {
        assert_eq!(strip_task_marker("TODO buy milk"), "buy milk");
        assert_eq!(strip_task_marker("DONE buy milk"), "buy milk");
    }

    #[test]
    fn strip_task_marker_no_prefix_unchanged() {
        assert_eq!(strip_task_marker("TODOIST app"), "TODOIST app");
        assert_eq!(strip_task_marker("plain text"), "plain text");
    }

    #[test]
    fn strip_heading_marker_removes_prefix() {
        assert_eq!(strip_heading_marker("## Title"), "Title");
        assert_eq!(strip_heading_marker("###### Six"), "Six");
    }

    #[test]
    fn strip_heading_marker_seven_hashes_left_alone() {
        // Beyond H6 the marker is no longer a heading prefix.
        assert_eq!(strip_heading_marker("####### Title"), "####### Title");
    }

    #[test]
    fn set_task_replaces_existing() {
        assert_eq!(
            set_task_marker("TODO foo", Some(TaskMarker::Done)),
            "DONE foo"
        );
    }

    #[test]
    fn set_task_preserves_heading_prefix() {
        assert_eq!(
            set_task_marker("## DOING fix bug", Some(TaskMarker::Done)),
            "## DONE fix bug"
        );
    }

    #[test]
    fn set_task_none_strips_marker() {
        assert_eq!(set_task_marker("DOING foo", None), "foo");
    }

    #[test]
    fn set_heading_replaces_existing() {
        assert_eq!(set_heading_level("## Title", Some(3)), "### Title");
        assert_eq!(set_heading_level("plain", Some(1)), "# plain");
    }

    #[test]
    fn set_heading_none_strips() {
        assert_eq!(set_heading_level("## Title", None), "Title");
    }

    #[test]
    fn leading_task_marker_detects() {
        assert_eq!(leading_task_marker("TODO foo"), Some(TaskMarker::Todo));
        assert_eq!(leading_task_marker("plain"), None);
    }

    #[test]
    fn render_kind_today_inserts_date() {
        let (txt, _) = render_kind(CommandKind::InsertDate(DateRef::Today));
        assert!(txt.starts_with("[[20"));
        assert!(txt.ends_with("]]"));
    }
}

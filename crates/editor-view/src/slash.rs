//! Slash-command palette. Trigger on `/`, fuzzy filter the
//! catalog, dispatch the chosen command as a `TransactionSpec`.
//!
//! Ported from `~/Development/Task/crates/editor/src/handler/commands.rs`
//! and adapted for our editor's `EditorState` / `Changes` API
//! instead of Task's block-based document model. The
//! `CommandKind` enum mirrors the Task code closely; the
//! catalog is markdown-flavored (callouts, code fences, math
//! blocks, headings, etc.).
//!
//! Architecture follows CodeMirror's `@codemirror/autocomplete`:
//! a `SlashState` signal holds the open menu + query; each
//! state update re-runs `detect_slash` against the current
//! line; selection nav fires `move_selection`; pick fires
//! `run_command` and clears the state.

use dioxus::prelude::*;
use editor_state::{Changes, EditorState, Range, Selection, TransactionSpec};

/// Slash-command popup. Reads the open state from the
/// `slash` signal threaded down from the host (typically the
/// playground or whichever shell embeds the editor). Renders
/// rows grouped by `group`; clicks pick a command. Keyboard
/// nav lives in `Editor`'s `onkeydown` rather than here so it
/// works without the menu element being focused.
#[component]
pub fn SlashMenu(
    state: Signal<EditorState>,
    slash: Signal<Option<SlashState>>,
) -> Element {
    let snapshot = slash.read().clone();
    let Some(current) = snapshot else {
        return rsx! { Fragment {} };
    };
    let hits = filter_commands(&current.query);
    if hits.is_empty() {
        return rsx! {
            div { class: "slash-menu",
                div { class: "slash-empty", "No commands match." }
            }
        };
    }
    let selected = current.selected.min(hits.len().saturating_sub(1));
    let mut last_group: Option<&str> = None;
    let mut row_idx: usize = 0;
    rsx! {
        div { class: "slash-menu",
            for entry in hits.iter().cloned() {
                {
                    let show_header = last_group.map(|g| g != entry.group).unwrap_or(true);
                    last_group = Some(entry.group);
                    let is_selected = row_idx == selected;
                    let idx_for_click = row_idx;
                    let entry_for_click = entry.clone();
                    let mut state_for_click = state;
                    let mut slash_for_click = slash;
                    let current_for_click = current.clone();
                    row_idx += 1;
                    rsx! {
                        {
                            if show_header {
                                rsx! { div { class: "slash-group", "{entry.group}" } }
                            } else { rsx! {} }
                        }
                        div {
                            class: if is_selected { "slash-row selected" } else { "slash-row" },
                            // Mousedown.preventDefault keeps the
                            // editor's caret from blurring as the
                            // click lands, so the next render keeps
                            // selection state coherent.
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            onclick: move |_| {
                                let cur = state_for_click.read().clone();
                                let end = current_for_click.slash_start + 1 + current_for_click.query.len();
                                if let Some(spec) = run_command(
                                    &cur,
                                    current_for_click.slash_start..end,
                                    entry_for_click.kind,
                                ) {
                                    state_for_click.set(cur.update(spec));
                                }
                                slash_for_click.set(None);
                            },
                            key: "{idx_for_click}",
                            div { class: "slash-row-label", "{entry.label}" }
                            if !entry.desc.is_empty() {
                                div { class: "slash-row-desc", "{entry.desc}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// One menu entry. Title shows in the row, group is the header
/// above it ("Heading", "Format", "Callout", …).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommandEntry {
    pub label: &'static str,
    pub group: &'static str,
    pub desc: &'static str,
    pub kind: CommandKind,
}

/// What the command does. Each variant carries the data the
/// runner needs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommandKind {
    /// Splice a literal snippet at the slash range. `caret_back`
    /// is how many bytes to move the caret back from the end of
    /// the insert (e.g. `("[[]]", 2)` lands the caret between
    /// the brackets).
    InsertSnippet(&'static str, usize),
    /// Replace the slash range with a block-shaped snippet that
    /// starts on its own line. If the line containing the slash
    /// has other content, the runner inserts a newline first.
    /// Same `caret_back` semantics as `InsertSnippet`.
    InsertBlockSnippet(&'static str, usize),
    /// Set the heading level of the current line (1-6, or 0 to
    /// strip). Strips any existing `#…#` prefix first.
    SetHeading(u8),
    /// Promote the current line to a list item of the given
    /// kind. Replaces any existing list marker.
    SetList(ListKind),
    /// Toggle the current line's task checkbox (`[ ]` ↔ `[x]`).
    ToggleTask,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListKind {
    Unordered,
    Ordered,
    Task,
}

/// Open-state of the slash menu. `None` when the menu's closed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SlashState {
    /// Byte offset of the `/` that triggered the menu.
    pub slash_start: usize,
    /// Body typed after the slash (does NOT include the `/`).
    pub query: String,
    /// Currently highlighted row.
    pub selected: usize,
}

/// Scan back from the caret for a `/` that hasn't been closed
/// by whitespace, returning the trigger position + the query
/// typed after it. Mirrors `~/Development/Task/.../editable.rs::detect_slash`
/// but operates on the slice from the start of the current line
/// up to the caret, so a `/` deep in the doc doesn't keep the
/// menu open across line breaks.
pub fn detect_slash(doc: &str, caret: usize) -> Option<(usize, String)> {
    let caret = caret.min(doc.len());
    let line_start = doc[..caret].rfind('\n').map(|n| n + 1).unwrap_or(0);
    let segment = &doc[line_start..caret];
    let bytes = segment.as_bytes();
    let mut i = bytes.len();
    while i > 0 {
        let c = bytes[i - 1];
        if c == b'/' {
            // The `/` must be at start-of-line or preceded by
            // whitespace — otherwise `https://` would trigger.
            let preceded_by_word =
                i >= 2 && !(bytes[i - 2] as char).is_whitespace();
            if preceded_by_word {
                return None;
            }
            let query = segment[i..].to_string();
            return Some((line_start + i - 1, query));
        }
        if (c as char).is_whitespace() {
            return None;
        }
        i -= 1;
    }
    None
}

/// Case-insensitive substring filter over label / group / desc.
pub fn filter_commands(query: &str) -> Vec<CommandEntry> {
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

/// Resolve a picked command into a `TransactionSpec`. Removes
/// the `/query` first, then either splices a snippet or runs a
/// line-level transform (heading / list / task).
pub fn run_command(
    state: &EditorState,
    slash_range: std::ops::Range<usize>,
    cmd: CommandKind,
) -> Option<TransactionSpec> {
    let doc = state.doc.to_string();
    if slash_range.end > doc.len() || slash_range.start > slash_range.end {
        return None;
    }
    match cmd {
        CommandKind::InsertSnippet(text, caret_back) => {
            let new_caret = slash_range.start + text.len() - caret_back;
            Some(
                TransactionSpec::new()
                    .changes(Changes::replace(slash_range, text))
                    .selection(Selection::caret(new_caret))
                    .annotate("origin", "slash"),
            )
        }
        CommandKind::InsertBlockSnippet(text, caret_back) => {
            // Snap to the start of the current line. If there's
            // other text before the slash on this line, drop the
            // whole block on a fresh line below.
            let line_start = doc[..slash_range.start]
                .rfind('\n')
                .map(|n| n + 1)
                .unwrap_or(0);
            let prefix_text = &doc[line_start..slash_range.start];
            let line_has_content = !prefix_text.trim().is_empty();
            let snippet = if line_has_content {
                format!("\n{text}")
            } else {
                text.to_string()
            };
            // Build the final doc directly: strip the `/query`,
            // then insert the block snippet at the line-aware
            // anchor.
            let before = &doc[..slash_range.start];
            let after = &doc[slash_range.end..];
            let stripped = format!("{before}{after}");
            let anchor = if line_has_content {
                slash_range.start
            } else {
                line_start
            };
            let head = &stripped[..anchor];
            let tail = &stripped[anchor..];
            let final_doc = format!("{head}{snippet}{tail}");
            let new_caret = anchor + snippet.len() - caret_back;
            Some(
                TransactionSpec::new()
                    .changes(Changes::replace(0..doc.len(), final_doc))
                    .selection(Selection::caret(new_caret))
                    .annotate("origin", "slash"),
            )
        }
        CommandKind::SetHeading(level) => {
            // First strip the slash, then run set_heading on the
            // cleaned state.
            let stripped = remove_range(&doc, &slash_range);
            let new_state = state_with_doc(state, stripped, slash_range.start);
            editor_state::commands::set_heading(&new_state, level)
        }
        CommandKind::SetList(kind) => {
            let stripped = remove_range(&doc, &slash_range);
            let new_state = state_with_doc(state, stripped, slash_range.start);
            let target = match kind {
                ListKind::Unordered => "- ",
                ListKind::Ordered => "1. ",
                ListKind::Task => "- [ ] ",
            };
            // Replace any existing list marker on the line, or
            // prepend.
            let doc2 = new_state.doc.to_string();
            let line_start = doc2[..slash_range.start]
                .rfind('\n')
                .map(|n| n + 1)
                .unwrap_or(0);
            let line_end = doc2[line_start..]
                .find('\n')
                .map(|n| line_start + n)
                .unwrap_or(doc2.len());
            let line = &doc2[line_start..line_end];
            let stripped_line = strip_list_marker(line);
            let new_line = format!("{target}{stripped_line}");
            let caret = line_start + new_line.len();
            Some(
                TransactionSpec::new()
                    .changes(Changes::replace(0..doc.len(), {
                        let mut s = doc2.clone();
                        s.replace_range(line_start..line_end, &new_line);
                        s
                    }))
                    .selection(Selection::caret(caret))
                    .annotate("origin", "slash"),
            )
        }
        CommandKind::ToggleTask => {
            let stripped = remove_range(&doc, &slash_range);
            let new_state = state_with_doc(state, stripped, slash_range.start);
            editor_state::commands::toggle_task(&new_state)
        }
    }
}

fn remove_range(doc: &str, range: &std::ops::Range<usize>) -> String {
    let mut s = String::with_capacity(doc.len() - (range.end - range.start));
    s.push_str(&doc[..range.start]);
    s.push_str(&doc[range.end..]);
    s
}

fn state_with_doc(state: &EditorState, doc: String, caret: usize) -> EditorState {
    let mut s = state.clone();
    s.doc = doc.into();
    let caret = caret.min(s.doc.len());
    s.selection = Selection::single(Range::caret(caret));
    s
}

fn strip_list_marker(line: &str) -> &str {
    let b = line.as_bytes();
    // task: `- [X] `
    if b.len() >= 6
        && matches!(b[0], b'-' | b'*' | b'+')
        && b[1] == b' '
        && b[2] == b'['
        && b[4] == b']'
        && b[5] == b' '
    {
        return &line[6..];
    }
    // ordered: `N. `
    if b.len() >= 3 && b[0].is_ascii_digit() && b[1] == b'.' && b[2] == b' ' {
        return &line[3..];
    }
    // unordered: `- `
    if b.len() >= 2 && matches!(b[0], b'-' | b'*' | b'+') && b[1] == b' ' {
        return &line[2..];
    }
    line
}

/// The full catalog. Lives as a function (not const) so the
/// `&'static str` text gets the right ownership semantics
/// through the filter pipeline.
pub fn all_commands() -> Vec<CommandEntry> {
    let mut out = Vec::new();

    // ── Headings ───────────────────────────────────────────
    for (level, label) in [
        (1u8, "Heading 1"),
        (2, "Heading 2"),
        (3, "Heading 3"),
        (4, "Heading 4"),
        (5, "Heading 5"),
        (6, "Heading 6"),
    ] {
        out.push(CommandEntry {
            label,
            group: "Heading",
            desc: "",
            kind: CommandKind::SetHeading(level),
        });
    }

    // ── Lists ──────────────────────────────────────────────
    out.extend([
        CommandEntry {
            label: "Bulleted list",
            group: "Structure",
            desc: "- item",
            kind: CommandKind::SetList(ListKind::Unordered),
        },
        CommandEntry {
            label: "Numbered list",
            group: "Structure",
            desc: "1. item",
            kind: CommandKind::SetList(ListKind::Ordered),
        },
        CommandEntry {
            label: "Task list",
            group: "Structure",
            desc: "- [ ] task",
            kind: CommandKind::SetList(ListKind::Task),
        },
        CommandEntry {
            label: "Toggle task",
            group: "Structure",
            desc: "Mark current line as / un-task",
            kind: CommandKind::ToggleTask,
        },
        CommandEntry {
            label: "Quote",
            group: "Structure",
            desc: "> blockquote",
            kind: CommandKind::InsertSnippet("> ", 0),
        },
        CommandEntry {
            label: "Horizontal rule",
            group: "Structure",
            desc: "---",
            kind: CommandKind::InsertBlockSnippet("---\n", 0),
        },
        CommandEntry {
            label: "Table",
            group: "Structure",
            desc: "GFM pipe table skeleton",
            kind: CommandKind::InsertBlockSnippet(
                "| col1 | col2 |\n| ---- | ---- |\n|      |      |\n",
                21,
            ),
        },
    ]);

    // ── Code & math ────────────────────────────────────────
    out.extend([
        CommandEntry {
            label: "Code block",
            group: "Code",
            desc: "```lang \\n … \\n```",
            kind: CommandKind::InsertBlockSnippet("```\n\n```\n", 5),
        },
        CommandEntry {
            label: "Rust code block",
            group: "Code",
            desc: "```rust",
            kind: CommandKind::InsertBlockSnippet("```rust\n\n```\n", 5),
        },
        CommandEntry {
            label: "TypeScript code block",
            group: "Code",
            desc: "```ts",
            kind: CommandKind::InsertBlockSnippet("```ts\n\n```\n", 5),
        },
        CommandEntry {
            label: "Typst block",
            group: "Code",
            desc: "Compiled Typst — math, diagrams, layout",
            kind: CommandKind::InsertBlockSnippet("```typst\n\n```\n", 5),
        },
        CommandEntry {
            label: "Mermaid diagram",
            group: "Code",
            desc: "```mermaid",
            kind: CommandKind::InsertBlockSnippet("```mermaid\n\n```\n", 5),
        },
        CommandEntry {
            label: "Inline math",
            group: "Math",
            desc: "$x$",
            kind: CommandKind::InsertSnippet("$$", 1),
        },
        CommandEntry {
            label: "Math block",
            group: "Math",
            desc: "$$\\n…\\n$$",
            kind: CommandKind::InsertBlockSnippet("$$\n\n$$\n", 4),
        },
    ]);

    // ── Callouts ────────────────────────────────────────────
    // All 13 canonical Obsidian types — the renderer maps the
    // type name to a color in CSS.
    for (kind, label, desc) in [
        ("note", "Callout: Note", "> [!note]"),
        ("abstract", "Callout: Abstract", "> [!abstract]"),
        ("info", "Callout: Info", "> [!info]"),
        ("todo", "Callout: Todo", "> [!todo]"),
        ("tip", "Callout: Tip", "> [!tip]"),
        ("success", "Callout: Success", "> [!success]"),
        ("question", "Callout: Question", "> [!question]"),
        ("warning", "Callout: Warning", "> [!warning]"),
        ("failure", "Callout: Failure", "> [!failure]"),
        ("danger", "Callout: Danger", "> [!danger]"),
        ("bug", "Callout: Bug", "> [!bug]"),
        ("example", "Callout: Example", "> [!example]"),
        ("quote", "Callout: Quote", "> [!quote]"),
    ] {
        let snippet: &'static str = match kind {
            "note" => "> [!note]\n> ",
            "abstract" => "> [!abstract]\n> ",
            "info" => "> [!info]\n> ",
            "todo" => "> [!todo]\n> ",
            "tip" => "> [!tip]\n> ",
            "success" => "> [!success]\n> ",
            "question" => "> [!question]\n> ",
            "warning" => "> [!warning]\n> ",
            "failure" => "> [!failure]\n> ",
            "danger" => "> [!danger]\n> ",
            "bug" => "> [!bug]\n> ",
            "example" => "> [!example]\n> ",
            "quote" => "> [!quote]\n> ",
            _ => unreachable!(),
        };
        out.push(CommandEntry {
            label,
            group: "Callout",
            desc,
            kind: CommandKind::InsertBlockSnippet(snippet, 0),
        });
    }

    // ── Embeds & links ─────────────────────────────────────
    out.extend([
        CommandEntry {
            label: "Link",
            group: "Link",
            desc: "[text](url)",
            kind: CommandKind::InsertSnippet("[]()", 3),
        },
        CommandEntry {
            label: "Wikilink",
            group: "Link",
            desc: "[[Page]]",
            kind: CommandKind::InsertSnippet("[[]]", 2),
        },
        CommandEntry {
            label: "Embed",
            group: "Link",
            desc: "![[file]] — image / audio / video / pdf",
            kind: CommandKind::InsertSnippet("![[]]", 2),
        },
        CommandEntry {
            label: "Footnote ref",
            group: "Link",
            desc: "[^id]",
            kind: CommandKind::InsertSnippet("[^]", 1),
        },
        CommandEntry {
            label: "Inline footnote",
            group: "Link",
            desc: "^[note]",
            kind: CommandKind::InsertSnippet("^[]", 1),
        },
    ]);

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_slash_at_start_of_doc() {
        assert_eq!(detect_slash("/cal", 4), Some((0, "cal".to_string())));
    }

    #[test]
    fn detect_slash_after_whitespace() {
        assert_eq!(detect_slash("hi /code", 8), Some((3, "code".to_string())));
    }

    #[test]
    fn detect_slash_ignores_url_form() {
        assert_eq!(detect_slash("https://anthropic.com", 21), None);
    }

    #[test]
    fn detect_slash_closes_on_space() {
        assert_eq!(detect_slash("/foo bar", 8), None);
    }

    #[test]
    fn detect_slash_scoped_to_current_line() {
        // A slash on a previous line shouldn't keep the menu
        // open across newlines.
        assert_eq!(detect_slash("/old\nnew here", 13), None);
    }

    #[test]
    fn filter_matches_label() {
        let hits = filter_commands("call");
        assert!(hits.iter().any(|c| c.label.contains("Callout")));
    }
}

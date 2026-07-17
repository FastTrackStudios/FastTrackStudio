//! Note header — the Obsidian-style control surface that renders
//! ABOVE the editor body inside `/vault`:
//!
//! - an **editable H1 title** = the note's filename basename; committing
//!   a change RENAMES the file (`put_file` create-only at the new path →
//!   navigate → `delete_file` the old path);
//! - a **Properties panel** over the note's leading `---…---` YAML
//!   frontmatter: ordered key→value rows, scalars and simple sequences
//!   (`tags`, `aliases`) as removable chips, add/remove per row.
//!
//! The editor keeps holding the FULL markdown (frontmatter + body). The
//! header never edits the buffer character-by-character; it re-serializes
//! the whole frontmatter region and writes the spliced document back
//! through [`DocumentSession`] — collab-aware:
//!
//! - **collab live** → [`crate::collab::push_full_text`] pushes the diff
//!   into the shared replica; the collab revision effect echoes it back
//!   into the editor (a host-driven `state.set` would never reach peers);
//! - **sha mode** → a host-driven `state.set` + explicit `save()`.
//!
//! Malformed frontmatter (an opening `---` with no close) is shown as a
//! read-only raw fallback rather than risking a double-fence rewrite.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronRight, Plus, X};
use fts_ui::prelude::*;

use crate::collab::CollabHandles;
use crate::document_session::DocumentSession;
use crate::pages::vault::basename_of;

// ── Frontmatter model ─────────────────────────────────────────────

/// A single frontmatter value: a scalar string, or a simple YAML
/// sequence (rendered as chips). List-ness is preserved across a
/// round-trip so `tags`/`aliases` stay sequences and `folder` stays a
/// scalar wikilink.
#[derive(Clone, PartialEq, Debug)]
enum FmValue {
    Scalar(String),
    List(Vec<String>),
}

/// One ordered `key: value` frontmatter property.
#[derive(Clone, PartialEq, Debug)]
struct Property {
    key: String,
    value: FmValue,
}

/// Parsed note = its frontmatter properties + the body that follows,
/// preserved verbatim for round-trip splicing.
#[derive(Clone, PartialEq, Debug)]
struct Frontmatter {
    props: Vec<Property>,
    /// Everything after the closing `---` fence line (verbatim).
    body: String,
    /// The raw text opened with a `---…---` block (vs. gaining one on
    /// first property add).
    had_fm: bool,
    /// Opened with `---` but never closed — editing is disabled and the
    /// raw text is shown as a fallback so we never double-fence.
    malformed: bool,
}

/// Parse the leading `---\n…\n---\n` frontmatter into ordered rows +
/// the trailing body. Scalars, inline flow lists (`[a, b]`), and block
/// lists (`key:` then indented `- item`) are all recognised; anything
/// else on a frontmatter line is kept as a scalar. Never panics.
fn parse_frontmatter(raw: &str) -> Frontmatter {
    let Some(rest) = raw.strip_prefix("---\n") else {
        return Frontmatter {
            props: Vec::new(),
            body: raw.to_owned(),
            had_fm: false,
            malformed: false,
        };
    };
    let Some(end) = rest.find("\n---") else {
        // Opened but never closed → malformed. Keep the whole text as
        // body so the fallback shows it; editing is disabled.
        return Frontmatter {
            props: Vec::new(),
            body: raw.to_owned(),
            had_fm: true,
            malformed: true,
        };
    };
    let fm_text = &rest[..end];
    // After the `\n---` fence marker, skip the rest of that line (the
    // fence may carry trailing spaces) up to and including its newline;
    // everything after is the body, verbatim.
    let after_fence = &rest[end + 4..];
    let body = match after_fence.find('\n') {
        Some(nl) => after_fence[nl + 1..].to_owned(),
        None => String::new(),
    };
    Frontmatter {
        props: parse_props(fm_text),
        body,
        had_fm: true,
        malformed: false,
    }
}

/// Parse the inner frontmatter text (between the fences) into ordered
/// properties. Block-list continuation lines (`  - item`) attach to the
/// preceding `key:` with an empty inline value.
fn parse_props(fm_text: &str) -> Vec<Property> {
    let mut props: Vec<Property> = Vec::new();
    for line in fm_text.lines() {
        let trimmed = line.trim_start();
        // Block-list continuation: `- item` under the previous key.
        if let Some(item) = trimmed.strip_prefix("- ") {
            if let Some(last) = props.last_mut() {
                let item = unquote(item.trim()).to_owned();
                match &mut last.value {
                    FmValue::List(items) => items.push(item),
                    FmValue::Scalar(s) if s.is_empty() => {
                        last.value = FmValue::List(vec![item]);
                    }
                    FmValue::Scalar(_) => {
                        last.value = FmValue::List(vec![item]);
                    }
                }
            }
            continue;
        }
        let Some((k, v)) = line.split_once(':') else {
            continue;
        };
        let key = k.trim().to_owned();
        if key.is_empty() {
            continue;
        }
        props.push(Property {
            key,
            value: parse_value(v.trim()),
        });
    }
    props
}

/// Classify a scalar-position value: a quoted string stays a scalar
/// (so `folder: "[[Parent]]"` never reads as a nested list), an
/// unquoted `[…]` is an inline sequence, everything else is a scalar.
fn parse_value(v: &str) -> FmValue {
    if v.starts_with('"') || v.starts_with('\'') {
        return FmValue::Scalar(unquote(v).to_owned());
    }
    if let Some(inner) = v.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        let inner = inner.trim();
        if inner.is_empty() {
            return FmValue::List(Vec::new());
        }
        let items = inner
            .split(',')
            .map(|it| unquote(it.trim()).to_owned())
            .filter(|it| !it.is_empty())
            .collect();
        return FmValue::List(items);
    }
    FmValue::Scalar(v.to_owned())
}

/// Strip one matching pair of surrounding quotes.
fn unquote(s: &str) -> &str {
    for q in ['"', '\''] {
        if s.len() >= 2 && s.starts_with(q) && s.ends_with(q) {
            return &s[1..s.len() - 1];
        }
    }
    s
}

/// Re-serialise the full document: `---` block (only if there are
/// properties) + the verbatim body. An emptied frontmatter drops the
/// block entirely (Obsidian behaviour).
fn serialize(props: &[Property], body: &str) -> String {
    if props.is_empty() {
        return body.to_owned();
    }
    let mut out = String::from("---\n");
    for p in props {
        match &p.value {
            FmValue::Scalar(s) => {
                out.push_str(&p.key);
                out.push_str(": ");
                out.push_str(&quote_if_needed(s));
                out.push('\n');
            }
            FmValue::List(items) => {
                out.push_str(&p.key);
                out.push_str(": [");
                let rendered: Vec<String> = items.iter().map(|i| quote_if_needed(i)).collect();
                out.push_str(&rendered.join(", "));
                out.push_str("]\n");
            }
        }
    }
    out.push_str("---\n");
    out.push_str(body);
    out
}

/// Quote a scalar/list item when leaving it bare would change how YAML
/// reads it (empty, YAML-significant punctuation, wikilinks like
/// `[[Parent]]`, leading `-`, or edge whitespace). Dates and plain
/// words stay bare, matching the seed scaffold.
fn quote_if_needed(s: &str) -> String {
    let needs = s.is_empty()
        || s.starts_with('-')
        || s.starts_with(' ')
        || s.ends_with(' ')
        || s.chars()
            .any(|c| matches!(c, ':' | '#' | '[' | ']' | '{' | '}' | ',' | '"' | '\'' | '&' | '*' | '!' | '|' | '>' | '%' | '@' | '`'));
    if needs {
        format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
    } else {
        s.to_owned()
    }
}

// ── Component ─────────────────────────────────────────────────────

/// Header rendered above the editor body: editable title + properties.
///
/// Reads the open document text reactively from `session.state`, so it
/// re-parses whenever the buffer changes (its own writes included). The
/// [`DocumentSession`] comes from context (provided by the vault page,
/// same as [`crate::collab::CollabSession`]) since it isn't `PartialEq`
/// and so can't be a memoised component prop.
#[component]
pub fn NoteHeader(
    collab: Signal<Option<CollabHandles>>,
    home: Memo<String>,
    /// Refresh the folder index after a rename commits (the tree row
    /// path changed).
    on_renamed: EventHandler<()>,
) -> Element {
    let session = use_context::<DocumentSession>();
    let notify = architect::try_use_notifications();
    let nav = use_navigator();

    let Some(path) = session.current_path() else {
        return rsx! {};
    };
    let title = basename_of(&path).to_owned();

    // Reactive parse of the current buffer.
    let raw = session.state.read().doc.to_string();
    let fm = parse_frontmatter(&raw);

    // ── Write path: re-serialise props over the frontmatter region and
    //    push the spliced document through the session (collab-aware).
    let write_props = move |props: Vec<Property>| {
        let raw = session.state.peek().doc.to_string();
        let cur = parse_frontmatter(&raw);
        if cur.malformed {
            return; // never rewrite malformed frontmatter
        }
        let next = serialize(&props, &cur.body);
        if next == raw {
            return;
        }
        // Collab live → push into the shared replica (which echoes back
        // into the editor); otherwise host-set the buffer + save.
        let live = collab
            .peek()
            .as_ref()
            .map(|c| c.is_live())
            .unwrap_or(false);
        if live {
            if let Some(c) = collab.peek().as_ref() {
                crate::collab::push_full_text(c, &next);
            }
        } else {
            let mut state = session.state;
            state.set(editor::EditorState::new(next));
            session.save();
        }
    };

    // ── Rename: title commit → put_file(new, CreateOnly) → nav → delete.
    let cur_title = title.clone();
    let do_rename = move |new_title: String| {
        let new_title = new_title.trim().to_owned();
        if new_title.is_empty() || new_title == cur_title {
            return;
        }
        let Some(new_path) = rename_path(&path, &new_title) else {
            if let Some(n) = notify {
                n.error("Invalid note name");
            }
            return;
        };
        if new_path == path {
            return;
        }
        let old_path = path.clone();
        let bytes = session.state.peek().doc.to_string().into_bytes();
        let slug = home.peek().clone();
        spawn(async move {
            // Save any pending edits to the OLD path first so nothing is
            // lost if the create fails and we stay put. Best-effort.
            session.save();
            match put_file_create(slug.clone(), new_path.clone(), bytes).await {
                Ok(_) => {
                    nav.push(crate::routes::Route::VaultRoute {
                        path: new_path.clone(),
                    });
                    // The new file is committed and we've navigated; drop
                    // the old one. A delete failure only leaves a stale
                    // copy behind — surface it but don't block.
                    if let Err(e) = delete_file(slug, old_path).await {
                        if let Some(n) = notify {
                            n.error(format!("Renamed, but couldn't remove the old note: {e}"));
                        }
                    }
                    on_renamed.call(());
                }
                Err(e) => {
                    if let Some(n) = notify {
                        n.error(format!("Rename failed: {e}"));
                    }
                }
            }
        });
    };

    rsx! {
        div { class: "flex flex-col gap-3 border-b border-border/60 px-6 pb-4 pt-5",
            // ── Title (H1) ───────────────────────────────
            TitleField { title, on_commit: do_rename }
            // ── Properties ───────────────────────────────
            if fm.malformed {
                div { class: "rounded-md border border-border/60 bg-muted/30 p-3",
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "Frontmatter looks malformed (an opening \"---\" with no closing fence), so it can't be edited here. Fix it in the note body below."
                    }
                }
            } else {
                PropertiesPanel { rows: fm.props.clone(), write: write_props }
            }
        }
    }
}

/// Big, click-to-edit title styled like an H1. Click swaps the heading
/// for an input; Enter or blur commits, Escape cancels.
#[component]
fn TitleField(title: String, on_commit: EventHandler<String>) -> Element {
    let mut editing = use_signal(|| false);
    let mut draft = use_signal(String::new);

    if editing() {
        rsx! {
            input {
                class: "w-full bg-transparent text-3xl font-bold tracking-tight text-foreground outline-none",
                value: "{draft}",
                autofocus: true,
                onmounted: move |el| async move {
                    let _ = el.set_focus(true).await;
                },
                oninput: move |e| draft.set(e.value()),
                onkeydown: move |e: KeyboardEvent| {
                    e.stop_propagation();
                    if e.key() == Key::Enter {
                        editing.set(false);
                        on_commit.call(draft.peek().clone());
                    } else if e.key() == Key::Escape {
                        editing.set(false);
                    }
                },
                onfocusout: move |_| {
                    editing.set(false);
                    on_commit.call(draft.peek().clone());
                },
            }
        }
    } else {
        let display = title.clone();
        rsx! {
            button {
                class: "-mx-1 w-full truncate rounded px-1 text-left text-3xl font-bold tracking-tight text-foreground hover:bg-accent/40",
                title: "Click to rename",
                onclick: move |_| {
                    draft.set(display.clone());
                    editing.set(true);
                },
                "{title}"
            }
        }
    }
}

/// The collapsible "Properties" section: one row per property + an
/// "Add property" control. `write` receives the FULL new property list
/// for any mutation (add / remove / edit / chip change).
#[component]
fn PropertiesPanel(rows: Vec<Property>, write: EventHandler<Vec<Property>>) -> Element {
    let mut open = use_signal(|| true);
    let props_now = rows.clone();

    // Mutate helper: clone the current list, apply, hand the whole thing
    // to `write`.
    let apply = move |mutate: &dyn Fn(&mut Vec<Property>)| {
        let mut next = props_now.clone();
        mutate(&mut next);
        write.call(next);
    };

    rsx! {
        div { class: "flex flex-col gap-1",
            button {
                class: "flex items-center gap-1 text-xs font-semibold uppercase tracking-wide text-muted-foreground hover:text-foreground",
                onclick: move |_| {
                    let cur = *open.peek();
                    open.set(!cur);
                },
                span {
                    class: if open() { "transition-transform rotate-90" } else { "transition-transform" },
                    ChevronRight { size: 12 }
                }
                "Properties"
            }
            if open() {
                div { class: "flex flex-col gap-1 pt-1",
                    for (idx, prop) in rows.iter().cloned().enumerate() {
                        PropRow {
                            key: "{prop.key}-{idx}",
                            prop: prop.clone(),
                            on_set_scalar: {
                                let apply = apply.clone();
                                move |(i, v): (usize, String)| {
                                    let apply = apply.clone();
                                    apply(&move |p: &mut Vec<Property>| {
                                        if let Some(row) = p.get_mut(i) {
                                            row.value = FmValue::Scalar(v.clone());
                                        }
                                    });
                                }
                            },
                            on_add_item: {
                                let apply = apply.clone();
                                move |(i, v): (usize, String)| {
                                    let apply = apply.clone();
                                    apply(&move |p: &mut Vec<Property>| {
                                        if let Some(row) = p.get_mut(i) {
                                            if let FmValue::List(items) = &mut row.value {
                                                if !v.is_empty() && !items.contains(&v) {
                                                    items.push(v.clone());
                                                }
                                            }
                                        }
                                    });
                                }
                            },
                            on_remove_item: {
                                let apply = apply.clone();
                                move |(i, j): (usize, usize)| {
                                    let apply = apply.clone();
                                    apply(&move |p: &mut Vec<Property>| {
                                        if let Some(row) = p.get_mut(i) {
                                            if let FmValue::List(items) = &mut row.value {
                                                if j < items.len() {
                                                    items.remove(j);
                                                }
                                            }
                                        }
                                    });
                                }
                            },
                            on_remove_row: {
                                let apply = apply.clone();
                                move |i: usize| {
                                    let apply = apply.clone();
                                    apply(&move |p: &mut Vec<Property>| {
                                        if i < p.len() {
                                            p.remove(i);
                                        }
                                    });
                                }
                            },
                            index: idx,
                        }
                    }
                    AddProperty {
                        on_add: move |(k, v): (String, String)| {
                            apply(&move |p: &mut Vec<Property>| {
                                if p.iter().any(|row| row.key == k) {
                                    return;
                                }
                                // A `[a, b]` value seeds a list; anything else a scalar.
                                p.push(Property {
                                    key: k.clone(),
                                    value: parse_value(v.trim()),
                                });
                            });
                        },
                    }
                }
            }
        }
    }
}

/// One property row: a fixed-width key label + an editable value —
/// scalars as an inline text input, lists as removable chips with an
/// inline add. A trailing "×" removes the whole row.
#[component]
fn PropRow(
    prop: Property,
    index: usize,
    on_set_scalar: EventHandler<(usize, String)>,
    on_add_item: EventHandler<(usize, String)>,
    on_remove_item: EventHandler<(usize, usize)>,
    on_remove_row: EventHandler<usize>,
) -> Element {
    rsx! {
        div { class: "group flex items-start gap-2",
            span { class: "w-24 shrink-0 select-none pt-1.5 text-xs text-muted-foreground", "{prop.key}" }
            div { class: "flex min-w-0 flex-1 flex-wrap items-center gap-1",
                match prop.value.clone() {
                    FmValue::Scalar(s) => rsx! {
                        ScalarValue {
                            value: s,
                            on_commit: move |v: String| on_set_scalar.call((index, v)),
                        }
                    },
                    FmValue::List(items) => rsx! {
                        for (j, item) in items.iter().cloned().enumerate() {
                            Badge {
                                key: "{item}-{j}",
                                variant: BadgeVariant::Secondary,
                                class: "gap-1",
                                "{item}"
                                button {
                                    class: "opacity-60 hover:opacity-100",
                                    onclick: move |_| on_remove_item.call((index, j)),
                                    X { size: 11 }
                                }
                            }
                        }
                        AddChip { on_add: move |v: String| on_add_item.call((index, v)) }
                    },
                }
            }
            button {
                class: "invisible shrink-0 pt-1.5 text-muted-foreground hover:text-destructive group-hover:visible",
                title: "Remove property",
                onclick: move |_| on_remove_row.call(index),
                X { size: 13 }
            }
        }
    }
}

/// A scalar value shown as text; click reveals an input that commits on
/// Enter/blur.
#[component]
fn ScalarValue(value: String, on_commit: EventHandler<String>) -> Element {
    let mut editing = use_signal(|| false);
    let mut draft = use_signal(String::new);

    if editing() {
        rsx! {
            input {
                class: "w-full rounded border border-input bg-input/30 px-2 py-0.5 text-sm text-foreground outline-none focus-visible:border-ring",
                value: "{draft}",
                autofocus: true,
                onmounted: move |el| async move {
                    let _ = el.set_focus(true).await;
                },
                oninput: move |e| draft.set(e.value()),
                onkeydown: move |e: KeyboardEvent| {
                    e.stop_propagation();
                    if e.key() == Key::Enter {
                        editing.set(false);
                        on_commit.call(draft.peek().clone());
                    } else if e.key() == Key::Escape {
                        editing.set(false);
                    }
                },
                onfocusout: move |_| {
                    editing.set(false);
                    on_commit.call(draft.peek().clone());
                },
            }
        }
    } else {
        let display = value.clone();
        let shown = if value.is_empty() { "Empty".to_owned() } else { value.clone() };
        let cls = if value.is_empty() {
            "w-full rounded px-2 py-0.5 text-left text-sm text-muted-foreground/60 hover:bg-accent/40"
        } else {
            "w-full rounded px-2 py-0.5 text-left text-sm text-foreground hover:bg-accent/40"
        };
        rsx! {
            button {
                class: cls,
                onclick: move |_| {
                    draft.set(display.clone());
                    editing.set(true);
                },
                "{shown}"
            }
        }
    }
}

/// Inline "+ add" for a list value: a compact link that reveals a tiny
/// input, committing a chip on Enter/blur.
#[component]
fn AddChip(on_add: EventHandler<String>) -> Element {
    let mut editing = use_signal(|| false);
    let mut draft = use_signal(String::new);

    if editing() {
        rsx! {
            input {
                class: "w-24 rounded border border-input bg-input/30 px-1.5 py-0.5 text-xs text-foreground outline-none focus-visible:border-ring",
                value: "{draft}",
                autofocus: true,
                onmounted: move |el| async move {
                    let _ = el.set_focus(true).await;
                },
                oninput: move |e| draft.set(e.value()),
                onkeydown: move |e: KeyboardEvent| {
                    e.stop_propagation();
                    if e.key() == Key::Enter {
                        let v = draft.peek().trim().to_owned();
                        draft.set(String::new());
                        if !v.is_empty() {
                            on_add.call(v);
                        }
                    } else if e.key() == Key::Escape {
                        editing.set(false);
                    }
                },
                onfocusout: move |_| {
                    let v = draft.peek().trim().to_owned();
                    editing.set(false);
                    draft.set(String::new());
                    if !v.is_empty() {
                        on_add.call(v);
                    }
                },
            }
        }
    } else {
        rsx! {
            button {
                class: "flex items-center gap-0.5 rounded px-1 text-xs text-muted-foreground hover:text-foreground",
                onclick: move |_| editing.set(true),
                Plus { size: 11 }
                "add"
            }
        }
    }
}

/// The "Add property" control at the bottom of the panel: a key + value
/// input pair revealed by a button, committing a new row on Add/Enter.
#[component]
fn AddProperty(on_add: EventHandler<(String, String)>) -> Element {
    let mut open = use_signal(|| false);
    let key = use_signal(String::new);
    let value = use_signal(String::new);

    let mut commit = move || {
        let k = key.peek().trim().to_owned();
        let v = value.peek().trim().to_owned();
        if k.is_empty() {
            return;
        }
        on_add.call((k, v));
        let mut key = key;
        let mut value = value;
        key.set(String::new());
        value.set(String::new());
        open.set(false);
    };

    if open() {
        rsx! {
            div { class: "flex items-center gap-2 pt-1",
                div { class: "w-24 shrink-0",
                    Input {
                        value: key,
                        size: InputSize::Small,
                        placeholder: "key",
                    }
                }
                div { class: "min-w-0 flex-1",
                    Input {
                        value,
                        size: InputSize::Small,
                        placeholder: "value (or [a, b])",
                    }
                }
                Button {
                    variant: ButtonVariant::Secondary,
                    size: ButtonSize::Small,
                    on_click: move |_| commit(),
                    "Add"
                }
            }
        }
    } else {
        rsx! {
            button {
                class: "flex w-fit items-center gap-1 pt-1 text-xs text-muted-foreground hover:text-foreground",
                onclick: move |_| open.set(true),
                Plus { size: 12 }
                "Add property"
            }
        }
    }
}

// ── Path helpers ──────────────────────────────────────────────────

/// Sanitise `title` into a vault path in the SAME folder as `old_path`,
/// with a `.md` extension. Returns `None` if nothing usable survives
/// sanitisation.
fn rename_path(old_path: &str, title: &str) -> Option<String> {
    let name = sanitize_filename(title);
    if name.is_empty() {
        return None;
    }
    let file = format!("{name}.md");
    Some(match old_path.rsplit_once('/') {
        Some((dir, _)) => format!("{dir}/{file}"),
        None => file,
    })
}

/// Keep the title filesystem-safe: drop path separators and characters
/// that break paths on common filesystems, collapse whitespace.
fn sanitize_filename(title: &str) -> String {
    let cleaned: String = title
        .chars()
        .map(|c| match c {
            '/' | '\\' | ':' | '*' | '?' | '"' | '<' | '>' | '|' => ' ',
            c if c.is_control() => ' ',
            c => c,
        })
        .collect();
    // Collapse runs of whitespace, trim, and strip a trailing `.md` the
    // user may have typed (we re-append it).
    let collapsed = cleaned.split_whitespace().collect::<Vec<_>>().join(" ");
    collapsed
        .strip_suffix(".md")
        .unwrap_or(&collapsed)
        .trim()
        .to_owned()
}

// ── RPC helpers (wasm-only bodies, mirroring the vault page) ───────

/// Create-only write of `bytes` at `path` (the rename target). Fails if
/// the target already exists — the caller surfaces that as a toast and
/// keeps the old file.
async fn put_file_create(slug: String, path: String, bytes: Vec<u8>) -> Result<String, String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let ack = client
            .put_file(
                crate::document_session::VAULT_ID.to_owned(),
                path,
                bytes,
                IfMatch::CreateOnly,
            )
            .await
            .map_err(|e| format!("put_file: {e:?}"))?;
        Ok(ack.sha256)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path, bytes);
        Err("native client not wired yet".to_owned())
    }
}

/// Delete `path` unconditionally (the old name after a rename).
async fn delete_file(slug: String, path: String) -> Result<(), String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        client
            .delete_file(
                crate::document_session::VAULT_ID.to_owned(),
                path,
                IfMatch::Force,
            )
            .await
            .map_err(|e| format!("delete_file: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scalar(k: &str, v: &str) -> Property {
        Property { key: k.to_owned(), value: FmValue::Scalar(v.to_owned()) }
    }
    fn list(k: &str, items: &[&str]) -> Property {
        Property {
            key: k.to_owned(),
            value: FmValue::List(items.iter().map(|s| (*s).to_owned()).collect()),
        }
    }

    #[test]
    fn parses_scalars_and_lists() {
        let raw = "---\ncreated: 2026-07-17\ntags: [idea, note]\naliases: []\nfolder: \"[[Parent]]\"\n---\n\n# Body\n";
        let fm = parse_frontmatter(raw);
        assert!(fm.had_fm && !fm.malformed);
        assert_eq!(fm.props[0], scalar("created", "2026-07-17"));
        assert_eq!(fm.props[1], list("tags", &["idea", "note"]));
        assert_eq!(fm.props[2], list("aliases", &[]));
        // Quoted wikilink stays a scalar, not a nested list.
        assert_eq!(fm.props[3], scalar("folder", "[[Parent]]"));
        // The blank line after the closing fence is body, kept verbatim.
        assert_eq!(fm.body, "\n# Body\n");
    }

    #[test]
    fn parses_block_lists() {
        let raw = "---\ntags:\n  - idea\n  - note\n---\nbody";
        let fm = parse_frontmatter(raw);
        assert_eq!(fm.props[0], list("tags", &["idea", "note"]));
        assert_eq!(fm.body, "body");
    }

    #[test]
    fn round_trips_and_preserves_order_and_body() {
        let raw = "---\ncreated: 2026-07-17\ntags: [a, b]\nfolder: \"[[Parent]]\"\n---\nHello [[world]]\n";
        let fm = parse_frontmatter(raw);
        let out = serialize(&fm.props, &fm.body);
        assert_eq!(out, raw);
    }

    #[test]
    fn no_frontmatter_gains_one_on_add() {
        let raw = "# Just a body\n";
        let fm = parse_frontmatter(raw);
        assert!(!fm.had_fm && fm.props.is_empty());
        assert_eq!(fm.body, raw);
        let props = vec![list("tags", &["x"])];
        let out = serialize(&props, &fm.body);
        assert_eq!(out, "---\ntags: [x]\n---\n# Just a body\n");
    }

    #[test]
    fn emptying_frontmatter_drops_the_block() {
        assert_eq!(serialize(&[], "body only"), "body only");
    }

    #[test]
    fn malformed_frontmatter_is_flagged() {
        let raw = "---\ncreated: 2026-07-17\nno closing fence\n";
        let fm = parse_frontmatter(raw);
        assert!(fm.malformed);
        assert!(fm.props.is_empty());
    }

    #[test]
    fn quotes_only_when_needed() {
        assert_eq!(quote_if_needed("2026-07-17"), "2026-07-17");
        assert_eq!(quote_if_needed("idea"), "idea");
        assert_eq!(quote_if_needed("[[Parent]]"), "\"[[Parent]]\"");
        assert_eq!(quote_if_needed("a: b"), "\"a: b\"");
        assert_eq!(quote_if_needed(""), "\"\"");
    }

    #[test]
    fn rename_keeps_folder_and_sanitizes() {
        assert_eq!(
            rename_path("Notes/Old Name.md", "New Title").as_deref(),
            Some("Notes/New Title.md")
        );
        assert_eq!(
            rename_path("Untitled 1a2b.md", "a/b:c").as_deref(),
            Some("a b c.md")
        );
        assert_eq!(rename_path("x.md", "My Note.md").as_deref(), Some("My Note.md"));
        assert_eq!(rename_path("x.md", "   "), None);
    }
}

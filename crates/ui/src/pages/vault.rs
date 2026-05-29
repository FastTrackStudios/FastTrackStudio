//! `/vault` — browse the org's vault as a **virtual-folder
//! tree** and edit files live in the rich `editor::Editor`.
//!
//! The vault is organized the Obsidian "folder-note" way: each
//! note's `folder: "[[Parent]]"` frontmatter is a wikilink to a
//! parent *folder note*. The sidebar builds an expandable tree
//! from that property (not from physical directories) via the
//! server's [`folder_index`] rpc, which parses frontmatter once
//! and returns lightweight [`PageMeta`]s. Folder notes are real
//! notes: clicking the **name opens the note**, the **chevron
//! toggles** its children. Notes can be **re-filed** from the
//! sidebar (a "move to folder" picker rewrites the `folder`
//! property through [`set_folder`]).
//!
//! Editing path is unchanged: selecting a note pulls its bytes
//! via `get_file`, saves through `put_file` with an
//! `IfMatch::Sha` conditional write, and surfaces conflicts.
//!
//! The server registers exactly one vault per org under the id
//! `"default"`.
//!
//! [`folder_index`]: vault_proto::VaultSync::folder_index
//! [`set_folder`]: vault_proto::VaultSync::set_folder
//! [`PageMeta`]: vault_proto::PageMeta

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use dioxus::prelude::*;
use editor::editor_view::slash::{SlashMenu, SlashState};
use editor::editor_vim::VimState;
use editor::{Editor, EditorState, editor_view};
use fts_ui::lucide_dioxus::{ChevronRight, FileText, Folder};
use fts_ui::prelude::*;
use vault_proto::PageMeta;

/// The single vault id the server hosts per org. Referenced
/// only from the wasm client calls.
#[cfg(target_arch = "wasm32")]
const VAULT_ID: &str = "default";

/// Minimal payload to open a file: its path + last-known sha.
#[derive(Clone, PartialEq)]
struct FileMeta {
    path: String,
    sha256: String,
}

/// The server's version of a file we tried to save over.
#[derive(Clone, PartialEq)]
struct ConflictInfo {
    server_sha: String,
    server_text: String,
}

/// Outcome of a `put_file` attempt. `Saved` / `Conflict` are
/// only constructed in the wasm client path.
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
enum SaveOutcome {
    Saved(String),
    Conflict {
        server_sha: String,
        server_text: String,
    },
    Failed(String),
}

/// One node of the virtual-folder tree.
#[derive(Clone, PartialEq)]
struct TreeNode {
    meta: PageMeta,
    children: Vec<usize>,
    is_folder: bool,
}

#[component]
pub fn VaultView() -> Element {
    let mut files = use_resource(|| async move { fetch_folder_index().await });

    // Open file + its editing state.
    let mut selected = use_signal(|| None::<String>);
    let mut state = use_signal(|| EditorState::new(String::new()));
    let mut sha = use_signal(|| None::<String>);
    let mut saved_text = use_signal(String::new);
    let mut dirty = use_signal(|| false);
    let mut status = use_signal(String::new);
    let mut conflict = use_signal(|| None::<ConflictInfo>);
    let mut new_name = use_signal(String::new);

    // Tree UI state. `collapsed` holds folders the user has
    // closed — default-empty means the whole tree starts
    // expanded. `move_target` is the path of a note being
    // re-filed (drives the folder picker). `create_parent` is
    // the folder a new note will be filed under.
    let collapsed = use_signal(HashSet::<String>::new);
    let mut move_target = use_signal(|| None::<String>);
    let mut create_parent = use_signal(|| None::<String>);

    // Editor extensions — same standard markdown setup as the
    // turnkey `editor::EditorApp`.
    let keymap = use_signal(editor::standard_markdown_keymap);
    let vim = use_signal(VimState::new);
    let slash = use_signal(|| None::<SlashState>);

    use_effect(move || {
        let cur = state.read().doc.to_string();
        dirty.set(cur != *saved_text.read());
    });

    // Open a note: pull bytes, seed a fresh state, remember the
    // server sha so the next save is a conditional write.
    let on_open = use_callback(move |meta: FileMeta| {
        spawn(async move {
            status.set(String::new());
            conflict.set(None);
            match fetch_file(meta.path.clone()).await {
                Ok(text) => {
                    saved_text.set(text.clone());
                    state.set(EditorState::new(text));
                    sha.set(Some(meta.sha256));
                    selected.set(Some(meta.path));
                    dirty.set(false);
                }
                Err(e) => status.set(format!("Load failed: {e}")),
            }
        });
    });

    // Re-file a note under `parent` (None = root) via set_folder,
    // then refresh the tree.
    let do_move = use_callback(
        move |(path, prev_sha, parent): (String, String, Option<String>)| {
            spawn(async move {
                status.set("Moving…".to_owned());
                match move_to_folder(path, parent, prev_sha).await {
                    Ok(()) => {
                        status.set("Moved".to_owned());
                        move_target.set(None);
                        files.restart();
                    }
                    Err(e) => status.set(format!("Move failed: {e}")),
                }
            });
        },
    );

    // Save the current buffer. `force` skips the sha guard.
    let do_save = move |force: bool| {
        let Some(path) = selected.peek().clone() else {
            return;
        };
        let cur_sha = sha.peek().clone();
        let text = state.peek().doc.to_string();
        spawn(async move {
            status.set("Saving…".to_owned());
            match save_file(path, text.clone(), cur_sha, force).await {
                SaveOutcome::Saved(new_sha) => {
                    sha.set(Some(new_sha));
                    saved_text.set(text);
                    dirty.set(false);
                    conflict.set(None);
                    status.set("Saved".to_owned());
                }
                SaveOutcome::Conflict {
                    server_sha,
                    server_text,
                } => {
                    conflict.set(Some(ConflictInfo {
                        server_sha,
                        server_text,
                    }));
                    status.set("Conflict — file changed on server".to_owned());
                }
                SaveOutcome::Failed(e) => status.set(format!("Save failed: {e}")),
            }
        });
    };

    // Create a new empty note. If a folder was chosen (via a
    // folder row's "+"), file it there right after creating.
    let create_file = move || {
        let mut name = new_name.peek().trim().to_owned();
        if name.is_empty() {
            return;
        }
        if !name.to_ascii_lowercase().ends_with(".md") {
            name.push_str(".md");
        }
        let parent = create_parent.peek().clone();
        spawn(async move {
            status.set("Creating…".to_owned());
            match create_new_file(name.clone()).await {
                Ok(new_sha) => {
                    new_name.set(String::new());
                    create_parent.set(None);
                    let mut open_sha = new_sha;
                    if let Some(parent) = parent {
                        match move_to_folder(name.clone(), Some(parent), open_sha.clone()).await {
                            Ok(()) => {}
                            Err(e) => status.set(format!("Filed, but move failed: {e}")),
                        }
                        // The folder write changed the sha; reload by
                        // path on open below handles it, but seed an
                        // empty buffer either way.
                        open_sha = String::new();
                    }
                    saved_text.set(String::new());
                    state.set(EditorState::new(String::new()));
                    sha.set(Some(open_sha));
                    selected.set(Some(name));
                    dirty.set(false);
                    conflict.set(None);
                    status.set("Created".to_owned());
                    files.restart();
                }
                Err(e) => status.set(format!("Create failed: {e}")),
            }
        });
    };

    // Build the tree from the folder index.
    let tree = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => Some(Rc::new(build_tree(pages))),
        _ => None,
    });

    let sidebar_body = match &*files.read_unchecked() {
        Some(Ok(_)) => {
            let Some(t) = tree() else { unreachable!() };
            let (nodes, roots) = (Rc::new(t.0.clone()), t.1.clone());
            rsx! {
                nav { class: "flex flex-col gap-0.5 px-2 pb-4",
                    if roots.is_empty() {
                        div { class: "px-2 py-1 text-sm text-muted-foreground", "Empty vault. Create a note above." }
                    }
                    for &root in roots.iter() {
                        {render_node(nodes.clone(), root, 0, collapsed, selected, on_open, move_target, create_parent)}
                    }
                }
            }
        }
        Some(Err(e)) => rsx! {
            div { class: "px-3 py-2 text-sm text-destructive", "Couldn't reach the vault service: {e}" }
        },
        None => rsx! {
            div { class: "flex items-center gap-2 px-3 py-2 text-sm text-muted-foreground",
                Spinner { size: SpinnerSize::Small }
                "Loading vault…"
            }
        },
    };

    // Folder targets for the move picker: every node that is a
    // folder, by basename + title.
    let folder_targets: Vec<(String, String)> = tree()
        .map(|t| {
            t.0.iter()
                .filter(|n| n.is_folder)
                .map(|n| (n.meta.basename.clone(), n.meta.title.clone()))
                .collect()
        })
        .unwrap_or_default();

    let has_file = selected.read().is_some();
    let current = selected.read().clone().unwrap_or_default();
    let is_dirty = *dirty.read();
    let status_msg = status.read().clone();
    let conflict_open = conflict.read().is_some();
    let moving = move_target.read().clone();
    let create_under = create_parent.read().clone();

    rsx! {
        div { class: "flex h-full min-h-[80vh]",
            // ── Virtual-folder tree ───────────────────────
            aside { class: "flex w-72 shrink-0 flex-col overflow-y-auto border-r border-border bg-muted/30",
                div { class: "flex flex-col gap-2 px-3 py-3",
                    Heading { level: HeadingLevel::H3, "Vault" }
                    if let Some(parent) = create_under.clone() {
                        Text { variant: TextVariant::Muted, class: "text-xs",
                            "New note will be filed under {parent}."
                        }
                    }
                    div { class: "flex items-center gap-2",
                        Input {
                            value: new_name,
                            placeholder: "New note…",
                            on_change: move |_| {},
                        }
                        Button {
                            variant: ButtonVariant::Secondary,
                            size: ButtonSize::Small,
                            on_click: move |_| create_file(),
                            "Create"
                        }
                    }
                }
                // ── Move-to-folder picker ─────────────────
                if let Some(path) = moving.clone() {
                    div { class: "mx-2 mb-2 rounded border border-border bg-background p-2",
                        div { class: "flex items-center justify-between gap-2 pb-1",
                            Text { variant: TextVariant::Muted, class: "text-xs truncate", "Move to…" }
                            button {
                                class: "text-xs text-muted-foreground hover:text-foreground",
                                onclick: move |_| move_target.set(None),
                                "Cancel"
                            }
                        }
                        div { class: "flex max-h-48 flex-col gap-0.5 overflow-y-auto",
                            {
                                let p = path.clone();
                                rsx! {
                                    button {
                                        class: "rounded px-2 py-1 text-left text-sm hover:bg-accent/50",
                                        onclick: move |_| do_move.call((p.clone(), String::new(), None)),
                                        "(Root)"
                                    }
                                }
                            }
                            for (base, title) in folder_targets.iter().cloned() {
                                {
                                    let p = path.clone();
                                    let b = base.clone();
                                    rsx! {
                                        button {
                                            key: "{base}",
                                            class: "truncate rounded px-2 py-1 text-left text-sm hover:bg-accent/50",
                                            onclick: move |_| do_move.call((p.clone(), String::new(), Some(b.clone()))),
                                            "{title}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                {sidebar_body}
            }
            // ── Editor pane ───────────────────────────────
            div {
                class: "flex min-w-0 flex-1 flex-col",
                onkeydown: move |evt: Event<KeyboardData>| {
                    let m = evt.modifiers();
                    if (m.ctrl() || m.meta()) && evt.key().to_string() == "s" {
                        evt.prevent_default();
                        do_save(false);
                    }
                },
                div { class: "flex items-center justify-between gap-3 border-b border-border px-4 py-2",
                    div { class: "flex min-w-0 items-center gap-2",
                        if has_file && is_dirty {
                            span { class: "size-2 shrink-0 rounded-full bg-primary", title: "Unsaved changes" }
                        }
                        div { class: "min-w-0 truncate text-sm font-medium",
                            if has_file { "{current}" } else { "No file selected" }
                        }
                    }
                    div { class: "flex items-center gap-3",
                        if !status_msg.is_empty() {
                            Text { variant: TextVariant::Muted, class: "text-xs", "{status_msg}" }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            disabled: !has_file,
                            on_click: move |_| do_save(false),
                            "Save"
                        }
                    }
                }
                if conflict_open {
                    div { class: "flex items-center justify-between gap-3 border-b border-destructive/40 bg-destructive/10 px-4 py-2 text-sm",
                        span { "This file changed on the server since you opened it." }
                        div { class: "flex items-center gap-2",
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| {
                                    let snapshot = conflict.peek().clone();
                                    if let Some(c) = snapshot {
                                        saved_text.set(c.server_text.clone());
                                        state.set(EditorState::new(c.server_text));
                                        sha.set(Some(c.server_sha));
                                        dirty.set(false);
                                        conflict.set(None);
                                        status.set("Reloaded from server".to_owned());
                                    }
                                },
                                "Reload"
                            }
                            Button {
                                variant: ButtonVariant::Destructive,
                                size: ButtonSize::Small,
                                on_click: move |_| do_save(true),
                                "Overwrite"
                            }
                        }
                    }
                }
                div { class: "flex min-h-0 flex-1 flex-col overflow-y-auto",
                    if has_file {
                        div { class: "editor-app",
                            div { class: "editor-frame",
                                Editor {
                                    state,
                                    keymap: keymap.read().clone(),
                                    decorations: editor::combined_decorations as editor_view::DecorationSource,
                                    vim: Some(vim),
                                    slash: Some(slash),
                                }
                                SlashMenu { state, slash }
                            }
                        }
                    } else {
                        div { class: "flex h-full items-center justify-center p-8",
                            Text { variant: TextVariant::Muted,
                                "Select a note from the tree to start editing."
                            }
                        }
                    }
                }
            }
        }
        document::Link { rel: "stylesheet", href: editor::EDITOR_STYLE }
    }
}

/// Render one tree node (and, when a folder is expanded, its
/// children) recursively. Folders: chevron toggles `collapsed`,
/// the name opens the folder note, "+" files a new note under
/// it. Leaves: the name opens the note. Every row has a hover
/// "move" affordance.
#[allow(clippy::too_many_arguments)]
fn render_node(
    nodes: Rc<Vec<TreeNode>>,
    idx: usize,
    depth: usize,
    mut collapsed: Signal<HashSet<String>>,
    selected: Signal<Option<String>>,
    on_open: Callback<FileMeta>,
    mut move_target: Signal<Option<String>>,
    mut create_parent: Signal<Option<String>>,
) -> Element {
    let node = nodes[idx].clone();
    let key = node.meta.basename.to_lowercase();
    let is_expanded = !collapsed.read().contains(&key);
    let is_active = selected.read().as_deref() == Some(node.meta.path.as_str());
    let indent = depth * 14 + 8;

    let open_meta = FileMeta {
        path: node.meta.path.clone(),
        sha256: node.meta.sha256.clone(),
    };
    let move_path = node.meta.path.clone();
    let create_base = node.meta.basename.clone();
    let toggle_key = key.clone();

    let row_cls = if is_active {
        "group flex items-center gap-1 rounded pr-1 text-sm bg-accent text-accent-foreground"
    } else {
        "group flex items-center gap-1 rounded pr-1 text-sm hover:bg-accent/50"
    };

    rsx! {
        div {
            div { class: row_cls, style: "padding-left: {indent}px",
                if node.is_folder {
                    button {
                        class: "flex size-5 shrink-0 items-center justify-center text-muted-foreground",
                        onclick: move |_| {
                            let mut c = collapsed.write();
                            if !c.remove(&toggle_key) { c.insert(toggle_key.clone()); }
                        },
                        span {
                            class: if is_expanded { "transition-transform rotate-90" } else { "transition-transform" },
                            ChevronRight { size: 14 }
                        }
                    }
                    span { class: "flex size-4 shrink-0 items-center justify-center text-muted-foreground",
                        Folder { size: 14 }
                    }
                } else {
                    span { class: "ml-5 flex size-4 shrink-0 items-center justify-center text-muted-foreground",
                        FileText { size: 14 }
                    }
                }
                button {
                    class: "min-w-0 flex-1 truncate py-1 text-left",
                    onclick: move |_| on_open.call(open_meta.clone()),
                    "{node.meta.title}"
                }
                if node.is_folder {
                    button {
                        class: "hidden size-5 shrink-0 items-center justify-center text-muted-foreground hover:text-foreground group-hover:flex",
                        title: "New note in this folder",
                        onclick: move |_| create_parent.set(Some(create_base.clone())),
                        "+"
                    }
                }
                button {
                    class: "hidden size-5 shrink-0 items-center justify-center text-muted-foreground hover:text-foreground group-hover:flex",
                    title: "Move to folder",
                    onclick: move |_| move_target.set(Some(move_path.clone())),
                    "⋯"
                }
            }
            if node.is_folder && is_expanded {
                for &child in node.children.iter() {
                    {render_node(nodes.clone(), child, depth + 1, collapsed, selected, on_open, move_target, create_parent)}
                }
            }
        }
    }
}

/// Build the virtual-folder tree from the flat page list.
/// Parent = each page's `folder` (already a basename); roots are
/// pages with no/unresolved parent. Cycles are broken (the node
/// falls back to a root). Children sort folders-first, then by
/// title.
fn build_tree(pages: &[PageMeta]) -> (Vec<TreeNode>, Vec<usize>) {
    let mut nodes: Vec<TreeNode> = pages
        .iter()
        .map(|m| TreeNode {
            meta: m.clone(),
            children: Vec::new(),
            is_folder: false,
        })
        .collect();

    // basename (lowercased) → first node with it.
    let mut by_base: HashMap<String, usize> = HashMap::new();
    for (i, n) in nodes.iter().enumerate() {
        by_base.entry(n.meta.basename.to_lowercase()).or_insert(i);
    }

    // Resolve each node's parent index (None = root). Self-parent
    // and unknown targets resolve to root.
    let parent_of: Vec<Option<usize>> = (0..nodes.len())
        .map(|i| {
            let f = nodes[i].meta.folder.to_lowercase();
            if f.is_empty() {
                return None;
            }
            match by_base.get(&f) {
                Some(&p) if p != i => Some(p),
                _ => None,
            }
        })
        .collect();

    // A node is a tree child only if walking its ancestry reaches
    // a root within N steps — otherwise it's in a cycle and we
    // treat it as a root so the tree stays finite.
    let max = nodes.len();
    let resolves = |start: usize| -> bool {
        let mut cur = start;
        for _ in 0..=max {
            match parent_of[cur] {
                None => return true,
                Some(p) => cur = p,
            }
        }
        false
    };

    let mut roots = Vec::new();
    for (i, parent) in parent_of.iter().enumerate() {
        match parent {
            Some(p) if resolves(i) => nodes[*p].children.push(i),
            _ => roots.push(i),
        }
    }

    for n in &mut nodes {
        let t = n.meta.page_type.to_lowercase();
        n.is_folder = !n.children.is_empty() || t == "folder" || t == "index";
    }

    // Sort key per node — captured up front so the child/root
    // sorts borrow it (not `nodes`).
    let sort_key: Vec<(bool, String)> = nodes
        .iter()
        .map(|n| (!n.is_folder, n.meta.title.to_lowercase()))
        .collect();
    roots.sort_by(|a, b| sort_key[*a].cmp(&sort_key[*b]));
    for n in &mut nodes {
        n.children.sort_by(|a, b| sort_key[*a].cmp(&sort_key[*b]));
    }

    (nodes, roots)
}

/// Frontmatter-derived page index for the folder tree.
async fn fetch_folder_index() -> Result<Vec<PageMeta>, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        let idx = client
            .folder_index(VAULT_ID.to_owned())
            .await
            .map_err(|e| format!("folder_index: {e:?}"))?;
        let mut pages: Vec<PageMeta> = idx
            .pages
            .into_iter()
            .filter(|p| {
                std::path::Path::new(&p.path)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("md"))
            })
            .collect();
        pages.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
        Ok(pages)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = client;
        Err("native client not wired yet".to_owned())
    }
}

/// Read one file's bytes as UTF-8 text.
async fn fetch_file(path: String) -> Result<String, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        let bytes = client
            .get_file(VAULT_ID.to_owned(), path)
            .await
            .map_err(|e| format!("get_file: {e:?}"))?;
        Ok(String::from_utf8_lossy(&bytes.0).into_owned())
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

/// Conditional-write the file back. `prev_sha` is the
/// last-known-server hash (`None` → create-only); `force`
/// writes unconditionally.
async fn save_file(
    path: String,
    text: String,
    prev_sha: Option<String>,
    force: bool,
) -> SaveOutcome {
    let client = match crate::vox_clients::vault_client().await {
        Ok(c) => c,
        Err(e) => return SaveOutcome::Failed(e),
    };
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        use vault_proto::VaultSyncError;
        use vox::VoxError;
        let if_match = if force {
            IfMatch::Force
        } else {
            match prev_sha {
                Some(s) => IfMatch::Sha(s),
                None => IfMatch::CreateOnly,
            }
        };
        match client
            .put_file(VAULT_ID.to_owned(), path, text.into_bytes(), if_match)
            .await
        {
            Ok(ack) => SaveOutcome::Saved(ack.sha256),
            Err(VoxError::User(VaultSyncError::Conflict {
                server_sha,
                server_bytes,
            })) => SaveOutcome::Conflict {
                server_sha,
                server_text: String::from_utf8_lossy(&server_bytes).into_owned(),
            },
            Err(e) => SaveOutcome::Failed(format!("{e:?}")),
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path, text, prev_sha, force);
        SaveOutcome::Failed("native client not wired yet".to_owned())
    }
}

/// Re-file a note: set its `folder` to `parent` (None = root)
/// via the server-side frontmatter splice. `prev_sha` empty →
/// unconditional.
async fn move_to_folder(
    path: String,
    parent: Option<String>,
    prev_sha: String,
) -> Result<(), String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let if_match = if prev_sha.is_empty() {
            IfMatch::Force
        } else {
            IfMatch::Sha(prev_sha)
        };
        client
            .set_folder(VAULT_ID.to_owned(), path, parent, if_match)
            .await
            .map_err(|e| format!("set_folder: {e:?}"))?;
        Ok(())
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path, parent, prev_sha);
        Err("native client not wired yet".to_owned())
    }
}

/// Create a new empty file (create-only). Returns its sha.
async fn create_new_file(path: String) -> Result<String, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let ack = client
            .put_file(VAULT_ID.to_owned(), path, Vec::new(), IfMatch::CreateOnly)
            .await
            .map_err(|e| format!("put_file: {e:?}"))?;
        Ok(ack.sha256)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

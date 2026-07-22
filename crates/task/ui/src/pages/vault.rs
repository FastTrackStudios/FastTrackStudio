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
//! **Document tabs + split panes.** The document area is a set of
//! **panes** (a single 2-way horizontal split, max [`MAX_PANES`]);
//! each pane carries its own **tab strip** of open notes and an
//! active tab. Clicking a note in the tree (or a backlink / graph
//! node, or a `?path=` deep link) **opens-or-focuses** a tab in the
//! *focused* pane. Each open note renders as a
//! [`NoteView`](crate::pages::note_view::NoteView) — the per-note
//! `DocumentSession` + collab + `type:`-dispatch, extracted so it can
//! be instantiated once per tab/pane. With a single open note the
//! page looks exactly as it did before tabs existed.
//!
//! The open/save/conflict lifecycle lives in
//! [`DocumentSession`](crate::document_session::DocumentSession):
//! typed conflicts, a debounced autosave, explicit save (Ctrl+S /
//! toolbar), force-save (the conflict banner's *Overwrite*), and
//! reload — each `NoteView` renders from its typed state instead of a
//! hand-rolled signal cluster.
//!
//! Wikilinks + embeds resolve through the client-side
//! [`ClientVaultIndex`](crate::vault_lookup::ClientVaultIndex)
//! (folder-index metadata + lazy `get_file` content LRU), passed
//! to the editor as a stateful `DecorationSource`; `[[` and `#`
//! autocomplete ride the editor's trigger `CompletionSource`
//! (basenames + aliases from the folder index, tags from the
//! `VaultGraph` RPC). A right-side **backlinks panel** lists
//! pages linking to the *focused* note via the same RPC and refreshes
//! after every save.
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
use fts_ui::lucide_dioxus::{ChevronRight, FileText, Folder};
use fts_ui::prelude::*;
use vault_proto::{PageMeta, TagCount};
use view_knowledge_graph::{GraphEdge, GraphNode, KnowledgeGraphView, WikiGraph};

use crate::pages::note_view::NoteView;
use crate::shell::mobile::{BottomSheet, MobileActionBar};
use crate::vault_lookup;

#[cfg(target_arch = "wasm32")]
use crate::document_session::VAULT_ID;

/// Minimal payload to open a file: its path + last-known sha.
#[derive(Clone, PartialEq)]
pub(crate) struct FileMeta {
    pub(crate) path: String,
    pub(crate) sha256: String,
}

/// One open note in a pane's tab strip: its path + last-known sha
/// (the `DocumentSession` conditional-write base).
#[derive(Clone, PartialEq)]
struct OpenTab {
    path: String,
    sha: String,
}

/// A document pane — an ordered set of open tabs and the active one.
/// Each pane mounts exactly its active tab's [`NoteView`] (inactive
/// tabs are unmounted; switching remounts a fresh session).
#[derive(Clone, PartialEq)]
struct Pane {
    tabs: Vec<OpenTab>,
    active: usize,
}

/// Hard cap on side-by-side panes — a single 2-way horizontal split.
const MAX_PANES: usize = 2;

/// The right sidebar's active tab. Properties (the focused note's
/// frontmatter, edited live) and Links (backlinks + outgoing links +
/// local graph).
#[derive(Clone, Copy, PartialEq, Eq)]
enum RightTab {
    Properties,
    Links,
    Share,
}

/// One node of the virtual-folder tree.
#[derive(Clone, PartialEq)]
pub(crate) struct TreeNode {
    pub(crate) meta: PageMeta,
    pub(crate) children: Vec<usize>,
    pub(crate) is_folder: bool,
}

#[component]
pub fn VaultView(#[props(default)] initial_path: ReadSignal<String>) -> Element {
    // The vault lives in the home org; resolve its slug from the
    // discovered org list (re-runs when discovery lands).
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let home = use_memo(move || crate::orgs::home_slug(&org_list.read()));
    let mut files = use_resource(move || {
        let slug = home();
        async move { fetch_folder_index(slug).await }
    });

    let mut new_name = use_signal(String::new);
    // Failures from tree operations (move / create) outlive their
    // buttons via the app-wide notification queue.
    let notify = architect::try_use_notifications();

    // Tree UI state. `collapsed` holds folders the user has closed.
    // `move_target` is a note being re-filed; `create_parent` is the
    // folder a new note will be filed under.
    let collapsed = use_signal(HashSet::<String>::new);
    let mut move_target = use_signal(|| None::<String>);
    let mut create_parent = use_signal(|| None::<String>);
    // Mobile-only: the file tree lives in a bottom sheet once a note
    // is open (inline full-width while nothing is selected).
    let mut files_open = use_signal(|| false);

    // ── Tabs + split state ────────────────────────────────────
    // `panes` is 1–2 panes, each an ordered tab set + active index;
    // `focused` is the pane that tree/deep-link opens route into and
    // that owns the status line + backlinks. `focus_tick` is bumped by
    // the focused NoteView after each save so the backlinks/links/graph
    // panel refreshes (it used to read `session.save_count`).
    let mut panes = use_signal(|| {
        vec![Pane {
            tabs: Vec::new(),
            active: 0,
        }]
    });
    let mut focused = use_signal(|| 0usize);
    let focus_tick = use_signal(|| 0u64);

    // The focused note's live editor doc, published by its `NoteView`
    // and consumed by the right-sidebar Properties tab.
    use_context_provider(|| Signal::new(None::<crate::pages::note_properties::FocusedDoc>));
    // Which right-sidebar tab is showing. Properties first — it's the
    // one used most while writing.
    let mut right_tab = use_signal(|| RightTab::Properties);
    let nav = use_navigator();

    // Focused pane's active-tab path — drives the tree highlight, the
    // backlinks panel, and the "any note open?" layout switches.
    let selected = use_memo(move || {
        let p = panes.read();
        let f = (*focused.read()).min(p.len().saturating_sub(1));
        p.get(f)
            .and_then(|pane| pane.tabs.get(pane.active))
            .map(|t| t.path.clone())
    });

    // Open-or-focus a note in the focused pane (tree rows, backlinks,
    // wikilinks, graph nodes, `.base` rows, freshly-created notes).
    let on_open = use_callback(move |meta: FileMeta| {
        files_open.set(false);
        let mut p = panes.write();
        let f = (*focused.peek()).min(p.len().saturating_sub(1));
        let Some(pane) = p.get_mut(f) else { return };
        if let Some(i) = pane.tabs.iter().position(|t| t.path == meta.path) {
            pane.active = i;
        } else {
            pane.tabs.push(OpenTab {
                path: meta.path,
                sha: meta.sha256,
            });
            pane.active = pane.tabs.len() - 1;
        }
    });

    // ── Tab / pane controls ───────────────────────────────────
    let focus_tab = use_callback(move |(pi, idx): (usize, usize)| {
        focused.set(pi);
        let mut p = panes.write();
        if let Some(pane) = p.get_mut(pi) {
            if idx < pane.tabs.len() {
                pane.active = idx;
            }
        }
    });
    let close_tab = use_callback(move |(pi, idx): (usize, usize)| {
        let mut removed_pane = false;
        {
            let mut p = panes.write();
            let Some(pane) = p.get_mut(pi) else { return };
            if idx >= pane.tabs.len() {
                return;
            }
            pane.tabs.remove(idx);
            if pane.active >= pane.tabs.len() {
                pane.active = pane.tabs.len().saturating_sub(1);
            }
            // Drop an emptied pane when it isn't the last one.
            if pane.tabs.is_empty() && p.len() > 1 {
                p.remove(pi);
                removed_pane = true;
            }
        }
        if removed_pane {
            let len = panes.read().len();
            if *focused.peek() >= len {
                focused.set(len.saturating_sub(1));
            }
        }
    });
    let split = use_callback(move |()| {
        let new_idx = {
            let mut p = panes.write();
            if p.len() >= MAX_PANES {
                return;
            }
            p.push(Pane {
                tabs: Vec::new(),
                active: 0,
            });
            p.len() - 1
        };
        focused.set(new_idx);
    });
    let close_pane = use_callback(move |pi: usize| {
        let len = {
            let mut p = panes.write();
            if p.len() <= 1 || pi >= p.len() {
                return;
            }
            p.remove(pi);
            p.len()
        };
        if *focused.peek() >= len {
            focused.set(len.saturating_sub(1));
        }
    });
    let focus_pane = use_callback(move |pi: usize| focused.set(pi));

    // Refresh the folder index after a rename commits (tree row path
    // changed) — threaded into every mounted NoteView.
    let on_renamed = use_callback(move |()| files.restart());

    // Deep-link + shell-tree navigation: `/vault?path=<path>` opens a
    // tab in the focused pane once the folder index lands. Reactive on
    // the query param — every NEW `?path=` opens; `last_link` remembers
    // the one already honored so in-page selection isn't stomped.
    let mut last_link = use_signal(String::new);
    use_effect(move || {
        let want = initial_path();
        if want.is_empty() || *last_link.peek() == want {
            return;
        }
        if let Some(Ok(pages)) = &*files.read() {
            let hit = pages.iter().find(|p| p.path == want).or_else(|| {
                pages
                    .iter()
                    .find(|p| basename_of(&p.path) == basename_of(&want))
            });
            if let Some(p) = hit {
                last_link.set(want);
                on_open.call(FileMeta {
                    path: p.path.clone(),
                    sha256: p.sha256.clone(),
                });
            }
        }
    });

    // Pane → route sync: keep the top tab strip (the ONE tab UI now
    // that the inner note-tab bar is hidden in single-pane mode)
    // tracking the focused pane's active note. Shares `last_link` with
    // the route→pane effect above so the two directions can't ping-pong:
    // whichever side moves first stamps `last_link`, and the other sees
    // it already matches and stops.
    use_effect(move || {
        let Some(sel) = selected() else { return };
        if *last_link.peek() == sel {
            return;
        }
        last_link.set(sel.clone());
        nav.push(crate::routes::Route::VaultRoute { path: sel });
    });

    // Autocomplete tags — `#` completes vault tags pulled once per org
    // (re-pulled after each save via `focus_tick`, since saves mint
    // tags). Shared by every mounted NoteView's completion source.
    let mut tag_rows = use_signal(Vec::<TagCount>::new);
    use_effect(move || {
        let slug = home();
        let _refresh = *focus_tick.read();
        spawn(async move {
            if let Ok(tags) = vault_lookup::tag_candidates(slug).await {
                tag_rows.set(tags);
            }
        });
    });

    // Re-file a note under `parent` (None = root) via set_folder,
    // then refresh the tree.
    let do_move = use_callback(
        move |(path, prev_sha, parent): (String, String, Option<String>)| {
            spawn(async move {
                match move_to_folder(home(), path, parent, prev_sha).await {
                    Ok(_new_sha) => {
                        move_target.set(None);
                        files.restart();
                    }
                    Err(e) => {
                        if let Some(n) = notify {
                            n.error(format!("Move failed: {e}"));
                        }
                    }
                }
            });
        },
    );

    // Create a new empty note. If a folder was chosen, file it there,
    // then open a tab — the open re-fetches, so the buffer reflects the
    // server-spliced `folder:` frontmatter.
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
            match create_new_file(home(), name.clone()).await {
                Ok(created_sha) => {
                    new_name.set(String::new());
                    create_parent.set(None);
                    let mut open_sha = created_sha.clone();
                    if let Some(parent) = parent {
                        match move_to_folder(home(), name.clone(), Some(parent), created_sha).await {
                            Ok(new_sha) => open_sha = new_sha,
                            Err(e) => {
                                if let Some(n) = notify {
                                    n.error(format!("Created, but filing failed: {e}"));
                                }
                            }
                        }
                    }
                    on_open.call(FileMeta {
                        path: name,
                        sha256: open_sha,
                    });
                    files.restart();
                }
                Err(e) => {
                    if let Some(n) = notify {
                        n.error(format!("Create failed: {e}"));
                    }
                }
            }
        });
    };

    // Build the tree from the folder index.
    let tree = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => Some(Rc::new(build_tree(pages))),
        _ => None,
    });

    // Folder-index pages threaded into every NoteView (wikilink
    // candidates + cross-file lookup + the `type:` dispatch).
    let pages_memo = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => pages.clone(),
        _ => Vec::new(),
    });

    // path → (title, sha) for the backlinks panel rows.
    let page_lookup = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => pages
            .iter()
            .map(|p| (p.path.clone(), (p.title.clone(), p.sha256.clone())))
            .collect::<HashMap<String, (String, String)>>(),
        _ => HashMap::new(),
    });

    // Backlinks for the focused note, re-pulled when the selection
    // changes and after every committed save (`focus_tick`).
    let shell_right = use_context::<Signal<crate::chrome::RightPanelOpen>>();
    let backlinks_open = use_memo(move || shell_right.read().0);
    let backlinks = use_resource(move || {
        let slug = home();
        let path = selected();
        let _refresh = *focus_tick.read();
        async move {
            match path {
                Some(p) => fetch_backlinks(slug, p).await,
                None => Ok(Vec::new()),
            }
        }
    });

    // Outgoing wikilinks of the focused note.
    let outlinks = use_resource(move || {
        let slug = home();
        let path = selected();
        let _refresh = *focus_tick.read();
        async move {
            match path {
                Some(p) => fetch_links(slug, p).await,
                None => Ok(Vec::new()),
            }
        }
    });

    // Verses the focused note references (from synced note→verse
    // links), with their text — the inline scripture reader.
    let verses = use_resource(move || {
        let slug = home();
        let path = selected();
        let _refresh = *focus_tick.read();
        async move {
            let Some(p) = path else { return Vec::new() };
            let links = crate::feeds::fetch_links_for(&slug, &format!("note:{p}"))
                .await
                .unwrap_or_default();
            let mut refs: Vec<String> = links
                .iter()
                .filter(|l| l.target.kind == links_proto::NodeKind::Verse)
                .map(|l| l.target.id.clone())
                .collect();
            refs.sort();
            refs.dedup();
            refs.truncate(16);
            let mut out = Vec::new();
            for osis in refs {
                let human = osis_to_ref(&osis);
                let text = crate::feeds::fetch_verse_text(&slug, "WEB", &human)
                    .await
                    .ok();
                out.push((osis, human, text));
            }
            out
        }
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

    // Folder targets for the move picker.
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
    let verse_list = verses.read().clone();
    let moving = move_target.read().clone();
    let create_under = create_parent.read().clone();
    let panel_open = *backlinks_open.read();

    // ── Status line (focused NoteView writes it; the page reads it for
    //    the mobile action bar's Save affordance) ─────────────────────
    let status_info = use_context::<crate::chrome::StatusBarInfo>().0;
    // Clear the status segments when nothing is open, and on page leave.
    use_effect(move || {
        if selected().is_none() {
            let mut info = status_info;
            info.set(None);
        }
    });
    use_drop(move || {
        let mut info = status_info;
        info.set(None);
    });

    // ── Tree pane content ─────────────────────────────────
    let tree_content = rsx! {
        div { class: "flex flex-col gap-2 px-3 py-3",
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
    };

    // ── Backlinks + verses content ────────────────────────
    let backlinks_body = rsx! {
        if let Some(vs) = verse_list.as_ref().filter(|v| !v.is_empty()) {
            div { class: "border-b border-border/60 px-3 py-3",
                Heading { level: HeadingLevel::H3, class: "mb-2", "Referenced verses" }
                div { class: "flex flex-col gap-2",
                    for (osis, human, text) in vs.clone() {
                        div { key: "{osis}", class: "rounded-md bg-background/60 p-2",
                            span { class: "text-xs font-semibold text-primary", "{human}" }
                            if let Some(t) = text {
                                p { class: "mt-0.5 text-xs leading-snug text-muted-foreground", "{t}" }
                            }
                        }
                    }
                }
            }
        }
        match &*backlinks.read_unchecked() {
            Some(Ok(list)) if list.is_empty() => rsx! {
                div { class: "px-3 py-2 text-sm text-muted-foreground",
                    "No backlinks yet. Link to this note with [[{basename_of(&current)}]]."
                }
            },
            Some(Ok(list)) => rsx! {
                nav { class: "flex flex-col gap-0.5 px-2 pb-4",
                    for path in list.iter().cloned() {
                        {
                            let (title, sha) = page_lookup
                                .read()
                                .get(&path)
                                .cloned()
                                .unwrap_or_else(|| (basename_of(&path).to_owned(), String::new()));
                            let target = FileMeta { path: path.clone(), sha256: sha };
                            rsx! {
                                button {
                                    key: "{path}",
                                    class: "group flex flex-col items-start gap-0.5 rounded px-2 py-1.5 text-left text-sm hover:bg-accent/50",
                                    onclick: move |_| on_open.call(target.clone()),
                                    span { class: "font-medium", "{title}" }
                                    span { class: "text-xs text-muted-foreground", "{path}" }
                                }
                            }
                        }
                    }
                }
            },
            Some(Err(e)) => rsx! {
                div { class: "px-3 py-2 text-sm text-destructive",
                    "Couldn't load backlinks: {e}"
                }
            },
            None => rsx! {
                div { class: "flex flex-col gap-2 px-3 py-2",
                    Skeleton { class: "h-4 w-3/4" }
                    Skeleton { class: "h-4 w-1/2" }
                    Skeleton { class: "h-4 w-2/3" }
                }
            },
        }
        div { class: "border-t border-border/60 px-3 pb-1 pt-3",
            Heading { level: HeadingLevel::H3, "Links" }
        }
        match &*outlinks.read_unchecked() {
            Some(Ok(links)) if links.is_empty() => rsx! {
                div { class: "px-3 py-2 text-sm text-muted-foreground", "No outgoing links." }
            },
            Some(Ok(links)) => rsx! {
                nav { class: "flex flex-col gap-0.5 px-2 pb-4",
                    for link in links.iter().cloned() {
                        {
                            let label = link.alias.clone().unwrap_or_else(|| link.linkpath.clone());
                            match link.resolved.clone() {
                                Some(target_path) => {
                                    let (_, sha) = page_lookup
                                        .read()
                                        .get(&target_path)
                                        .cloned()
                                        .unwrap_or_default();
                                    let target = FileMeta { path: target_path.clone(), sha256: sha };
                                    rsx! {
                                        button {
                                            key: "{link.linkpath}",
                                            class: "flex flex-col items-start gap-0.5 rounded px-2 py-1.5 text-left text-sm hover:bg-accent/50",
                                            onclick: move |_| on_open.call(target.clone()),
                                            span { class: "font-medium", "{label}" }
                                            span { class: "text-xs text-muted-foreground", "{target_path}" }
                                        }
                                    }
                                }
                                None => rsx! {
                                    div {
                                        key: "{link.linkpath}",
                                        class: "px-2 py-1.5 text-sm text-muted-foreground/70",
                                        title: "Unresolved link",
                                        "{label}"
                                    }
                                },
                            }
                        }
                    }
                }
            },
            Some(Err(e)) => rsx! {
                div { class: "px-3 py-2 text-sm text-destructive", "Couldn't load links: {e}" }
            },
            None => rsx! {
                div { class: "flex flex-col gap-2 px-3 py-2",
                    Skeleton { class: "h-4 w-2/3" }
                    Skeleton { class: "h-4 w-1/2" }
                }
            },
        }
        if has_file {
            div { class: "mt-auto border-t border-border/60 px-3 pb-1 pt-3",
                Heading { level: HeadingLevel::H3, "Local graph" }
            }
            match (&*backlinks.read_unchecked(), &*outlinks.read_unchecked()) {
                (Some(Ok(bl)), Some(Ok(ol))) => {
                    let graph = build_local_graph(&current, bl, ol, &page_lookup.read());
                    let cur = current.clone();
                    rsx! {
                        div { class: "mx-2 mb-3 h-64 shrink-0 overflow-hidden rounded-lg border border-border/70",
                            KnowledgeGraphView {
                                graph,
                                node_scale: 0.3,
                                spacing: 1.5,
                                active: Some(current.clone()),
                                on_node_click: move |id: String| {
                                    if id == cur {
                                        return;
                                    }
                                    let (_, sha) = page_lookup.peek().get(&id).cloned().unwrap_or_default();
                                    on_open.call(FileMeta { path: id, sha256: sha });
                                },
                            }
                        }
                    }
                }
                (Some(Err(e)), _) | (_, Some(Err(e))) => rsx! {
                    div { class: "px-3 py-2 text-sm text-muted-foreground",
                        "Couldn't build the local graph: {e}"
                    }
                },
                _ => rsx! {
                    div { class: "mx-2 mb-3",
                        Skeleton { class: "h-64 w-full rounded-lg" }
                    }
                },
            }
        }
    };

    // Snapshot of the panes for this render pass (the mount loop).
    let pane_list = panes.read().clone();
    let n_panes = pane_list.len();
    let focused_idx = (*focused.read()).min(n_panes.saturating_sub(1));

    rsx! {
        div { class: "flex h-full min-h-[80vh]",
            // ── Virtual-folder tree (mobile-only) ─────────
            aside {
                class: if has_file { "hidden" } else { "flex w-full flex-col overflow-y-auto pb-14 md:hidden" },
                {tree_content.clone()}
            }
            // ── Document area: panes + backlinks ──────────
            div {
                class: if has_file { "flex min-w-0 flex-1" } else { "hidden min-w-0 flex-1 md:flex" },
                // Panes container (1–2 panes side by side).
                div { class: "flex min-h-0 min-w-0 flex-1",
                    for (pi, pane) in pane_list.iter().cloned().enumerate() {
                        div {
                            key: "pane-{pi}",
                            class: if pi == 0 { "flex min-h-0 min-w-0 flex-1 flex-col" } else { "hidden min-h-0 min-w-0 flex-1 flex-col border-l border-border md:flex" },
                            onfocusin: move |_| focus_pane.call(pi),
                            // Inner note-tab bar only in SPLIT mode — a single
                            // router route can't represent two panes, so split
                            // keeps its own tab strip. In single-pane mode the
                            // top strip is the one tab UI (pane↔route synced).
                            if n_panes > 1 {
                                {render_tab_bar(pi, &pane, n_panes, focused_idx, focus_tab, close_tab, split, close_pane)}
                            }
                            if let Some(tab) = pane.tabs.get(pane.active).cloned() {
                                NoteView {
                                    key: "{pi}:{tab.path}",
                                    path: tab.path.clone(),
                                    sha: tab.sha.clone(),
                                    home,
                                    pane_index: pi,
                                    focused,
                                    pages: pages_memo,
                                    tag_rows,
                                    focus_tick,
                                    on_open,
                                    on_renamed,
                                }
                            } else {
                                div { class: "flex h-full items-center justify-center p-8",
                                    Text { variant: TextVariant::Muted,
                                        "Select a note from the tree to open it here."
                                    }
                                }
                            }
                        }
                    }
                }
                // ── Right sidebar (md+, focused note): Properties | Links ──
                if has_file && panel_open {
                    aside { class: "hidden w-72 shrink-0 flex-col overflow-y-auto border-l border-border bg-muted/30 md:flex",
                        // Tab header: Properties / Links + a Hide control.
                        div { class: "flex items-center gap-1 border-b border-border/60 px-2 py-1.5",
                            for (tab, label) in [
                                (RightTab::Properties, "Properties"),
                                (RightTab::Links, "Links"),
                                (RightTab::Share, "Share"),
                            ] {
                                button {
                                    key: "{label}",
                                    class: if right_tab() == tab {
                                        "rounded px-2 py-1 text-xs font-medium text-foreground bg-accent"
                                    } else {
                                        "rounded px-2 py-1 text-xs text-muted-foreground hover:text-foreground"
                                    },
                                    onclick: move |_| right_tab.set(tab),
                                    "{label}"
                                }
                            }
                            div { class: "ml-auto flex items-center gap-1.5",
                                if n_panes < MAX_PANES {
                                    button {
                                        class: "rounded px-1 text-sm text-muted-foreground hover:bg-accent hover:text-foreground",
                                        title: "Split right",
                                        onclick: move |_| split.call(()),
                                        "⇥"
                                    }
                                }
                                button {
                                    class: "text-xs text-muted-foreground hover:text-foreground",
                                    onclick: move |_| {
                                        let mut o = shell_right;
                                        o.set(crate::chrome::RightPanelOpen(false));
                                    },
                                    "Hide"
                                }
                            }
                        }
                        if right_tab() == RightTab::Properties {
                            crate::pages::note_properties::NoteProperties {}
                        } else if right_tab() == RightTab::Share {
                            crate::pages::share_panel::SharePanel { slug: home(), path: selected() }
                        } else {
                            {backlinks_body.clone()}
                        }
                    }
                }
            }
        }
        // ── Mobile chrome ─────────────────────────────────
        MobileActionBar {
            button {
                r#type: "button",
                class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg border border-border px-3 py-2 text-sm font-medium text-foreground active:bg-accent",
                onclick: move |_| files_open.set(true),
                Folder { size: 16 }
                "Files"
            }
            button {
                r#type: "button",
                class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg bg-primary px-3 py-2 text-sm font-medium text-primary-foreground active:bg-primary/85 disabled:opacity-50",
                disabled: !has_file,
                onclick: move |_| {
                    if let Some(cb) = status_info.peek().as_ref().and_then(|d| d.on_save) {
                        cb.call(());
                    }
                },
                if status_info.read().as_ref().is_some_and(|d| d.dirty) { "Save •" } else { "Save" }
            }
            button {
                r#type: "button",
                class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg border border-border px-3 py-2 text-sm font-medium text-foreground active:bg-accent disabled:opacity-50",
                disabled: !has_file,
                onclick: move |_| {
                    let mut o = shell_right;
                    let cur = o.peek().0;
                    o.set(crate::chrome::RightPanelOpen(!cur));
                },
                "Backlinks"
            }
        }
        BottomSheet {
            open: files_open(),
            on_close: move |_| files_open.set(false),
            title: "Vault",
            {tree_content}
        }
        BottomSheet {
            open: has_file && panel_open,
            on_close: move |_| {
                let mut o = shell_right;
                o.set(crate::chrome::RightPanelOpen(false));
            },
            title: match right_tab() {
                RightTab::Properties => "Properties",
                RightTab::Links => "Links",
                RightTab::Share => "Share",
            },
            div { class: "flex items-center gap-1 border-b border-border/60 px-2 py-1.5",
                for (tab, label) in [
                                (RightTab::Properties, "Properties"),
                                (RightTab::Links, "Links"),
                                (RightTab::Share, "Share"),
                            ] {
                    button {
                        key: "{label}",
                        class: if right_tab() == tab {
                            "rounded px-2 py-1 text-xs font-medium text-foreground bg-accent"
                        } else {
                            "rounded px-2 py-1 text-xs text-muted-foreground hover:text-foreground"
                        },
                        onclick: move |_| right_tab.set(tab),
                        "{label}"
                    }
                }
            }
            if right_tab() == RightTab::Properties {
                crate::pages::note_properties::NoteProperties {}
            } else if right_tab() == RightTab::Share {
                crate::pages::share_panel::SharePanel { slug: home(), path: selected() }
            } else {
                {backlinks_body}
            }
        }
        document::Link { rel: "stylesheet", href: editor::EDITOR_STYLE }
        document::Style { {crate::collab::COLLAB_STYLE} }
    }
}

/// One pane's tab strip: a button per open tab (active = primary
/// underline; a dimmer underline marks the active tab of an
/// *unfocused* pane), each with a close ✕, plus split / close-pane
/// controls docked at the right edge.
#[allow(clippy::too_many_arguments)]
fn render_tab_bar(
    pi: usize,
    pane: &Pane,
    n_panes: usize,
    focused: usize,
    focus_tab: Callback<(usize, usize)>,
    close_tab: Callback<(usize, usize)>,
    split: Callback<()>,
    close_pane: Callback<usize>,
) -> Element {
    let is_focused_pane = focused == pi;
    rsx! {
        div { class: "flex shrink-0 items-center gap-0.5 overflow-x-auto border-b border-border/60 bg-muted/20 px-1",
            for (idx, tab) in pane.tabs.iter().cloned().enumerate() {
                {
                    let is_active = idx == pane.active;
                    let title = basename_of(&tab.path).to_owned();
                    let cls = if is_active && is_focused_pane {
                        "flex items-center gap-1 border-b-2 border-primary px-2 py-1.5 text-xs font-medium text-foreground"
                    } else if is_active {
                        "flex items-center gap-1 border-b-2 border-border px-2 py-1.5 text-xs font-medium text-foreground"
                    } else {
                        "flex items-center gap-1 border-b-2 border-transparent px-2 py-1.5 text-xs text-muted-foreground hover:text-foreground"
                    };
                    rsx! {
                        div { key: "{tab.path}", class: cls,
                            button {
                                class: "min-w-0 max-w-[12rem] truncate text-left",
                                title: "{tab.path}",
                                onclick: move |_| focus_tab.call((pi, idx)),
                                "{title}"
                            }
                            button {
                                class: "shrink-0 rounded px-1 text-muted-foreground hover:bg-accent hover:text-foreground",
                                title: "Close tab",
                                onclick: move |_| close_tab.call((pi, idx)),
                                "×"
                            }
                        }
                    }
                }
            }
            div { class: "ml-auto flex shrink-0 items-center gap-0.5 pl-1",
                if n_panes < MAX_PANES {
                    button {
                        class: "rounded px-1.5 py-1 text-xs text-muted-foreground hover:bg-accent hover:text-foreground",
                        title: "Split right",
                        onclick: move |_| split.call(()),
                        "⇥"
                    }
                }
                if n_panes > 1 {
                    button {
                        class: "rounded px-1.5 py-1 text-xs text-muted-foreground hover:bg-accent hover:text-foreground",
                        title: "Close pane",
                        onclick: move |_| close_pane.call(pi),
                        "⊟"
                    }
                }
            }
        }
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
    selected: Memo<Option<String>>,
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
        div { key: "{node.meta.path}",
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
pub(crate) fn build_tree(pages: &[PageMeta]) -> (Vec<TreeNode>, Vec<usize>) {
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

/// Filename without dirs/extension — display fallback for paths
/// missing from the folder index. Also the note title shown +
/// edited by the [`note_header`](crate::pages::note_header) H1.
pub(crate) fn basename_of(path: &str) -> &str {
    let file = path.rsplit('/').next().unwrap_or(path);
    file.strip_suffix(".md").unwrap_or(file)
}

/// Media slug for a `type: song` note. Prefers a `song_slug:` (or `slug:`)
/// key in the leading YAML frontmatter block; otherwise slugifies the note
/// basename. The slug selects `/media/songs/{slug}/…` (served same-origin).
pub(crate) fn song_slug_from(text: &str, basename: &str) -> String {
    if let Some(v) = frontmatter_value(text, "song_slug").or_else(|| frontmatter_value(text, "slug"))
    {
        let v = v.trim().trim_matches(['"', '\'']).trim();
        if !v.is_empty() {
            return v.to_owned();
        }
    }
    slugify(basename)
}

/// Ordered media slugs for a `type: setlist` note, parsed from the `songs:`
/// YAML list in the leading frontmatter. Accepts both the block form
///
/// ```yaml
/// songs:
///   - song-a
///   - song-b
/// ```
///
/// and the inline flow form `songs: [song-a, song-b]`. Each entry is trimmed
/// of quotes/whitespace; blanks are dropped.
pub(crate) fn setlist_songs_from(text: &str) -> Vec<String> {
    let Some(rest) = text.strip_prefix("---") else {
        return Vec::new();
    };
    let Some((front, _)) = rest.split_once("\n---") else {
        return Vec::new();
    };
    let clean = |s: &str| s.trim().trim_matches(['"', '\'']).trim().to_owned();

    let mut lines = front.lines();
    let mut out = Vec::new();
    while let Some(line) = lines.next() {
        let trimmed = line.trim_start();
        let Some(after) = trimmed.strip_prefix("songs:") else {
            continue;
        };
        let after = after.trim();
        // Inline flow list: songs: [a, b, c]
        if let Some(inner) = after.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            out.extend(inner.split(',').map(clean).filter(|s| !s.is_empty()));
            return out;
        }
        // Block list: subsequent `- item` lines.
        for l in lines.by_ref() {
            let t = l.trim_start();
            if let Some(item) = t.strip_prefix("- ").or_else(|| t.strip_prefix('-')) {
                let v = clean(item);
                if !v.is_empty() {
                    out.push(v);
                }
            } else if t.is_empty() {
                continue;
            } else {
                break; // next frontmatter key ends the list
            }
        }
        return out;
    }
    out
}

// ── song-note frontmatter (stems as attachments) ────────────────────────────

/// One stem parsed from a song note's frontmatter `stems:` block. The
/// audio lives in the org's content-addressed blob store; `content_hash`
/// is resolved to a signed `/blobs/download` URL at play time.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrontStem {
    pub name: String,
    pub group: Option<String>,
    pub default_muted: bool,
    pub content_hash: String,
}

/// One section parsed from the frontmatter `sections:` block (song-local
/// seconds, 0-based).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FrontSection {
    pub name: String,
    pub start_sec: f64,
    pub end_sec: f64,
}

/// Song metadata + stems parsed from a `type: song` note's frontmatter.
/// When `stems` is non-empty the player streams from the attachment
/// blob store instead of `/media/songs/{slug}/…`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SongFront {
    pub artist: Option<String>,
    pub key: Option<String>,
    pub bpm: Option<f64>,
    pub time_signature: Option<String>,
    pub duration_sec: Option<f64>,
    pub sections: Vec<FrontSection>,
    pub stems: Vec<FrontStem>,
}

/// Parse the song frontmatter (scalars + the `stems:` / `sections:`
/// block lists) from a note's text.
pub(crate) fn song_front_from(text: &str) -> SongFront {
    let fv = |k: &str| {
        frontmatter_value(text, k)
            .map(|v| v.trim().trim_matches(['"', '\'']).trim().to_owned())
            .filter(|v| !v.is_empty())
    };
    let num = |k: &str| fv(k).and_then(|v| v.parse::<f64>().ok());

    let stems = front_block_maps(text, "stems")
        .into_iter()
        .filter_map(|pairs| {
            let get = |k: &str| {
                pairs
                    .iter()
                    .find(|(pk, _)| pk == k)
                    .map(|(_, v)| v.clone())
                    .filter(|v| !v.is_empty())
            };
            let hash = get("content_hash")?;
            Some(FrontStem {
                name: get("name")?,
                group: get("group"),
                default_muted: get("default_muted").is_some_and(|v| v == "true"),
                content_hash: hash,
            })
        })
        .collect();

    let sections = front_block_maps(text, "sections")
        .into_iter()
        .filter_map(|pairs| {
            let get = |k: &str| {
                pairs
                    .iter()
                    .find(|(pk, _)| pk == k)
                    .map(|(_, v)| v.clone())
            };
            Some(FrontSection {
                name: get("name")?,
                start_sec: get("start_sec")?.parse().ok()?,
                end_sec: get("end_sec")?.parse().ok()?,
            })
        })
        .collect();

    SongFront {
        artist: fv("artist"),
        key: fv("key"),
        bpm: num("bpm"),
        time_signature: fv("time_signature"),
        duration_sec: num("duration_sec"),
        sections,
        stems,
    }
}

/// Parse a frontmatter block list of maps under `key:`:
///
/// ```yaml
/// key:
///   - name: Click
///     group: Guide
/// ```
///
/// Each `-` starts a new entry; indented `k: v` lines extend the current
/// one. Values are trimmed of quotes/whitespace. Stops at the next
/// top-level (unindented) key.
fn front_block_maps(text: &str, key: &str) -> Vec<Vec<(String, String)>> {
    let Some(rest) = text.strip_prefix("---") else {
        return Vec::new();
    };
    let Some((front, _)) = rest.split_once("\n---") else {
        return Vec::new();
    };
    let clean = |s: &str| s.trim().trim_matches(['"', '\'']).trim().to_owned();
    let lines: Vec<&str> = front.lines().collect();

    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];
        i += 1;
        // The opening `key:` line, top-level (unindented) with no value.
        let is_open = line
            .strip_prefix(key)
            .and_then(|r| r.strip_prefix(':'))
            .is_some_and(|r| r.trim().is_empty());
        if !is_open {
            continue;
        }
        let mut out = Vec::new();
        let mut cur: Vec<(String, String)> = Vec::new();
        while i < lines.len() {
            let raw = lines[i];
            let t = raw.trim_start();
            if t.is_empty() {
                i += 1;
                continue;
            }
            if !raw.starts_with(' ') && !raw.starts_with('\t') {
                break; // next top-level key
            }
            if let Some(after_dash) = t.strip_prefix('-') {
                if !cur.is_empty() {
                    out.push(std::mem::take(&mut cur));
                }
                if let Some((k, v)) = after_dash.trim_start().split_once(':') {
                    cur.push((k.trim().to_owned(), clean(v)));
                }
            } else if let Some((k, v)) = t.split_once(':') {
                cur.push((k.trim().to_owned(), clean(v)));
            }
            i += 1;
        }
        if !cur.is_empty() {
            out.push(cur);
        }
        return out;
    }
    Vec::new()
}

/// Read a scalar `key: value` from the note's leading `---` frontmatter block.
pub(crate) fn frontmatter_value(text: &str, key: &str) -> Option<String> {
    let rest = text.strip_prefix("---")?;
    let (front, _) = rest.split_once("\n---")?;
    for line in front.lines() {
        if let Some((k, v)) = line.split_once(':') {
            if k.trim() == key {
                return Some(v.to_owned());
            }
        }
    }
    None
}

/// Lowercase, spaces/underscores → hyphens, drop other punctuation.
fn slugify(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_dash = false;
    for c in s.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c.to_ascii_lowercase());
            prev_dash = false;
        } else if c == ' ' || c == '_' || c == '-' {
            if !prev_dash && !out.is_empty() {
                out.push('-');
                prev_dash = true;
            }
        }
    }
    while out.ends_with('-') {
        out.pop();
    }
    out
}

/// Starter scaffold for a freshly-created note: an empty-but-present
/// frontmatter block so the Properties panel has something to show
/// (a `created` date + empty `tags`/`aliases` sequences). No `title`
/// key — the note's title IS its filename (see `note_header`).
pub(crate) fn seed_note_bytes() -> Vec<u8> {
    let today = chrono::Local::now().date_naive();
    format!("---\ncreated: {today}\ntags: []\naliases: []\n---\n\n").into_bytes()
}

/// OSIS verse id → a human reference the scripture service parses
/// (`John.3.16` → `John 3:16`; a range keeps its start). Best-effort.
fn osis_to_ref(osis: &str) -> String {
    let first = osis.split('-').next().unwrap_or(osis);
    let mut it = first.rsplitn(3, '.');
    match (it.next(), it.next(), it.next()) {
        (Some(v), Some(c), Some(b)) => format!("{b} {c}:{v}"),
        _ => first.to_string(),
    }
}

/// Frontmatter-derived page index for the folder tree.
pub(crate) async fn fetch_folder_index(slug: String) -> Result<Vec<PageMeta>, String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
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
                // Notes AND base views — a `.base` is a first-class
                // vault citizen (plans/vault-views.md): it appears in
                // the tree, deep-links, and renders its view in place.
                std::path::Path::new(&p.path)
                    .extension()
                    .is_some_and(|ext| {
                        ext.eq_ignore_ascii_case("md") || ext.eq_ignore_ascii_case("base")
                    })
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

/// The focused note + its 1-hop neighbourhood as a [`WikiGraph`], built
/// client-side from what the right panel already fetched: backlink
/// sources point AT `current`, resolved outgoing wikilinks point FROM
/// it. Node ids are vault-relative paths, so a node click maps
/// straight back onto the panel's `on_open` flow; labels come from the
/// folder-index title lookup (basename fallback). Unresolved links are
/// skipped — they have no note to open. Duplicate connections (a page
/// that both links here and is linked from here) collapse to one edge.
fn build_local_graph(
    current: &str,
    backlinks: &[String],
    outlinks: &[vault_proto::GraphLink],
    titles: &HashMap<String, (String, String)>,
) -> WikiGraph {
    // Edge list, deduped as unordered pairs (self-links dropped).
    let mut seen: HashSet<(String, String)> = HashSet::new();
    let mut edges: Vec<(String, String)> = Vec::new();
    let mut push = |source: String, target: String| {
        if source == target {
            return;
        }
        let key = if source < target {
            (source.clone(), target.clone())
        } else {
            (target.clone(), source.clone())
        };
        if seen.insert(key) {
            edges.push((source, target));
        }
    };
    for b in backlinks {
        push(b.clone(), current.to_owned());
    }
    for l in outlinks {
        if let Some(t) = &l.resolved {
            push(current.to_owned(), t.clone());
        }
    }

    // Node set = focal + everything an edge touches; link_count is
    // the in-graph degree (sizes the focal node as the hub).
    let mut degree: HashMap<&str, u32> = HashMap::new();
    for (s, t) in &edges {
        *degree.entry(s.as_str()).or_default() += 1;
        *degree.entry(t.as_str()).or_default() += 1;
    }
    let mut paths: Vec<&str> = std::iter::once(current)
        .chain(edges.iter().flat_map(|(s, t)| [s.as_str(), t.as_str()]))
        .collect();
    paths.sort_unstable();
    paths.dedup();
    let nodes = paths
        .into_iter()
        .map(|p| GraphNode {
            id: p.to_owned(),
            label: titles
                .get(p)
                .map(|(title, _)| title.clone())
                .unwrap_or_else(|| basename_of(p).to_owned()),
            kind: "other".to_owned(),
            path: p.to_owned(),
            link_count: degree.get(p).copied().unwrap_or(0),
            community: 0,
        })
        .collect();
    let edges = edges
        .into_iter()
        .map(|(s, t)| GraphEdge::wikilink(s, t, 1.0))
        .collect();
    WikiGraph {
        nodes,
        edges,
        communities: Vec::new(),
    }
}

/// Outgoing wikilinks of `path`, via the `VaultGraph` RPC.
async fn fetch_links(slug: String, path: String) -> Result<Vec<vault_proto::GraphLink>, String> {
    let client = crate::vox_clients::vault_graph_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        client
            .links(VAULT_ID.to_owned(), path)
            .await
            .map_err(|e| format!("links: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

/// Pages linking to `path`, via the `VaultGraph` RPC.
async fn fetch_backlinks(slug: String, path: String) -> Result<Vec<String>, String> {
    let client = crate::vox_clients::vault_graph_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        client
            .backlinks(VAULT_ID.to_owned(), path)
            .await
            .map_err(|e| format!("backlinks: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

/// Re-file a note: set its `folder` to `parent` (None = root)
/// via the server-side frontmatter splice. `prev_sha` empty →
/// unconditional. Returns the freshly committed sha.
async fn move_to_folder(
    slug: String,
    path: String,
    parent: Option<String>,
    prev_sha: String,
) -> Result<String, String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let if_match = if prev_sha.is_empty() {
            IfMatch::Force
        } else {
            IfMatch::Sha(prev_sha)
        };
        let ack = client
            .set_folder(VAULT_ID.to_owned(), path, parent, if_match)
            .await
            .map_err(|e| format!("set_folder: {e:?}"))?;
        Ok(ack.sha256)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path, parent, prev_sha);
        Err("native client not wired yet".to_owned())
    }
}

/// Create a new note (create-only), seeded with the starter
/// frontmatter scaffold ([`seed_note_bytes`]) so the Properties
/// panel opens with `created`/`tags`/`aliases` already present.
/// Returns its sha.
pub(crate) async fn create_new_file(slug: String, path: String) -> Result<String, String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let ack = client
            .put_file(VAULT_ID.to_owned(), path, seed_note_bytes(), IfMatch::CreateOnly)
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

#[cfg(test)]
mod song_front_tests {
    use super::song_front_from;

    const NOTE: &str = "---\ntype: song\nartist: Elevation Worship\nkey: B\nbpm: 128\ntime_signature: \"4/4\"\nduration_sec: 372.5\nsections:\n  - name: Intro\n    start_sec: 0\n    end_sec: 15.2\n  - name: Verse 1\n    start_sec: 15.2\n    end_sec: 45\nstems:\n  - name: Click\n    group: Guide\n    default_muted: true\n    content_hash: aaa111\n  - name: \"Electric Guitar 1\"\n    group: Guitars\n    content_hash: bbb222\n---\n# Praise\nbody text\n";

    #[test]
    fn parses_full_song_front() {
        let f = song_front_from(NOTE);
        assert_eq!(f.artist.as_deref(), Some("Elevation Worship"));
        assert_eq!(f.key.as_deref(), Some("B"));
        assert_eq!(f.bpm, Some(128.0));
        assert_eq!(f.time_signature.as_deref(), Some("4/4"));
        assert_eq!(f.duration_sec, Some(372.5));
        assert_eq!(f.sections.len(), 2);
        assert_eq!(f.sections[1].name, "Verse 1");
        assert_eq!(f.sections[1].start_sec, 15.2);
        assert_eq!(f.stems.len(), 2);
        assert_eq!(f.stems[0].name, "Click");
        assert_eq!(f.stems[0].group.as_deref(), Some("Guide"));
        assert!(f.stems[0].default_muted);
        assert_eq!(f.stems[0].content_hash, "aaa111");
        assert_eq!(f.stems[1].name, "Electric Guitar 1");
        assert!(!f.stems[1].default_muted);
    }

    #[test]
    fn missing_front_is_empty() {
        let f = song_front_from("# Just a note\n");
        assert!(f.stems.is_empty());
        assert!(f.sections.is_empty());
        assert_eq!(f.bpm, None);
    }

    #[test]
    fn stem_without_hash_is_dropped() {
        let text = "---\nstems:\n  - name: Click\n  - name: Bass\n    content_hash: ccc\n---\n";
        let f = song_front_from(text);
        assert_eq!(f.stems.len(), 1);
        assert_eq!(f.stems[0].name, "Bass");
    }
}

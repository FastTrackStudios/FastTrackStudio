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
//! The open/save/conflict lifecycle lives in
//! [`DocumentSession`](crate::document_session::DocumentSession):
//! typed conflicts, a debounced autosave, explicit save (Ctrl+S /
//! toolbar), force-save (the conflict banner's *Overwrite*), and
//! reload — the page renders from its typed state instead of a
//! hand-rolled signal cluster.
//!
//! Wikilinks + embeds resolve through the client-side
//! [`ClientVaultIndex`](crate::vault_lookup::ClientVaultIndex)
//! (folder-index metadata + lazy `get_file` content LRU), passed
//! to the editor as a stateful `DecorationSource`; `[[` and `#`
//! autocomplete ride the editor's trigger `CompletionSource`
//! (basenames + aliases from the folder index, tags from the
//! `VaultGraph` RPC). A right-side **backlinks panel** lists
//! pages linking to the open note via the same RPC and refreshes
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
use editor::Editor;
use editor::editor_view::slash::{SlashMenu, SlashState};
use editor::editor_vim::VimState;
use fts_ui::lucide_dioxus::{ChevronRight, FileText, Folder};
use fts_ui::prelude::*;
use vault_proto::{PageMeta, TagCount};

use crate::document_session::{SaveStatus, use_document_session};
use crate::vault_lookup::{self, ClientVaultIndex};

#[cfg(target_arch = "wasm32")]
use crate::document_session::VAULT_ID;

/// Minimal payload to open a file: its path + last-known sha.
#[derive(Clone, PartialEq)]
struct FileMeta {
    path: String,
    sha256: String,
}

/// One node of the virtual-folder tree.
#[derive(Clone, PartialEq)]
struct TreeNode {
    meta: PageMeta,
    children: Vec<usize>,
    is_folder: bool,
}

#[component]
pub fn VaultView(#[props(default)] initial_path: String) -> Element {
    // The vault lives in the home org; resolve its slug from the
    // discovered org list (re-runs when discovery lands).
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let home = use_memo(move || crate::orgs::home_slug(&org_list.read()));
    let mut files = use_resource(move || {
        let slug = home();
        async move { fetch_folder_index(slug).await }
    });

    // Open file + its editing state — the whole
    // open/save/autosave/conflict lifecycle in one handle. Provided
    // as context for the keyed `CollabSession` child.
    let session = use_document_session(home);
    use_context_provider(|| session);
    let selected = use_memo(move || session.current_path());

    // Deep-link support: `/vault?path=<vault-relative path>` opens
    // that note once the folder index lands (e.g. a knowledge-graph
    // node click). One-shot — after the first open the user owns the
    // selection again.
    let mut deep_link_done = use_signal(|| false);
    {
        let want = initial_path.clone();
        use_effect(move || {
            if want.is_empty() || *deep_link_done.peek() {
                return;
            }
            if let Some(Ok(pages)) = &*files.read() {
                // Exact vault-relative path first; fall back to the
                // basename so graph node ids (file stems) resolve too.
                let hit = pages.iter().find(|p| p.path == want).or_else(|| {
                    pages
                        .iter()
                        .find(|p| basename_of(&p.path) == basename_of(&want))
                });
                if let Some(p) = hit {
                    deep_link_done.set(true);
                    session.open(p.path.clone(), p.sha256.clone());
                }
            }
        });
    }

    let mut new_name = use_signal(String::new);
    // Failures from tree operations (move / create) outlive their
    // buttons via the app-wide notification queue.
    let notify = architect::try_use_notifications();

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

    // Cross-file lookup for the decoration pass: rebuild the
    // client index whenever the folder index (or org) changes.
    // The decoration source below captures the *signal*, so the
    // swap doesn't rebuild the source (Rc identity = the editor's
    // prop-diff contract).
    let mut lookup = use_signal(|| None::<Rc<ClientVaultIndex>>);
    // Lazy-fetch worker for embeds/previews — page-owned, so the
    // fetch's awaits and editor pokes never touch signals from the
    // root scope (the suite's console gate treats that as fatal).
    let fetcher = vault_lookup::use_vault_fetch_worker(home, lookup);
    use_effect(move || {
        let pages = match &*files.read() {
            Some(Ok(pages)) => pages.clone(),
            _ => Vec::new(),
        };
        lookup.set(Some(ClientVaultIndex::new(&pages, session.state, fetcher)));
    });

    // Autocomplete candidates. `[[` completes basenames + aliases
    // straight off the folder index; `#` completes vault tags
    // pulled once per org (and re-pulled after each save, since
    // saves can mint tags).
    let link_candidates = use_memo(move || match &*files.read() {
        Some(Ok(pages)) => vault_lookup::wikilink_candidates(pages),
        _ => Vec::new(),
    });
    let mut tag_rows = use_signal(Vec::<TagCount>::new);
    use_effect(move || {
        let slug = home();
        let _refresh = session.save_count();
        spawn(async move {
            if let Ok(tags) = vault_lookup::tag_candidates(slug).await {
                tag_rows.set(tags);
            }
        });
    });

    // ── Per-file CRDT collaboration ───────────────────────────
    // When a file opens, register it via `open_collab` and mount a
    // keyed `CollabSession` (synced replica + presence cursors).
    // While the session is live the server write-behind owns
    // persistence and the sha autosave pauses; if the sync session
    // drops, tear down and fall back to sha saves (a fresh replica
    // is opened on the next file open — never a stale outbox).
    //
    // OWNERSHIP: `handles` (and every signal inside it) is created
    // HERE, at the page scope, via `use_collab_handles`. The keyed
    // `CollabSession` child only *drives* the slots. The Editor's
    // decoration source + on_transaction sink capture `handles`
    // through `collab`; because the page scope outlives the Editor,
    // a session remount (file switch, reconnect-generation re-key,
    // Live→Offline teardown) can never leave the Editor's keydown
    // path holding dropped signals — the bug that used to kill all
    // input (backspace, vim) after the first re-key.
    let handles = crate::collab::use_collab_handles();
    let mut collab = use_signal(|| None::<crate::collab::CollabHandles>);
    let mut collab_doc = use_signal(|| None::<uuid::Uuid>);
    let account = try_use_context::<Signal<Option<crate::auth::ActiveAccount>>>();
    let conn = architect::use_connection::<vox_core::Caller>();
    use_effect(move || {
        let path = selected();
        // Reactive read: re-run on every (re-)establish of the shared
        // org connection. After an outage the Live→Offline teardown
        // below clears the session; the generation bump is what re-opens
        // collab for the still-open file once the socket is back — no
        // refresh, fresh replica, delta resync by version vector.
        let _generation = conn.generation();
        collab_doc.set(None);
        collab.set(None);
        handles.reset();
        let Some(path) = path else { return };
        let slug = home.peek().clone();
        spawn(async move {
            match crate::collab::open_collab(slug, path.clone()).await {
                Ok(ack) => {
                    // Only arm if this file is still the open one.
                    if session.current_path().as_deref() == Some(path.as_str()) {
                        collab_doc.set(Some(ack.doc_id));
                        collab.set(Some(handles));
                    }
                }
                Err(e) => {
                    // No collab (older server / native shell) — the
                    // page simply stays in plain sha mode.
                    tracing::debug!("vault collab unavailable for {path}: {e}");
                }
            }
        });
    });
    // Autosave pauses exactly while collab is live (the server
    // write-behind owns persistence then).
    use_effect(move || {
        let live = collab.read().as_ref().is_some_and(|c| c.is_live());
        session.set_autosave_paused(live);
    });
    // Live → Offline teardown: unmount the session so offline edits
    // go back through sha saves instead of a buffered CRDT outbox
    // (re-syncing that outbox AND sha-saving the same edits would
    // double-apply them server-side).
    use_effect(move || {
        let went_offline = collab
            .read()
            .as_ref()
            .is_some_and(|c| (c.live)() && c.doc.status() == crdt::SyncStatus::Offline);
        if went_offline {
            collab_doc.set(None);
            collab.set(None);
            handles.reset();
        }
    });
    let collab_status = use_memo(move || {
        collab.read().as_ref().map(|c| {
            if c.is_live() {
                "Collab: live"
            } else {
                "Collab: connecting…"
            }
        })
    });
    // Browser-conformance hook (tests/multiplayer): mirror the exact
    // editor buffer + collab state into `window.__taskVault`. The
    // decorated DOM can't be scraped back into doc text — hidden
    // live-preview replacements render *nothing* (see editor-view's
    // `render_dx.rs` Widget arm), so DOM reconstruction would drop
    // those bytes. Cost is one string clone per buffer change.
    #[cfg(target_arch = "wasm32")]
    use_effect(move || {
        let text = session.state.read().doc.to_string();
        // Reading `revision()` subscribes this mirror to remote
        // imports, so `replica` stays current; replica-vs-text is
        // exactly the split the conformance suites need to localize
        // a sync stall (transport vs editor-apply bridge).
        let (live, status, rev, replica) = match &*collab.read() {
            Some(c) => (
                c.is_live(),
                format!("{:?}", c.doc.status()),
                c.doc.revision(),
                c.doc
                    .doc()
                    .map(|d| {
                        d.loro()
                            .get_text(vault_proto::COLLAB_TEXT_CONTAINER)
                            .to_string()
                    })
                    .unwrap_or_default(),
            ),
            None => (false, "none".to_owned(), 0, String::new()),
        };
        let path = selected.read().clone().unwrap_or_default();
        let Some(win) = web_sys::window() else { return };
        let obj = js_sys::Object::new();
        let set = |k: &str, v: wasm_bindgen::JsValue| {
            let _ = js_sys::Reflect::set(&obj, &wasm_bindgen::JsValue::from_str(k), &v);
        };
        set("text", wasm_bindgen::JsValue::from_str(&text));
        set("live", wasm_bindgen::JsValue::from_bool(live));
        set("status", wasm_bindgen::JsValue::from_str(&status));
        set("rev", wasm_bindgen::JsValue::from_f64(rev as f64));
        set("replica", wasm_bindgen::JsValue::from_str(&replica));
        set("path", wasm_bindgen::JsValue::from_str(&path));
        let _ = js_sys::Reflect::set(&win, &wasm_bindgen::JsValue::from_str("__taskVault"), &obj);
    });
    // Editor → replica bridge + presence cursor publish.
    let on_transaction = use_callback(move |event: editor::TransactionEvent| {
        let Some(c) = *collab.peek() else { return };
        let who = account
            .and_then(|a| a.peek().as_ref().map(|acct| acct.name.clone()))
            .unwrap_or_else(|| "anonymous".to_owned());
        crate::collab::on_editor_transaction(&c, &session, &event, &who);
    });

    // Editor sources — created once, capturing the signals above.
    // Decorations = the vault pass + remote presence cursors.
    let decorations = use_hook(|| crate::collab::collab_decoration_source(lookup, collab));
    let completion = use_hook(|| vault_lookup::vault_completion_source(link_candidates, tag_rows));

    // Open a note through the session (fetch + seed + sha
    // bookkeeping).
    let on_open = use_callback(move |meta: FileMeta| {
        session.open(meta.path, meta.sha256);
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

    // Create a new empty note. If a folder was chosen (via a
    // folder row's "+"), file it there right after creating, then
    // open through the session — the open re-fetches, so the
    // buffer reflects the server-spliced `folder:` frontmatter.
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
                        match move_to_folder(home(), name.clone(), Some(parent), created_sha).await
                        {
                            Ok(new_sha) => open_sha = new_sha,
                            Err(e) => {
                                if let Some(n) = notify {
                                    n.error(format!("Created, but filing failed: {e}"));
                                }
                            }
                        }
                    }
                    session.open(name, open_sha);
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

    // path → (title, sha) for the backlinks panel rows.
    let page_lookup = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => pages
            .iter()
            .map(|p| (p.path.clone(), (p.title.clone(), p.sha256.clone())))
            .collect::<HashMap<String, (String, String)>>(),
        _ => HashMap::new(),
    });

    // Backlinks for the open note, re-pulled when the selection
    // changes and after every committed save.
    let backlinks_open = use_signal(|| true);
    let backlinks = use_resource(move || {
        let slug = home();
        let path = selected();
        let _refresh = session.save_count();
        async move {
            match path {
                Some(p) => fetch_backlinks(slug, p).await,
                None => Ok(Vec::new()),
            }
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
    let is_dirty = session.dirty();
    let status_msg = match session.status() {
        SaveStatus::Idle => String::new(),
        SaveStatus::Saving => "Saving…".to_owned(),
        SaveStatus::Saved => "Saved".to_owned(),
        SaveStatus::Failed(msg) => msg,
    };
    let conflict_open = session.conflict().is_some();
    let moving = move_target.read().clone();
    let create_under = create_parent.read().clone();
    let panel_open = *backlinks_open.read();

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
                        session.save();
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
                        if let Some(cs) = collab_status() {
                            Text { variant: TextVariant::Muted, class: "text-xs", "{cs}" }
                        }
                        if !status_msg.is_empty() {
                            Text { variant: TextVariant::Muted, class: "text-xs", "{status_msg}" }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            disabled: !has_file,
                            on_click: move |_| session.save(),
                            "Save"
                        }
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            disabled: !has_file,
                            on_click: move |_| {
                                let mut o = backlinks_open;
                                let cur = *o.peek();
                                o.set(!cur);
                            },
                            if panel_open { "Hide backlinks" } else { "Backlinks" }
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
                                on_click: move |_| session.reload_from_server(),
                                "Reload"
                            }
                            Button {
                                variant: ButtonVariant::Destructive,
                                size: ButtonSize::Small,
                                on_click: move |_| session.force_save(),
                                "Overwrite"
                            }
                        }
                    }
                }
                div { class: "flex min-h-0 flex-1",
                    div { class: "flex min-h-0 min-w-0 flex-1 flex-col overflow-y-auto",
                        if has_file {
                            div { class: "editor-app",
                                // --flush: no card chrome — the vault page is a
                                // full-page embed; the editor sits directly on
                                // the app background (Obsidian-style).
                                div { class: "editor-frame editor-frame--flush",
                                    Editor {
                                        state: session.state,
                                        keymap: keymap.read().clone(),
                                        decorations: decorations.clone(),
                                        vim: Some(vim),
                                        slash: Some(slash),
                                        completion: completion.clone(),
                                        on_transaction,
                                    }
                                    SlashMenu { state: session.state, slash }
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
                    // ── Backlinks panel ───────────────────
                    if has_file && panel_open {
                        aside { class: "flex w-72 shrink-0 flex-col overflow-y-auto border-l border-border bg-muted/30",
                            div { class: "flex items-center justify-between px-3 py-3",
                                Heading { level: HeadingLevel::H3, "Backlinks" }
                                button {
                                    class: "text-xs text-muted-foreground hover:text-foreground",
                                    onclick: move |_| {
                                        let mut o = backlinks_open;
                                        o.set(false);
                                    },
                                    "Hide"
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
                        }
                    }
                }
            }
        }
        // Keyed collab child: remount per doc id = fresh replica,
        // driven INTO the page-owned `handles` slots.
        if let Some(doc_id) = collab_doc() {
            crate::collab::CollabSession { key: "{doc_id}", doc_id, handles }
        }
        document::Link { rel: "stylesheet", href: editor::EDITOR_STYLE }
        document::Style { {crate::collab::COLLAB_STYLE} }
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

/// Filename without dirs/extension — display fallback for paths
/// missing from the folder index.
fn basename_of(path: &str) -> &str {
    let file = path.rsplit('/').next().unwrap_or(path);
    file.strip_suffix(".md").unwrap_or(file)
}

/// Frontmatter-derived page index for the folder tree.
async fn fetch_folder_index(slug: String) -> Result<Vec<PageMeta>, String> {
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

/// Create a new empty file (create-only). Returns its sha.
async fn create_new_file(slug: String, path: String) -> Result<String, String> {
    let client = crate::vox_clients::vault_client(&slug).await?;
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

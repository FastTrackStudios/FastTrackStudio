//! `NoteView` — one open vault note, rendered self-contained.
//!
//! Extracted from [`VaultView`](crate::pages::vault::VaultView) so the
//! vault page can mount it **once per tab / split-pane**, each with its
//! own [`DocumentSession`](crate::document_session::DocumentSession) and
//! its own per-file collab replica. It owns everything that used to be
//! per-open-file in the vault page:
//!
//! - the `DocumentSession` (open / autosave / conflict / save), provided
//!   as context for its `NoteHeader` + `CollabSession` children;
//! - the per-file CRDT collab lifecycle (page-owned handles + a keyed
//!   `CollabSession`) — **armed only while this pane is focused**, so a
//!   split never runs two live CRDT sessions at once (the unfocused
//!   pane falls back to the sha autosave, which still persists edits);
//! - the editor sources (vault decorations + presence cursors, the
//!   `[[`/`#` completion) and the `type:`-dispatch that renders a
//!   `type: song` / `type: setlist` player, a `type: video` watch view,
//!   a `.base` table, or the rich markdown editor;
//! - while focused, the shell status line (file · dirty · save · collab
//!   · vim) and the `window.__taskVault` conformance mirror.
//!
//! The instance is keyed by `pane:path` at the mount site, so a tab
//! switch remounts a fresh session (never a stale outbox) and the
//! path prop is stable for the life of one instance.

use std::rc::Rc;

use dioxus::prelude::*;
use editor::Editor;
use editor::editor_view::slash::{SlashMenu, SlashState};
use editor::editor_vim::VimState;
use fts_ui::prelude::*;
use vault_proto::{PageMeta, TagCount};

use crate::document_session::{SaveStatus, use_document_session};
use crate::pages::vault::{FileMeta, basename_of, setlist_songs_from, song_slug_from};
use crate::vault_lookup::{self, ClientVaultIndex};

/// One open note. Mount keyed by `"{pane}:{path}"`.
#[allow(clippy::too_many_arguments)]
#[component]
pub(crate) fn NoteView(
    /// Vault-relative path of the note this view owns (stable for the
    /// instance — a different note remounts via the mount-site key).
    path: String,
    /// Last-known server sha (the conditional-write base for the open).
    sha: String,
    /// Home-org slug the vault lives under.
    home: Memo<String>,
    /// Which pane this view lives in, and the focused-pane signal — the
    /// two combine into a reactive `is_focused` that gates collab, the
    /// status line, and the conformance mirror to the active pane.
    pane_index: usize,
    focused: ReadSignal<usize>,
    /// Folder-index pages (wikilink candidates + cross-file lookup +
    /// the `type:` dispatch).
    pages: ReadSignal<Vec<PageMeta>>,
    /// Vault tags for `#` completion (fetched once per org by the page).
    tag_rows: Signal<Vec<TagCount>>,
    /// Bumped by the *focused* view after each committed save so the
    /// page's backlinks/links/graph panel refreshes.
    focus_tick: Signal<u64>,
    /// Open a note (wikilink / `.base` row / graph node) — routes to the
    /// page's "open-or-focus in the focused pane" flow.
    on_open: Callback<FileMeta>,
    /// Refresh the folder index after a rename commits.
    on_renamed: Callback<()>,
) -> Element {
    let is_focused = use_memo(move || *focused.read() == pane_index);

    // ── Session ───────────────────────────────────────────────
    let session = use_document_session(home);
    use_context_provider(|| session);
    // Open exactly once — this instance is keyed by pane:path, so the
    // props never change under it (a tab switch remounts). No signal
    // reads → the effect fires a single time after mount.
    {
        let open_path = path.clone();
        let open_sha = sha.clone();
        use_effect(move || session.open(open_path.clone(), open_sha.clone()));
    }

    // ── Editor extensions ─────────────────────────────────────
    let keymap = use_signal(editor::standard_markdown_keymap);
    // Frontmatter properties now live in the right sidebar (Properties
    // tab), so the editor's inline `.md-properties` widget stays
    // collapsed — the class below is always `props-collapsed`.
    let props_open = use_signal(|| false);
    // Setlist notes open straight into the full-screen Experience; `Esc`
    // (or the close control) drops back to the embedded view.
    let mut setlist_fullscreen =
        use_signal(|| crate::pages::experience::ExperienceKind::Setlist.auto_fullscreen());
    let vim = use_signal(VimState::new);
    // Vim is a physical-keyboard idiom — decide once at mount.
    let vim = (!use_hook(editor::editor_view::coarse_pointer)).then_some(vim);
    let slash = use_signal(|| None::<SlashState>);

    // ── Cross-file lookup + lazy fetch worker ─────────────────
    let mut lookup = use_signal(|| None::<Rc<ClientVaultIndex>>);
    let fetcher = vault_lookup::use_vault_fetch_worker(home, lookup);
    use_effect(move || {
        let list = pages.read().clone();
        lookup.set(Some(ClientVaultIndex::new(&list, session.state, fetcher)));
    });

    // ── Autocomplete candidates ───────────────────────────────
    let link_candidates = use_memo(move || vault_lookup::wikilink_candidates(&pages.read()));

    // ── Per-file CRDT collaboration (focused pane only) ───────
    // Same page-owned-handles + keyed-`CollabSession` split as the
    // original vault page (see collab.rs module docs). Armed only while
    // this pane is focused: a 2-way split never runs two live CRDT
    // sessions — the unfocused pane's edits persist through the sha
    // autosave instead, and collab re-arms the instant it regains focus.
    let handles = crate::collab::use_collab_handles();
    let mut collab = use_signal(|| None::<crate::collab::CollabHandles>);
    let mut collab_doc = use_signal(|| None::<uuid::Uuid>);
    let account = try_use_context::<Signal<Option<crate::auth::ActiveAccount>>>();
    let conn = architect::use_connection::<vox_core::Caller>();
    let collab_path = path.clone();
    use_effect(move || {
        // Reactive reads: re-open collab on reconnect (generation bump)
        // and whenever this pane gains/loses focus.
        let _generation = conn.generation();
        let focused_now = is_focused();
        collab_doc.set(None);
        collab.set(None);
        handles.reset();
        if !focused_now {
            return; // unfocused pane stays in plain sha mode
        }
        let slug = home.peek().clone();
        let path = collab_path.clone();
        spawn(async move {
            match crate::collab::open_collab(slug, path.clone()).await {
                Ok(ack) => {
                    if session.current_path().as_deref() == Some(path.as_str()) {
                        collab_doc.set(Some(ack.doc_id));
                        collab.set(Some(handles));
                    }
                }
                Err(e) => tracing::debug!("vault collab unavailable for {path}: {e}"),
            }
        });
    });
    // Autosave pauses exactly while collab is live.
    use_effect(move || {
        let live = collab.read().as_ref().is_some_and(|c| c.is_live());
        session.set_autosave_paused(live);
    });
    // Live → Offline teardown: fall back to sha saves.
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

    // ── Conformance mirror (focused pane only) ────────────────
    #[cfg(target_arch = "wasm32")]
    {
        let mirror_path = path.clone();
        use_effect(move || {
            if !is_focused() {
                return;
            }
            let text = session.state.read().doc.to_string();
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
            set("path", wasm_bindgen::JsValue::from_str(&mirror_path));
            let _ =
                js_sys::Reflect::set(&win, &wasm_bindgen::JsValue::from_str("__taskVault"), &obj);
        });
    }

    // ── Editor → replica bridge + presence publish ────────────
    let on_transaction = use_callback(move |event: editor::TransactionEvent| {
        let Some(c) = *collab.peek() else { return };
        let who = account
            .and_then(|a| a.peek().as_ref().map(|acct| acct.name.clone()))
            .unwrap_or_else(|| "anonymous".to_owned());
        crate::collab::on_editor_transaction(&c, &session, &event, &who);
    });

    // Publish this doc to the sidebar Properties panel while focused, so
    // it edits the SAME live buffer through this note's transaction sink.
    // The `claim` id makes clears identity-safe: a pane only relinquishes
    // the context while it is still the holder, so a split-view focus swap
    // (or a keyed remount) never drops the newly-focused doc.
    let claim = use_hook(crate::pages::note_properties::next_claim);
    if let Some(mut focused_doc) =
        try_use_context::<Signal<Option<crate::pages::note_properties::FocusedDoc>>>()
    {
        use_effect(move || {
            if is_focused() {
                focused_doc.set(Some(crate::pages::note_properties::FocusedDoc {
                    claim,
                    state: session.state,
                    on_transaction,
                }));
            } else if (*focused_doc.peek()).map(|d| d.claim) == Some(claim) {
                focused_doc.set(None);
            }
        });
        use_drop(move || {
            if (*focused_doc.peek()).map(|d| d.claim) == Some(claim) {
                focused_doc.set(None);
            }
        });
    }

    // Editor sources — created once, capturing the signals above.
    let decorations = use_hook(|| crate::collab::collab_decoration_source(lookup, collab));
    let completion = use_hook(|| vault_lookup::vault_completion_source(link_candidates, tag_rows));

    // ── `type:` dispatch (from the folder index) ──────────────
    let type_path = path.clone();
    let current_type = use_memo(move || {
        pages
            .read()
            .iter()
            .find(|p| p.path == type_path)
            .map(|p| p.page_type.to_lowercase())
    });
    let is_video = current_type.read().as_deref() == Some("video");
    let is_song = current_type.read().as_deref() == Some("song");
    // Immersive Experience from `type:` + an optional `experience:`
    // frontmatter key (`type: setlist` implies the setlist experience;
    // `experience: <name>` selects one for any note).
    let note_experience = {
        let doc = session.state.peek().doc.to_string();
        let exp = crate::pages::vault::frontmatter_value(&doc, "experience")
            .map(|v| v.trim().trim_matches(['"', '\'']).trim().to_owned());
        crate::pages::experience::experience_of(current_type.read().as_deref(), exp.as_deref())
    };
    let is_setlist = note_experience == Some(crate::pages::experience::ExperienceKind::Setlist);
    let is_base = path
        .rsplit_once('.')
        .is_some_and(|(_, e)| e.eq_ignore_ascii_case("base"));
    let (song_slug_value, song_front_value) = if is_song {
        let doc = session.state.peek().doc.to_string();
        (
            song_slug_from(&doc, basename_of(&path)),
            crate::pages::vault::song_front_from(&doc),
        )
    } else {
        (String::new(), Default::default())
    };
    let setlist_songs_value = if is_setlist {
        setlist_songs_from(&session.state.peek().doc.to_string())
    } else {
        Vec::new()
    };
    // A non-setlist note (e.g. an event) that EMBEDS a setlist via a
    // standalone wikilink still gets a headless player so the embed's ▶
    // buttons work: resolve the first embedded setlist through the vault
    // index and queue its songs.
    let embedded_setlist_songs: Vec<String> = if !is_setlist {
        let doc = session.state.peek().doc.to_string();
        crate::pages::vault::setlist_song_links_from_body(&doc)
            .iter()
            .find_map(|target| {
                let guard = lookup.peek();
                let ix = guard.as_ref()?;
                let meta = ix.meta(target)?;
                let raw = ix.content(&meta.path)?;
                let is_sl = crate::pages::vault::frontmatter_value(&raw, "type")
                    .map(|v| v.trim().trim_matches(['"', '\'']).trim() == "setlist")
                    .unwrap_or(false);
                is_sl.then(|| crate::pages::vault::setlist_songs_from(&raw))
            })
            .unwrap_or_default()
    } else {
        Vec::new()
    };

    // Editor link clicks: `song-play:<name>` drives the mounted stream
    // player (inline song-strip play buttons); wikilink targets navigate
    // to the note (resolved through the vault index when possible).
    let nav_links = use_navigator();
    let lookup_for_links = lookup;
    let mut play_req = use_context::<crate::chrome::SongPlayRequest>().0;
    let on_link_click = use_callback(move |href: String| {
        if let Some(name) = href.strip_prefix("song-play:") {
            let generation = play_req.peek().0 + 1;
            play_req.set((generation, name.to_string()));
            return;
        }
        if href.starts_with("setlist-play:") {
            let generation = play_req.peek().0 + 1;
            play_req.set((generation, String::new())); // empty = toggle
            return;
        }
        if let Some(tab) = href.strip_prefix("event-tab:") {
            *crate::event_tabs::EVENT_ACTIVE_TAB.write() = tab.to_string();
            return;
        }
        if href.starts_with("http://") || href.starts_with("https://") {
            return; // the editor already window.open()s external links
        }
        let page = href.split(['#', '|']).next().unwrap_or(&href).trim();
        let path = lookup_for_links
            .peek()
            .as_ref()
            .and_then(|ix| ix.meta(page).map(|m| m.path.clone()))
            .unwrap_or_else(|| format!("{page}.md"));
        nav_links.push(crate::routes::Route::VaultRoute { path });
    });

    // ── Save / conflict state ─────────────────────────────────
    let is_dirty = session.dirty();
    let status_msg = match session.status() {
        SaveStatus::Idle => String::new(),
        SaveStatus::Saving => "Saving…".to_owned(),
        SaveStatus::Saved => "Saved".to_owned(),
        SaveStatus::Failed(msg) => msg,
    };
    let conflict_open = session.conflict().is_some();
    let mut edit_base_source = use_signal(|| false);
    let current = path.clone();

    // ── Shell status line (focused pane only) ─────────────────
    let status_info = use_context::<crate::chrome::StatusBarInfo>().0;
    let on_save_cb = use_callback(move |_: ()| session.save());
    use_effect(move || {
        if !is_focused() {
            return;
        }
        let mut info = status_info;
        let Some(file) = session.current_path() else {
            info.set(None);
            return;
        };
        // Refresh the page's backlinks/links panel after each commit.
        let mut ft = focus_tick;
        ft.set(session.save_count());
        let save = match session.status() {
            SaveStatus::Idle => String::new(),
            SaveStatus::Saving => "Saving…".to_owned(),
            SaveStatus::Saved => "Saved".to_owned(),
            SaveStatus::Failed(msg) => msg,
        };
        let vim_label = vim.map(|v| {
            match v.read().mode {
                editor::editor_vim::Mode::Normal => "NORMAL",
                editor::editor_vim::Mode::Insert => "INSERT",
                editor::editor_vim::Mode::VisualChar => "VISUAL",
                editor::editor_vim::Mode::VisualLine => "V-LINE",
                editor::editor_vim::Mode::VisualBlock => "V-BLOCK",
                editor::editor_vim::Mode::Replace => "REPLACE",
                editor::editor_vim::Mode::Command => "COMMAND",
            }
            .to_owned()
        });
        let collab_label = collab
            .read()
            .as_ref()
            .map(|c| if c.is_live() { "live" } else { "connecting…" });
        info.set(Some(crate::chrome::DocStatus {
            file,
            dirty: session.dirty(),
            save,
            collab: collab_label.map(str::to_owned),
            vim: vim_label,
            on_save: Some(on_save_cb),
        }));
    });

    rsx! {
        div {
            class: "flex min-h-0 min-w-0 flex-1 flex-col",
            onkeydown: move |evt: Event<KeyboardData>| {
                let m = evt.modifiers();
                if (m.ctrl() || m.meta()) && evt.key().to_string() == "s" {
                    evt.prevent_default();
                    session.save();
                }
            },
            // Mobile-only name + save-state strip.
            div { class: "flex items-center justify-between gap-3 border-b border-border/60 px-4 py-1.5 md:hidden",
                div { class: "flex min-w-0 items-center gap-2",
                    if is_dirty {
                        span { class: "size-2 shrink-0 rounded-full bg-primary", title: "Unsaved changes" }
                    }
                    div { class: "min-w-0 truncate text-sm font-medium", "{current}" }
                }
                if !status_msg.is_empty() {
                    Text { variant: TextVariant::Muted, class: "shrink-0 text-xs", "{status_msg}" }
                }
            }
            // `.base` table↔source flip.
            if is_base {
                div { class: "flex items-center justify-end border-b border-border/40 px-4 py-1",
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            let cur = *edit_base_source.peek();
                            edit_base_source.set(!cur);
                        },
                        if edit_base_source() { "View table" } else { "Edit source" }
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
            div { class: "flex min-h-0 min-w-0 flex-1 flex-col overflow-y-auto pb-14 md:pb-0",
                if is_base && !edit_base_source() {
                    crate::pages::bases::BaseDoc {
                        base_path: current.clone(),
                        on_open: move |p: String| on_open.call(FileMeta { path: p, sha256: String::new() }),
                    }
                } else if is_video {
                    crate::pages::watch::WatchView {
                        v: basename_of(&current).to_string(),
                        node: format!("video:{}", basename_of(&current)),
                    }
                } else {
                    if is_song {
                        crate::pages::song_session::SongView {
                            slug: song_slug_value.clone(),
                            org: home.read().clone(),
                            title: basename_of(&path).to_string(),
                            front: song_front_value.clone(),
                        }
                    }
                    if !is_setlist && !embedded_setlist_songs.is_empty() {
                        // Headless queue for embedded-setlist playback.
                        crate::pages::setlist_stream::SetlistStreamPlayer {
                            org: home.read().clone(),
                            title: basename_of(&path).to_string(),
                            songs: embedded_setlist_songs.clone(),
                            show_rows: false,
                            headless: true,
                        }
                    }
                    if is_setlist {
                        // Setlist Experience: auto full-screen (an overlay
                        // that escapes the pane + sidebars), Esc to exit.
                        // Embedded fallback keeps a re-enter control.
                        if setlist_fullscreen() {
                            crate::pages::experience::FullscreenExperience {
                                title: basename_of(&path).to_string(),
                                on_exit: move |_| setlist_fullscreen.set(false),
                                crate::pages::setlist_session::SetlistPlayer {
                                    songs: setlist_songs_value.clone(),
                                    fullscreen: true,
                                }
                            }
                        } else {
                            crate::pages::experience::EnterExperienceButton {
                                label: "Setlist",
                                on_enter: move |_| setlist_fullscreen.set(true),
                            }
                            // Streaming header (icon · title · play) — the
                            // setlist's H1 replacement. Rows render INLINE in
                            // the editor as song strips; this header owns
                            // playback (one reference track at a time over
                            // vox MediaService) and answers strip play clicks.
                            // HEADLESS: the editor's setlist-header widget
                            // (the note's own H1) is the visible header; this
                            // just owns playback + answers play requests.
                            crate::pages::setlist_stream::SetlistStreamPlayer {
                                org: home.read().clone(),
                                title: basename_of(&path).to_string(),
                                songs: setlist_songs_value.clone(),
                                show_rows: false,
                                headless: true,
                            }
                        }
                    }
                    // The note body (frontmatter + Markdown editor) is bound to
                    // `session.state`, which autosaves to the note file. A
                    // fullscreen experience (e.g. the setlist) draws its OWN
                    // editors on top; if we also kept this one mounted beneath the
                    // overlay it would still catch stray keystrokes/edits and
                    // autosave them into the note — which corrupted the setlist's
                    // frontmatter when the keyflow-source editor was used. So skip
                    // it entirely while a fullscreen experience owns the screen.
                    if note_body_visible(is_setlist, setlist_fullscreen()) {
                        // Setlists: the streaming header above IS the title —
                        // skip the duplicate note header.
                        if !is_setlist {
                            crate::pages::note_header::NoteHeader {
                                home,
                                props_open,
                                on_renamed: move |_| on_renamed.call(()),
                            }
                        }
                        // Raw view (status-bar toggle): the literal file text
                        // MINUS the YAML frontmatter — properties stay in the
                        // right-sidebar Properties tab. Read currently renders
                        // the editor (the reading-view design comes later).
                        div { class: "mx-auto w-full max-w-3xl",
                        if use_context::<crate::chrome::NoteViewMode>().0() == crate::chrome::ViewMode::Raw {
                            pre { class: "whitespace-pre-wrap px-6 py-4 font-mono text-sm leading-6 text-foreground",
                                {raw_body_text(&session.state.read().doc.to_string())}
                            }
                        } else {
                            div { class: if props_open() { "editor-app" } else { "editor-app props-collapsed" },
                                div { class: "editor-frame editor-frame--flush",
                                    Editor {
                                        state: session.state,
                                        keymap: keymap.read().clone(),
                                        decorations: decorations.clone(),
                                        vim,
                                        slash: Some(slash),
                                        completion: completion.clone(),
                                        on_transaction,
                                        on_link_click,
                                    }
                                    SlashMenu { state: session.state, slash }
                                }
                            }
                        }
                        }
                    }
                }
            }
            // Keyed collab child: remount per doc id = fresh replica.
            if let Some(doc_id) = collab_doc() {
                crate::collab::CollabSession { key: "{doc_id}", doc_id, handles }
            }
        }
    }
}

/// The Raw view's text: the file minus its YAML frontmatter fence (the
/// properties live in the right-sidebar Properties tab).
fn raw_body_text(text: &str) -> String {
    text.strip_prefix("---")
        .and_then(|rest| rest.split_once("\n---"))
        .map(|(_, body)| body.trim_start_matches(['\r', '\n']).to_owned())
        .unwrap_or_else(|| text.to_owned())
}

/// Whether the note body (frontmatter + Markdown editor, bound to the
/// autosaving `session.state`) should be mounted.
///
/// It is hidden while a fullscreen experience owns the screen: that experience
/// renders its own editors (e.g. the setlist's keyflow-source editor), and
/// keeping the note's editor mounted underneath let stray keystrokes/edits fall
/// into it and autosave into the note — which corrupted the setlist note's
/// frontmatter (chart text written to the top of the file, breaking the `---`
/// fence so `type: setlist` no longer parsed and the experience stopped
/// opening). Not mounting it removes that sink entirely.
fn note_body_visible(is_setlist: bool, setlist_fullscreen: bool) -> bool {
    !(is_setlist && setlist_fullscreen)
}

#[cfg(test)]
mod note_body_tests {
    use super::note_body_visible;

    #[test]
    fn note_body_hidden_only_under_fullscreen_setlist() {
        // The one case that caused the corruption: a setlist note shown
        // fullscreen. The note-body editor (the autosave sink) must NOT mount,
        // so the keyflow-source editor's input can never land in the note.
        assert!(
            !note_body_visible(true, true),
            "note body must be hidden while the fullscreen setlist experience is open"
        );

        // Every other combination keeps the normal note editor.
        assert!(
            note_body_visible(true, false),
            "a setlist note shown embedded still edits normally"
        );
        assert!(
            note_body_visible(false, true),
            "a non-setlist note is never suppressed by the setlist flag"
        );
        assert!(note_body_visible(false, false), "a plain note edits normally");
    }
}

//! Ctrl+P command palette — fuzzy jump to any page or vault note.
//!
//! One global overlay ([`CommandPalette`]), toggled by a
//! [`PaletteOpen`] context signal the app shell provides (same shape
//! as `chrome::FleetingOpen`). Ctrl+P / Cmd+P opens it from anywhere
//! via a document-level keydown listener; Escape closes it.
//!
//! Results mix two sections, both nucleo-ranked (fzf-style, matching
//! `fts_ui`'s combobox filter):
//!
//! - **Pages** — every app destination from [`crate::nav::nav_tabs`].
//! - **Notes** — every vault note from the home org's folder index
//!   (title → `Route::VaultRoute { path }`), fetched when the palette
//!   opens.
//!
//! FUTURE: the hotkey moves to the `input` crate's keymap
//! (`use_input_processor` + `KeymapConfig`) with the keyboard
//! customizer subtask — this hand-rolled listener is the stopgap.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{FileText, Search};
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Matcher, Utf32Str,
};

use crate::nav::nav_tabs;
use crate::orgs::OrgMeta;
use crate::routes::Route;

// ── shared context ──────────────────────────────────────────────────

/// Visibility of the global command palette. Provided once by the app
/// shell; flipped by the Ctrl/Cmd+P hotkey.
#[derive(Clone, Copy)]
pub struct PaletteOpen(pub Signal<bool>);

/// Install the palette context. Call once in the app shell.
pub fn provide_palette_context() {
    use_context_provider(|| PaletteOpen(Signal::new(false)));
}

pub(crate) fn use_palette_open() -> Signal<bool> {
    use_context::<PaletteOpen>().0
}

// ── data ────────────────────────────────────────────────────────────

/// Rows rendered at once — keeps ranking + render cheap over big
/// vaults (the folder index can be hundreds of notes).
const MAX_RESULTS: usize = 12;

/// One selectable row: an app page or a vault note.
#[derive(Clone, PartialEq)]
#[allow(unpredictable_function_pointer_comparisons)]
struct PaletteEntry {
    label: String,
    /// Secondary match/display text — the vault-relative path for
    /// notes, empty for pages.
    detail: String,
    route: Route,
    /// Nav-tab icon for pages; notes render a file icon.
    icon: Option<fn() -> Element>,
    is_page: bool,
}

/// Rank `entries` against `query` and keep the top [`MAX_RESULTS`],
/// pages before notes for display grouping. Empty query = everything
/// in given order (pages lead), capped the same way.
fn rank(query: &str, entries: Vec<PaletteEntry>) -> Vec<PaletteEntry> {
    let survivors: Vec<PaletteEntry> = if query.is_empty() {
        entries.into_iter().take(MAX_RESULTS).collect()
    } else {
        // Same nucleo setup as `fts_ui`'s combobox filter, but keeping
        // the score so the list is ranked, not just filtered.
        let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
        let pattern = Pattern::parse(query, CaseMatching::Ignore, Normalization::Smart);
        let mut buf = Vec::new();
        let mut scored: Vec<(u32, PaletteEntry)> = entries
            .into_iter()
            .filter_map(|e| {
                // Notes match on title AND path, so folder names hit too.
                let hay = if e.detail.is_empty() {
                    e.label.clone()
                } else {
                    format!("{} {}", e.label, e.detail)
                };
                let score = pattern.score(Utf32Str::new(&hay, &mut buf), &mut matcher)?;
                Some((score, e))
            })
            .collect();
        scored.sort_by(|a, b| b.0.cmp(&a.0));
        scored.truncate(MAX_RESULTS);
        scored.into_iter().map(|(_, e)| e).collect()
    };
    // Group for display: pages section, then notes — rank order kept
    // within each.
    let (pages, notes): (Vec<_>, Vec<_>) = survivors.into_iter().partition(|e| e.is_page);
    pages.into_iter().chain(notes).collect()
}

// ── hotkey ──────────────────────────────────────────────────────────

enum HotkeyMsg {
    Toggle,
    Close,
}

/// Document-level Ctrl+P / Cmd+P (toggle) + Escape (close) listener.
/// The raw DOM closure runs outside the Dioxus runtime, so it bridges
/// back through a channel a spawned task drains. Web-only; native
/// desktop wiring arrives with the input-crate migration.
fn use_palette_hotkey(open: Signal<bool>) {
    #[cfg(target_arch = "wasm32")]
    use_hook(move || {
        use futures_util::StreamExt;
        use wasm_bindgen::prelude::Closure;
        use wasm_bindgen::JsCast;

        let mut open = open;
        let (tx, mut rx) = futures_channel::mpsc::unbounded::<HotkeyMsg>();
        spawn(async move {
            while let Some(msg) = rx.next().await {
                match msg {
                    HotkeyMsg::Toggle => {
                        let cur = *open.peek();
                        open.set(!cur);
                    }
                    HotkeyMsg::Close => {
                        if *open.peek() {
                            open.set(false);
                        }
                    }
                }
            }
        });

        let cb = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(
            move |e: web_sys::KeyboardEvent| {
                if (e.ctrl_key() || e.meta_key())
                    && !e.alt_key()
                    && !e.shift_key()
                    && e.key().eq_ignore_ascii_case("p")
                {
                    // Swallow the browser's print dialog.
                    e.prevent_default();
                    let _ = tx.unbounded_send(HotkeyMsg::Toggle);
                } else if e.key() == "Escape" {
                    let _ = tx.unbounded_send(HotkeyMsg::Close);
                }
            },
        );
        if let Some(doc) = web_sys::window().and_then(|w| w.document()) {
            let _ = doc.add_event_listener_with_callback("keydown", cb.as_ref().unchecked_ref());
        }
        // The shell never unmounts — the listener lives app-long.
        cb.forget();
    });
    #[cfg(not(target_arch = "wasm32"))]
    let _ = open;
}

// ── overlay ─────────────────────────────────────────────────────────

/// The global command palette. Render once (in the app shell).
/// Centered top-third modal over a scrim; type to fuzzy-filter,
/// Up/Down + Enter (or click) to jump, Escape / click outside closes.
#[component]
pub fn CommandPalette() -> Element {
    let mut open = use_palette_open();
    use_palette_hotkey(open);

    let nav = use_navigator();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let home = use_memo(move || crate::orgs::home_slug(&org_list.read()));

    let mut query = use_signal(String::new);
    let mut cursor = use_signal(|| 0usize);

    // Vault note index — the home org's folder index, fetched each
    // time the palette opens so new notes appear without a reload.
    let notes = use_resource(move || {
        let want = open();
        let slug = home();
        async move {
            if !want {
                return Vec::new();
            }
            crate::pages::vault::fetch_folder_index(slug)
                .await
                .unwrap_or_default()
        }
    });

    if !open() {
        return rsx! {};
    }

    let mut close = move || {
        open.set(false);
        query.set(String::new());
        cursor.set(0);
    };

    // Pages from the nav set + notes from the vault index, ranked.
    let mut entries: Vec<PaletteEntry> = nav_tabs()
        .into_iter()
        .map(|tab| PaletteEntry {
            label: tab.label.to_string(),
            detail: String::new(),
            route: tab.route,
            icon: Some(tab.icon),
            is_page: true,
        })
        .collect();
    if let Some(pages) = &*notes.read() {
        for meta in pages {
            let label = if meta.title.is_empty() {
                meta.basename.clone()
            } else {
                meta.title.clone()
            };
            entries.push(PaletteEntry {
                label,
                detail: meta.path.clone(),
                route: Route::VaultRoute {
                    path: meta.path.clone(),
                },
                icon: None,
                is_page: false,
            });
        }
    }
    let ranked = rank(&query(), entries);
    let count = ranked.len();
    let sel = cursor().min(count.saturating_sub(1));

    let go = {
        let ranked = ranked.clone();
        move |i: usize| {
            if let Some(entry) = ranked.get(i) {
                nav.push(entry.route.clone());
            }
            close();
        }
    };
    let mut go_key = go.clone();

    // Section split: `rank` puts pages first, so the notes group is
    // the tail starting at the first non-page row.
    let first_note = ranked.iter().position(|e| !e.is_page).unwrap_or(count);

    rsx! {
        div {
            class: "fixed inset-0 z-50 flex items-start justify-center bg-black/40 p-4 pt-[12vh]",
            onclick: move |_| close(),
            div {
                class: "flex w-full max-w-xl flex-col overflow-hidden rounded-xl border border-border bg-background shadow-xl",
                onclick: move |e| e.stop_propagation(),
                div { class: "flex items-center gap-2 border-b border-border px-3",
                    span { class: "text-muted-foreground", Search { size: 15 } }
                    input {
                        class: "h-11 w-full bg-transparent text-sm outline-none placeholder:text-muted-foreground",
                        placeholder: "Jump to a page or note…",
                        // `autofocus` only fires on initial page load, not
                        // when the modal is inserted dynamically — focus on
                        // mount (same dance as the fleeting modal).
                        onmounted: move |e: Event<MountedData>| {
                            spawn(async move {
                                let _ = e.data().set_focus(true).await;
                            });
                        },
                        value: "{query}",
                        oninput: move |e| {
                            query.set(e.value());
                            cursor.set(0);
                        },
                        onkeydown: move |e| match e.key() {
                            Key::ArrowDown => {
                                e.prevent_default();
                                if count > 0 {
                                    cursor.set((sel + 1).min(count - 1));
                                }
                            }
                            Key::ArrowUp => {
                                e.prevent_default();
                                cursor.set(sel.saturating_sub(1));
                            }
                            Key::Enter => {
                                e.prevent_default();
                                go_key(sel);
                            }
                            Key::Escape => close(),
                            _ => {}
                        },
                    }
                    span { class: "shrink-0 text-xs text-muted-foreground", "↵ to open · esc to close" }
                }
                div { class: "max-h-[50vh] overflow-y-auto py-1",
                    if count == 0 {
                        div { class: "px-3 py-4 text-center text-sm text-muted-foreground", "No matches." }
                    }
                    if first_note > 0 {
                        SectionLabel { text: "Pages" }
                        for (i, entry) in ranked[..first_note].iter().enumerate() {
                            PaletteRow {
                                key: "{entry.label}",
                                entry: entry.clone(),
                                selected: i == sel,
                                on_hover: move |_| cursor.set(i),
                                on_pick: {
                                    let mut go = go.clone();
                                    move |_| go(i)
                                },
                            }
                        }
                    }
                    if first_note < count {
                        SectionLabel { text: "Notes" }
                        for (off, entry) in ranked[first_note..].iter().enumerate() {
                            PaletteRow {
                                key: "{entry.detail}",
                                entry: entry.clone(),
                                selected: first_note + off == sel,
                                on_hover: move |_| cursor.set(first_note + off),
                                on_pick: {
                                    let mut go = go.clone();
                                    move |_| go(first_note + off)
                                },
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn SectionLabel(text: &'static str) -> Element {
    rsx! {
        div { class: "px-3 pb-1 pt-2 text-xs font-medium uppercase tracking-wide text-muted-foreground",
            "{text}"
        }
    }
}

/// One result row: icon + label (+ muted path for notes). Hover moves
/// the keyboard cursor so mouse and arrows never fight.
#[component]
fn PaletteRow(
    entry: PaletteEntry,
    selected: bool,
    on_hover: EventHandler<()>,
    on_pick: EventHandler<()>,
) -> Element {
    let icon = entry.icon;
    rsx! {
        button {
            r#type: "button",
            class: if selected {
                "flex w-full items-center gap-2.5 bg-accent/60 px-3 py-2 text-left text-sm text-foreground"
            } else {
                "flex w-full items-center gap-2.5 px-3 py-2 text-left text-sm text-muted-foreground hover:text-foreground"
            },
            onmouseenter: move |_| on_hover.call(()),
            onclick: move |_| on_pick.call(()),
            span { class: "flex h-4 w-4 shrink-0 items-center justify-center",
                if let Some(icon) = icon {
                    {icon()}
                } else {
                    FileText { size: 15 }
                }
            }
            span { class: "truncate", "{entry.label}" }
            if !entry.detail.is_empty() {
                span { class: "ml-auto max-w-[40%] truncate text-xs text-muted-foreground/70",
                    "{entry.detail}"
                }
            }
        }
    }
}

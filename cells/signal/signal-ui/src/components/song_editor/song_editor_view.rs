//! Song & Setlist Editor View — manage songs, their scenes, and setlists.
//!
//! Left: Song/Setlist browser with toggle
//! Center: Song scene table with preset/snapshot assignments, reordering
//! Right: Available presets picker + setlist composition

use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::{performance_song, preset_entity, setlist, setlist_song, song_scene};
use uuid::Uuid;

// ── Global Signals ───────────────────────────────────────────────────────────

static SONG_LIST: GlobalSignal<Vec<performance_song::Model>> = Signal::global(Vec::new);
static SELECTED_SONG_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);
static SONG_SCENES: GlobalSignal<Vec<song_scene::Model>> = Signal::global(Vec::new);
static SETLIST_LIST: GlobalSignal<Vec<setlist::Model>> = Signal::global(Vec::new);
static SELECTED_SETLIST_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);
static SETLIST_SONGS: GlobalSignal<Vec<setlist_song::Model>> = Signal::global(Vec::new);
static SONG_AVAILABLE_PRESETS: GlobalSignal<Vec<preset_entity::Model>> = Signal::global(Vec::new);
static SONG_STATUS: GlobalSignal<String> = Signal::global(String::new);
static BROWSER_TAB: GlobalSignal<BrowserTab> = Signal::global(BrowserTab::default);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum BrowserTab {
    #[default]
    Songs,
    Setlists,
}

// ── Async Refresh Helpers ────────────────────────────────────────────────────

async fn refresh_songs() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_songs().await {
        Ok(songs) => *SONG_LIST.write() = songs,
        Err(e) => warn!("Failed to load songs: {e}"),
    }
}

async fn refresh_song_scenes(song_id: Uuid) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_song_scenes(song_id).await {
        Ok(scenes) => {
            let mut sorted = scenes;
            sorted.sort_by_key(|s| s.sort_order);
            *SONG_SCENES.write() = sorted;
        }
        Err(e) => warn!("Failed to load song scenes: {e}"),
    }
}

async fn refresh_setlists() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_setlists().await {
        Ok(setlists) => *SETLIST_LIST.write() = setlists,
        Err(e) => warn!("Failed to load setlists: {e}"),
    }
}

async fn refresh_setlist_songs(setlist_id: Uuid) {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_setlist_songs(setlist_id).await {
        Ok(songs) => {
            let mut sorted = songs;
            sorted.sort_by_key(|s| s.sort_order);
            *SETLIST_SONGS.write() = sorted;
        }
        Err(e) => warn!("Failed to load setlist songs: {e}"),
    }
}

async fn refresh_song_presets() {
    let Some(ctl) = RIG_SERVICE.read().clone() else {
        return;
    };
    match ctl.list_rig_presets().await {
        Ok(presets) => *SONG_AVAILABLE_PRESETS.write() = presets,
        Err(e) => warn!("Failed to load presets: {e}"),
    }
}

// ── Main Component ───────────────────────────────────────────────────────────

#[component]
pub fn SongEditorView() -> Element {
    let songs = SONG_LIST.read();
    let selected_song_id = *SELECTED_SONG_ID.read();
    let scenes = SONG_SCENES.read();
    let setlists = SETLIST_LIST.read();
    let selected_setlist_id = *SELECTED_SETLIST_ID.read();
    let setlist_songs_list = SETLIST_SONGS.read();
    let presets = SONG_AVAILABLE_PRESETS.read();
    let status = SONG_STATUS.read();
    let browser_tab = *BROWSER_TAB.read();

    // Local state
    let mut show_new_song_dialog = use_signal(|| false);
    let mut new_song_name = use_signal(String::new);
    let mut show_new_setlist_dialog = use_signal(|| false);
    let mut new_setlist_name = use_signal(String::new);
    let mut show_add_scene_dialog = use_signal(|| false);
    let mut new_scene_name = use_signal(String::new);

    // Load data on mount
    use_effect(move || {
        spawn(async move {
            refresh_songs().await;
            refresh_setlists().await;
            refresh_song_presets().await;
        });
    });

    // Load scenes when song selection changes
    use_effect(move || {
        let sid = *SELECTED_SONG_ID.read();
        if let Some(id) = sid {
            spawn(async move {
                refresh_song_scenes(id).await;
            });
        } else {
            SONG_SCENES.write().clear();
        }
    });

    // Load setlist songs when setlist selection changes
    use_effect(move || {
        let sid = *SELECTED_SETLIST_ID.read();
        if let Some(id) = sid {
            spawn(async move {
                refresh_setlist_songs(id).await;
            });
        } else {
            SETLIST_SONGS.write().clear();
        }
    });

    let selected_song = selected_song_id.and_then(|id| songs.iter().find(|s| s.id == id));

    rsx! {
        div { class: "flex h-full w-full overflow-hidden bg-card",
            // ══════════════════════════════════════════════════════════════
            // LEFT PANEL — Song / Setlist Browser
            // ══════════════════════════════════════════════════════════════
            div { class: "w-56 flex flex-col border-r border-border/30 bg-zinc-900/20 flex-shrink-0 h-full",
                // Tab toggle
                div { class: "px-3 py-2 border-b border-border/30 flex-shrink-0",
                    div { class: "flex bg-zinc-800/80 rounded-lg p-0.5",
                        button {
                            class: if browser_tab == BrowserTab::Songs {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold bg-primary text-primary-foreground transition-colors"
                            } else {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-400 hover:text-zinc-200 transition-colors"
                            },
                            onclick: move |_| *BROWSER_TAB.write() = BrowserTab::Songs,
                            "Songs"
                        }
                        button {
                            class: if browser_tab == BrowserTab::Setlists {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold bg-primary text-primary-foreground transition-colors"
                            } else {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-400 hover:text-zinc-200 transition-colors"
                            },
                            onclick: move |_| *BROWSER_TAB.write() = BrowserTab::Setlists,
                            "Setlists"
                        }
                    }
                }

                // List content
                div { class: "flex-1 overflow-y-auto min-h-0",
                    match browser_tab {
                        BrowserTab::Songs => rsx! {
                            for song in songs.iter() {
                                {
                                    let sid = song.id;
                                    let is_selected = selected_song_id == Some(sid);
                                    let sname = song.name.clone();
                                    let artist = song.artist.clone().unwrap_or_default();
                                    rsx! {
                                        div {
                                            key: "{sid}",
                                            class: if is_selected {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-blue-500 bg-blue-500/10 transition-colors"
                                            } else {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-transparent hover:bg-zinc-800/40 transition-colors"
                                            },
                                            onclick: move |_| {
                                                *SELECTED_SONG_ID.write() = Some(sid);
                                            },
                                            div { class: "flex items-center justify-between",
                                                div { class: "flex-1 min-w-0",
                                                    span { class: "text-xs font-medium text-zinc-200 truncate block", "{sname}" }
                                                    if !artist.is_empty() {
                                                        span { class: "text-[10px] text-zinc-500 truncate block", "{artist}" }
                                                    }
                                                }
                                                if is_selected {
                                                    button {
                                                        class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors flex-shrink-0",
                                                        title: "Delete",
                                                        onclick: move |evt| {
                                                            evt.stop_propagation();
                                                            spawn(async move {
                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                if let Err(e) = ctl.delete_song(sid).await {
                                                                    warn!("Delete song failed: {e}");
                                                                }
                                                                if *SELECTED_SONG_ID.read() == Some(sid) {
                                                                    *SELECTED_SONG_ID.write() = None;
                                                                }
                                                                refresh_songs().await;
                                                            });
                                                        },
                                                        span { class: "text-[9px]", "\u{2715}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        BrowserTab::Setlists => rsx! {
                            for sl in setlists.iter() {
                                {
                                    let slid = sl.id;
                                    let is_selected = selected_setlist_id == Some(slid);
                                    let slname = sl.name.clone();
                                    rsx! {
                                        div {
                                            key: "{slid}",
                                            class: if is_selected {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-teal-500 bg-teal-500/10 transition-colors"
                                            } else {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-transparent hover:bg-zinc-800/40 transition-colors"
                                            },
                                            onclick: move |_| {
                                                *SELECTED_SETLIST_ID.write() = Some(slid);
                                            },
                                            div { class: "flex items-center justify-between",
                                                span { class: "text-xs font-medium text-zinc-200 truncate", "{slname}" }
                                                if is_selected {
                                                    button {
                                                        class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors flex-shrink-0",
                                                        title: "Delete",
                                                        onclick: move |evt| {
                                                            evt.stop_propagation();
                                                            spawn(async move {
                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                if let Err(e) = ctl.delete_setlist(slid).await {
                                                                    warn!("Delete setlist failed: {e}");
                                                                }
                                                                if *SELECTED_SETLIST_ID.read() == Some(slid) {
                                                                    *SELECTED_SETLIST_ID.write() = None;
                                                                }
                                                                refresh_setlists().await;
                                                            });
                                                        },
                                                        span { class: "text-[9px]", "\u{2715}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }
                }

                // New song/setlist button
                div { class: "px-3 py-2 border-t border-border/30 flex-shrink-0",
                    match browser_tab {
                        BrowserTab::Songs => rsx! {
                            if *show_new_song_dialog.read() {
                                input {
                                    class: "w-full px-2 py-1.5 rounded text-xs bg-zinc-800 border border-blue-500/40 \
                                            text-zinc-200 placeholder-zinc-600 outline-none",
                                    placeholder: "Song name...",
                                    value: "{new_song_name}",
                                    autofocus: true,
                                    oninput: move |evt| new_song_name.set(evt.value().clone()),
                                    onkeydown: move |evt| {
                                        if evt.key() == Key::Enter {
                                            let val = new_song_name().trim().to_string();
                                            if !val.is_empty() {
                                                show_new_song_dialog.set(false);
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    match ctl.create_song(&val, None, false).await {
                                                        Ok(id) => {
                                                            refresh_songs().await;
                                                            *SELECTED_SONG_ID.write() = Some(id);
                                                        }
                                                        Err(e) => warn!("Create song failed: {e}"),
                                                    }
                                                });
                                            }
                                        } else if evt.key() == Key::Escape {
                                            show_new_song_dialog.set(false);
                                        }
                                    },
                                }
                            } else {
                                button {
                                    class: "w-full px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                            bg-blue-500/15 text-blue-300 border border-blue-500/25 \
                                            hover:bg-blue-500/25 hover:border-blue-500/40 transition-all duration-150",
                                    onclick: move |_| {
                                        new_song_name.set(String::new());
                                        show_new_song_dialog.set(true);
                                    },
                                    "+ New Song"
                                }
                            }
                        },
                        BrowserTab::Setlists => rsx! {
                            if *show_new_setlist_dialog.read() {
                                input {
                                    class: "w-full px-2 py-1.5 rounded text-xs bg-zinc-800 border border-teal-500/40 \
                                            text-zinc-200 placeholder-zinc-600 outline-none",
                                    placeholder: "Setlist name...",
                                    value: "{new_setlist_name}",
                                    autofocus: true,
                                    oninput: move |evt| new_setlist_name.set(evt.value().clone()),
                                    onkeydown: move |evt| {
                                        if evt.key() == Key::Enter {
                                            let val = new_setlist_name().trim().to_string();
                                            if !val.is_empty() {
                                                show_new_setlist_dialog.set(false);
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    match ctl.create_setlist(&val).await {
                                                        Ok(id) => {
                                                            refresh_setlists().await;
                                                            *SELECTED_SETLIST_ID.write() = Some(id);
                                                        }
                                                        Err(e) => warn!("Create setlist failed: {e}"),
                                                    }
                                                });
                                            }
                                        } else if evt.key() == Key::Escape {
                                            show_new_setlist_dialog.set(false);
                                        }
                                    },
                                }
                            } else {
                                button {
                                    class: "w-full px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                            bg-teal-500/15 text-teal-300 border border-teal-500/25 \
                                            hover:bg-teal-500/25 hover:border-teal-500/40 transition-all duration-150",
                                    onclick: move |_| {
                                        new_setlist_name.set(String::new());
                                        show_new_setlist_dialog.set(true);
                                    },
                                    "+ New Setlist"
                                }
                            }
                        },
                    }
                }
            }

            // ══════════════════════════════════════════════════════════════
            // CENTER PANEL — Song Scenes / Setlist Songs
            // ══════════════════════════════════════════════════════════════
            div { class: "flex-1 flex flex-col min-h-0 min-w-0",
                match browser_tab {
                    BrowserTab::Songs => rsx! {
                        if let Some(song) = selected_song {
                            // Song header
                            div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                                span { class: "text-xs font-bold text-zinc-200 tracking-wide",
                                    "{song.name}"
                                }
                                if let Some(ref artist) = song.artist {
                                    span { class: "text-[10px] text-zinc-500 italic", "by {artist}" }
                                }
                                {
                                    let sc = scenes.len();
                                    let sp = if sc != 1 { "s" } else { "" };
                                    rsx! {
                                        span { class: "text-[10px] text-zinc-600 font-mono", "{sc} scene{sp}" }
                                    }
                                }
                                div { class: "flex-1" }
                                // Auto-advance toggle
                                button {
                                    class: if song.auto_advance {
                                        "px-2 py-1 rounded text-[9px] font-semibold bg-green-500/20 text-green-300 border border-green-500/30"
                                    } else {
                                        "px-2 py-1 rounded text-[9px] font-semibold bg-zinc-800/50 text-zinc-500 border border-zinc-700/50"
                                    },
                                    title: "Auto-advance to next scene",
                                    onclick: {
                                        let song_id = song.id;
                                        let current = song.auto_advance;
                                        move |_| {
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                if let Err(e) = ctl.update_song(song_id, None, None, Some(!current), None, None).await {
                                                    warn!("Toggle auto-advance failed: {e}");
                                                }
                                                refresh_songs().await;
                                            });
                                        }
                                    },
                                    if song.auto_advance { "Auto \u{2713}" } else { "Auto" }
                                }
                                // Add scene
                                button {
                                    class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                            bg-blue-500/15 text-blue-300 border border-blue-500/25 \
                                            hover:bg-blue-500/25 hover:border-blue-500/40 transition-all duration-150",
                                    onclick: move |_| {
                                        new_scene_name.set(String::new());
                                        show_add_scene_dialog.set(true);
                                    },
                                    span { class: "text-blue-400", "+" }
                                    "Add Scene"
                                }
                            }

                            // Add scene dialog
                            if *show_add_scene_dialog.read() {
                                div { class: "px-4 py-2 border-b border-border/30 bg-zinc-800/50 flex items-center gap-2 flex-shrink-0",
                                    span { class: "text-[10px] text-zinc-400", "Scene name:" }
                                    input {
                                        class: "flex-1 px-2 py-1 rounded text-xs bg-zinc-900 border border-zinc-700 \
                                                text-zinc-200 outline-none focus:border-blue-500/50",
                                        value: "{new_scene_name}",
                                        autofocus: true,
                                        oninput: move |evt| new_scene_name.set(evt.value().clone()),
                                        onkeydown: {
                                            let song_id = song.id;
                                            move |evt| {
                                                if evt.key() == Key::Enter {
                                                    let val = new_scene_name().trim().to_string();
                                                    if !val.is_empty() {
                                                        show_add_scene_dialog.set(false);
                                                        let sort_order = SONG_SCENES.read().len() as i32;
                                                        spawn(async move {
                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                            match ctl.add_song_scene(song_id, &val, Uuid::nil(), None, sort_order).await {
                                                                Ok(_) => refresh_song_scenes(song_id).await,
                                                                Err(e) => warn!("Add scene failed: {e}"),
                                                            }
                                                        });
                                                    }
                                                } else if evt.key() == Key::Escape {
                                                    show_add_scene_dialog.set(false);
                                                }
                                            }
                                        },
                                    }
                                }
                            }

                            // Scene table
                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if scenes.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center py-12",
                                            div { class: "text-lg text-zinc-700 mb-1", "\u{1F3B5}" }
                                            p { class: "text-xs text-zinc-500", "No scenes yet" }
                                            p { class: "text-[10px] text-zinc-600 mt-1", "Add scenes to define the song structure" }
                                        }
                                    }
                                } else {
                                    // Table header
                                    div { class: "px-4 py-1.5 flex items-center gap-3 text-[9px] font-bold text-zinc-600 uppercase tracking-[0.1em] \
                                                  border-b border-border/20 bg-zinc-900/20 flex-shrink-0 sticky top-0",
                                        div { class: "w-8 text-center", "#" }
                                        div { class: "flex-1 min-w-0", "Scene" }
                                        div { class: "w-40", "Preset" }
                                        div { class: "w-24 text-center", "Actions" }
                                    }

                                    for (idx, scene) in scenes.iter().enumerate() {
                                        {
                                            let scene_id = scene.id;
                                            let scene_name = scene.name.clone();
                                            let preset_id = scene.preset_id;
                                            let preset_name = presets.iter()
                                                .find(|p| p.id == preset_id)
                                                .map(|p| p.name.as_str())
                                                .unwrap_or(if preset_id == Uuid::nil() { "\u{2014} unassigned \u{2014}" } else { "Unknown" });
                                            let song_id = selected_song_id.unwrap();

                                            rsx! {
                                                div {
                                                    key: "{scene_id}",
                                                    class: "px-4 py-2 flex items-center gap-3 border-b border-border/10 \
                                                            hover:bg-zinc-800/30 transition-colors group",
                                                    div { class: "w-8 text-center text-[10px] text-zinc-600 font-mono",
                                                        "{idx + 1}"
                                                    }
                                                    div { class: "flex-1 min-w-0",
                                                        span { class: "text-xs font-medium text-zinc-200 truncate block",
                                                            "{scene_name}"
                                                        }
                                                    }
                                                    div { class: "w-40",
                                                        span {
                                                            class: if preset_id == Uuid::nil() {
                                                                "text-[10px] text-zinc-600 italic"
                                                            } else {
                                                                "text-[10px] text-blue-300/70 font-mono"
                                                            },
                                                            "{preset_name}"
                                                        }
                                                    }
                                                    div { class: "w-24 flex items-center justify-center gap-1 opacity-0 group-hover:opacity-100 transition-opacity",
                                                        if idx > 0 {
                                                            button {
                                                                class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                title: "Move up",
                                                                onclick: move |_| {
                                                                    let mut ids: Vec<Uuid> = SONG_SCENES.read().iter().map(|s| s.id).collect();
                                                                    ids.swap(idx, idx - 1);
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                        if let Err(e) = ctl.reorder_song_scenes(song_id, &ids).await {
                                                                            warn!("Reorder failed: {e}");
                                                                        }
                                                                        refresh_song_scenes(song_id).await;
                                                                    });
                                                                },
                                                                span { class: "text-[9px]", "\u{2191}" }
                                                            }
                                                        }
                                                        {
                                                            let scene_count = scenes.len();
                                                            if idx < scene_count - 1 {
                                                                rsx! {
                                                                    button {
                                                                        class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                        title: "Move down",
                                                                        onclick: move |_| {
                                                                            let mut ids: Vec<Uuid> = SONG_SCENES.read().iter().map(|s| s.id).collect();
                                                                            ids.swap(idx, idx + 1);
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                                if let Err(e) = ctl.reorder_song_scenes(song_id, &ids).await {
                                                                                    warn!("Reorder failed: {e}");
                                                                                }
                                                                                refresh_song_scenes(song_id).await;
                                                                            });
                                                                        },
                                                                        span { class: "text-[9px]", "\u{2193}" }
                                                                    }
                                                                }
                                                            } else {
                                                                rsx! {}
                                                            }
                                                        }
                                                        button {
                                                            class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors",
                                                            title: "Delete",
                                                            onclick: move |_| {
                                                                spawn(async move {
                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                    if let Err(e) = ctl.delete_song_scene(scene_id).await {
                                                                        warn!("Delete scene failed: {e}");
                                                                    }
                                                                    refresh_song_scenes(song_id).await;
                                                                });
                                                            },
                                                            span { class: "text-[9px]", "\u{2715}" }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            div { class: "flex-1 flex items-center justify-center",
                                div { class: "text-center",
                                    div { class: "text-2xl text-zinc-700 mb-2", "\u{1F3B6}" }
                                    p { class: "text-sm text-zinc-500 font-medium", "Select a song" }
                                    p { class: "text-[10px] text-zinc-600 mt-1",
                                        "Songs define ordered scene sequences for live performance"
                                    }
                                }
                            }
                        }
                    },
                    BrowserTab::Setlists => rsx! {
                        if let Some(_setlist_id) = selected_setlist_id {
                            // Setlist song list
                            div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                                {
                                    let sl = setlists.iter().find(|s| s.id == _setlist_id);
                                    let name = sl.map(|s| s.name.as_str()).unwrap_or("Setlist");
                                    rsx! {
                                        span { class: "text-xs font-bold text-zinc-200 tracking-wide", "{name}" }
                                    }
                                }
                                {
                                    let count = setlist_songs_list.len();
                                    let sp = if count != 1 { "s" } else { "" };
                                    rsx! {
                                        span { class: "text-[10px] text-zinc-600 font-mono", "{count} song{sp}" }
                                    }
                                }
                            }

                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if setlist_songs_list.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center py-12",
                                            div { class: "text-lg text-zinc-700 mb-1", "\u{1F4CB}" }
                                            p { class: "text-xs text-zinc-500", "Empty setlist" }
                                            p { class: "text-[10px] text-zinc-600 mt-1", "Drag songs from the left to add them" }
                                        }
                                    }
                                } else {
                                    for (idx, sl_song) in setlist_songs_list.iter().enumerate() {
                                        {
                                            let ss_id = sl_song.id;
                                            let song_ref = songs.iter().find(|s| s.id == sl_song.song_id);
                                            let song_name = song_ref.map(|s| s.name.as_str()).unwrap_or("Unknown");
                                            let setlist_id = _setlist_id;

                                            rsx! {
                                                div {
                                                    key: "{ss_id}",
                                                    class: "px-4 py-2 flex items-center gap-3 border-b border-border/10 \
                                                            hover:bg-zinc-800/30 transition-colors group",
                                                    div { class: "w-8 text-center text-[10px] text-zinc-600 font-mono",
                                                        "{idx + 1}"
                                                    }
                                                    div { class: "flex-1 min-w-0",
                                                        span { class: "text-xs font-medium text-zinc-200 truncate block",
                                                            "{song_name}"
                                                        }
                                                    }
                                                    div { class: "flex items-center gap-1 opacity-0 group-hover:opacity-100 transition-opacity",
                                                        button {
                                                            class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors",
                                                            title: "Remove from setlist",
                                                            onclick: move |_| {
                                                                spawn(async move {
                                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                    if let Err(e) = ctl.remove_song_from_setlist(ss_id).await {
                                                                        warn!("Remove from setlist failed: {e}");
                                                                    }
                                                                    refresh_setlist_songs(setlist_id).await;
                                                                });
                                                            },
                                                            span { class: "text-[9px]", "\u{2715}" }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            div { class: "flex-1 flex items-center justify-center",
                                div { class: "text-center",
                                    div { class: "text-2xl text-zinc-700 mb-2", "\u{1F4CB}" }
                                    p { class: "text-sm text-zinc-500 font-medium", "Select a setlist" }
                                    p { class: "text-[10px] text-zinc-600 mt-1",
                                        "Setlists order songs for a show"
                                    }
                                }
                            }
                        }
                    },
                }

                // Status bar
                if !status.is_empty() {
                    div { class: "px-4 py-1 border-t border-border/30 flex-shrink-0",
                        span { class: "text-[9px] text-zinc-600 font-mono", "{status}" }
                    }
                }
            }

            // ══════════════════════════════════════════════════════════════
            // RIGHT PANEL — Presets / Add to Setlist
            // ══════════════════════════════════════════════════════════════
            div { class: "w-56 flex flex-col border-l border-border/30 bg-zinc-900/20 flex-shrink-0 h-full",
                div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                    span { class: "text-xs font-bold text-zinc-400 uppercase tracking-[0.15em]",
                        match browser_tab {
                            BrowserTab::Songs => "Available Presets",
                            BrowserTab::Setlists => "Available Songs",
                        }
                    }
                }

                div { class: "flex-1 overflow-y-auto min-h-0",
                    match browser_tab {
                        BrowserTab::Songs => rsx! {
                            for preset in presets.iter() {
                                {
                                    let pid = preset.id;
                                    let pname = preset.name.clone();
                                    rsx! {
                                        div {
                                            key: "{pid}",
                                            class: "px-3 py-2 cursor-pointer hover:bg-zinc-800/40 border-b border-border/10 transition-colors",
                                            title: "Click to assign to selected scene",
                                            onclick: move |_| {
                                                *SONG_STATUS.write() = format!("Selected: {pname}");
                                            },
                                            div { class: "flex items-center gap-2",
                                                div { class: "w-2 h-2 rounded-full bg-amber-500/40 flex-shrink-0" }
                                                span { class: "text-xs text-zinc-300 truncate", "{pname}" }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        BrowserTab::Setlists => rsx! {
                            for song in songs.iter() {
                                {
                                    let sid = song.id;
                                    let sname = song.name.clone();
                                    rsx! {
                                        div {
                                            key: "{sid}",
                                            class: "px-3 py-2 cursor-pointer hover:bg-zinc-800/40 border-b border-border/10 transition-colors",
                                            title: "Click to add to setlist",
                                            onclick: move |_| {
                                                if let Some(setlist_id) = *SELECTED_SETLIST_ID.read() {
                                                    let sort_order = SETLIST_SONGS.read().len() as i32;
                                                    spawn(async move {
                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                        match ctl.add_song_to_setlist(setlist_id, sid, sort_order).await {
                                                            Ok(_) => refresh_setlist_songs(setlist_id).await,
                                                            Err(e) => warn!("Add to setlist failed: {e}"),
                                                        }
                                                    });
                                                }
                                            },
                                            div { class: "flex items-center gap-2",
                                                div { class: "w-2 h-2 rounded-full bg-blue-500/40 flex-shrink-0" }
                                                span { class: "text-xs text-zinc-300 truncate", "{sname}" }
                                            }
                                        }
                                    }
                                }
                            }
                        },
                    }
                }

                // Quick reference
                div { class: "px-4 py-3 border-t border-border/30 flex-shrink-0",
                    div { class: "space-y-1",
                        p { class: "text-[9px] text-zinc-600 font-semibold uppercase tracking-wider mb-1", "Workflow" }
                        p { class: "text-[9px] text-zinc-500", "1. Create a song" }
                        p { class: "text-[9px] text-zinc-500", "2. Add scenes (verse, chorus, etc.)" }
                        p { class: "text-[9px] text-zinc-500", "3. Assign presets to each scene" }
                        p { class: "text-[9px] text-zinc-500", "4. Create setlists for shows" }
                    }
                }
            }
        }
    }
}

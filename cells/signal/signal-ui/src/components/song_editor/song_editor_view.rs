//! Song & Setlist Editor View — manage songs, their sections, and setlists.
//!
//! Left: Song/Setlist browser with toggle
//! Center: Song section table with preset/snapshot assignments, reordering
//! Right: Available presets picker + setlist composition

use crate::components::shared::EntityEditor;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::{performance_song, preset_entity, setlist, setlist_song, song_scene};
use uuid::Uuid;

/// Sentinel UUID for the virtual "All Songs" setlist.
const ALL_SONGS_ID: Uuid = Uuid::from_u128(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum BrowserTab {
    #[default]
    Songs,
    Setlists,
}

// ── Main Component ───────────────────────────────────────────────────────────

#[component]
pub fn SongEditorView() -> Element {
    // All editor state is component-local — fresh on each mount, no stale globals.
    let mut song_list = use_signal(Vec::<performance_song::Model>::new);
    let mut selected_song_id = use_signal(|| None::<Uuid>);
    let mut song_scenes = use_signal(Vec::<song_scene::Model>::new);
    let mut setlist_list = use_signal(Vec::<setlist::Model>::new);
    let mut selected_setlist_id = use_signal(|| None::<Uuid>);
    let mut setlist_songs = use_signal(Vec::<setlist_song::Model>::new);
    let mut song_available_presets = use_signal(Vec::<preset_entity::Model>::new);
    let mut song_status = use_signal(String::new);
    let mut browser_tab = use_signal(BrowserTab::default);

    let mut show_new_song_dialog = use_signal(|| false);
    let mut new_song_name = use_signal(String::new);
    let mut show_new_setlist_dialog = use_signal(|| false);
    let mut new_setlist_name = use_signal(String::new);

    // ── Async Refresh Helpers (capture local signals) ────────────────────────

    let refresh_songs = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_songs().await {
                Ok(songs) => song_list.set(songs),
                Err(e) => warn!("Failed to load songs: {e}"),
            }
        })
    };

    let refresh_song_scenes = move |song_id: Uuid| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_song_scenes(song_id).await {
                Ok(scenes) => {
                    let mut sorted = scenes;
                    sorted.sort_by_key(|s| s.sort_order);
                    song_scenes.set(sorted);
                }
                Err(e) => warn!("Failed to load song scenes: {e}"),
            }
        })
    };

    let refresh_setlists = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_setlists().await {
                Ok(setlists) => setlist_list.set(setlists),
                Err(e) => warn!("Failed to load setlists: {e}"),
            }
        })
    };

    let refresh_setlist_songs = move |setlist_id: Uuid| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_setlist_songs(setlist_id).await {
                Ok(songs) => {
                    let mut sorted = songs;
                    sorted.sort_by_key(|s| s.sort_order);
                    setlist_songs.set(sorted);
                }
                Err(e) => warn!("Failed to load setlist songs: {e}"),
            }
        })
    };

    let refresh_song_presets = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_rig_presets().await {
                Ok(presets) => song_available_presets.set(presets),
                Err(e) => warn!("Failed to load presets: {e}"),
            }
        })
    };

    // Load data on mount
    use_effect(move || {
        refresh_songs();
        refresh_setlists();
        refresh_song_presets();
    });

    // Load scenes when song selection changes
    use_effect(move || {
        let sid = *selected_song_id.read();
        if let Some(id) = sid {
            refresh_song_scenes(id);
        } else {
            song_scenes.write().clear();
        }
    });

    // Load setlist songs when setlist selection changes
    use_effect(move || {
        let sid = *selected_setlist_id.read();
        if let Some(id) = sid {
            if id == ALL_SONGS_ID {
                // Virtual setlist — no DB query needed, uses songs list directly
                setlist_songs.write().clear();
            } else {
                refresh_setlist_songs(id);
            }
        } else {
            setlist_songs.write().clear();
        }
    });

    // Clone data out of signals so read guards are dropped before event handlers.
    let songs = song_list.cloned();
    let sel_song_id = *selected_song_id.read();
    let scenes = song_scenes.cloned();
    let setlists = setlist_list.cloned();
    let sel_setlist_id = *selected_setlist_id.read();
    let setlist_songs_list = setlist_songs.cloned();
    let presets = song_available_presets.cloned();
    let status = song_status.cloned();
    let current_tab = *browser_tab.read();

    let selected_song = sel_song_id.and_then(|id| songs.iter().find(|s| s.id == id));

    rsx! {
        EntityEditor {
            left: rsx! {
                // Tab toggle
                div { class: "px-3 py-2 border-b border-border/30 flex-shrink-0",
                    div { class: "flex bg-zinc-800/80 rounded-lg p-0.5",
                        button {
                            class: if current_tab == BrowserTab::Songs {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold bg-primary text-primary-foreground transition-colors"
                            } else {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-400 hover:text-zinc-200 transition-colors"
                            },
                            onclick: move |_| browser_tab.set(BrowserTab::Songs),
                            "Songs"
                        }
                        button {
                            class: if current_tab == BrowserTab::Setlists {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold bg-primary text-primary-foreground transition-colors"
                            } else {
                                "flex-1 px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-400 hover:text-zinc-200 transition-colors"
                            },
                            onclick: move |_| browser_tab.set(BrowserTab::Setlists),
                            "Setlists"
                        }
                    }
                }

                // List content
                div { class: "flex-1 overflow-y-auto min-h-0",
                    match current_tab {
                        BrowserTab::Songs => rsx! {
                            for song in songs.iter() {
                                {
                                    let sid = song.id;
                                    let is_selected = sel_song_id == Some(sid);
                                    let sname = song.name.clone();
                                    let artist = song.artist.clone().unwrap_or_default();
                                    let song_is_template = song.is_template;
                                    rsx! {
                                        div {
                                            key: "{sid}",
                                            class: if is_selected {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-blue-500 bg-blue-500/10 transition-colors"
                                            } else {
                                                "px-3 py-2.5 cursor-pointer border-l-2 border-transparent hover:bg-zinc-800/40 transition-colors"
                                            },
                                            onclick: move |_| {
                                                selected_song_id.set(Some(sid));
                                            },
                                            div { class: "flex items-center justify-between",
                                                div { class: "flex-1 min-w-0",
                                                    div { class: "flex items-center gap-1.5",
                                                        span { class: "text-xs font-medium text-zinc-200 truncate", "{sname}" }
                                                        if song_is_template {
                                                            span { class: "text-[9px] px-1 py-0.5 rounded border border-dashed border-amber-500/50 text-amber-400/80 whitespace-nowrap leading-none",
                                                                "Template"
                                                            }
                                                        }
                                                    }
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
                                                                if *selected_song_id.read() == Some(sid) {
                                                                    selected_song_id.set(None);
                                                                }
                                                            });
                                                            refresh_songs();
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
                            // Virtual "All Songs" entry
                            {
                                let is_all_selected = sel_setlist_id == Some(ALL_SONGS_ID);
                                rsx! {
                                    div {
                                        class: if is_all_selected {
                                            "px-3 py-2.5 cursor-pointer border-l-2 border-amber-500 bg-amber-500/10 transition-colors"
                                        } else {
                                            "px-3 py-2.5 cursor-pointer border-l-2 border-transparent hover:bg-zinc-800/40 transition-colors"
                                        },
                                        onclick: move |_| {
                                            selected_setlist_id.set(Some(ALL_SONGS_ID));
                                        },
                                        div { class: "flex items-center gap-2",
                                            span { class: "text-[10px] text-amber-400/70", "\u{2605}" }
                                            span { class: "text-xs font-medium text-zinc-200 truncate", "All Songs" }
                                        }
                                    }
                                }
                            }
                            // Separator
                            div { class: "mx-3 border-b border-zinc-800/40" }
                            // Real setlists
                            for sl in setlists.iter() {
                                {
                                    let slid = sl.id;
                                    let is_selected = sel_setlist_id == Some(slid);
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
                                                selected_setlist_id.set(Some(slid));
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
                                                                if *selected_setlist_id.read() == Some(slid) {
                                                                    selected_setlist_id.set(None);
                                                                }
                                                            });
                                                            refresh_setlists();
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
                    match current_tab {
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
                                                let current_setlist = *selected_setlist_id.read();
                                                let setlist_count = setlist_songs.read().len() as i32;
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    match ctl.create_song(&val, None, false).await {
                                                        Ok(id) => {
                                                            selected_song_id.set(Some(id));
                                                            // Auto-create "Intro" section
                                                            if let Err(e) = ctl.add_song_scene(id, "Intro", Uuid::nil(), None, 0).await {
                                                                warn!("Auto-create Intro section failed: {e}");
                                                            }
                                                            // Auto-add to current setlist (skip the virtual "All Songs")
                                                            if let Some(sl_id) = current_setlist {
                                                                if sl_id != ALL_SONGS_ID {
                                                                    if let Err(e) = ctl.add_song_to_setlist(sl_id, id, setlist_count).await {
                                                                        warn!("Auto-add to setlist failed: {e}");
                                                                    }
                                                                }
                                                            }
                                                        }
                                                        Err(e) => warn!("Create song failed: {e}"),
                                                    }
                                                });
                                                refresh_songs();
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
                                                            selected_setlist_id.set(Some(id));
                                                        }
                                                        Err(e) => warn!("Create setlist failed: {e}"),
                                                    }
                                                });
                                                refresh_setlists();
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
            },
            center: rsx! {
                match current_tab {
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
                                        span { class: "text-[10px] text-zinc-600 font-mono", "{sc} section{sp}" }
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
                                    title: "Auto-advance to next section",
                                    onclick: {
                                        let song_id = song.id;
                                        let current = song.auto_advance;
                                        move |_| {
                                            spawn(async move {
                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                if let Err(e) = ctl.update_song(song_id, None, None, Some(!current), None, None).await {
                                                    warn!("Toggle auto-advance failed: {e}");
                                                }
                                            });
                                            refresh_songs();
                                        }
                                    },
                                    if song.auto_advance { "Auto \u{2713}" } else { "Auto" }
                                }
                                // Quick-add section (auto-named)
                                {
                                    let quick_song_id = song.id;
                                    let section_count = scenes.len();
                                    rsx! {
                                        button {
                                            class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                                    bg-blue-500/15 text-blue-300 border border-blue-500/25 \
                                                    hover:bg-blue-500/25 hover:border-blue-500/40 transition-all duration-150",
                                            onclick: move |_| {
                                                let name = format!("Section {}", section_count + 1);
                                                let sort = section_count as i32;
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    if let Err(e) = ctl.add_song_scene(quick_song_id, &name, Uuid::nil(), None, sort).await {
                                                        warn!("Quick-add section failed: {e}");
                                                    }
                                                });
                                                refresh_song_scenes(quick_song_id);
                                            },
                                            span { class: "text-blue-400", "+" }
                                            "Add Section"
                                        }
                                    }
                                }
                            }

                            // Scene table
                            div { class: "flex-1 overflow-y-auto min-h-0",
                                if scenes.is_empty() {
                                    div { class: "flex items-center justify-center h-full",
                                        div { class: "text-center py-12",
                                            div { class: "text-lg text-zinc-700 mb-1", "\u{1F3B5}" }
                                            p { class: "text-xs text-zinc-500", "No sections yet" }
                                            p { class: "text-[10px] text-zinc-600 mt-1", "Add sections to define the song structure" }
                                        }
                                    }
                                } else {
                                    // Table header
                                    div { class: "px-4 py-1.5 flex items-center gap-3 text-[9px] font-bold text-zinc-600 uppercase tracking-[0.1em] \
                                                  border-b border-border/20 bg-zinc-900/20 flex-shrink-0 sticky top-0",
                                        div { class: "w-8 text-center", "#" }
                                        div { class: "flex-1 min-w-0", "Section" }
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
                                            let song_id = sel_song_id.unwrap();

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
                                                                    let mut ids: Vec<Uuid> = song_scenes.read().iter().map(|s| s.id).collect();
                                                                    ids.swap(idx, idx - 1);
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                        if let Err(e) = ctl.reorder_song_scenes(song_id, &ids).await {
                                                                            warn!("Reorder failed: {e}");
                                                                        }
                                                                    });
                                                                    refresh_song_scenes(song_id);
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
                                                                            let mut ids: Vec<Uuid> = song_scenes.read().iter().map(|s| s.id).collect();
                                                                            ids.swap(idx, idx + 1);
                                                                            spawn(async move {
                                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                                if let Err(e) = ctl.reorder_song_scenes(song_id, &ids).await {
                                                                                    warn!("Reorder failed: {e}");
                                                                                }
                                                                            });
                                                                            refresh_song_scenes(song_id);
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
                                                                });
                                                                refresh_song_scenes(song_id);
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
                                        "Songs define ordered section sequences for live performance"
                                    }
                                }
                            }
                        }
                    },
                    BrowserTab::Setlists => rsx! {
                        if let Some(_setlist_id) = sel_setlist_id {
                            if _setlist_id == ALL_SONGS_ID {
                                // ── Virtual "All Songs" view ──
                                div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                                    span { class: "text-[10px] text-amber-400/70", "\u{2605}" }
                                    span { class: "text-xs font-bold text-zinc-200 tracking-wide", "All Songs" }
                                    {
                                        let count = songs.len();
                                        let sp = if count != 1 { "s" } else { "" };
                                        rsx! {
                                            span { class: "text-[10px] text-zinc-600 font-mono", "{count} song{sp}" }
                                        }
                                    }
                                }
                                div { class: "flex-1 overflow-y-auto min-h-0",
                                    if songs.is_empty() {
                                        div { class: "flex items-center justify-center h-full",
                                            div { class: "text-center py-12",
                                                div { class: "text-lg text-zinc-700 mb-1", "\u{1F3B6}" }
                                                p { class: "text-xs text-zinc-500", "No songs yet" }
                                                p { class: "text-[10px] text-zinc-600 mt-1", "Create a song from the Songs tab" }
                                            }
                                        }
                                    } else {
                                        {
                                            let mut sorted_songs: Vec<&performance_song::Model> = songs.iter().collect();
                                            sorted_songs.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));
                                            rsx! {
                                                for (idx, song) in sorted_songs.iter().enumerate() {
                                                    {
                                                        let sid = song.id;
                                                        let sname = song.name.clone();
                                                        let artist = song.artist.clone().unwrap_or_default();
                                                        rsx! {
                                                            div {
                                                                key: "{sid}",
                                                                class: "px-4 py-2 flex items-center gap-3 border-b border-border/10 \
                                                                        hover:bg-zinc-800/30 transition-colors",
                                                                div { class: "w-8 text-center text-[10px] text-zinc-600 font-mono",
                                                                    "{idx + 1}"
                                                                }
                                                                div { class: "flex-1 min-w-0",
                                                                    span { class: "text-xs font-medium text-zinc-200 truncate block",
                                                                        "{sname}"
                                                                    }
                                                                    if !artist.is_empty() {
                                                                        span { class: "text-[10px] text-zinc-500 truncate block", "{artist}" }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                                // ── Regular setlist view with reordering ──
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
                                                p { class: "text-[10px] text-zinc-600 mt-1", "Add songs from the right panel" }
                                            }
                                        }
                                    } else {
                                        for (idx, sl_song) in setlist_songs_list.iter().enumerate() {
                                            {
                                                let ss_id = sl_song.id;
                                                let song_ref = songs.iter().find(|s| s.id == sl_song.song_id);
                                                let song_name = song_ref.map(|s| s.name.as_str()).unwrap_or("Unknown");
                                                let setlist_id = _setlist_id;
                                                let song_count = setlist_songs_list.len();

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
                                                            // Move up
                                                            if idx > 0 {
                                                                button {
                                                                    class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                    title: "Move up",
                                                                    onclick: move |_| {
                                                                        let mut ids: Vec<Uuid> = setlist_songs.read().iter().map(|s| s.id).collect();
                                                                        ids.swap(idx, idx - 1);
                                                                        spawn(async move {
                                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                            if let Err(e) = ctl.reorder_setlist_songs(setlist_id, &ids).await {
                                                                                warn!("Reorder failed: {e}");
                                                                            }
                                                                        });
                                                                        refresh_setlist_songs(setlist_id);
                                                                    },
                                                                    span { class: "text-[9px]", "\u{2191}" }
                                                                }
                                                            }
                                                            // Move down
                                                            if idx < song_count - 1 {
                                                                button {
                                                                    class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                    title: "Move down",
                                                                    onclick: move |_| {
                                                                        let mut ids: Vec<Uuid> = setlist_songs.read().iter().map(|s| s.id).collect();
                                                                        ids.swap(idx, idx + 1);
                                                                        spawn(async move {
                                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                            if let Err(e) = ctl.reorder_setlist_songs(setlist_id, &ids).await {
                                                                                warn!("Reorder failed: {e}");
                                                                            }
                                                                        });
                                                                        refresh_setlist_songs(setlist_id);
                                                                    },
                                                                    span { class: "text-[9px]", "\u{2193}" }
                                                                }
                                                            }
                                                            // Remove
                                                            button {
                                                                class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors",
                                                                title: "Remove from setlist",
                                                                onclick: move |_| {
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                        if let Err(e) = ctl.remove_song_from_setlist(ss_id).await {
                                                                            warn!("Remove from setlist failed: {e}");
                                                                        }
                                                                    });
                                                                    refresh_setlist_songs(setlist_id);
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

            },
            right: Some(rsx! {
                div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                    span { class: "text-xs font-bold text-zinc-400 uppercase tracking-[0.15em]",
                        match current_tab {
                            BrowserTab::Songs => "Available Presets",
                            BrowserTab::Setlists => "Available Songs",
                        }
                    }
                }

                div { class: "flex-1 overflow-y-auto min-h-0",
                    match current_tab {
                        BrowserTab::Songs => rsx! {
                            for preset in presets.iter() {
                                {
                                    let pid = preset.id;
                                    let pname = preset.name.clone();
                                    rsx! {
                                        div {
                                            key: "{pid}",
                                            class: "px-3 py-2 cursor-pointer hover:bg-zinc-800/40 border-b border-border/10 transition-colors",
                                            title: "Click to assign to selected section",
                                            onclick: move |_| {
                                                song_status.set(format!("Selected: {pname}"));
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
                                                if let Some(setlist_id) = *selected_setlist_id.read() {
                                                    let sort_order = setlist_songs.read().len() as i32;
                                                    spawn(async move {
                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                        if let Err(e) = ctl.add_song_to_setlist(setlist_id, sid, sort_order).await {
                                                            warn!("Add to setlist failed: {e}");
                                                        }
                                                    });
                                                    refresh_setlist_songs(setlist_id);
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
                        p { class: "text-[9px] text-zinc-500", "2. Add sections (verse, chorus, etc.)" }
                        p { class: "text-[9px] text-zinc-500", "3. Assign presets to each section" }
                        p { class: "text-[9px] text-zinc-500", "4. Create setlists for shows" }
                    }
                }
            }),
            status: rsx! {
                if !status.is_empty() {
                    span { class: "text-[9px] text-zinc-600 font-mono", "{status}" }
                }
            },
        }
    }
}

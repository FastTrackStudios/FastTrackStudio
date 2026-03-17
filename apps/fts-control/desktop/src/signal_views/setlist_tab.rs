use dioxus::prelude::*;
use std::{collections::HashMap, rc::Rc};

use signal::setlist::{SetlistEntry, SetlistEntryId};

use super::SIGNAL_SELECTED_SETLIST_ID;

#[derive(Clone, PartialEq)]
struct SetlistSummary {
    id: String,
    name: String,
    song_count: usize,
}

#[derive(Clone, PartialEq)]
struct LibrarySong {
    id: String,
    name: String,
    section_count: usize,
}

#[derive(Clone, PartialEq)]
struct SetlistSong {
    entry_id: String,
    song_id: String,
    name: String,
    section_count: usize,
}

#[component]
pub(crate) fn SignalSetlistTab() -> Element {
    let signal = signal_ui::use_signal_service();

    let mut setlists = use_signal(Vec::<SetlistSummary>::new);
    let mut selected_setlist_id = use_signal(|| None::<String>);
    let mut selected_library_song_id = use_signal(|| None::<String>);
    let mut selected_entry_id = use_signal(|| None::<String>);
    let mut dragging_library_song_id = use_signal(|| None::<String>);
    let mut dragging_setlist_entry_id = use_signal(|| None::<String>);
    let mut drop_target_entry_id = use_signal(|| None::<String>);
    let mut setlist_drop_active = use_signal(|| false);
    let mut library_filter = use_signal(String::new);
    let mut library_songs = use_signal(Vec::<LibrarySong>::new);
    let mut setlist_songs = use_signal(Vec::<SetlistSong>::new);

    let refresh: Rc<dyn Fn(Option<String>)> = Rc::new({
        let signal = signal.clone();
        move |preferred_setlist_id: Option<String>| {
            let signal = signal.clone();
            spawn(async move {
                let all_songs = signal.songs().list().await.unwrap_or_default();
                let song_lookup: HashMap<String, (String, usize)> = all_songs
                    .iter()
                    .map(|s| (s.id.to_string(), (s.name.clone(), s.sections.len())))
                    .collect();

                library_songs.set(
                    all_songs
                        .iter()
                        .map(|s| LibrarySong {
                            id: s.id.to_string(),
                            name: s.name.clone(),
                            section_count: s.sections.len(),
                        })
                        .collect(),
                );

                let all_setlists = signal.setlists().list().await.unwrap_or_default();
                let summaries: Vec<SetlistSummary> = all_setlists
                    .iter()
                    .map(|s| SetlistSummary {
                        id: s.id.to_string(),
                        name: s.name.clone(),
                        song_count: s.entries.len(),
                    })
                    .collect();
                let summary_ids: std::collections::HashSet<String> =
                    summaries.iter().map(|s| s.id.clone()).collect();

                let chosen = preferred_setlist_id
                    .or_else(|| selected_setlist_id())
                    .or_else(|| {
                        let global = SIGNAL_SELECTED_SETLIST_ID();
                        if global == "all" {
                            None
                        } else {
                            Some(global)
                        }
                    })
                    .filter(|id| summary_ids.contains(id))
                    .or_else(|| summaries.first().map(|s| s.id.clone()));

                setlists.set(summaries);
                selected_setlist_id.set(chosen.clone());
                selected_entry_id.set(None);

                if selected_library_song_id().is_none() {
                    selected_library_song_id.set(
                        all_songs
                            .first()
                            .map(|song| song.id.to_string())
                            .or_else(|| None),
                    );
                }

                if let Some(setlist_id) = chosen {
                    if let Some(setlist) = signal
                        .setlists()
                        .load(setlist_id.clone())
                        .await
                        .ok()
                        .flatten()
                    {
                        setlist_songs.set(
                            setlist
                                .entries
                                .iter()
                                .map(|entry| {
                                    let (song_name, section_count) = song_lookup
                                        .get(&entry.song_id.to_string())
                                        .cloned()
                                        .unwrap_or_else(|| ("Missing song".to_string(), 0));
                                    SetlistSong {
                                        entry_id: entry.id.to_string(),
                                        song_id: entry.song_id.to_string(),
                                        name: if entry.name.trim().is_empty() {
                                            song_name
                                        } else {
                                            entry.name.clone()
                                        },
                                        section_count,
                                    }
                                })
                                .collect(),
                        );
                    } else {
                        setlist_songs.set(Vec::new());
                    }
                } else {
                    setlist_songs.set(Vec::new());
                }
            });
        }
    });

    {
        let refresh = refresh.clone();
        use_effect(move || {
            refresh(None);
        });
    }

    let setlist_song_ids: std::collections::HashSet<String> =
        setlist_songs().iter().map(|s| s.song_id.clone()).collect();
    let filtered_library: Vec<LibrarySong> = {
        let q = library_filter().trim().to_lowercase();
        library_songs()
            .iter()
            .filter(|s| q.is_empty() || s.name.to_lowercase().contains(&q))
            .cloned()
            .collect()
    };

    let can_add_song = selected_setlist_id().is_some()
        && selected_library_song_id()
            .as_ref()
            .is_some_and(|sid| !setlist_song_ids.contains(sid));
    let can_remove_song = selected_setlist_id().is_some() && selected_entry_id().is_some();
    let top_slot_target_id = "__top_slot__".to_string();

    let persist_reorder: Rc<dyn Fn(String, Vec<String>)> = Rc::new({
        let signal = signal.clone();
        move |setlist_id: String, ordered_ids: Vec<String>| {
            let signal = signal.clone();
            spawn(async move {
                if let Ok(Some(mut setlist)) = signal.setlists().load(setlist_id).await {
                    let pos: HashMap<String, usize> = ordered_ids
                        .iter()
                        .enumerate()
                        .map(|(idx, id)| (id.clone(), idx))
                        .collect();
                    setlist.entries.sort_by_key(|entry| {
                        pos.get(&entry.id.to_string())
                            .copied()
                            .unwrap_or(usize::MAX)
                    });
                    let _ = signal.setlists().save(setlist).await;
                }
            });
        }
    });

    let drop_library_song: Rc<dyn Fn(String, Option<String>)> = Rc::new({
        let signal = signal.clone();
        let refresh = refresh.clone();
        move |song_id: String, before_entry_id: Option<String>| {
            let signal = signal.clone();
            let refresh = refresh.clone();
            let Some(setlist_id) = selected_setlist_id() else {
                return;
            };
            spawn(async move {
                let Ok(Some(song)) = signal.songs().load(song_id).await else {
                    return;
                };
                let Ok(Some(mut setlist)) = signal.setlists().load(setlist_id.clone()).await else {
                    return;
                };
                if setlist
                    .entries
                    .iter()
                    .any(|entry| entry.song_id.to_string() == song.id.to_string())
                {
                    return;
                }
                let new_entry =
                    SetlistEntry::new(SetlistEntryId::new(), song.name.clone(), song.id.clone());
                if let Some(target_id) = before_entry_id {
                    if let Some(target_pos) = setlist
                        .entries
                        .iter()
                        .position(|entry| entry.id.to_string() == target_id)
                    {
                        setlist.entries.insert(target_pos, new_entry);
                    } else {
                        setlist.entries.push(new_entry);
                    }
                } else {
                    setlist.entries.push(new_entry);
                }
                let _ = signal.setlists().save(setlist).await;
                refresh(Some(setlist_id));
            });
        }
    });

    rsx! {
        div { class: "h-full w-full flex flex-col bg-zinc-950 text-zinc-100",
            div { class: "px-4 py-3 border-b border-zinc-800 flex items-center justify-between gap-3 flex-shrink-0",
                div { class: "flex items-center gap-2 min-w-0",
                    h2 { class: "text-sm font-semibold tracking-wide uppercase text-zinc-300", "Setlist Manager" }
                    span { class: "text-xs text-zinc-500", "{setlist_songs().len()} songs in active setlist" }
                }
                div { class: "flex items-center gap-2",
                    {
                        let refresh = refresh.clone();
                        rsx! {
                            signal_ui::components::CreateSetlistButton {
                                on_created: move |_| refresh(None),
                            }
                        }
                    }
                    {
                        let refresh = refresh.clone();
                        rsx! {
                            button {
                                class: "px-2 py-1 text-xs rounded bg-zinc-800 hover:bg-zinc-700 border border-zinc-700",
                                onclick: move |_| refresh(selected_setlist_id()),
                                "Refresh"
                            }
                        }
                    }
                }
            }

            div { class: "flex-1 min-h-0 flex",
                // Setlists + current setlist entries
                div { class: "w-1/2 min-w-0 h-full border-r border-zinc-800 flex flex-col",
                    div { class: "px-4 py-2 border-b border-zinc-800 flex items-center gap-2",
                        span { class: "text-[11px] font-semibold uppercase tracking-wider text-zinc-500", "Setlists" }
                        div { class: "flex flex-wrap gap-1",
                            for setlist in setlists().iter() {
                                {
                                    let sid = setlist.id.clone();
                                    let is_selected = selected_setlist_id().as_deref() == Some(sid.as_str());
                                    let refresh = refresh.clone();
                                    rsx! {
                                        button {
                                            class: if is_selected {
                                                "px-2 py-1 text-xs rounded bg-blue-600 text-white"
                                            } else {
                                                "px-2 py-1 text-xs rounded bg-zinc-800 hover:bg-zinc-700 text-zinc-300"
                                            },
                                            onclick: move |_| {
                                                *SIGNAL_SELECTED_SETLIST_ID.write() = sid.clone();
                                                refresh(Some(sid.clone()))
                                            },
                                            "{setlist.name} ({setlist.song_count})"
                                        }
                                    }
                                }
                            }
                        }
                    }

                    div { class: "px-4 py-2 border-b border-zinc-800 flex items-center justify-between",
                        h3 { class: "text-xs uppercase tracking-wider text-zinc-400", "Current Setlist Songs" }
                        div { class: "flex items-center gap-1",
                            {
                                let refresh = refresh.clone();
                                let signal = signal.clone();
                                rsx! {
                                    button {
                                        class: if can_remove_song {
                                            "px-2 py-1 text-[11px] rounded bg-red-700 hover:bg-red-600 text-white"
                                        } else {
                                            "px-2 py-1 text-[11px] rounded bg-zinc-800 text-zinc-500 cursor-not-allowed"
                                        },
                                        disabled: !can_remove_song,
                                        onclick: move |_| {
                                            let Some(setlist_id) = selected_setlist_id() else { return; };
                                            let Some(entry_id) = selected_entry_id() else { return; };
                                            let refresh = refresh.clone();
                                            let signal = signal.clone();
                                            spawn(async move {
                                                let _ = signal.setlists().remove_entry(setlist_id.clone(), entry_id).await;
                                                refresh(Some(setlist_id));
                                            });
                                        },
                                        "Remove"
                                    }
                                }
                            }
                            {
                                let signal = signal.clone();
                                rsx! {
                                    button {
                                        class: if can_remove_song {
                                            "px-2 py-1 text-[11px] rounded bg-zinc-700 hover:bg-zinc-600 text-zinc-100"
                                        } else {
                                            "px-2 py-1 text-[11px] rounded bg-zinc-800 text-zinc-500 cursor-not-allowed"
                                        },
                                        disabled: !can_remove_song,
                                        onclick: move |_| {
                                            let Some(setlist_id) = selected_setlist_id() else { return; };
                                            let Some(entry_id) = selected_entry_id() else { return; };
                                            let mut rows = setlist_songs();
                                            let Some(index) = rows.iter().position(|r| r.entry_id == entry_id) else { return; };
                                            if index == 0 {
                                                return;
                                            }
                                            rows.swap(index, index - 1);
                                            let ordered_ids: Vec<String> = rows.iter().map(|r| r.entry_id.clone()).collect();
                                            let moved_id = rows[index - 1].entry_id.clone();
                                            setlist_songs.set(rows);
                                            selected_entry_id.set(Some(moved_id));
                                            let signal = signal.clone();
                                            spawn(async move {
                                                if let Ok(Some(mut setlist)) = signal.setlists().load(setlist_id.clone()).await {
                                                    let pos: HashMap<String, usize> = ordered_ids
                                                        .iter()
                                                        .enumerate()
                                                        .map(|(idx, id)| (id.clone(), idx))
                                                        .collect();
                                                    setlist.entries.sort_by_key(|entry| {
                                                        pos.get(&entry.id.to_string()).copied().unwrap_or(usize::MAX)
                                                    });
                                                    let _ = signal.setlists().save(setlist).await;
                                                }
                                            });
                                        },
                                        "Up"
                                    }
                                }
                            }
                            {
                                let signal = signal.clone();
                                rsx! {
                                    button {
                                        class: if can_remove_song {
                                            "px-2 py-1 text-[11px] rounded bg-zinc-700 hover:bg-zinc-600 text-zinc-100"
                                        } else {
                                            "px-2 py-1 text-[11px] rounded bg-zinc-800 text-zinc-500 cursor-not-allowed"
                                        },
                                        disabled: !can_remove_song,
                                        onclick: move |_| {
                                            let Some(setlist_id) = selected_setlist_id() else { return; };
                                            let Some(entry_id) = selected_entry_id() else { return; };
                                            let mut rows = setlist_songs();
                                            let Some(index) = rows.iter().position(|r| r.entry_id == entry_id) else { return; };
                                            if index + 1 >= rows.len() {
                                                return;
                                            }
                                            rows.swap(index, index + 1);
                                            let ordered_ids: Vec<String> = rows.iter().map(|r| r.entry_id.clone()).collect();
                                            let moved_id = rows[index + 1].entry_id.clone();
                                            setlist_songs.set(rows);
                                            selected_entry_id.set(Some(moved_id));
                                            let signal = signal.clone();
                                            spawn(async move {
                                                if let Ok(Some(mut setlist)) = signal.setlists().load(setlist_id.clone()).await {
                                                    let pos: HashMap<String, usize> = ordered_ids
                                                        .iter()
                                                        .enumerate()
                                                        .map(|(idx, id)| (id.clone(), idx))
                                                        .collect();
                                                    setlist.entries.sort_by_key(|entry| {
                                                        pos.get(&entry.id.to_string()).copied().unwrap_or(usize::MAX)
                                                    });
                                                    let _ = signal.setlists().save(setlist).await;
                                                }
                                            });
                                        },
                                        "Down"
                                    }
                                }
                            }
                        }
                    }

                    {
                        let persist_reorder = persist_reorder.clone();
                        let drop_library_song = drop_library_song.clone();
                        rsx! {
                    div {
                        class: if setlist_drop_active() {
                            "flex-1 min-h-0 overflow-y-auto border-2 border-dashed border-blue-400/60 bg-blue-500/5"
                        } else {
                            "flex-1 min-h-0 overflow-y-auto"
                        },
                        ondragover: move |e| {
                            e.prevent_default();
                            setlist_drop_active.set(true);
                        },
                        ondragleave: move |_| {
                            setlist_drop_active.set(false);
                            drop_target_entry_id.set(None);
                        },
                        ondrop: move |e| {
                            e.prevent_default();
                            setlist_drop_active.set(false);
                            let dragged_entry = dragging_setlist_entry_id();
                            let dragged_song = dragging_library_song_id();
                            if let Some(entry_id) = dragged_entry {
                                let mut rows = setlist_songs();
                                let Some(index) = rows.iter().position(|r| r.entry_id == entry_id) else { return; };
                                if index + 1 != rows.len() {
                                    let moved = rows.remove(index);
                                    rows.push(moved.clone());
                                    let ordered_ids: Vec<String> =
                                        rows.iter().map(|r| r.entry_id.clone()).collect();
                                    setlist_songs.set(rows);
                                    selected_entry_id.set(Some(moved.entry_id));
                                    if let Some(setlist_id) = selected_setlist_id() {
                                        persist_reorder.clone()(setlist_id, ordered_ids);
                                    }
                                }
                            } else if let Some(song_id) = dragged_song {
                                drop_library_song.clone()(song_id, None);
                            }
                            dragging_setlist_entry_id.set(None);
                            dragging_library_song_id.set(None);
                            drop_target_entry_id.set(None);
                        },
                        if setlist_songs().is_empty() {
                            div { class: "p-4 text-xs text-zinc-500", "No songs in this setlist yet." }
                        } else {
                            {
                                let top_slot_target_id = top_slot_target_id.clone();
                                let drop_library_song = drop_library_song.clone();
                                let persist_reorder = persist_reorder.clone();
                                let is_top_drop_target = drop_target_entry_id()
                                    .as_deref()
                                    == Some(top_slot_target_id.as_str());
                                rsx! {
                                    div {
                                        class: if is_top_drop_target {
                                            "mx-4 mt-2 mb-1 h-6 rounded border border-blue-400 bg-blue-500/15 text-[10px] text-blue-300 uppercase tracking-wider flex items-center justify-center"
                                        } else {
                                            "mx-4 mt-2 mb-1 h-6 rounded border border-zinc-700/80 bg-zinc-900/60 text-[10px] text-zinc-500 uppercase tracking-wider flex items-center justify-center"
                                        },
                                        ondragover: move |e| {
                                            e.prevent_default();
                                            setlist_drop_active.set(true);
                                            drop_target_entry_id.set(Some(top_slot_target_id.clone()));
                                        },
                                        ondrop: move |e| {
                                            e.prevent_default();
                                            setlist_drop_active.set(false);
                                            let dragged_entry = dragging_setlist_entry_id();
                                            let dragged_song = dragging_library_song_id();
                                            if let Some(song_id) = dragged_song {
                                                let first_entry_id = setlist_songs()
                                                    .first()
                                                    .map(|r| r.entry_id.clone());
                                                drop_library_song.clone()(song_id, first_entry_id);
                                            } else if let Some(entry_id) = dragged_entry {
                                                let mut rows = setlist_songs();
                                                let Some(from_idx) =
                                                    rows.iter().position(|r| r.entry_id == entry_id)
                                                else {
                                                    return;
                                                };
                                                if from_idx > 0 {
                                                    let moved = rows.remove(from_idx);
                                                    rows.insert(0, moved.clone());
                                                    let ordered_ids: Vec<String> =
                                                        rows.iter().map(|r| r.entry_id.clone()).collect();
                                                    setlist_songs.set(rows);
                                                    selected_entry_id.set(Some(moved.entry_id));
                                                    if let Some(setlist_id) = selected_setlist_id() {
                                                        persist_reorder.clone()(setlist_id, ordered_ids);
                                                    }
                                                }
                                            }
                                            dragging_setlist_entry_id.set(None);
                                            dragging_library_song_id.set(None);
                                            drop_target_entry_id.set(None);
                                        },
                                        "Drop To Top"
                                    }
                                }
                            }
                            for (idx, item) in setlist_songs().iter().enumerate() {
                                {
                                    let entry_id = item.entry_id.clone();
                                    let is_selected = selected_entry_id().as_deref() == Some(entry_id.as_str());
                                    let is_drop_target =
                                        drop_target_entry_id().as_deref() == Some(entry_id.as_str());
                                    let drag_start_id = entry_id.clone();
                                    let drag_over_id = entry_id.clone();
                                    let drop_target_id = entry_id.clone();
                                    let click_id = entry_id.clone();
                                    let drop_library_song = drop_library_song.clone();
                                    let persist_reorder = persist_reorder.clone();
                                    rsx! {
                                        button {
                                            key: "{entry_id}",
                                            class: if is_selected {
                                                if is_drop_target {
                                                    "w-full text-left px-4 py-2 border-b border-blue-400 bg-zinc-800/80"
                                                } else {
                                                    "w-full text-left px-4 py-2 border-b border-zinc-800 bg-zinc-800/80"
                                                }
                                            } else {
                                                if is_drop_target {
                                                    "w-full text-left px-4 py-2 border-b border-blue-400 hover:bg-zinc-900"
                                                } else {
                                                    "w-full text-left px-4 py-2 border-b border-zinc-800 hover:bg-zinc-900"
                                                }
                                            },
                                            draggable: true,
                                            ondragstart: move |_| {
                                                dragging_setlist_entry_id.set(Some(drag_start_id.clone()));
                                                dragging_library_song_id.set(None);
                                            },
                                            ondragend: move |_| {
                                                dragging_setlist_entry_id.set(None);
                                                setlist_drop_active.set(false);
                                                drop_target_entry_id.set(None);
                                            },
                                            ondragover: move |e| {
                                                e.prevent_default();
                                                setlist_drop_active.set(true);
                                                drop_target_entry_id.set(Some(drag_over_id.clone()));
                                            },
                                            ondrop: move |e| {
                                                e.prevent_default();
                                                setlist_drop_active.set(false);
                                                if let Some(song_id) = dragging_library_song_id() {
                                                    drop_library_song.clone()(song_id, Some(drop_target_id.clone()));
                                                    dragging_library_song_id.set(None);
                                                    dragging_setlist_entry_id.set(None);
                                                    drop_target_entry_id.set(None);
                                                    return;
                                                }
                                                let Some(dragged_entry_id) = dragging_setlist_entry_id() else {
                                                    return;
                                                };
                                                if dragged_entry_id == drop_target_id {
                                                    return;
                                                }
                                                let mut rows = setlist_songs();
                                                let Some(from_idx) = rows.iter().position(|r| r.entry_id == dragged_entry_id) else {
                                                    return;
                                                };
                                                let Some(to_idx) = rows.iter().position(|r| r.entry_id == drop_target_id) else {
                                                    return;
                                                };
                                                let moved = rows.remove(from_idx);
                                                let insert_idx = if from_idx < to_idx { to_idx - 1 } else { to_idx };
                                                rows.insert(insert_idx, moved.clone());
                                                let ordered_ids: Vec<String> = rows.iter().map(|r| r.entry_id.clone()).collect();
                                                setlist_songs.set(rows);
                                                selected_entry_id.set(Some(moved.entry_id));
                                                if let Some(setlist_id) = selected_setlist_id() {
                                                    persist_reorder.clone()(setlist_id, ordered_ids);
                                                }
                                                dragging_setlist_entry_id.set(None);
                                                dragging_library_song_id.set(None);
                                                drop_target_entry_id.set(None);
                                            },
                                            onclick: move |_| selected_entry_id.set(Some(click_id.clone())),
                                            div { class: "flex items-center justify-between gap-2",
                                                div { class: "flex items-center gap-2 w-14 flex-shrink-0",
                                                    span {
                                                        class: "text-zinc-500 text-sm leading-none cursor-grab select-none",
                                                        title: "Drag to reorder",
                                                        "⋮⋮"
                                                    }
                                                    span { class: "text-xs text-zinc-500 w-5 text-right", "{idx + 1}" }
                                                }
                                                div { class: "flex-1 min-w-0",
                                                    div { class: "text-sm text-zinc-100 truncate", "{item.name}" }
                                                    div { class: "text-[11px] text-zinc-500 truncate", "{item.section_count} sections" }
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
                }

                // Song library
                div { class: "w-1/2 min-w-0 h-full flex flex-col",
                    div { class: "px-4 py-2 border-b border-zinc-800 flex items-center justify-between gap-2",
                        h3 { class: "text-xs uppercase tracking-wider text-zinc-400", "Song Library" }
                        {
                            let refresh = refresh.clone();
                            let signal = signal.clone();
                            rsx! {
                                button {
                                    class: if can_add_song {
                                        "px-2 py-1 text-[11px] rounded bg-blue-600 hover:bg-blue-500 text-white"
                                    } else {
                                        "px-2 py-1 text-[11px] rounded bg-zinc-800 text-zinc-500 cursor-not-allowed"
                                    },
                                    disabled: !can_add_song,
                                    onclick: move |_| {
                                        let Some(setlist_id) = selected_setlist_id() else { return; };
                                        let Some(song_id) = selected_library_song_id() else { return; };
                                        let refresh = refresh.clone();
                                        let signal = signal.clone();
                                        spawn(async move {
                                            if let Ok(Some(song)) = signal.songs().load(song_id.clone()).await {
                                                let entry = SetlistEntry::new(
                                                    SetlistEntryId::new(),
                                                    song.name.clone(),
                                                    song.id.clone(),
                                                );
                                                let _ = signal.setlists().add_entry(setlist_id.clone(), entry).await;
                                                refresh(Some(setlist_id));
                                            }
                                        });
                                    },
                                    "Add To Setlist"
                                }
                            }
                        }
                    }

                    div { class: "px-4 py-2 border-b border-zinc-800",
                        input {
                            class: "w-full px-2 py-1.5 rounded bg-zinc-900 border border-zinc-700 text-xs text-zinc-100 placeholder:text-zinc-500 outline-none",
                            placeholder: "Filter songs...",
                            value: "{library_filter}",
                            oninput: move |e| library_filter.set(e.value()),
                        }
                    }

                    div { class: "flex-1 min-h-0 overflow-y-auto",
                        if filtered_library.is_empty() {
                            div { class: "p-4 text-xs text-zinc-500", "No songs match the current filter." }
                        } else {
                            for song in filtered_library.iter() {
                                {
                                    let song_id = song.id.clone();
                                    let is_selected = selected_library_song_id().as_deref() == Some(song_id.as_str());
                                    let already_in_setlist = setlist_song_ids.contains(song_id.as_str());
                                    let drag_song_id = song_id.clone();
                                    let click_song_id = song_id.clone();
                                    rsx! {
                                        button {
                                            key: "{song_id}",
                                            class: if is_selected {
                                                "w-full text-left px-4 py-2 border-b border-zinc-800 bg-zinc-800/80"
                                            } else {
                                                "w-full text-left px-4 py-2 border-b border-zinc-800 hover:bg-zinc-900"
                                            },
                                            draggable: true,
                                            ondragstart: move |_| {
                                                dragging_library_song_id.set(Some(drag_song_id.clone()));
                                                dragging_setlist_entry_id.set(None);
                                                setlist_drop_active.set(true);
                                            },
                                            ondragend: move |_| {
                                                dragging_library_song_id.set(None);
                                                setlist_drop_active.set(false);
                                            },
                                            onclick: move |_| selected_library_song_id.set(Some(click_song_id.clone())),
                                            div { class: "flex items-center justify-between gap-2",
                                                div { class: "min-w-0 flex-1",
                                                    div { class: "text-sm text-zinc-100 truncate", "{song.name}" }
                                                    div { class: "text-[11px] text-zinc-500", "{song.section_count} sections" }
                                                }
                                                if already_in_setlist {
                                                    span { class: "text-[10px] px-1.5 py-0.5 rounded bg-blue-500/20 text-blue-300 border border-blue-500/30", "In Setlist" }
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
        }
    }
}

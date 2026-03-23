use dioxus::prelude::*;
use input_dioxus::TEXT_INPUT_FOCUS_COUNT;
use std::rc::Rc;

use lumen_blocks::components::dropdown::{
    Dropdown, DropdownContent, DropdownItem, DropdownTrigger,
};

use super::EditTarget;
use crate::signal_views::ManageProfileItem;
use signal_ui::views::{SectionEntry, SongEntry};

/// Renders the Song mode right panel: Sections (top) + Songs (bottom).
pub(crate) fn render_song_panel(
    // Section data
    song_sections: Signal<Vec<SectionEntry>>,
    selected_section_id: Signal<Option<String>>,
    active_song_name: Signal<String>,
    selected_song_id: Signal<Option<String>>,
    // Song list
    songs: Signal<Vec<SongEntry>>,
    setlist_options: Signal<Vec<(String, String)>>,
    selected_setlist_id: Signal<String>,
    // Profile data for "New Song from Profile" dropdown + base profile display
    manage_profiles: Signal<Vec<ManageProfileItem>>,
    active_base_profile_name: Signal<Option<String>>,
    // Create callbacks
    mut on_section_created: impl FnMut() + 'static + Clone,
    mut on_song_created: impl FnMut() + 'static + Clone,
    mut on_setlist_created: impl FnMut() + 'static + Clone,
    mut on_base_profile_changed: impl FnMut() + 'static + Clone,
    // Inline editing
    mut editing_target: Signal<Option<EditTarget>>,
    mut editing_text: Signal<String>,
    commit_rename: Rc<dyn Fn()>,
    // Context menu
    mut ctx_target: Signal<Option<EditTarget>>,
    mut ctx_pos: Signal<(f64, f64)>,
    // Callbacks
    mut navigate_to_section: impl FnMut(String) + 'static + Clone,
    mut load_song_sections: impl FnMut(String) + 'static + Clone,
    mut change_setlist: impl FnMut(String) + 'static + Clone,
    // Service
    signal: signal::Signal,
) -> Element {
    rsx! {
        div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
            // ── Sections (top) ──
            div { class: "flex-[3] min-h-0 flex flex-col border-b border-border",
                div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 flex flex-col gap-1",
                    div { class: "flex items-center justify-between",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                            "{active_song_name}"
                        }
                        {
                            let signal = signal.clone();
                            let mut cb = on_section_created.clone();
                        rsx! {
                            button {
                                class: "px-2 py-0.5 text-[10px] rounded bg-blue-600 hover:bg-blue-700 text-white font-medium",
                                onclick: move |_| {
                                    let signal = signal.clone();
                                    let mut cb = cb.clone();
                                    if let Some(song_id) = selected_song_id() {
                                        spawn(async move {
                                            let profiles = signal.profiles().list().await.unwrap_or_default();
                                            if let Some(prof) = profiles.first() {
                                                if let Some(patch) = prof.patches.first() {
                                                    use signal::song::{Section, SectionId};
                                                    let section = Section::from_patch(
                                                        SectionId::new(),
                                                        "New Section",
                                                        patch.id.clone(),
                                                    );
                                                    let _ = signal.songs().add_section(song_id, section).await;
                                                    cb();
                                                }
                                            }
                                        });
                                    }
                                },
                                "+ Section"
                            }
                        }
                    }
                    }
                    // Base profile label + change dropdown
                    if active_base_profile_name().is_some() || selected_song_id().is_some() {
                        {
                            let signal = signal.clone();
                            let mut cb = on_base_profile_changed.clone();
                            let current_name = active_base_profile_name().unwrap_or_else(|| "None".to_string());
                            rsx! {
                                div { class: "flex items-center gap-1.5",
                                    span { class: "text-[10px] text-zinc-500", "Base:" }
                                    Dropdown {
                                        DropdownTrigger {
                                            button {
                                                class: "text-[10px] text-zinc-300 hover:text-zinc-100 bg-zinc-800/50 hover:bg-zinc-700 border border-zinc-700/50 rounded px-1.5 py-0.5 transition-colors",
                                                "{current_name}"
                                                span { class: "text-zinc-500 ml-0.5", "\u{25BE}" }
                                            }
                                        }
                                        DropdownContent {
                                            width: "w-48".to_string(),
                                            // "None" option to remove base profile
                                            {
                                                let signal = signal.clone();
                                                let mut cb = cb.clone();
                                                rsx! {
                                                    DropdownItem {
                                                        value: "none".to_string(),
                                                        index: 0usize,
                                                        on_select: move |_: String| {
                                                            let signal = signal.clone();
                                                            let mut cb = cb.clone();
                                                            if let Some(song_id) = selected_song_id() {
                                                                spawn(async move {
                                                                    // Clear base_profile_id by loading, clearing, saving
                                                                    if let Ok(Some(mut song)) = signal.songs().load(song_id.as_str()).await {
                                                                        song.metadata.base_profile_id = None;
                                                                        let _ = signal.songs().save(song).await;
                                                                        cb();
                                                                    }
                                                                });
                                                            }
                                                        },
                                                        "None"
                                                    }
                                                }
                                            }
                                            for (idx, prof) in manage_profiles().iter().enumerate() {
                                                {
                                                    let prof_id = prof.id.clone();
                                                    let prof_name = prof.name.clone();
                                                    let signal = signal.clone();
                                                    let mut cb = cb.clone();
                                                    rsx! {
                                                        DropdownItem {
                                                            value: prof_id.clone(),
                                                            index: idx + 1,
                                                            on_select: move |pid: String| {
                                                                let signal = signal.clone();
                                                                let mut cb = cb.clone();
                                                                if let Some(song_id) = selected_song_id() {
                                                                    spawn(async move {
                                                                        match signal.songs().change_base_profile(song_id.as_str(), pid.as_str()).await {
                                                                            Ok(_) => {
                                                                                tracing::info!("Changed base profile");
                                                                                cb();
                                                                            }
                                                                            Err(e) => {
                                                                                tracing::error!("Failed to change base profile: {e:?}");
                                                                            }
                                                                        }
                                                                    });
                                                                }
                                                            },
                                                            "{prof_name}"
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
                div { class: "flex-1 overflow-y-auto",
                    div { class: "flex flex-col gap-1 p-2",
                        for (i, section) in song_sections().iter().enumerate() {
                            {
                                let section_id = section.id.clone();
                                let is_selected = selected_section_id() == Some(section.id.clone());
                                let selected_class = if is_selected {
                                    "border-primary bg-primary/5"
                                } else {
                                    "border-border hover:border-border/80"
                                };
                                let mut nav = navigate_to_section.clone();
                                let nav_id = section_id.clone();
                                let song_id_for_ctx = selected_song_id().unwrap_or_default();
                                let ctx_section_target = EditTarget::Section { song_id: song_id_for_ctx.clone(), section_id: section_id.clone() };
                                let edit_target = EditTarget::Section { song_id: song_id_for_ctx, section_id: section_id.clone() };
                                let is_editing = editing_target() == Some(edit_target.clone());
                                let section_name = section.name.clone();
                                let commit = commit_rename.clone();
                                let commit_blur = commit.clone();
                                let scene_label = section.rig_scene_name.clone();
                                let patch_label = section.profile_patch_name.clone();
                                let override_accent = if section.is_base_profile_section == Some(false) {
                                    "border-l-2 border-l-amber-500"
                                } else {
                                    ""
                                };
                                rsx! {
                                    div {
                                        key: "{section_id}",
                                        class: "flex items-start gap-2 px-2 py-1.5 rounded border cursor-pointer transition-colors {selected_class} {override_accent}",
                                        onclick: move |_| { nav(nav_id.clone()); },
                                        oncontextmenu: move |e: Event<MouseData>| {
                                            e.prevent_default();
                                            ctx_target.set(Some(ctx_section_target.clone()));
                                            ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                                        },
                                        span { class: "text-xs text-muted-foreground w-4 text-right pt-0.5",
                                            {(i + 1).to_string()}
                                        }
                                        div { class: "flex-1 flex flex-col gap-0.5",
                                            if is_editing {
                                                input {
                                                    class: "text-xs font-medium text-zinc-200 bg-zinc-800 border border-zinc-600 rounded px-1 w-full outline-none",
                                                    value: "{editing_text}",
                                                    autofocus: true,
                                                    oninput: move |e| editing_text.set(e.value()),
                                                    onkeydown: move |e: KeyboardEvent| {
                                                        e.stop_propagation();
                                                        if e.key() == Key::Enter { commit(); }
                                                        if e.key() == Key::Escape { editing_target.set(None); }
                                                    },
                                                    onfocusin: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() += 1; },
                                                    onfocusout: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() -= 1; commit_blur(); },
                                                }
                                            } else {
                                                div {
                                                    class: "text-xs font-medium",
                                                    ondoubleclick: move |_| {
                                                        editing_target.set(Some(edit_target.clone()));
                                                        editing_text.set(section_name.clone());
                                                    },
                                                    "{section.name}"
                                                }
                                            }
                                            div { class: "flex gap-2 text-[10px] text-muted-foreground",
                                                if let Some(scene) = &scene_label {
                                                    span { "Scene: {scene}" }
                                                }
                                                if let Some(patch) = &patch_label {
                                                    span { "Patch: {patch}" }
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

            // ── Songs (bottom) ──
            div { class: "flex-[1] min-h-0 flex flex-col",
                // Setlist selector dropdown with + Song button
                div { class: "px-3 py-1.5 border-b border-border flex-shrink-0 flex items-center justify-between gap-2",
                    {
                        let current_label = setlist_options()
                            .iter()
                            .find(|(id, _)| id == &selected_setlist_id())
                            .map(|(_, name)| name.clone())
                            .unwrap_or_else(|| "All Songs".to_string());
                        rsx! {
                            Dropdown {
                                DropdownTrigger {
                                    button {
                                        class: "w-full flex items-center justify-between bg-zinc-800 border border-zinc-700 rounded px-2 py-1 text-xs text-zinc-200 hover:bg-zinc-700 transition-colors",
                                        span { "{current_label}" }
                                        span { class: "text-zinc-500 text-[10px] ml-1", "\u{25BE}" }
                                    }
                                }
                                DropdownContent {
                                    width: "w-56".to_string(),
                                    for (idx, (sid, sname)) in setlist_options().iter().enumerate() {
                                        {
                                            let sid_val = sid.clone();
                                            let sname_val = sname.clone();
                                            let mut change = change_setlist.clone();
                                            rsx! {
                                                DropdownItem {
                                                    value: sid_val.clone(),
                                                    index: idx,
                                                    on_select: move |val: String| { change(val); },
                                                    "{sname_val}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    {
                        let signal = signal.clone();
                        let mut cb = on_song_created.clone();
                        rsx! {
                            Dropdown {
                                DropdownTrigger {
                                    button {
                                        class: "px-2 py-0.5 text-[10px] rounded bg-blue-600 hover:bg-blue-700 text-white font-medium",
                                        "+ Song"
                                    }
                                }
                                DropdownContent {
                                    width: "w-48".to_string(),
                                    for (idx, prof) in manage_profiles().iter().enumerate() {
                                        {
                                            let prof_id = prof.id.clone();
                                            let prof_name = prof.name.clone();
                                            let signal = signal.clone();
                                            let mut cb = cb.clone();
                                            rsx! {
                                                DropdownItem {
                                                    value: prof_id.clone(),
                                                    index: idx,
                                                    on_select: move |pid: String| {
                                                        let signal = signal.clone();
                                                        let mut cb = cb.clone();
                                                        let sl_id = selected_setlist_id();
                                                        spawn(async move {
                                                            match signal.songs().create_from_profile("New Song", pid.as_str()).await {
                                                                Ok(song) => {
                                                                    // If a specific setlist is selected, add the song to it
                                                                    if sl_id != "all" {
                                                                        use signal::setlist::{SetlistEntry, SetlistEntryId};
                                                                        let entry = SetlistEntry::new(
                                                                            SetlistEntryId::new(),
                                                                            &song.name,
                                                                            song.id.clone(),
                                                                        );
                                                                        let _ = signal.setlists().add_entry(sl_id, entry).await;
                                                                    }
                                                                    tracing::info!("Created song from profile: {}", song.name);
                                                                    cb();
                                                                }
                                                                Err(e) => {
                                                                    tracing::error!("Failed to create song from profile: {e:?}");
                                                                }
                                                            }
                                                        });
                                                    },
                                                    "{prof_name}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    {
                        let mut cb = on_setlist_created.clone();
                        rsx! {
                            signal_ui::components::CreateSetlistButton {
                                on_created: move |_| { cb(); },
                            }
                        }
                    }
                }
                div { class: "flex-1 overflow-y-auto",
                    for song in songs().iter() {
                        {
                            let is_sel = selected_song_id().as_deref() == Some(song.id.as_str());
                            let song_id = song.id.clone();
                            let mut load = load_song_sections.clone();
                            let edit_target = EditTarget::Song { id: song.id.clone() };
                            let is_editing = editing_target() == Some(edit_target.clone());
                            let song_name = song.name.clone();
                            let commit = commit_rename.clone();
                            let commit_blur = commit.clone();
                            let ctx_song_target = edit_target.clone();
                            rsx! {
                                button {
                                    key: "{song_id}",
                                    class: if is_sel {
                                        "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                    } else {
                                        "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                    },
                                    oncontextmenu: move |e: Event<MouseData>| {
                                        e.prevent_default();
                                        ctx_target.set(Some(ctx_song_target.clone()));
                                        ctx_pos.set((e.page_coordinates().x, e.page_coordinates().y));
                                    },
                                    onclick: move |_| { load(song_id.clone()); },
                                    div { class: "flex items-center gap-1.5",
                                        if is_editing {
                                            input {
                                                class: "text-sm text-zinc-200 bg-zinc-800 border border-zinc-600 rounded px-1 flex-1 min-w-0 outline-none",
                                                value: "{editing_text}",
                                                autofocus: true,
                                                oninput: move |e| editing_text.set(e.value()),
                                                onkeydown: move |e: KeyboardEvent| {
                                                    e.stop_propagation();
                                                    if e.key() == Key::Enter { commit(); }
                                                    if e.key() == Key::Escape { editing_target.set(None); }
                                                },
                                                onfocusin: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() += 1; },
                                                onfocusout: move |_| { *TEXT_INPUT_FOCUS_COUNT.write() -= 1; commit_blur(); },
                                            }
                                        } else {
                                            span {
                                                class: "text-sm text-zinc-200 truncate flex-1",
                                                ondoubleclick: move |_| {
                                                    editing_target.set(Some(edit_target.clone()));
                                                    editing_text.set(song_name.clone());
                                                },
                                                "{song.name}"
                                            }
                                        }
                                        span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                            "{song.section_count}"
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

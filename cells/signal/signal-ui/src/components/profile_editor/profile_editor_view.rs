//! Profile Editor View — manage profiles and their scene templates.
//!
//! Left: Profile list with create/rename/delete
//! Center: Scene template table with preset/snapshot assignments, reordering
//! Right: Available presets picker for scene assignment

use crate::components::shared::EntityEditor;
use crate::prelude::*;
use crate::signals::RIG_SERVICE;
use signal_control::{preset_entity, profile_entity, scene_template};
use uuid::Uuid;

// ── Main Component ───────────────────────────────────────────────────────────

#[component]
pub fn ProfileEditorView() -> Element {
    // All editor state is component-local — fresh on each mount, no stale globals.
    let mut profile_list = use_signal(Vec::<profile_entity::Model>::new);
    let mut selected_profile_id = use_signal(|| None::<Uuid>);
    let mut scene_templates = use_signal(Vec::<scene_template::Model>::new);
    let mut available_presets = use_signal(Vec::<preset_entity::Model>::new);
    let mut profile_status = use_signal(String::new);

    // ── Async Refresh Helpers (capture local signals) ────────────────────────

    let refresh_profiles = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_profiles().await {
                Ok(profiles) => profile_list.set(profiles),
                Err(e) => warn!("Failed to load profiles: {e}"),
            }
        })
    };

    let refresh_scene_templates = move |profile_id: Uuid| {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_scene_templates(profile_id).await {
                Ok(templates) => {
                    let mut sorted = templates;
                    sorted.sort_by_key(|t| t.sort_order);
                    scene_templates.set(sorted);
                }
                Err(e) => warn!("Failed to load scene templates: {e}"),
            }
        })
    };

    let refresh_available_presets = move || {
        spawn(async move {
            let Some(ctl) = RIG_SERVICE.read().clone() else {
                return;
            };
            match ctl.list_rig_presets().await {
                Ok(presets) => available_presets.set(presets),
                Err(e) => warn!("Failed to load presets: {e}"),
            }
        })
    };

    // Local state for dialogs
    let mut show_new_dialog = use_signal(|| false);
    let mut new_name = use_signal(String::new);
    let mut show_rename_dialog = use_signal(|| false);
    let mut rename_name = use_signal(String::new);
    let mut show_add_scene_dialog = use_signal(|| false);
    let mut new_scene_name = use_signal(String::new);

    // Load data on mount
    use_effect(move || {
        refresh_profiles();
        refresh_available_presets();
    });

    // Load scene templates when profile selection changes
    use_effect(move || {
        let pid = *selected_profile_id.read();
        if let Some(id) = pid {
            refresh_scene_templates(id);
        } else {
            scene_templates.write().clear();
        }
    });

    // Clone data out of signals so read guards are dropped before event handlers.
    let profiles = profile_list.cloned();
    let selected_id = *selected_profile_id.read();
    let scenes = scene_templates.cloned();
    let presets = available_presets.cloned();
    let status = profile_status.cloned();

    let selected_profile = selected_id.and_then(|id| profiles.iter().find(|p| p.id == id));

    rsx! {
        EntityEditor {
            left: rsx! {
                // Header
                div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                    div { class: "flex items-center justify-between",
                        span { class: "text-xs font-bold text-zinc-400 uppercase tracking-[0.15em]",
                            "Profiles"
                        }
                        {
                            let count = profiles.len();
                            rsx! {
                                span { class: "text-[10px] text-zinc-600 font-mono", "{count}" }
                            }
                        }
                    }
                }

                // Profile list
                div { class: "flex-1 overflow-y-auto min-h-0",
                    for profile in profiles.iter() {
                        {
                            let pid = profile.id;
                            let is_selected = selected_id == Some(pid);
                            let pname = profile.name.clone();
                            rsx! {
                                div {
                                    key: "{pid}",
                                    class: if is_selected {
                                        "px-3 py-2.5 cursor-pointer border-l-2 border-purple-500 bg-purple-500/10 transition-colors"
                                    } else {
                                        "px-3 py-2.5 cursor-pointer border-l-2 border-transparent hover:bg-zinc-800/40 transition-colors"
                                    },
                                    onclick: move |_| {
                                        selected_profile_id.set(Some(pid));
                                    },
                                    div { class: "flex items-center justify-between",
                                        span { class: "text-xs font-medium text-zinc-200 truncate", "{pname}" }
                                        if is_selected {
                                            div { class: "flex items-center gap-0.5 flex-shrink-0",
                                                button {
                                                    class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                    title: "Rename",
                                                    onclick: move |evt| {
                                                        evt.stop_propagation();
                                                        rename_name.set(pname.clone());
                                                        show_rename_dialog.set(true);
                                                    },
                                                    span { class: "text-[9px]", "\u{270E}" }
                                                }
                                                button {
                                                    class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors",
                                                    title: "Delete",
                                                    onclick: move |evt| {
                                                        evt.stop_propagation();
                                                        spawn(async move {
                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                            if let Err(e) = ctl.delete_profile(pid).await {
                                                                warn!("Delete profile failed: {e}");
                                                            }
                                                            if *selected_profile_id.read() == Some(pid) {
                                                                selected_profile_id.set(None);
                                                            }
                                                            if let Ok(list) = ctl.list_profiles().await {
                                                                profile_list.set(list);
                                                            }
                                                        });
                                                    },
                                                    span { class: "text-[9px]", "\u{2715}" }
                                                }
                                            }
                                        }
                                    }
                                    if let Some(desc) = &profile.description {
                                        if !desc.is_empty() {
                                            p { class: "text-[10px] text-zinc-500 mt-0.5 truncate", "{desc}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // New profile button
                div { class: "px-3 py-2 border-t border-border/30 flex-shrink-0",
                    if *show_new_dialog.read() {
                        input {
                            class: "w-full px-2 py-1.5 rounded text-xs bg-zinc-800 border border-purple-500/40 \
                                    text-zinc-200 placeholder-zinc-600 outline-none",
                            placeholder: "Profile name...",
                            value: "{new_name}",
                            autofocus: true,
                            oninput: move |evt| new_name.set(evt.value().clone()),
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    let val = new_name().trim().to_string();
                                    if !val.is_empty() {
                                        show_new_dialog.set(false);
                                        spawn(async move {
                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                            let rig_id = Uuid::nil(); // Default rig
                                            match ctl.create_profile(&val, rig_id, None).await {
                                                Ok(id) => {
                                                    profile_status.set(format!("Created '{val}'"));
                                                    if let Ok(list) = ctl.list_profiles().await {
                                                        profile_list.set(list);
                                                    }
                                                    selected_profile_id.set(Some(id));
                                                }
                                                Err(e) => {
                                                    warn!("Create profile failed: {e}");
                                                    profile_status.set(format!("Failed: {e}"));
                                                }
                                            }
                                        });
                                    }
                                } else if evt.key() == Key::Escape {
                                    show_new_dialog.set(false);
                                }
                            },
                        }
                    } else {
                        button {
                            class: "w-full px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                    bg-purple-500/15 text-purple-300 border border-purple-500/25 \
                                    hover:bg-purple-500/25 hover:border-purple-500/40 transition-all duration-150",
                            onclick: move |_| {
                                new_name.set(String::new());
                                show_new_dialog.set(true);
                            },
                            "+ New Profile"
                        }
                    }
                }
            },
            center: rsx! {
                if let Some(profile) = selected_profile {
                    // Profile header
                    div { class: "px-4 py-2.5 border-b border-border/30 flex items-center gap-3 flex-shrink-0 bg-zinc-900/30",
                        span { class: "text-xs font-bold text-zinc-200 tracking-wide",
                            "{profile.name}"
                        }
                        {
                            let scene_count = scenes.len();
                            let scene_plural = if scene_count != 1 { "s" } else { "" };
                            rsx! {
                                span { class: "text-[10px] text-zinc-600 font-mono",
                                    "{scene_count} scene{scene_plural}"
                                }
                            }
                        }
                        div { class: "flex-1" }
                        // Add scene button
                        button {
                            class: "flex items-center gap-1.5 px-3 py-1.5 rounded-md text-[10px] font-semibold \
                                    bg-emerald-500/15 text-emerald-300 border border-emerald-500/25 \
                                    hover:bg-emerald-500/25 hover:border-emerald-500/40 transition-all duration-150",
                            onclick: move |_| {
                                new_scene_name.set(String::new());
                                show_add_scene_dialog.set(true);
                            },
                            span { class: "text-emerald-400", "+" }
                            "Add Scene"
                        }
                    }

                    // Rename dialog
                    if *show_rename_dialog.read() {
                        div { class: "px-4 py-2 border-b border-border/30 bg-zinc-800/50 flex items-center gap-2 flex-shrink-0",
                            span { class: "text-[10px] text-zinc-400", "Rename:" }
                            input {
                                class: "flex-1 px-2 py-1 rounded text-xs bg-zinc-900 border border-zinc-700 \
                                        text-zinc-200 outline-none focus:border-purple-500/50",
                                value: "{rename_name}",
                                autofocus: true,
                                oninput: move |evt| rename_name.set(evt.value().clone()),
                                onkeydown: move |evt| {
                                    if evt.key() == Key::Enter {
                                        let val = rename_name().trim().to_string();
                                        if !val.is_empty() {
                                            show_rename_dialog.set(false);
                                            if let Some(pid) = *selected_profile_id.read() {
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    if let Err(e) = ctl.update_profile(pid, Some(&val), None, None, None, None).await {
                                                        warn!("Rename failed: {e}");
                                                    } else if let Ok(list) = ctl.list_profiles().await {
                                                        profile_list.set(list);
                                                    }
                                                });
                                            }
                                        }
                                    } else if evt.key() == Key::Escape {
                                        show_rename_dialog.set(false);
                                    }
                                },
                            }
                        }
                    }

                    // Add scene dialog
                    if *show_add_scene_dialog.read() {
                        div { class: "px-4 py-2 border-b border-border/30 bg-zinc-800/50 flex items-center gap-2 flex-shrink-0",
                            span { class: "text-[10px] text-zinc-400", "Scene name:" }
                            input {
                                class: "flex-1 px-2 py-1 rounded text-xs bg-zinc-900 border border-zinc-700 \
                                        text-zinc-200 outline-none focus:border-emerald-500/50",
                                value: "{new_scene_name}",
                                autofocus: true,
                                oninput: move |evt| new_scene_name.set(evt.value().clone()),
                                onkeydown: move |evt| {
                                    if evt.key() == Key::Enter {
                                        let val = new_scene_name().trim().to_string();
                                        if !val.is_empty() {
                                            show_add_scene_dialog.set(false);
                                            if let Some(pid) = *selected_profile_id.read() {
                                                let sort_order = scene_templates.read().len() as i32;
                                                spawn(async move {
                                                    let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                    // Use a nil preset_id — user will assign later
                                                    match ctl.add_scene_template(pid, &val, Uuid::nil(), None, sort_order).await {
                                                        Ok(_id) => {
                                                            if let Ok(templates) = ctl.list_scene_templates(pid).await {
                                                                let mut sorted = templates;
                                                                sorted.sort_by_key(|t| t.sort_order);
                                                                scene_templates.set(sorted);
                                                            }
                                                        }
                                                        Err(e) => warn!("Add scene failed: {e}"),
                                                    }
                                                });
                                            }
                                        }
                                    } else if evt.key() == Key::Escape {
                                        show_add_scene_dialog.set(false);
                                    }
                                },
                            }
                        }
                    }

                    // Scene template table
                    div { class: "flex-1 overflow-y-auto min-h-0",
                        if scenes.is_empty() {
                            div { class: "flex items-center justify-center h-full",
                                div { class: "text-center py-12",
                                    div { class: "text-lg text-zinc-700 mb-1", "\u{1F3AC}" }
                                    p { class: "text-xs text-zinc-500",
                                        "No scene templates yet"
                                    }
                                    p { class: "text-[10px] text-zinc-600 mt-1",
                                        "Click \"+ Add Scene\" to create one"
                                    }
                                }
                            }
                        } else {
                            // Table header
                            div { class: "px-4 py-1.5 flex items-center gap-3 text-[9px] font-bold text-zinc-600 uppercase tracking-[0.1em] \
                                          border-b border-border/20 bg-zinc-900/20 flex-shrink-0 sticky top-0",
                                div { class: "w-8 text-center", "#" }
                                div { class: "flex-1 min-w-0", "Scene Name" }
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
                                        .unwrap_or(if preset_id == Uuid::nil() { "— unassigned —" } else { "Unknown" });
                                    let profile_id = selected_id.unwrap();

                                    rsx! {
                                        div {
                                            key: "{scene_id}",
                                            class: "px-4 py-2 flex items-center gap-3 border-b border-border/10 \
                                                    hover:bg-zinc-800/30 transition-colors group",
                                            // Sort order
                                            div { class: "w-8 text-center text-[10px] text-zinc-600 font-mono",
                                                "{idx + 1}"
                                            }
                                            // Scene name
                                            div { class: "flex-1 min-w-0",
                                                span { class: "text-xs font-medium text-zinc-200 truncate block",
                                                    "{scene_name}"
                                                }
                                            }
                                            // Preset assignment
                                            div { class: "w-40",
                                                span {
                                                    class: if preset_id == Uuid::nil() {
                                                        "text-[10px] text-zinc-600 italic"
                                                    } else {
                                                        "text-[10px] text-amber-300/70 font-mono"
                                                    },
                                                    "{preset_name}"
                                                }
                                            }
                                            // Actions
                                            div { class: "w-24 flex items-center justify-center gap-1 opacity-0 group-hover:opacity-100 transition-opacity",
                                                // Move up
                                                if idx > 0 {
                                                    button {
                                                        class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                        title: "Move up",
                                                        onclick: move |_| {
                                                            let mut ids: Vec<Uuid> = scene_templates.read().iter().map(|s| s.id).collect();
                                                            ids.swap(idx, idx - 1);
                                                            spawn(async move {
                                                                let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                if let Err(e) = ctl.reorder_scene_templates(profile_id, &ids).await {
                                                                    warn!("Reorder failed: {e}");
                                                                }
                                                                if let Ok(templates) = ctl.list_scene_templates(profile_id).await {
                                                                    let mut sorted = templates;
                                                                    sorted.sort_by_key(|t| t.sort_order);
                                                                    scene_templates.set(sorted);
                                                                }
                                                            });
                                                        },
                                                        span { class: "text-[9px]", "\u{2191}" }
                                                    }
                                                }
                                                // Move down
                                                {
                                                    let scene_count = scenes.len();
                                                    if idx < scene_count - 1 {
                                                        rsx! {
                                                            button {
                                                                class: "p-1 rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-700/50 transition-colors",
                                                                title: "Move down",
                                                                onclick: move |_| {
                                                                    let mut ids: Vec<Uuid> = scene_templates.read().iter().map(|s| s.id).collect();
                                                                    ids.swap(idx, idx + 1);
                                                                    spawn(async move {
                                                                        let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                                        if let Err(e) = ctl.reorder_scene_templates(profile_id, &ids).await {
                                                                            warn!("Reorder failed: {e}");
                                                                        }
                                                                        if let Ok(templates) = ctl.list_scene_templates(profile_id).await {
                                                                            let mut sorted = templates;
                                                                            sorted.sort_by_key(|t| t.sort_order);
                                                                            scene_templates.set(sorted);
                                                                        }
                                                                    });
                                                                },
                                                                span { class: "text-[9px]", "\u{2193}" }
                                                            }
                                                        }
                                                    } else {
                                                        rsx! {}
                                                    }
                                                }
                                                // Delete
                                                button {
                                                    class: "p-1 rounded text-zinc-500 hover:text-red-400 hover:bg-zinc-700/50 transition-colors",
                                                    title: "Delete scene",
                                                    onclick: move |_| {
                                                        spawn(async move {
                                                            let Some(ctl) = RIG_SERVICE.read().clone() else { return };
                                                            if let Err(e) = ctl.delete_scene_template(scene_id).await {
                                                                warn!("Delete scene failed: {e}");
                                                            }
                                                            if let Ok(templates) = ctl.list_scene_templates(profile_id).await {
                                                                let mut sorted = templates;
                                                                sorted.sort_by_key(|t| t.sort_order);
                                                                scene_templates.set(sorted);
                                                            }
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
                    // No profile selected
                    div { class: "flex-1 flex items-center justify-center",
                        div { class: "text-center",
                            div { class: "text-2xl text-zinc-700 mb-2", "\u{1F3AD}" }
                            p { class: "text-sm text-zinc-500 font-medium", "Select a profile" }
                            p { class: "text-[10px] text-zinc-600 mt-1",
                                "Profiles define scene templates for different genres or contexts"
                            }
                        }
                    }
                }
            },
            right: Some(rsx! {
                // Header
                div { class: "px-4 py-3 border-b border-border/30 flex-shrink-0",
                    span { class: "text-xs font-bold text-zinc-400 uppercase tracking-[0.15em]",
                        "Available Presets"
                    }
                }

                // Preset list — click to assign to selected scene
                div { class: "flex-1 overflow-y-auto min-h-0",
                    if presets.is_empty() {
                        div { class: "px-4 py-8 text-center",
                            p { class: "text-[10px] text-zinc-600", "No presets available" }
                            p { class: "text-[10px] text-zinc-700 mt-1", "Create presets in the Presets tab" }
                        }
                    } else {
                        for preset in presets.iter() {
                            {
                                let pid = preset.id;
                                let pname = preset.name.clone();
                                rsx! {
                                    div {
                                        key: "{pid}",
                                        class: "px-3 py-2 cursor-pointer hover:bg-zinc-800/40 border-b border-border/10 transition-colors group",
                                        title: "Click to assign to selected scene template",
                                        onclick: move |_| {
                                            // For now, show which preset was clicked
                                            profile_status.set(format!("Selected preset: {pname}"));
                                        },
                                        div { class: "flex items-center gap-2",
                                            div { class: "w-2 h-2 rounded-full bg-amber-500/40 flex-shrink-0" }
                                            span { class: "text-xs text-zinc-300 truncate", "{pname}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Quick reference
                div { class: "px-4 py-3 border-t border-border/30 flex-shrink-0",
                    div { class: "space-y-1",
                        p { class: "text-[9px] text-zinc-600 font-semibold uppercase tracking-wider mb-1", "Workflow" }
                        p { class: "text-[9px] text-zinc-500", "1. Create a profile (e.g. \"Worship\")" }
                        p { class: "text-[9px] text-zinc-500", "2. Add scene templates" }
                        p { class: "text-[9px] text-zinc-500", "3. Assign a preset to each scene" }
                        p { class: "text-[9px] text-zinc-500", "4. Use in Songs for live performance" }
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

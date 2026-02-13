//! Guitar rig left sidebar component.
//!
//! Toggleable sidebar with:
//! - Top section: Preset browser with fuzzy search
//! - Bottom section: Profile selector

use std::collections::HashSet;

use crate::callback_types::{PresetSnapshotSelect, ProfileSceneSelect};
use crate::hooks::use_fuzzy_search;
use crate::prelude::*;
use crate::signals::{
    RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_CURRENT_PATCH, RIG_CURRENT_PRESET,
    RIG_LAST_APPLIED_SNAPSHOT, RIG_PROFILE,
};
use signal_control::id::{ProfileId, RigPresetId};
use signal_control::tags::{Tag, TagRegistry};
use signal_control::{ProfileInfo, RigPresetInfo};
use uuid::Uuid;

use super::view_mode::RigViewMode;

/// Props for the guitar rig left sidebar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigLeftSidebarProps {
    /// Whether the sidebar is open.
    pub is_open: bool,
    /// Current rig view mode (determines layout).
    pub rig_view_mode: RigViewMode,
    /// Callback when a preset is selected (loads with default scene).
    pub on_preset_select: Callback<RigPresetId>,
    /// Callback when a preset + scene is selected (by scene index).
    #[props(default)]
    pub on_preset_snapshot_select: Option<Callback<PresetSnapshotSelect>>,
    /// Callback when a profile is selected (loads with default scene).
    pub on_profile_select: Callback<ProfileId>,
    /// Callback when a profile + scene is selected (by scene index).
    #[props(default)]
    pub on_profile_scene_select: Option<Callback<ProfileSceneSelect>>,
    /// Callback to create a new preset.
    #[props(default)]
    pub on_create_preset: Option<Callback<()>>,
    /// Callback to create a new profile.
    #[props(default)]
    pub on_create_profile: Option<Callback<()>>,
}

/// Left sidebar for the guitar rig page.
///
/// Features:
/// - Collapsible with smooth animation
/// - Preset browser with fuzzy search (top)
/// - Profile selector (bottom)
#[component]
pub fn GuitarRigLeftSidebar(props: GuitarRigLeftSidebarProps) -> Element {
    if !props.is_open {
        return rsx! {};
    }

    // Tag registry for resolving tag names
    let tag_registry = use_memo(|| TagRegistry::with_defaults());

    // Local state
    let mut search_query = use_signal(String::new);
    let expanded_presets = use_signal(HashSet::<RigPresetId>::new);
    let expanded_profiles = use_signal(HashSet::<ProfileId>::new);

    // Read global signals
    let all_presets = RIG_AVAILABLE_PRESETS.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let all_profiles = RIG_AVAILABLE_PROFILES.read();
    let current_profile = RIG_PROFILE.read();

    // Memo that reads from the GlobalSignal so use_fuzzy_search can subscribe
    let presets_memo = use_memo(move || RIG_AVAILABLE_PRESETS.read().clone());

    // Filter presets based on fuzzy search
    let filtered_presets = use_fuzzy_search(presets_memo, search_query, |preset| {
        format!("{} {}", preset.name, preset.category)
    });

    let query_for_display = search_query();
    let preset_count = filtered_presets.read().len();
    let total_count = all_presets.len();
    let has_search = !query_for_display.is_empty();

    // Determine layout based on rig view mode
    let show_split = props.rig_view_mode == RigViewMode::Song;
    let preset_section_class = if show_split {
        "flex-[3] flex flex-col min-h-0" // 60% height in song mode
    } else {
        "flex-1 flex flex-col min-h-0" // Full height in preset/profile mode
    };

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card border-r border-border",
            // === PRESETS SECTION ===
            div { class: "{preset_section_class}",
                // Header with search
                div { class: "p-3 border-b border-zinc-800 flex-shrink-0",
                    div { class: "flex items-center justify-between mb-2",
                        h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Presets"
                        }
                        div { class: "flex items-center gap-1.5",
                            span { class: "text-xs text-zinc-600 bg-zinc-800 px-1.5 py-0.5 rounded",
                                if has_search {
                                    "{preset_count}/{total_count}"
                                } else {
                                    "{preset_count}"
                                }
                            }
                            if let Some(ref on_create) = props.on_create_preset {
                                {
                                    let on_create = on_create.clone();
                                    rsx! {
                                        button {
                                            class: "w-5 h-5 flex items-center justify-center rounded \
                                                    text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                                    transition-colors text-sm leading-none",
                                            title: "New Preset",
                                            onclick: move |_| on_create.call(()),
                                            "+"
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Search input
                    div { class: "relative",
                        input {
                            class: "w-full px-3 py-2 pl-8 text-sm bg-zinc-800 border border-zinc-700 rounded-md \
                                    placeholder:text-zinc-600 focus:outline-none focus:ring-1 focus:ring-zinc-600 text-zinc-300",
                            r#type: "text",
                            placeholder: "Search presets...",
                            value: "{search_query}",
                            oninput: move |e| search_query.set(e.value().clone()),
                        }
                        // Search icon
                        span { class: "absolute left-2.5 top-2.5 text-zinc-500 text-sm", "🔍" }
                        // Clear button
                        if has_search {
                            button {
                                class: "absolute right-2 top-2 text-zinc-500 hover:text-zinc-300 text-sm px-1",
                                onclick: move |_| search_query.set(String::new()),
                                "×"
                            }
                        }
                    }
                }

                // Preset list
                div { class: "flex-1 overflow-y-auto",
                    if filtered_presets.read().is_empty() {
                        div { class: "p-4 text-center text-zinc-500 text-sm",
                            if has_search {
                                p { "No matches for" }
                                p { class: "text-xs mt-1 text-blue-400", "\"{query_for_display}\"" }
                            } else {
                                p { "No presets available" }
                            }
                        }
                    } else {
                        for preset in filtered_presets.read().iter() {
                            {
                                let pid = preset.id;
                                let is_active = current_preset.as_ref().map(|p| p.id) == Some(pid);
                                let is_expanded = is_active || expanded_presets.read().contains(&pid);
                                rsx! {
                                    PresetItem {
                                        preset: preset.clone(),
                                        is_active,
                                        is_expanded,
                                        registry: tag_registry.read().clone(),
                                        on_click: props.on_preset_select.clone(),
                                        on_scene_click: props.on_preset_snapshot_select.clone(),
                                        on_toggle_expand: {
                                            let mut expanded_presets = expanded_presets.clone();
                                            Callback::new(move |id: RigPresetId| {
                                                let mut set = expanded_presets.write();
                                                if set.contains(&id) {
                                                    set.remove(&id);
                                                } else {
                                                    set.insert(id);
                                                }
                                            })
                                        },
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // === PROFILES SECTION (bottom) ===
            // Only show in Song mode - takes up 40% of the sidebar height
            if show_split {
                div { class: "flex-[2] border-t border-zinc-800 flex flex-col min-h-0",
                    div { class: "p-3 border-b border-zinc-800/50 flex-shrink-0",
                        div { class: "flex items-center justify-between",
                            h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                                "Profiles"
                            }
                            div { class: "flex items-center gap-1.5",
                                span { class: "text-xs text-zinc-600 bg-zinc-800 px-1.5 py-0.5 rounded",
                                    "{all_profiles.len()}"
                                }
                                if let Some(ref on_create) = props.on_create_profile {
                                    {
                                        let on_create = on_create.clone();
                                        rsx! {
                                            button {
                                                class: "w-5 h-5 flex items-center justify-center rounded \
                                                        text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                                        transition-colors text-sm leading-none",
                                                title: "New Profile",
                                                onclick: move |_| on_create.call(()),
                                                "+"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // Profile list (scrollable)
                    div { class: "flex-1 overflow-y-auto",
                        if all_profiles.is_empty() {
                            div { class: "p-4 text-center text-zinc-500 text-sm",
                                p { "No profiles available" }
                            }
                        } else {
                            for profile in all_profiles.iter() {
                                {
                                    let pid = profile.id;
                                    let is_active = current_profile.as_ref().map(|p| p.id) == Some(pid);
                                    let is_expanded = is_active || expanded_profiles.read().contains(&pid);
                                    rsx! {
                                        ProfileItem {
                                            profile: profile.clone(),
                                            is_active,
                                            is_expanded,
                                            current_preset_id: current_preset.as_ref().map(|p| p.id),
                                            on_click: props.on_profile_select.clone(),
                                            on_scene_click: props.on_profile_scene_select.clone(),
                                            on_toggle_expand: {
                                                let mut expanded_profiles = expanded_profiles.clone();
                                                Callback::new(move |id: ProfileId| {
                                                    let mut set = expanded_profiles.write();
                                                    if set.contains(&id) {
                                                        set.remove(&id);
                                                    } else {
                                                        set.insert(id);
                                                    }
                                                })
                                            },
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

/// Props for preset item.
#[derive(Props, Clone, PartialEq)]
struct PresetItemProps {
    preset: RigPresetInfo,
    is_active: bool,
    is_expanded: bool,
    registry: TagRegistry,
    on_click: Callback<RigPresetId>,
    on_scene_click: Option<Callback<PresetSnapshotSelect>>,
    on_toggle_expand: Callback<RigPresetId>,
}

/// Individual preset item with expandable scenes.
#[component]
fn PresetItem(props: PresetItemProps) -> Element {
    let preset_id = props.preset.id;
    let has_scenes = !props.preset.scenes.is_empty();
    let chevron = if props.is_expanded { "▼" } else { "▶" };

    rsx! {
        div { class: "border-b border-zinc-800/50",
            // Preset header
            div {
                class: if props.is_active {
                    "px-3 py-2 cursor-pointer bg-zinc-800 border-l-2 border-green-500 transition-colors"
                } else {
                    "px-3 py-2 cursor-pointer hover:bg-zinc-800/50 border-l-2 border-transparent transition-colors"
                },
                onclick: move |_| props.on_click.call(preset_id),

                div { class: "flex items-center justify-between",
                    div { class: "flex-1 min-w-0",
                        span { class: "font-medium text-sm text-zinc-200 truncate",
                            "{props.preset.name}"
                        }
                        div { class: "text-xs text-zinc-500",
                            "{props.preset.category}"
                        }

                        // Rating display
                        if props.preset.rating.is_rated() {
                            div { class: "text-xs text-yellow-500 mt-1",
                                {"★".repeat(props.preset.rating.get() as usize)}
                            }
                        }
                    }

                    // Expand/collapse chevron
                    if has_scenes {
                        button {
                            class: "ml-2 w-5 h-5 flex items-center justify-center rounded \
                                    text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                    transition-colors text-xs leading-none flex-shrink-0",
                            title: if props.is_expanded { "Collapse scenes" } else { "Expand scenes" },
                            onclick: move |e: Event<MouseData>| {
                                e.stop_propagation();
                                props.on_toggle_expand.call(preset_id);
                            },
                            "{chevron}"
                        }
                    }
                }
            }

            // Expanded scenes
            if props.is_expanded && has_scenes {
                div { class: "bg-zinc-900/50 py-1",
                    for (scene_index, scene) in props.preset.scenes.iter().enumerate() {
                        {
                            let scene_id = scene.id;
                            let on_scene = props.on_scene_click.clone();
                            rsx! {
                                ChildRow {
                                    key: "{scene_id}",
                                    name: scene.name.clone(),
                                    is_active: (*RIG_LAST_APPLIED_SNAPSHOT.read()).map(|s| s.as_uuid()) == Some(scene_id),
                                    is_default: scene.is_default,
                                    on_click: Callback::new(move |_| {
                                        if let Some(ref cb) = on_scene {
                                            cb.call(PresetSnapshotSelect { preset_id, snapshot_index: scene_index });
                                        }
                                    }),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for a generic expandable child row.
///
/// Used for preset scenes, profile patches, and song sections — any indented
/// child item under a parent row.
#[derive(Props, Clone, PartialEq)]
struct ChildRowProps {
    name: String,
    is_active: bool,
    #[props(default)]
    is_default: bool,
    on_click: Callback<()>,
}

/// Shared child row component for preset scenes, profile patches, and song sections.
#[component]
fn ChildRow(props: ChildRowProps) -> Element {
    let prefix = if props.is_default { "★" } else { "•" };

    rsx! {
        div {
            class: if props.is_active {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
            } else if props.is_default {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer text-yellow-500 hover:text-yellow-400 hover:bg-zinc-800/50 transition-colors"
            } else {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 transition-colors"
            },
            onclick: move |_| props.on_click.call(()),
            "{prefix} {props.name}"
        }
    }
}

/// Props for tags display.
#[derive(Props, Clone, PartialEq)]
struct TagsDisplayProps {
    tag_ids: Vec<Uuid>,
    registry: TagRegistry,
}

/// Display tags for a preset.
#[component]
fn TagsDisplay(props: TagsDisplayProps) -> Element {
    let tags: Vec<&Tag> = props
        .tag_ids
        .iter()
        .filter_map(|id| {
            props
                .registry
                .get(signal_control::id::TagId::from_uuid(*id))
        })
        .take(3)
        .collect();

    if tags.is_empty() {
        return rsx! {};
    }

    rsx! {
        div { class: "flex flex-wrap gap-1 mt-1",
            for tag in tags {
                span {
                    key: "{tag.id}",
                    class: "text-xs px-1.5 py-0.5 rounded bg-zinc-700 text-zinc-400",
                    "{tag.name}"
                }
            }
        }
    }
}

/// Props for profile item.
#[derive(Props, Clone, PartialEq)]
struct ProfileItemProps {
    profile: ProfileInfo,
    is_active: bool,
    is_expanded: bool,
    current_preset_id: Option<RigPresetId>,
    on_click: Callback<ProfileId>,
    on_scene_click: Option<Callback<ProfileSceneSelect>>,
    on_toggle_expand: Callback<ProfileId>,
}

/// Individual profile item with expandable patches.
#[component]
fn ProfileItem(props: ProfileItemProps) -> Element {
    let has_patches = !props.profile.patches.is_empty();
    let profile_id = props.profile.id;
    let chevron = if props.is_expanded { "▼" } else { "▶" };

    rsx! {
        div { class: "border-b border-zinc-800/50",
            // Profile header
            div {
                class: if props.is_active {
                    "px-3 py-2 cursor-pointer bg-zinc-800 border-l-2 border-green-500 transition-colors"
                } else {
                    "px-3 py-2 cursor-pointer hover:bg-zinc-800/50 border-l-2 border-transparent transition-colors"
                },
                onclick: move |_| props.on_click.call(profile_id),

                div { class: "flex items-center justify-between",
                    div { class: "flex-1 min-w-0",
                        span { class: "font-medium text-sm text-zinc-200 truncate",
                            "{props.profile.name}"
                        }
                        div { class: "text-xs text-zinc-500",
                            "{props.profile.patch_count} patches"
                        }
                    }

                    // Expand/collapse chevron
                    if has_patches {
                        button {
                            class: "ml-2 w-5 h-5 flex items-center justify-center rounded \
                                    text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                    transition-colors text-xs leading-none flex-shrink-0",
                            title: if props.is_expanded { "Collapse patches" } else { "Expand patches" },
                            onclick: move |e: Event<MouseData>| {
                                e.stop_propagation();
                                props.on_toggle_expand.call(profile_id);
                            },
                            "{chevron}"
                        }
                    }
                }
            }

            // Expanded patches
            if props.is_expanded && has_patches {
                {
                    let current_patch_id = RIG_CURRENT_PATCH.read().as_ref().map(|p| p.id);
                    rsx! {
                        div { class: "bg-zinc-900/50 py-1",
                            for (patch_index, patch) in props.profile.patches.iter().enumerate() {
                                {
                                    let on_scene = props.on_scene_click.clone();
                                    let patch_name = patch.name.clone();
                                    let is_patch_active = props.is_active && current_patch_id == Some(patch.id);
                                    rsx! {
                                        ChildRow {
                                            key: "{patch_index}",
                                            name: patch_name,
                                            is_active: is_patch_active,
                                            on_click: Callback::new(move |_| {
                                                if let Some(ref cb) = on_scene {
                                                    cb.call(ProfileSceneSelect { profile_id, scene_index: patch_index });
                                                }
                                            }),
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

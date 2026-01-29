//! Guitar rig left sidebar component.
//!
//! Toggleable sidebar with:
//! - Top section: Preset browser with fuzzy search
//! - Bottom section: Profile selector

use dioxus::prelude::*;
use fts::rig::core::{PresetTag, TagRegistry};
use fts::rig::{RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_PROFILES, RIG_CURRENT_PRESET, RIG_CURRENT_PRESET_SCENE_ID, RIG_PROFILE};
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Utf32Str,
};
use rig_control::{PresetInfo, PresetSceneInfo, ProfileInfo, ProfileSceneInfo};
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
    pub on_preset_select: Callback<Uuid>,
    /// Callback when a preset + scene is selected (by scene index).
    #[props(default)]
    pub on_preset_scene_select: Option<Callback<(Uuid, usize)>>,
    /// Callback when a profile is selected (loads with default scene).
    pub on_profile_select: Callback<Uuid>,
    /// Callback when a profile + scene is selected (by scene index).
    #[props(default)]
    pub on_profile_scene_select: Option<Callback<(Uuid, usize)>>,
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

    // Read global signals
    let all_presets = RIG_AVAILABLE_PRESETS.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let all_profiles = RIG_AVAILABLE_PROFILES.read();
    let current_profile = RIG_PROFILE.read();

    // Filter presets based on fuzzy search
    let filtered_presets = use_memo(move || {
        let query = search_query();
        let presets = RIG_AVAILABLE_PRESETS.read().clone();
        let registry = TagRegistry::with_defaults();

        if query.is_empty() {
            return presets;
        }

        // Use nucleo matcher for fuzzy search
        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = Pattern::parse(&query, CaseMatching::Smart, Normalization::Smart);

        let mut scored: Vec<(PresetInfo, u32)> = presets
            .into_iter()
            .filter_map(|preset| {
                // Build searchable text: name + category + description
                // TODO: Add tag names and scene names when rig-control supports them
                let description = preset.description.as_deref().unwrap_or("");
                let search_text = format!("{} {} {}", preset.name, preset.category, description);

                let mut buf = Vec::new();
                let haystack = Utf32Str::new(&search_text, &mut buf);

                pattern.score(haystack, &mut matcher).map(|score| (preset, score))
            })
            .collect();

        // Sort by score descending
        scored.sort_by(|a, b| b.1.cmp(&a.1));
        scored.into_iter().map(|(p, _)| p).collect()
    });

    let query_for_display = search_query();
    let preset_count = filtered_presets.read().len();
    let total_count = all_presets.len();
    let has_search = !query_for_display.is_empty();

    // Determine layout based on rig view mode
    let show_split = props.rig_view_mode == RigViewMode::Song;
    let preset_section_class = if show_split {
        "flex-[3] flex flex-col min-h-0"  // 60% height in song mode
    } else {
        "flex-1 flex flex-col min-h-0"    // Full height in preset/profile mode
    };

    rsx! {
        div { class: "w-64 flex flex-col bg-zinc-900 border-r border-zinc-800",
            // === PRESETS SECTION ===
            div { class: "{preset_section_class}",
                // Header with search
                div { class: "p-3 border-b border-zinc-800 flex-shrink-0",
                    div { class: "flex items-center justify-between mb-2",
                        h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Presets"
                        }
                        span { class: "text-xs text-zinc-600 bg-zinc-800 px-1.5 py-0.5 rounded",
                            if has_search {
                                "{preset_count}/{total_count}"
                            } else {
                                "{preset_count}"
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
                            // Only expand the currently selected preset
                            PresetItem {
                                preset: preset.clone(),
                                is_active: current_preset.as_ref().map(|p| p.id) == Some(preset.id),
                                is_expanded: current_preset.as_ref().map(|p| p.id) == Some(preset.id),
                                registry: tag_registry.read().clone(),
                                on_click: props.on_preset_select.clone(),
                                on_scene_click: props.on_preset_scene_select.clone(),
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
                        h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                            "Profiles"
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
                                // Only expand the currently selected profile
                                ProfileItem {
                                    profile: profile.clone(),
                                    is_active: current_profile.as_ref().map(|p| p.id) == Some(profile.id),
                                    is_expanded: current_profile.as_ref().map(|p| p.id) == Some(profile.id),
                                    current_preset_id: current_preset.as_ref().map(|p| p.id),
                                    current_snapshot_id: *RIG_CURRENT_PRESET_SCENE_ID.read(),
                                    on_click: props.on_profile_select.clone(),
                                    on_scene_click: props.on_profile_scene_select.clone(),
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
    preset: PresetInfo,
    is_active: bool,
    is_expanded: bool,
    registry: TagRegistry,
    on_click: Callback<Uuid>,
    on_scene_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual preset item with expandable scenes.
#[component]
fn PresetItem(props: PresetItemProps) -> Element {
    let _current_snapshot_id = *RIG_CURRENT_PRESET_SCENE_ID.read();
    let has_scenes = !props.preset.scene_names.is_empty();
    let preset_id = props.preset.id;

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

                div { class: "flex items-start justify-between",
                    div { class: "flex-1 min-w-0",
                        div { class: "font-medium text-sm text-zinc-200 truncate",
                            "{props.preset.name}"
                        }
                        div { class: "text-xs text-zinc-500",
                            "{props.preset.category}"
                            if has_scenes {
                                span { class: "ml-2 text-zinc-600",
                                    "• {props.preset.scene_count} scenes"
                                }
                            }
                        }

                        // Rating display
                        if props.preset.rating > 0 {
                            div { class: "text-xs text-yellow-500 mt-1",
                                {"★".repeat(props.preset.rating as usize)}
                            }
                        }
                    }
                }
            }

            // Expanded scenes (preset-level variations)
            if props.is_expanded && has_scenes {
                div { class: "bg-zinc-850 py-1",
                    for (scene_index, scene) in props.preset.scenes.iter().enumerate() {
                        PresetSceneItem {
                            key: "{scene_index}",
                            preset_id,
                            scene_index,
                            scene: scene.clone(),
                            is_active: _current_snapshot_id == Some(scene.id),
                            on_click: props.on_scene_click.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Props for preset scene item.
#[derive(Props, Clone, PartialEq)]
struct PresetSceneItemProps {
    preset_id: Uuid,
    scene_index: usize,
    scene: PresetSceneInfo,
    is_active: bool,
    on_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual preset scene item (preset-level variation).
#[component]
fn PresetSceneItem(props: PresetSceneItemProps) -> Element {
    let scene_index = props.scene_index;
    let preset_id = props.preset_id;
    let on_click = props.on_click.clone();

    rsx! {
        div {
            class: if props.is_active {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
            } else {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer hover:bg-zinc-800/50 text-zinc-400 hover:text-zinc-300 transition-colors"
            },
            onclick: move |_| {
                if let Some(ref cb) = on_click {
                    cb.call((preset_id, scene_index));
                }
            },
            "• {props.scene.name}"
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
    let tags: Vec<&PresetTag> = props
        .tag_ids
        .iter()
        .filter_map(|id| props.registry.get(*id))
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
    current_preset_id: Option<Uuid>,
    current_snapshot_id: Option<Uuid>,
    on_click: Callback<Uuid>,
    on_scene_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual profile item with expandable scenes.
#[component]
fn ProfileItem(props: ProfileItemProps) -> Element {
    let has_scenes = !props.profile.scene_names.is_empty();
    let profile_id = props.profile.id;

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

                div { class: "flex items-start justify-between",
                    div { class: "flex-1 min-w-0",
                        div { class: "font-medium text-sm text-zinc-200 truncate",
                            "{props.profile.name}"
                        }
                        div { class: "text-xs text-zinc-500",
                            "{props.profile.scene_count} scenes"
                        }
                    }
                }
            }

            // Expanded scenes
            if props.is_expanded && has_scenes {
                div { class: "bg-zinc-850 py-1",
                    for (scene_index, scene) in props.profile.scenes.iter().enumerate() {
                        {
                            // Check if this profile scene is active by comparing preset and preset scene
                            // A profile scene is active only if:
                            // 1. The current preset matches this scene's preset_id
                            // 2. The current preset scene ID matches this scene's preset_scene_id
                            let is_scene_active = props.current_preset_id == Some(scene.preset_id)
                                && scene.preset_scene_id == props.current_snapshot_id;

                            rsx! {
                                ProfileSceneItem {
                                    key: "{scene_index}",
                                    profile_id,
                                    scene_index,
                                    scene: scene.clone(),
                                    is_active: is_scene_active,
                                    on_click: props.on_scene_click.clone(),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for profile scene item.
#[derive(Props, Clone, PartialEq)]
struct ProfileSceneItemProps {
    profile_id: Uuid,
    scene_index: usize,
    scene: ProfileSceneInfo,
    is_active: bool,
    on_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual scene item within a profile.
#[component]
fn ProfileSceneItem(props: ProfileSceneItemProps) -> Element {
    let profile_id = props.profile_id;
    let scene_index = props.scene_index;
    let on_click = props.on_click.clone();

    rsx! {
        div {
            class: if props.is_active {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
            } else {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 transition-colors"
            },
            onclick: move |_| {
                if let Some(ref cb) = on_click {
                    cb.call((profile_id, scene_index));
                }
            },
            "• {props.scene.name}"
        }
    }
}

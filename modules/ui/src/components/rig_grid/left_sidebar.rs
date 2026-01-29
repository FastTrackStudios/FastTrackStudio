//! Guitar rig left sidebar component.
//!
//! Toggleable sidebar with:
//! - Top section: Preset browser with fuzzy search
//! - Bottom section: Profile selector

use dioxus::prelude::*;
use fts::rig::core::{PresetTag, TagRegistry};
use fts::rig::service::{PresetInfo, SnapshotInfo};
use fts::rig::{RIG_AVAILABLE_PRESETS, RIG_CURRENT_PRESET, RIG_CURRENT_SNAPSHOT_ID, RIG_PROFILE};
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Utf32Str,
};
use uuid::Uuid;

/// Props for the guitar rig left sidebar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigLeftSidebarProps {
    /// Whether the sidebar is open.
    pub is_open: bool,
    /// Callback when a preset is selected.
    pub on_preset_select: Callback<Uuid>,
    /// Callback when a preset + snapshot is selected.
    #[props(default)]
    pub on_snapshot_select: Option<Callback<(Uuid, Uuid)>>,
    /// Callback when a profile is selected.
    pub on_profile_select: Callback<Uuid>,
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
    let mut collapsed_preset_ids = use_signal(Vec::<Uuid>::new);

    // Read global signals
    let all_presets = RIG_AVAILABLE_PRESETS.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let _profile = RIG_PROFILE.read();

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
                // Build searchable text: name + tag names + snapshot names
                let tag_names: Vec<String> = preset
                    .tag_ids
                    .iter()
                    .filter_map(|id| registry.get(*id))
                    .map(|t| t.name.clone())
                    .collect();

                let snapshot_names = preset.snapshot_names.join(" ");
                let search_text = format!("{} {} {}", preset.name, tag_names.join(" "), snapshot_names);

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

    rsx! {
        div { class: "w-64 flex flex-col bg-zinc-900 border-r border-zinc-800",
            // === PRESETS SECTION (top) ===
            div { class: "flex-1 flex flex-col min-h-0",
                // Header with search
                div { class: "p-3 border-b border-zinc-800",
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
                            PresetItem {
                                preset: preset.clone(),
                                is_active: current_preset.as_ref().map(|p| p.id) == Some(preset.id),
                                is_expanded: !collapsed_preset_ids().contains(&preset.id),
                                registry: tag_registry.read().clone(),
                                on_click: props.on_preset_select.clone(),
                                on_toggle_expand: {
                                    let preset_id = preset.id;
                                    Callback::new(move |_: ()| {
                                        let mut collapsed = collapsed_preset_ids();
                                        if collapsed.contains(&preset_id) {
                                            collapsed.retain(|&id| id != preset_id);
                                        } else {
                                            collapsed.push(preset_id);
                                        }
                                        collapsed_preset_ids.set(collapsed);
                                    })
                                },
                                on_snapshot_click: props.on_snapshot_select.clone(),
                            }
                        }
                    }
                }
            }

            // === PROFILES SECTION (bottom) ===
            div { class: "min-h-48 border-t border-zinc-800",
                div { class: "p-3 border-b border-zinc-800/50",
                    h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                        "Profiles"
                    }
                }
                div { class: "p-2 space-y-1",
                    // Sample profiles - in production these would come from RIG_AVAILABLE_PROFILES
                    ProfileItem {
                        id: Uuid::nil(),
                        name: "Clean Tones".to_string(),
                        is_active: true,
                        on_click: props.on_profile_select.clone(),
                    }
                    ProfileItem {
                        id: Uuid::nil(),
                        name: "High Gain".to_string(),
                        is_active: false,
                        on_click: props.on_profile_select.clone(),
                    }
                    ProfileItem {
                        id: Uuid::nil(),
                        name: "Ambient".to_string(),
                        is_active: false,
                        on_click: props.on_profile_select.clone(),
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
    on_toggle_expand: Callback<()>,
    on_snapshot_click: Option<Callback<(Uuid, Uuid)>>,
}

/// Individual preset item with expandable snapshots.
#[component]
fn PresetItem(props: PresetItemProps) -> Element {
    let current_snapshot_id = *RIG_CURRENT_SNAPSHOT_ID.read();
    let has_snapshots = !props.preset.snapshot_names.is_empty();
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
                        div { class: "text-xs text-zinc-500", "{props.preset.category}" }

                        // Tags
                        if !props.preset.tag_ids.is_empty() {
                            TagsDisplay {
                                tag_ids: props.preset.tag_ids.clone(),
                                registry: props.registry.clone(),
                            }
                        }
                    }

                    // Expand button for snapshots
                    if has_snapshots {
                        button {
                            class: "ml-2 p-1 hover:bg-zinc-700 rounded text-zinc-500 flex-shrink-0",
                            onclick: move |e| {
                                e.stop_propagation();
                                props.on_toggle_expand.call(());
                            },
                            if props.is_expanded { "▼" } else { "▶" }
                        }
                    }
                }
            }

            // Expanded snapshots
            if props.is_expanded && has_snapshots {
                div { class: "bg-zinc-850 py-1",
                    for snapshot in &props.preset.snapshots {
                        SnapshotItem {
                            preset_id,
                            snapshot: snapshot.clone(),
                            is_active: current_snapshot_id == Some(snapshot.id),
                            on_click: props.on_snapshot_click.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Props for snapshot item.
#[derive(Props, Clone, PartialEq)]
struct SnapshotItemProps {
    preset_id: Uuid,
    snapshot: SnapshotInfo,
    is_active: bool,
    on_click: Option<Callback<(Uuid, Uuid)>>,
}

/// Individual snapshot item.
#[component]
fn SnapshotItem(props: SnapshotItemProps) -> Element {
    let snapshot_id = props.snapshot.id;
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
                    cb.call((preset_id, snapshot_id));
                }
            },
            "• {props.snapshot.name}"
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
    id: Uuid,
    name: String,
    is_active: bool,
    on_click: Callback<Uuid>,
}

/// Individual profile item.
#[component]
fn ProfileItem(props: ProfileItemProps) -> Element {
    let id = props.id;

    rsx! {
        button {
            class: if props.is_active {
                "w-full px-3 py-2 text-left rounded-lg bg-zinc-800 text-zinc-200 text-sm font-medium transition-colors"
            } else {
                "w-full px-3 py-2 text-left rounded-lg hover:bg-zinc-800/50 text-zinc-400 text-sm transition-colors"
            },
            onclick: move |_| props.on_click.call(id),
            "{props.name}"
        }
    }
}

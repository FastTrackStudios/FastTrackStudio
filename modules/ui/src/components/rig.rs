//! Rig Control View components
//!
//! This module provides UI components for controlling a live VST rig,
//! including preset selection, scene navigation, and block controls.

use audio_controls::prelude::{
    block_definitions, BlockCategory, BlockDefinition, BlockInstance, BlockParameter as AudioBlockParameter,
    BlockView, FormFactor, LayoutConstraints, LevelOfDetail, ParameterFormat, Pedalboard,
};
use dioxus::prelude::*;
use fts::rig::core::{PresetTag, TagRegistry};
use fts::rig::service::{
    BlockOverrideInfo, BlockParameterInfo, GlobalBlockInfo, PresetBlockInfo, PresetInfo, SceneInfo,
    SectionInfo, SetlistInfo, SnapshotInfo, SongInfo,
};
use fts::rig::{
    RIG_AVAILABLE_PRESETS, RIG_AVAILABLE_SETLISTS, RIG_CONNECTED, RIG_CURRENT_PRESET,
    RIG_CURRENT_SCENE, RIG_CURRENT_SETLIST, RIG_CURRENT_SONG, RIG_CURRENT_SNAPSHOT_ID,
    RIG_GLOBAL_BLOCKS, RIG_INFO, RIG_LOADING, RIG_PROFILE, RIG_SCENE_INDEX, RIG_SECTIONS,
    RIG_SETLIST_SONGS, RIG_SONG_INDEX,
};
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Utf32Str,
};
use uuid::Uuid;

use crate::hooks::{use_rig_actions, use_rig_subscription};

// ============================================================================
// Effective Block (with snapshot overrides applied)
// ============================================================================

/// A block with its effective parameter values (after applying snapshot overrides)
#[derive(Debug, Clone, PartialEq)]
pub struct EffectiveBlock {
    /// Block ID
    pub id: Uuid,
    /// Block name
    pub name: String,
    /// Block type for UI rendering
    pub block_type: String,
    /// Order in signal chain
    pub order: u8,
    /// Effective bypassed state (may be overridden by snapshot)
    pub bypassed: bool,
    /// Effective parameters (with snapshot overrides applied)
    pub parameters: Vec<EffectiveParameter>,
}

/// A parameter with its effective value
#[derive(Debug, Clone, PartialEq)]
pub struct EffectiveParameter {
    /// Parameter ID
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Effective value (0.0 - 1.0)
    pub value: f64,
    /// Min display value
    pub min_display: f64,
    /// Max display value
    pub max_display: f64,
    /// Unit suffix
    pub unit: String,
    /// Whether this value is overridden by the current snapshot
    pub is_overridden: bool,
}

impl EffectiveBlock {
    /// Create an effective block from a preset block and optional snapshot override
    pub fn from_preset_block(
        block: &PresetBlockInfo,
        snapshot_override: Option<&BlockOverrideInfo>,
    ) -> Self {
        // Determine effective bypass state
        let bypassed = snapshot_override
            .and_then(|o| o.bypassed)
            .unwrap_or(block.bypassed);

        // Build effective parameters
        let parameters = block
            .parameters
            .iter()
            .map(|param| {
                // Check if this parameter has a snapshot override
                let override_value = snapshot_override.and_then(|o| {
                    o.parameters
                        .iter()
                        .find(|p| p.param_id == param.id)
                        .map(|p| p.value)
                });

                EffectiveParameter {
                    id: param.id.clone(),
                    name: param.name.clone(),
                    value: override_value.unwrap_or(param.value),
                    min_display: param.min_display,
                    max_display: param.max_display,
                    unit: param.unit.clone(),
                    is_overridden: override_value.is_some(),
                }
            })
            .collect();

        Self {
            id: block.id,
            name: block.name.clone(),
            block_type: block.block_type.clone(),
            order: block.order,
            bypassed,
            parameters,
        }
    }
}

/// View modes for displaying blocks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BlockViewMode {
    /// Simple card/list view
    #[default]
    Cards,
    /// Vintage guitar pedalboard style
    Pedalboard,
    /// 19" rack mount style
    Rack1U,
    /// 500-Series modular style
    Series500,
}

impl BlockViewMode {
    /// Get display name for this view mode
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Pedalboard => "Pedalboard",
            Self::Cards => "Cards",
            Self::Rack1U => "1U Rack",
            Self::Series500 => "500 Series",
        }
    }

    /// Get icon/short label for this view mode
    pub const fn icon(&self) -> &'static str {
        match self {
            Self::Pedalboard => "🎸",
            Self::Cards => "📋",
            Self::Rack1U => "🔲",
            Self::Series500 => "▮▮",
        }
    }

    /// Get all view modes
    pub const fn all() -> &'static [BlockViewMode] {
        &[
            BlockViewMode::Cards,
            BlockViewMode::Pedalboard,
            BlockViewMode::Rack1U,
            BlockViewMode::Series500,
        ]
    }
}

/// Main rig control view component
///
/// This component:
/// - Subscribes to the rig service to populate global signals
/// - Provides action callbacks via `use_rig_actions()`
/// - Renders tag-based preset browser with fuzzy search
///
/// Must be wrapped in a `RigServiceProvider` to work.
#[component]
pub fn RigControlView() -> Element {
    // Subscribe to service and populate global signals
    use_rig_subscription();

    // Get action callbacks from service
    let actions = use_rig_actions();

    // Use memos to reactively track global signals
    let profile_memo = use_memo(move || RIG_PROFILE.read().clone());
    let rig_memo = use_memo(move || RIG_INFO.read().clone());
    let preset_memo = use_memo(move || RIG_CURRENT_PRESET.read().clone());
    let song_memo = use_memo(move || RIG_CURRENT_SONG.read().clone());
    let scene_memo = use_memo(move || RIG_CURRENT_SCENE.read().clone());
    let scene_index_memo = use_memo(move || *RIG_SCENE_INDEX.read());
    let sections_memo = use_memo(move || RIG_SECTIONS.read().clone());
    let _global_blocks_memo = use_memo(move || RIG_GLOBAL_BLOCKS.read().clone());
    let presets_memo = use_memo(move || RIG_AVAILABLE_PRESETS.read().clone());
    let loading_memo = use_memo(move || *RIG_LOADING.read());
    let connected_memo = use_memo(move || *RIG_CONNECTED.read());

    // Compute effective blocks: preset blocks with snapshot overrides applied
    let effective_blocks = use_memo(move || {
        let preset = RIG_CURRENT_PRESET.read();
        let snapshot_id = *RIG_CURRENT_SNAPSHOT_ID.read();

        tracing::debug!(
            "effective_blocks memo: preset={:?}, snapshot_id={:?}",
            preset.as_ref().map(|p| &p.name),
            snapshot_id
        );

        if let Some(preset) = preset.as_ref() {
            // Find the current snapshot's overrides
            let snapshot_overrides: Option<&SnapshotInfo> = snapshot_id
                .and_then(|sid| preset.snapshots.iter().find(|s| s.id == sid));

            tracing::debug!(
                "effective_blocks: found snapshot overrides: {:?}",
                snapshot_overrides.map(|s| (&s.name, s.block_overrides.len()))
            );

            // Build effective blocks with snapshot overrides
            preset
                .blocks
                .iter()
                .map(|block| {
                    let block_override = snapshot_overrides
                        .and_then(|s| s.block_overrides.iter().find(|o| o.block_id == block.id));

                    EffectiveBlock::from_preset_block(block, block_override)
                })
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        }
    });

    // Tag registry with default tags (for resolving tag names during search)
    let tag_registry = use_memo(|| TagRegistry::with_defaults());

    // Local state
    let mut search_query = use_signal(String::new);
    // Track collapsed presets (default is expanded, so we track which are collapsed)
    let mut collapsed_preset_ids = use_signal(Vec::<Uuid>::new);
    let mut block_view_mode = use_signal(BlockViewMode::default);

    // Filter presets based on fuzzy search (searches name + tag names + snapshot names)
    let filtered_presets = use_memo(move || {
        let query = search_query();
        let all_presets = presets_memo();
        let registry = tag_registry.read();

        if query.is_empty() {
            return all_presets;
        }

        // Use nucleo matcher for fuzzy search
        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = Pattern::parse(&query, CaseMatching::Smart, Normalization::Smart);

        let mut scored_presets: Vec<(PresetInfo, u32)> = all_presets
            .into_iter()
            .filter_map(|preset| {
                // Build searchable text: name + all tag names + all snapshot names
                let tag_names: Vec<String> = preset
                    .tag_ids
                    .iter()
                    .filter_map(|id| registry.get(*id))
                    .map(|t| t.name.clone())
                    .collect();

                let snapshot_names = preset.snapshot_names.join(" ");

                let search_text = format!("{} {} {}", preset.name, tag_names.join(" "), snapshot_names);

                // Score the match
                let mut buf = Vec::new();
                let haystack = Utf32Str::new(&search_text, &mut buf);

                pattern.score(haystack, &mut matcher).map(|score| (preset, score))
            })
            .collect();

        // Sort by score descending (best matches first)
        scored_presets.sort_by(|a, b| b.1.cmp(&a.1));

        scored_presets.into_iter().map(|(p, _)| p).collect::<Vec<_>>()
    });

    // Derive display values for search status
    let query_for_display = search_query();
    let preset_count = filtered_presets.read().len();
    let total_count = presets_memo().len();
    let has_search = !query_for_display.is_empty();

    rsx! {
        div { class: "flex flex-col h-full bg-background text-foreground",
            // Main content area
            div { class: "flex-1 flex overflow-hidden",
                // === PRESET LIST with search ===
                div { class: "w-64 border-r border-border bg-card flex flex-col",
                    // Header with search
                    div { class: "p-3 border-b border-border",
                        div { class: "flex items-center justify-between mb-2",
                            h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                                "Presets"
                            }
                            span { class: "text-xs text-muted-foreground bg-muted px-1.5 py-0.5 rounded",
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
                                class: "w-full px-3 py-2 pl-8 text-sm bg-muted border border-border rounded-md
                                        placeholder:text-muted-foreground focus:outline-none focus:ring-1 focus:ring-primary",
                                r#type: "text",
                                placeholder: "Search presets...",
                                value: "{search_query}",
                                oninput: move |e| search_query.set(e.value().clone()),
                            }
                            // Search icon
                            span { class: "absolute left-2.5 top-2.5 text-muted-foreground text-sm",
                                "?"
                            }
                            // Clear button
                            if has_search {
                                button {
                                    class: "absolute right-2 top-2 text-muted-foreground hover:text-foreground text-sm px-1",
                                    onclick: move |_| search_query.set(String::new()),
                                    "x"
                                }
                            }
                        }

                        // Search hint
                        p { class: "text-xs text-muted-foreground mt-2",
                            "Search by name or tags"
                        }
                    }

                    // Preset list with custom scrollbar
                    div { class: "preset-list flex-1 overflow-y-auto",
                        if filtered_presets.read().is_empty() {
                            div { class: "p-4 text-center text-muted-foreground text-sm",
                                if has_search {
                                    p { "No presets match" }
                                    p { class: "text-xs mt-1 text-blue-400", "\"{query_for_display}\"" }
                                } else {
                                    p { "No presets available" }
                                }
                            }
                        } else {
                            for preset in filtered_presets.read().iter() {
                                PresetWithSnapshots {
                                    preset: preset.clone(),
                                    is_active: preset_memo.read().as_ref().map(|p| p.id) == Some(preset.id),
                                    // Default expanded - only collapsed if in the collapsed list
                                    is_expanded: !collapsed_preset_ids().contains(&preset.id),
                                    registry: tag_registry.read().clone(),
                                    on_click: actions.load_preset.clone(),
                                    on_toggle_expand: {
                                        let preset_id = preset.id;
                                        Callback::new(move |_: ()| {
                                            let mut collapsed = collapsed_preset_ids();
                                            if collapsed.contains(&preset_id) {
                                                // Currently collapsed, expand it
                                                collapsed.retain(|&id| id != preset_id);
                                            } else {
                                                // Currently expanded, collapse it
                                                collapsed.push(preset_id);
                                            }
                                            collapsed_preset_ids.set(collapsed);
                                        })
                                    },
                                    on_snapshot_click: actions.load_preset_with_snapshot.clone(),
                                }
                            }
                        }
                    }
                }

                // === CENTER: Current state and controls ===
                div { class: "flex-1 flex flex-col overflow-hidden",
                    // Top bar with current preset info
                    div { class: "h-16 border-b border-border bg-card px-4 flex items-center justify-between",
                        div { class: "flex items-center gap-4",
                            // Current preset name
                            if let Some(preset) = preset_memo.read().as_ref() {
                                span { class: "text-lg font-semibold", "{preset.name}" }
                                span { class: "px-2 py-1 bg-secondary text-secondary-foreground text-xs rounded",
                                    "{preset.category}"
                                }
                            } else {
                                span { class: "text-lg font-semibold text-muted-foreground", "No Preset Selected" }
                            }
                        }
                        div { class: "flex items-center gap-3",
                            // Connection indicator
                            div { class: "flex items-center gap-2",
                                div {
                                    class: if connected_memo() { "w-2 h-2 rounded-full bg-green-500" } else { "w-2 h-2 rounded-full bg-red-500" },
                                }
                                span { class: "text-xs text-muted-foreground",
                                    if connected_memo() { "Connected" } else { "Disconnected" }
                                }
                            }
                            if loading_memo() {
                                span { class: "text-sm text-muted-foreground animate-pulse", "Loading..." }
                            }
                        }
                    }

                    // Current preset display
                    div { class: "p-4 border-b border-border bg-card/50",
                        CurrentPresetCardCompact {
                            preset: preset_memo.read().clone(),
                            scene: scene_memo.read().clone(),
                            search_query: query_for_display.clone(),
                            preset_count: preset_count,
                            total_count: total_count,
                        }
                    }

                    // Main content grid
                    div { class: "flex-1 p-4 overflow-y-auto",
                        // View mode selector and title
                        div { class: "flex items-center justify-between mb-4",
                            h3 { class: "text-lg font-semibold", "Blocks" }

                            // View mode buttons
                            div { class: "flex items-center gap-1 bg-muted rounded-lg p-1",
                                for mode in BlockViewMode::all() {
                                    ViewModeButton {
                                        mode: *mode,
                                        is_active: *block_view_mode.read() == *mode,
                                        on_click: move |m| block_view_mode.set(m),
                                    }
                                }
                            }
                        }

                        // Render blocks based on selected view mode
                        // Uses preset-specific blocks with snapshot parameter overrides
                        div { class: "mb-6",
                            match *block_view_mode.read() {
                                BlockViewMode::Pedalboard => rsx! {
                                    EffectivePedalboard {
                                        blocks: effective_blocks(),
                                        on_toggle_bypass: actions.toggle_block_bypass.clone(),
                                        on_param_change: actions.set_block_parameter.clone(),
                                    }
                                },
                                BlockViewMode::Cards => rsx! {
                                    EffectiveCardsView {
                                        blocks: effective_blocks(),
                                        on_toggle_bypass: actions.toggle_block_bypass.clone(),
                                        on_param_change: actions.set_block_parameter.clone(),
                                    }
                                },
                                BlockViewMode::Rack1U => rsx! {
                                    EffectiveRackView {
                                        blocks: effective_blocks(),
                                        on_toggle_bypass: actions.toggle_block_bypass.clone(),
                                        on_param_change: actions.set_block_parameter.clone(),
                                    }
                                },
                                BlockViewMode::Series500 => rsx! {
                                    Effective500View {
                                        blocks: effective_blocks(),
                                        on_toggle_bypass: actions.toggle_block_bypass.clone(),
                                        on_param_change: actions.set_block_parameter.clone(),
                                    }
                                },
                            }
                        }

                        // Engines (sections) - more relevant for Keys rigs
                        if !sections_memo().is_empty() {
                            h3 { class: "text-lg font-semibold mb-3", "Engines" }
                            div { class: "grid grid-cols-2 gap-3",
                                for section in sections_memo() {
                                    SectionCard {
                                        section: section.clone(),
                                        on_toggle: actions.toggle_section.clone(),
                                    }
                                }
                            }
                        }
                    }
                }

                // === RIGHT SIDEBAR: Scenes (top) and Songs (bottom) ===
                div { class: "w-64 border-l border-border bg-card flex flex-col",
                    // === TOP PANEL: SCENES ===
                    div { class: "flex-1 flex flex-col min-h-0 border-b border-border",
                        // Song header with scene info
                        if let Some(song) = song_memo() {
                            div { class: "p-3 border-b border-border bg-card/50",
                                h2 { class: "font-semibold text-base truncate", "{song.name}" }
                                p { class: "text-xs text-muted-foreground",
                                    {format!("{} scenes", song.scene_count)}
                                }
                            }
                        } else {
                            div { class: "p-3 border-b border-border",
                                h2 { class: "font-semibold text-base text-muted-foreground", "No Song" }
                            }
                        }

                        // Scene list
                        div { class: "flex-1 overflow-y-auto p-2",
                            h3 { class: "text-xs font-semibold text-muted-foreground mb-2 px-2", "SCENES" }
                            if let Some(song) = song_memo() {
                                for scene_idx in 0..song.scene_count {
                                    SceneItem {
                                        index: scene_idx,
                                        is_active: scene_idx == scene_index_memo(),
                                        name: song.scene_names.get(scene_idx).cloned().unwrap_or_else(|| format!("Scene {}", scene_idx + 1)),
                                        on_click: actions.go_to_scene.clone(),
                                    }
                                }
                            }
                        }

                        // Scene navigation buttons
                        div { class: "p-2 border-t border-border flex gap-2",
                            button {
                                class: "flex-1 px-2 py-1.5 bg-muted hover:bg-muted/80 rounded text-xs font-medium",
                                onclick: move |_| actions.prev_scene.call(()),
                                "← Prev"
                            }
                            button {
                                class: "flex-1 px-2 py-1.5 bg-muted hover:bg-muted/80 rounded text-xs font-medium",
                                onclick: move |_| actions.next_scene.call(()),
                                "Next →"
                            }
                        }
                    }

                    // === BOTTOM PANEL: SONGS ===
                    div { class: "flex-1 min-h-80 flex flex-col",
                        // Setlist header with dropdown
                        div { class: "p-2 border-b border-border bg-muted/30",
                            div { class: "flex items-center gap-2 px-2",
                                span { class: "text-xs font-semibold text-muted-foreground", "SETLIST:" }
                                SetlistDropdown {
                                    on_select: actions.load_setlist.clone(),
                                }
                            }
                        }

                        // Song list
                        div { class: "flex-1 overflow-y-auto p-2",
                            {
                                let songs = RIG_SETLIST_SONGS.read();
                                let current_song_idx = *RIG_SONG_INDEX.read();
                                rsx! {
                                    for song in songs.iter() {
                                        SongItem {
                                            song: song.clone(),
                                            is_active: song.index == current_song_idx,
                                            on_click: actions.go_to_song.clone(),
                                        }
                                    }
                                }
                            }
                        }

                        // Song navigation buttons
                        div { class: "p-2 border-t border-border flex gap-2",
                            button {
                                class: "flex-1 px-2 py-1.5 bg-muted hover:bg-muted/80 rounded text-xs font-medium",
                                onclick: move |_| actions.prev_song.call(()),
                                "← Prev Song"
                            }
                            button {
                                class: "flex-1 px-2 py-1.5 bg-muted hover:bg-muted/80 rounded text-xs font-medium",
                                onclick: move |_| actions.next_song.call(()),
                                "Next Song →"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Compact current preset card with search info
#[component]
fn CurrentPresetCardCompact(
    preset: Option<PresetInfo>,
    scene: Option<SceneInfo>,
    search_query: String,
    preset_count: usize,
    total_count: usize,
) -> Element {
    let has_search = !search_query.is_empty();

    rsx! {
        div { class: "flex items-center justify-between",
            div {
                // Current preset name
                h2 { class: "text-2xl font-bold",
                    if let Some(p) = &preset {
                        "{p.name}"
                    } else {
                        "No Preset Selected"
                    }
                }
                // Preset category and scene info
                div { class: "flex items-center gap-3 mt-1",
                    if let Some(p) = &preset {
                        span { class: "text-sm text-muted-foreground",
                            "{p.category}"
                        }
                    }
                    if let Some(s) = &scene {
                        span { class: "text-sm text-muted-foreground", "•" }
                        span { class: "text-sm text-muted-foreground",
                            "Scene: {s.name}"
                            if let Some(snapshot) = &s.snapshot_name {
                                span { class: "ml-1 opacity-75", "({snapshot})" }
                            }
                        }
                    }
                }
                // Search/count info
                if has_search {
                    p { class: "text-xs text-blue-400 mt-2",
                        "Showing {preset_count} of {total_count} presets"
                    }
                } else {
                    p { class: "text-xs text-muted-foreground mt-2",
                        "{total_count} presets available"
                    }
                }
            }
            // Snapshot pills
            if let Some(p) = &preset {
                if !p.snapshot_names.is_empty() {
                    div { class: "flex flex-col items-end gap-1",
                        span { class: "text-xs text-muted-foreground mb-1", "Snapshots:" }
                        div { class: "flex gap-2",
                            for snapshot_name in &p.snapshot_names {
                                span { class: "px-2 py-1 bg-muted text-muted-foreground text-xs rounded",
                                    "{snapshot_name}"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Preset Tag Display Component
// ============================================================================

/// Display tags for a preset with consistent colors
/// Shows top 3 tags by priority, with option to expand
#[component]
fn PresetTagsDisplay(
    tag_ids: Vec<Uuid>,
    registry: TagRegistry,
    max_visible: Option<usize>,
) -> Element {
    let max_visible = max_visible.unwrap_or(3);
    let mut show_all = use_signal(|| false);

    // Resolve tag IDs to actual tags and sort by priority
    let mut resolved_tags: Vec<&PresetTag> = tag_ids
        .iter()
        .filter_map(|id| registry.get(*id))
        .collect();

    // Sort by priority (highest first) for consistent display order
    resolved_tags.sort_by(|a, b| b.priority.cmp(&a.priority));

    if resolved_tags.is_empty() {
        return rsx! {};
    }

    let total_tags = resolved_tags.len();
    let has_more = total_tags > max_visible;
    let display_all = show_all();

    let visible_tags: Vec<&PresetTag> = if display_all {
        resolved_tags
    } else {
        resolved_tags.into_iter().take(max_visible).collect()
    };

    let hidden_count = if display_all { 0 } else { total_tags.saturating_sub(max_visible) };

    rsx! {
        div { class: "flex flex-wrap items-center gap-1 mt-1",
            for tag in visible_tags {
                TagChip {
                    key: "{tag.id}",
                    tag: tag.clone(),
                }
            }

            // "+N more" button to expand
            if has_more && !display_all {
                button {
                    class: "text-xs text-muted-foreground hover:text-foreground px-1.5 py-0.5 rounded hover:bg-muted transition-colors",
                    onclick: move |e| {
                        e.stop_propagation();
                        show_all.set(true);
                    },
                    "+{hidden_count}"
                }
            }

            // Collapse button when expanded
            if display_all && has_more {
                button {
                    class: "text-xs text-muted-foreground hover:text-foreground px-1.5 py-0.5 rounded hover:bg-muted transition-colors",
                    onclick: move |e| {
                        e.stop_propagation();
                        show_all.set(false);
                    },
                    "−"
                }
            }
        }
    }
}

/// Single tag chip with consistent category-based coloring (Scout-style)
#[component]
fn TagChip(tag: PresetTag) -> Element {
    // Scout-style colors - more saturated, category-based
    // Using colors that match Scout's vibrant tag palette
    let (bg_color, text_color) = match tag.category {
        fts::rig::core::TagCategory::BaseTone => ("#3B82F6", "#93C5FD"),     // Blue
        fts::rig::core::TagCategory::Genre => ("#22C55E", "#86EFAC"),        // Green
        fts::rig::core::TagCategory::SubGenre => ("#EAB308", "#FDE047"),     // Yellow
        fts::rig::core::TagCategory::Archetype => ("#A855F7", "#D8B4FE"),    // Purple
        fts::rig::core::TagCategory::Song => ("#EC4899", "#F9A8D4"),         // Pink
        fts::rig::core::TagCategory::Character => ("#F97316", "#FDBA74"),    // Orange
        fts::rig::core::TagCategory::Context => ("#06B6D4", "#67E8F9"),      // Cyan
        fts::rig::core::TagCategory::Gear => ("#6B7280", "#9CA3AF"),         // Gray
        fts::rig::core::TagCategory::Custom => ("#78716C", "#A8A29E"),       // Stone
    };

    // Scout-style: solid background with lighter text
    let style = format!(
        "background-color: {}; color: {}; text-shadow: 0 1px 2px rgba(0,0,0,0.3);",
        bg_color, text_color
    );

    rsx! {
        span {
            class: "text-xs px-1.5 py-0.5 rounded font-semibold whitespace-nowrap",
            style: "{style}",
            "{tag.name}"
        }
    }
}

/// Preset item with expandable snapshots
#[component]
fn PresetWithSnapshots(
    preset: PresetInfo,
    is_active: bool,
    is_expanded: bool,
    registry: TagRegistry,
    on_click: Callback<Uuid>,
    on_toggle_expand: Callback<()>,
    on_snapshot_click: Callback<(Uuid, Uuid)>,
) -> Element {
    // Read snapshot ID directly from global signal for reactivity
    let current_snapshot_id = *RIG_CURRENT_SNAPSHOT_ID.read();

    let has_snapshots = !preset.snapshot_names.is_empty();
    let preset_id = preset.id;

    let base_class = "px-3 py-2 cursor-pointer transition-colors";
    let active_class = if is_active {
        "bg-primary/20 border-l-2 border-primary"
    } else {
        "hover:bg-muted border-l-2 border-transparent"
    };

    rsx! {
        div { class: "border-b border-border/50",
            // Preset header
            div {
                class: "{base_class} {active_class}",
                onclick: move |_| on_click.call(preset_id),
                div { class: "flex items-start justify-between",
                    div { class: "flex-1 min-w-0",
                        div { class: "font-medium text-sm truncate", "{preset.name}" }
                        div { class: "text-xs text-muted-foreground", "{preset.category}" }

                        // Tag display
                        if !preset.tag_ids.is_empty() {
                            PresetTagsDisplay {
                                tag_ids: preset.tag_ids.clone(),
                                registry: registry.clone(),
                                max_visible: 3,
                            }
                        }
                    }
                    if has_snapshots {
                        button {
                            class: "ml-2 p-1 hover:bg-muted rounded text-muted-foreground flex-shrink-0",
                            onclick: move |e| {
                                e.stop_propagation();
                                on_toggle_expand.call(());
                            },
                            if is_expanded {
                                "▼"
                            } else {
                                "▶"
                            }
                        }
                    }
                }
            }

            // Expanded snapshots
            if is_expanded && has_snapshots {
                div { class: "bg-muted/30 py-1",
                    for snapshot in preset.snapshots.iter() {
                        SnapshotItem {
                            preset_id,
                            snapshot: snapshot.clone(),
                            is_active: current_snapshot_id == Some(snapshot.id),
                            registry: registry.clone(),
                            on_click: on_snapshot_click.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Individual snapshot item with optional tag display
#[component]
fn SnapshotItem(
    preset_id: Uuid,
    snapshot: SnapshotInfo,
    is_active: bool,
    registry: TagRegistry,
    on_click: Callback<(Uuid, Uuid)>,
) -> Element {
    let snapshot_id = snapshot.id;
    let has_tags = !snapshot.tag_ids.is_empty();

    let base_class = "pl-6 pr-3 py-1.5 text-xs cursor-pointer transition-colors";
    let active_class = if is_active {
        "bg-primary/10 text-primary"
    } else {
        "hover:bg-muted text-muted-foreground hover:text-foreground"
    };

    rsx! {
        div {
            class: "{base_class} {active_class}",
            onclick: move |_| on_click.call((preset_id, snapshot_id)),
            div { class: "flex items-center gap-2",
                span { "• {snapshot.name}" }
                // Show tags if there are any manual (non-auto-derived) tags
                if has_tags {
                    div { class: "flex gap-1",
                        for tag_id in &snapshot.tag_ids {
                            if let Some(tag) = registry.get(*tag_id) {
                                TagChip {
                                    key: "{tag.id}",
                                    tag: tag.clone(),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Song header in the scene navigator
#[component]
fn SongHeader(song: SongInfo) -> Element {
    rsx! {
        div { class: "p-3 border-b border-border",
            h2 { class: "font-semibold text-lg truncate", "{song.name}" }
            p { class: "text-sm text-muted-foreground",
                {format!("{} scenes", song.scene_count)}
            }
        }
    }
}

/// Individual scene item in the navigator
#[component]
fn SceneItem(
    index: usize,
    is_active: bool,
    name: String,
    on_click: Callback<usize>,
) -> Element {
    let base_class = "px-3 py-2 rounded cursor-pointer transition-colors mb-1";
    let active_class = if is_active {
        "bg-primary text-primary-foreground"
    } else {
        "hover:bg-muted"
    };

    rsx! {
        div {
            class: "{base_class} {active_class}",
            onclick: move |_| on_click.call(index),
            div { class: "flex items-center justify-between",
                span { class: "font-medium text-sm truncate", "{name}" }
                span { class: "text-xs opacity-60 ml-2", "{index + 1}" }
            }
        }
    }
}

/// Dropdown for selecting a setlist
#[component]
fn SetlistDropdown(on_select: Callback<Uuid>) -> Element {
    let current_setlist = RIG_CURRENT_SETLIST.read();
    let available_setlists = RIG_AVAILABLE_SETLISTS.read();
    let mut is_open = use_signal(|| false);

    let current_name = current_setlist
        .as_ref()
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "No Setlist".to_string());

    rsx! {
        div { class: "relative flex-1",
            // Dropdown button
            button {
                class: "w-full flex items-center justify-between px-2 py-1 text-xs font-medium bg-background border border-border rounded hover:bg-muted transition-colors",
                onclick: move |_| is_open.set(!is_open()),
                span { class: "truncate", "{current_name}" }
                span { class: "ml-1 text-muted-foreground",
                    if *is_open.read() { "▲" } else { "▼" }
                }
            }

            // Dropdown menu
            if *is_open.read() {
                div {
                    class: "absolute top-full left-0 right-0 mt-1 bg-popover border border-border rounded shadow-lg z-50 max-h-48 overflow-y-auto",
                    {
                        available_setlists.iter().map(|setlist| {
                            let setlist_id = setlist.id;
                            let setlist_name = setlist.name.clone();
                            let song_count = setlist.song_count;
                            let is_current = current_setlist.as_ref().map(|c| c.id == setlist_id).unwrap_or(false);
                            let on_select = on_select.clone();
                            let mut is_open = is_open.clone();
                            rsx! {
                                button {
                                    key: "{setlist_id}",
                                    class: if is_current {
                                        "w-full px-3 py-2 text-left text-xs hover:bg-muted flex items-center justify-between bg-primary/10"
                                    } else {
                                        "w-full px-3 py-2 text-left text-xs hover:bg-muted flex items-center justify-between"
                                    },
                                    onclick: move |_| {
                                        on_select.call(setlist_id);
                                        is_open.set(false);
                                    },
                                    span { class: "font-medium truncate", "{setlist_name}" }
                                    span { class: "text-muted-foreground ml-2",
                                        {format!("{} songs", song_count)}
                                    }
                                }
                            }
                        })
                    }
                }
            }
        }
    }
}

/// Individual song item in the setlist
#[component]
fn SongItem(
    song: SongInfo,
    is_active: bool,
    on_click: Callback<usize>,
) -> Element {
    let index = song.index;
    let base_class = "px-3 py-2 rounded cursor-pointer transition-colors mb-1";
    let active_class = if is_active {
        "bg-primary/20 border-l-2 border-primary"
    } else {
        "hover:bg-muted border-l-2 border-transparent"
    };

    rsx! {
        div {
            class: "{base_class} {active_class}",
            onclick: move |_| on_click.call(index),
            div { class: "flex items-center justify-between",
                div { class: "flex-1 min-w-0",
                    span { class: "font-medium text-sm truncate block", "{song.name}" }
                    span { class: "text-xs text-muted-foreground",
                        {format!("{} scenes", song.scene_count)}
                    }
                }
                span { class: "text-xs opacity-60 ml-2", "{index + 1}" }
            }
        }
    }
}


/// Card for a rig section
#[component]
fn SectionCard(section: SectionInfo, on_toggle: Callback<Uuid>) -> Element {
    let enabled_class = if section.enabled {
        "border-primary bg-card"
    } else {
        "border-border bg-muted/50 opacity-60"
    };

    rsx! {
        div {
            class: "border rounded-lg p-3 cursor-pointer transition-all {enabled_class}",
            onclick: move |_| on_toggle.call(section.id),
            div { class: "flex items-center justify-between mb-2",
                h4 { class: "font-semibold", "{section.name}" }
                div {
                    class: if section.enabled { "w-2 h-2 rounded-full bg-green-500" } else { "w-2 h-2 rounded-full bg-muted-foreground" },
                }
            }
            p { class: "text-sm text-muted-foreground",
                {format!("{} layer{}", section.layer_count, if section.layer_count == 1 { "" } else { "s" })}
            }
        }
    }
}

/// Card for a global effect block
#[component]
fn GlobalBlockCard(block: GlobalBlockInfo, on_toggle_bypass: Callback<Uuid>) -> Element {
    let enabled_class = if block.enabled {
        "border-primary bg-card"
    } else {
        "border-border bg-muted/50 opacity-60"
    };

    rsx! {
        div {
            class: "border rounded-lg p-3 cursor-pointer transition-all {enabled_class}",
            onclick: move |_| on_toggle_bypass.call(block.id),
            div { class: "flex items-center justify-between",
                span { class: "font-medium text-sm", "{block.name}" }
                div {
                    class: if block.enabled { "w-2 h-2 rounded-full bg-green-500" } else { "w-2 h-2 rounded-full bg-red-500" },
                }
            }
        }
    }
}

// ============================================================================
// View Mode Selector
// ============================================================================

/// Props for view mode button
#[derive(Props, Clone, PartialEq)]
struct ViewModeButtonProps {
    mode: BlockViewMode,
    is_active: bool,
    on_click: EventHandler<BlockViewMode>,
}

/// Button for selecting a view mode
#[component]
fn ViewModeButton(props: ViewModeButtonProps) -> Element {
    let base_class = "px-3 py-1.5 rounded-md text-xs font-medium transition-all cursor-pointer";
    let active_class = if props.is_active {
        "bg-primary text-primary-foreground shadow-sm"
    } else {
        "text-muted-foreground hover:text-foreground hover:bg-muted/50"
    };

    let mode = props.mode;

    rsx! {
        button {
            class: "{base_class} {active_class}",
            onclick: move |_| props.on_click.call(mode),
            title: "{props.mode.display_name()}",

            span { class: "mr-1", "{props.mode.icon()}" }
            span { class: "hidden sm:inline", "{props.mode.display_name()}" }
        }
    }
}

// ============================================================================
// Guitar Pedalboard Components
// ============================================================================

/// Get the appropriate BlockDefinition for a block name.
fn get_block_definition(name: &str) -> BlockDefinition {
    let name_lower = name.to_lowercase();

    if name_lower.contains("drive") || name_lower.contains("od") || name_lower.contains("dist") {
        block_definitions::drive()
    } else if name_lower.contains("amp") {
        block_definitions::amp()
    } else if name_lower.contains("cab") || name_lower.contains("ir") {
        block_definitions::cabinet()
    } else if name_lower.contains("delay") || name_lower.contains("echo") {
        block_definitions::delay()
    } else if name_lower.contains("reverb") || name_lower.contains("verb") || name_lower.contains("room") || name_lower.contains("hall") {
        block_definitions::reverb()
    } else {
        // Default to drive for unknown blocks
        let mut def = block_definitions::drive();
        def.name = name.to_string();
        def.type_id = name.to_lowercase().replace(' ', "_");
        def
    }
}

/// Create a BlockInstance from GlobalBlockInfo.
fn create_block_instance(block: &GlobalBlockInfo, definition: &BlockDefinition) -> BlockInstance {
    let mut instance = BlockInstance::from_definition(definition, block.id.to_string());
    instance.name = block.name.clone();
    instance.bypassed = block.bypassed;
    instance.form_factor = FormFactor::Pedal;
    instance.lod = LevelOfDetail::Compact;
    instance
}

/// Props for the GuitarPedalboard component.
#[derive(Props, Clone, PartialEq)]
struct GuitarPedalboardProps {
    blocks: Vec<GlobalBlockInfo>,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

/// A pedalboard view showing all guitar rig blocks as pedals.
#[component]
fn GuitarPedalboard(props: GuitarPedalboardProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in the current rig"
            }
        };
    }

    // Sort blocks by order
    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        // Pedalboard container with wood-grain aesthetic and custom scrollbar
        div {
            class: "pedalboard flex items-end gap-4 p-6
                    bg-gradient-to-b from-zinc-800 to-zinc-900
                    rounded-xl border-4 border-zinc-700
                    shadow-inner overflow-x-auto
                    scrollbar-thin scrollbar-track-zinc-800 scrollbar-thumb-zinc-600
                    hover:scrollbar-thumb-zinc-500",
            style: "scrollbar-width: thin; scrollbar-color: #52525b #27272a;",

            for block in sorted_blocks {
                PedalBlock {
                    key: "{block.id}",
                    block: block.clone(),
                    on_bypass_toggle: props.on_toggle_bypass.clone(),
                    on_param_change: props.on_param_change.clone(),
                }
            }
        }
    }
}

/// Props for a single pedal block.
#[derive(Props, Clone, PartialEq)]
struct PedalBlockProps {
    block: GlobalBlockInfo,
    on_bypass_toggle: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

/// A single pedal in the pedalboard.
#[component]
fn PedalBlock(props: PedalBlockProps) -> Element {
    let definition = get_block_definition(&props.block.name);
    let instance = create_block_instance(&props.block, &definition);

    let block_id = props.block.id;
    let on_bypass = props.on_bypass_toggle.clone();
    let on_param = props.on_param_change.clone();

    // Build a map of param_id -> index for converting string IDs to indices
    let param_id_to_index: std::collections::HashMap<String, u32> = definition
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    // Vintage pedal size - fits 3-6 knobs in a row with room for name and footswitch
    let constraints = LayoutConstraints::new(
        FormFactor::Pedal,
        200, // classic pedal width
        260, // room for knobs, name, and footswitch
        LevelOfDetail::Compact,
    );

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                // Convert param_id string to index
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {
                // Macro changes could update multiple params
                // For now, we don't handle this specially
            },
        }
    }
}

// ============================================================================
// Alternative View Modes
// ============================================================================

/// Props shared by all block view containers
#[derive(Props, Clone, PartialEq)]
struct BlockContainerProps {
    blocks: Vec<GlobalBlockInfo>,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

/// Simple cards view - blocks displayed as expandable cards
#[component]
fn BlockCardsView(props: BlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in the current rig"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        div { class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4",
            for block in sorted_blocks {
                BlockCard {
                    key: "{block.id}",
                    block: block.clone(),
                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                    on_param_change: props.on_param_change.clone(),
                }
            }
        }
    }
}

/// A single block as a card
#[component]
fn BlockCard(
    block: GlobalBlockInfo,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_block_definition(&block.name);
    let instance = create_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = definition
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    // Square card format
    let constraints = LayoutConstraints::new(
        FormFactor::Square,
        280,
        200,
        LevelOfDetail::Compact,
    );

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

/// 1U Rack view - horizontal strip layout
#[component]
fn RackView(props: BlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in the current rig"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        // Rack frame
        div {
            class: "bg-zinc-950 rounded-lg border-4 border-zinc-600 p-2 overflow-x-auto",
            style: "background: linear-gradient(180deg, #1a1a1a 0%, #0d0d0d 100%);",

            // Rack rails
            div { class: "flex gap-1",
                // Left rail
                div {
                    class: "w-6 flex flex-col justify-between py-2",
                    style: "background: linear-gradient(90deg, #404040, #2a2a2a);",
                    for _i in 0..4 {
                        div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                    }
                }

                // Blocks in rack
                div { class: "flex-1 flex gap-2 py-2",
                    for block in sorted_blocks {
                        RackBlock {
                            key: "{block.id}",
                            block: block.clone(),
                            on_toggle_bypass: props.on_toggle_bypass.clone(),
                            on_param_change: props.on_param_change.clone(),
                        }
                    }
                }

                // Right rail
                div {
                    class: "w-6 flex flex-col justify-between py-2",
                    style: "background: linear-gradient(90deg, #2a2a2a, #404040);",
                    for _i in 0..4 {
                        div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                    }
                }
            }
        }
    }
}

/// A single block in rack format
#[component]
fn RackBlock(
    block: GlobalBlockInfo,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_block_definition(&block.name);
    let instance = create_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = definition
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    // 1U rack format - wide and short
    let constraints = LayoutConstraints::new(
        FormFactor::Rack1U,
        200,
        80,
        LevelOfDetail::Compact,
    );

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

/// 500-Series view - vertical modular strips
#[component]
fn Series500View(props: BlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in the current rig"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        // 500-series chassis
        div {
            class: "bg-zinc-900 rounded-lg border-2 border-zinc-700 p-3 overflow-x-auto",
            style: "background: linear-gradient(180deg, #262626 0%, #171717 100%);",

            // Module slots
            div { class: "flex gap-1",
                for block in sorted_blocks {
                    Series500Block {
                        key: "{block.id}",
                        block: block.clone(),
                        on_toggle_bypass: props.on_toggle_bypass.clone(),
                        on_param_change: props.on_param_change.clone(),
                    }
                }
            }
        }
    }
}

/// A single 500-series module
#[component]
fn Series500Block(
    block: GlobalBlockInfo,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_block_definition(&block.name);
    let instance = create_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = definition
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    // 500-series format - narrow and tall
    let constraints = LayoutConstraints::new(
        FormFactor::Series500,
        100,
        320,
        LevelOfDetail::Compact,
    );

    rsx! {
        div {
            class: "bg-zinc-800 rounded border border-zinc-600",
            style: "background: linear-gradient(180deg, #3f3f46 0%, #27272a 5%, #27272a 95%, #3f3f46 100%);",

            BlockView {
                definition,
                instance,
                constraints: Some(constraints),
                on_bypass_toggle: move |_bypassed| {
                    on_bypass.call(block_id);
                },
                on_param_change: move |(param_id, value): (String, f32)| {
                    if let Some(&index) = param_id_to_index.get(&param_id) {
                        on_param.call((block_id, index, value));
                    }
                },
                on_macro_change: move |_value| {},
            }
        }
    }
}

// ============================================================================
// Effective Block Views (with snapshot overrides)
// ============================================================================

/// Props for effective block view containers
#[derive(Props, Clone, PartialEq)]
struct EffectiveBlockContainerProps {
    blocks: Vec<EffectiveBlock>,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
}

/// Get a block definition based on block type
fn get_effective_block_definition(block: &EffectiveBlock) -> BlockDefinition {
    let block_type = block.block_type.as_str();

    let mut def = match block_type {
        "Drive" => block_definitions::drive(),
        "Amp" => block_definitions::amp(),
        "Cabinet" => block_definitions::cabinet(),
        "Delay" => block_definitions::delay(),
        "Reverb" => block_definitions::reverb(),
        "Compressor" => {
            let mut d = block_definitions::drive();
            d.name = "Compressor".to_string();
            d.type_id = "compressor".to_string();
            d.category = BlockCategory::Dynamics;
            d
        }
        "Modulation" => {
            let mut d = block_definitions::delay();
            d.name = "Modulation".to_string();
            d.type_id = "modulation".to_string();
            d.category = BlockCategory::Modulation;
            d
        }
        "EQ" | "Eq" => {
            let mut d = block_definitions::drive();
            d.name = "EQ".to_string();
            d.type_id = "eq".to_string();
            d.category = BlockCategory::Eq;
            d
        }
        "Gate" => {
            let mut d = block_definitions::drive();
            d.name = "Gate".to_string();
            d.type_id = "gate".to_string();
            d.category = BlockCategory::Dynamics;
            d
        }
        _ => {
            let mut d = block_definitions::drive();
            d.name = block.name.clone();
            d.type_id = block.block_type.to_lowercase();
            d
        }
    };

    // Set the actual block name
    def.name = block.name.clone();

    // Update parameters from the effective block
    def.parameters = block
        .parameters
        .iter()
        .map(|p| {
            let format = if p.unit == "dB" {
                ParameterFormat::Decibels {
                    min: p.min_display as f32,
                    max: p.max_display as f32,
                }
            } else if p.unit == "ms" {
                ParameterFormat::Milliseconds {
                    min: p.min_display as f32,
                    max: p.max_display as f32,
                }
            } else if p.unit == "Hz" {
                ParameterFormat::Frequency {
                    min: p.min_display as f32,
                    max: p.max_display as f32,
                }
            } else {
                ParameterFormat::Percent
            };

            AudioBlockParameter {
                id: p.id.clone(),
                name: p.name.clone(),
                short_name: p.name.chars().take(4).collect(),
                value: p.value as f32,
                default: p.value as f32,
                unit: p.unit.clone(),
                format,
                priority: 0,
            }
        })
        .collect();

    def
}

/// Create a block instance with effective parameter values
fn create_effective_block_instance(block: &EffectiveBlock, definition: &BlockDefinition) -> BlockInstance {
    let mut instance = BlockInstance::from_definition(definition, block.id.to_string());
    instance.name = block.name.clone();
    instance.bypassed = block.bypassed;
    instance.form_factor = FormFactor::Pedal;
    instance.lod = LevelOfDetail::Compact;

    // Set parameter values from effective block
    for param in &block.parameters {
        if definition.parameters.iter().any(|p| p.id == param.id) {
            instance.set_param(&param.id, param.value as f32);
        }
    }

    instance
}

/// Pedalboard view for effective blocks
#[component]
fn EffectivePedalboard(props: EffectiveBlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in this preset"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        div {
            class: "pedalboard flex items-end gap-4 p-6
                    bg-gradient-to-b from-zinc-800 to-zinc-900
                    rounded-xl border-4 border-zinc-700
                    shadow-inner overflow-x-auto
                    scrollbar-thin scrollbar-track-zinc-800 scrollbar-thumb-zinc-600
                    hover:scrollbar-thumb-zinc-500",
            style: "scrollbar-width: thin; scrollbar-color: #52525b #27272a;",

            for block in sorted_blocks {
                // Include bypassed state and first param value in key to force re-render on snapshot change
                {
                    let param_hash = block.parameters.first().map(|p| (p.value * 1000.0) as i32).unwrap_or(0);
                    let key_str = format!("{}-{}-{}", block.id, block.bypassed, param_hash);
                    rsx! {
                        EffectivePedalBlock {
                            key: "{key_str}",
                            block: block.clone(),
                            on_bypass_toggle: props.on_toggle_bypass.clone(),
                            on_param_change: props.on_param_change.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Single pedal block with effective values
#[component]
fn EffectivePedalBlock(
    block: EffectiveBlock,
    on_bypass_toggle: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_effective_block_definition(&block);
    let instance = create_effective_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_bypass_toggle.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = block
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    let constraints = LayoutConstraints::new(FormFactor::Pedal, 200, 260, LevelOfDetail::Compact);

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

/// Cards view for effective blocks
#[component]
fn EffectiveCardsView(props: EffectiveBlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in this preset"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        div { class: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4",
            for block in sorted_blocks {
                // Include bypassed state and first param value in key to force re-render on snapshot change
                {
                    let param_hash = block.parameters.first().map(|p| (p.value * 1000.0) as i32).unwrap_or(0);
                    let key_str = format!("{}-{}-{}", block.id, block.bypassed, param_hash);
                    rsx! {
                        EffectiveCardBlock {
                            key: "{key_str}",
                            block: block.clone(),
                            on_toggle_bypass: props.on_toggle_bypass.clone(),
                            on_param_change: props.on_param_change.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Single card block with effective values
#[component]
fn EffectiveCardBlock(
    block: EffectiveBlock,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_effective_block_definition(&block);
    let instance = create_effective_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = block
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    let constraints = LayoutConstraints::new(FormFactor::Square, 280, 200, LevelOfDetail::Compact);

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

/// Rack view for effective blocks
#[component]
fn EffectiveRackView(props: EffectiveBlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in this preset"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        div {
            class: "bg-zinc-950 rounded-lg border-4 border-zinc-600 p-2 overflow-x-auto",
            style: "background: linear-gradient(180deg, #1a1a1a 0%, #0d0d0d 100%);",

            div { class: "flex gap-1",
                div {
                    class: "w-6 flex flex-col justify-between py-2",
                    style: "background: linear-gradient(90deg, #404040, #2a2a2a);",
                    for _i in 0..4 {
                        div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                    }
                }

                div { class: "flex-1 flex gap-2 py-2",
                    for block in sorted_blocks {
                        // Include bypassed state and first param value in key to force re-render on snapshot change
                        {
                            let param_hash = block.parameters.first().map(|p| (p.value * 1000.0) as i32).unwrap_or(0);
                            let key_str = format!("{}-{}-{}", block.id, block.bypassed, param_hash);
                            rsx! {
                                EffectiveRackBlock {
                                    key: "{key_str}",
                                    block: block.clone(),
                                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                                    on_param_change: props.on_param_change.clone(),
                                }
                            }
                        }
                    }
                }

                div {
                    class: "w-6 flex flex-col justify-between py-2",
                    style: "background: linear-gradient(90deg, #2a2a2a, #404040);",
                    for _i in 0..4 {
                        div { class: "w-3 h-3 mx-auto rounded-full bg-zinc-800 border border-zinc-600" }
                    }
                }
            }
        }
    }
}

/// Single rack block with effective values
#[component]
fn EffectiveRackBlock(
    block: EffectiveBlock,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_effective_block_definition(&block);
    let instance = create_effective_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = block
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    let constraints = LayoutConstraints::new(FormFactor::Rack1U, 200, 80, LevelOfDetail::Compact);

    rsx! {
        BlockView {
            definition,
            instance,
            constraints: Some(constraints),
            on_bypass_toggle: move |_bypassed| {
                on_bypass.call(block_id);
            },
            on_param_change: move |(param_id, value): (String, f32)| {
                if let Some(&index) = param_id_to_index.get(&param_id) {
                    on_param.call((block_id, index, value));
                }
            },
            on_macro_change: move |_value| {},
        }
    }
}

/// 500-series view for effective blocks
#[component]
fn Effective500View(props: EffectiveBlockContainerProps) -> Element {
    if props.blocks.is_empty() {
        return rsx! {
            div { class: "text-center text-muted-foreground py-8",
                "No blocks in this preset"
            }
        };
    }

    let mut sorted_blocks = props.blocks.clone();
    sorted_blocks.sort_by_key(|b| b.order);

    rsx! {
        div {
            class: "bg-zinc-900 rounded-lg border-2 border-zinc-700 p-3 overflow-x-auto",
            style: "background: linear-gradient(180deg, #262626 0%, #171717 100%);",

            div { class: "flex gap-1",
                for block in sorted_blocks {
                    // Include bypassed state and first param value in key to force re-render on snapshot change
                    {
                        let param_hash = block.parameters.first().map(|p| (p.value * 1000.0) as i32).unwrap_or(0);
                        let key_str = format!("{}-{}-{}", block.id, block.bypassed, param_hash);
                        rsx! {
                            Effective500Block {
                                key: "{key_str}",
                                block: block.clone(),
                                on_toggle_bypass: props.on_toggle_bypass.clone(),
                                on_param_change: props.on_param_change.clone(),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Single 500-series block with effective values
#[component]
fn Effective500Block(
    block: EffectiveBlock,
    on_toggle_bypass: Callback<Uuid>,
    on_param_change: Callback<(Uuid, u32, f32)>,
) -> Element {
    let definition = get_effective_block_definition(&block);
    let instance = create_effective_block_instance(&block, &definition);

    let block_id = block.id;
    let on_bypass = on_toggle_bypass.clone();
    let on_param = on_param_change.clone();

    let param_id_to_index: std::collections::HashMap<String, u32> = block
        .parameters
        .iter()
        .enumerate()
        .map(|(i, p)| (p.id.clone(), i as u32))
        .collect();

    let constraints = LayoutConstraints::new(FormFactor::Series500, 100, 320, LevelOfDetail::Compact);

    rsx! {
        div {
            class: "bg-zinc-800 rounded border border-zinc-600",
            style: "background: linear-gradient(180deg, #3f3f46 0%, #27272a 5%, #27272a 95%, #3f3f46 100%);",

            BlockView {
                definition,
                instance,
                constraints: Some(constraints),
                on_bypass_toggle: move |_bypassed| {
                    on_bypass.call(block_id);
                },
                on_param_change: move |(param_id, value): (String, f32)| {
                    if let Some(&index) = param_id_to_index.get(&param_id) {
                        on_param.call((block_id, index, value));
                    }
                },
                on_macro_change: move |_value| {},
            }
        }
    }
}

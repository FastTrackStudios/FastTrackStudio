//! Multi-column cascading browser for the signal domain.
//!
//! Column 1 is a fixed navigation sidebar (Presets / Engines / Modules / Blocks).
//! Everything is scoped to the active **rig type** (status bar selector).
//!
//! | Nav      | Col 2 (auto)              | Col 3 (click)     | Col 4 (click)  |
//! |----------|---------------------------|-------------------|----------------|
//! | Presets  | Presets for rig type      | Scenes            | —              |
//! | Engines  | Engines for rig type      | Layers for engine | —              |
//! | Modules  | Module presets            | Snapshots         | —              |
//! | Blocks   | Block types (color dots)  | Presets for type  | Snapshots      |

use dioxus::prelude::*;
use signal::layer::Layer;
use signal::metadata::Metadata as MetadataModel;
use signal::rig::RigType;
use signal::tagging::{StructuredTag, TagCategory, TagSet};
use signal::traits::HasMetadata;
use signal::{BlockType, SignalChain, ALL_BLOCK_TYPES};
use signal::{Preset, SignalController};

use super::metadata_display::MetadataDisplay;
use super::signal_chain_layout::{layout_module_chains, layout_signal_chain};
use crate::components::dynamic_grid::{
    BlockPickerDropdown, DynamicGridView, GridConnection as DynGridConnection, GridSelection,
    GridSlot, PICKER_CELL, PICKER_CLICK_POS,
};
use crate::components::SignalChainGrid;

// region: --- Types

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NavCategory {
    Presets,
    Engines,
    Modules,
    Blocks,
}

impl NavCategory {
    const ALL: &[NavCategory] = &[Self::Presets, Self::Engines, Self::Modules, Self::Blocks];

    fn label(self) -> &'static str {
        match self {
            Self::Presets => "Presets",
            Self::Engines => "Engines",
            Self::Modules => "Modules",
            Self::Blocks => "Blocks",
        }
    }

    fn accent(self) -> &'static str {
        match self {
            Self::Presets => "from-amber-500 via-orange-400 to-red-500",
            Self::Engines => "from-rose-500 via-pink-400 to-fuchsia-500",
            Self::Modules => "from-blue-500 via-indigo-400 to-violet-500",
            Self::Blocks => "from-orange-500 via-amber-400 to-yellow-500",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SortMode {
    Name,
    NameDesc,
    Variants,
    BlockType,
}

impl SortMode {
    const ALL: &[SortMode] = &[Self::Name, Self::NameDesc, Self::Variants, Self::BlockType];

    fn label(self) -> &'static str {
        match self {
            Self::Name => "A \u{2192} Z",
            Self::NameDesc => "Z \u{2192} A",
            Self::Variants => "Most Variants",
            Self::BlockType => "Type",
        }
    }

    fn value(self) -> &'static str {
        match self {
            Self::Name => "name",
            Self::NameDesc => "name_desc",
            Self::Variants => "variants",
            Self::BlockType => "block_type",
        }
    }

    fn from_value(s: &str) -> Self {
        match s {
            "name_desc" => Self::NameDesc,
            "variants" => Self::Variants,
            "block_type" => Self::BlockType,
            _ => Self::Name,
        }
    }
}

/// The filterable tag categories shown as chip filters in the toolbar.
const FILTER_CATEGORIES: &[TagCategory] = &[
    TagCategory::Tone,
    TagCategory::Character,
    TagCategory::Genre,
    TagCategory::Vendor,
    TagCategory::Plugin,
    TagCategory::Context,
    TagCategory::Instrument,
    TagCategory::Block,
    TagCategory::Module,
];

#[derive(Clone, PartialEq)]
struct ColumnItem {
    id: String,
    name: String,
    subtitle: Option<String>,
    badge: Option<String>,
    metadata: Option<MetadataModel>,
    /// Structured tags for filtering/sorting.
    structured_tags: TagSet,
    /// Nested detail data (params, blocks, modules) for the detail panel.
    detail: DetailData,
    /// Extra data for context (e.g. block type index for Blocks nav).
    tag: Option<usize>,
}

#[derive(Clone, PartialEq)]
struct DetailParam {
    name: String,
    value: f32,
}

/// A module's signal chain data for grid rendering.
#[derive(Clone, PartialEq)]
pub struct ModuleChainData {
    pub name: String,
    pub color_bg: String,
    pub color_fg: String,
    pub color_border: String,
    pub chain: SignalChain,
    pub module_type: Option<signal::ModuleType>,
}

/// A layer's resolved module chains for rig-level display.
#[derive(Clone, PartialEq)]
pub struct LayerFlowData {
    pub name: String,
    pub module_chains: Vec<ModuleChainData>,
}

/// An engine's resolved layer data for rig-level display.
#[derive(Clone, PartialEq)]
pub struct EngineFlowData {
    pub name: String,
    pub layers: Vec<LayerFlowData>,
}

/// Nested detail data for the detail panel.
#[derive(Clone, PartialEq, Default)]
struct DetailData {
    /// Standalone parameters (block snapshots).
    params: Vec<DetailParam>,
    /// Raw signal chain for grid rendering (module snapshots).
    chain: Option<SignalChain>,
    /// Module chains for layer/engine detail.
    module_chains: Vec<ModuleChainData>,
    /// Full rig hierarchy (engines → layers → modules) for preset detail.
    engines: Vec<EngineFlowData>,
}

const RIG_TYPES: &[RigType] = &[
    RigType::Guitar,
    RigType::Bass,
    RigType::Keys,
    RigType::Drums,
    RigType::DrumReplacement,
    RigType::Vocals,
];

// endregion: --- Types

// region: --- Public API

/// Which domain level to browse. Kept for external API compatibility.
#[derive(Debug, Clone, PartialEq)]
pub enum BrowseLevel {
    Presets,
    Engines,
    Modules,
    Blocks(BlockType),
}

impl BrowseLevel {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Presets => "Presets",
            Self::Engines => "Engines",
            Self::Modules => "Modules",
            Self::Blocks(_) => "Block Presets",
        }
    }
}

// endregion

// region: --- CollectionBrowser

#[component]
pub fn CollectionBrowser(controller: SignalController) -> Element {
    let mut nav = use_signal(|| NavCategory::Presets);
    let mut rig_type = use_signal(|| RigType::Guitar);

    let mut col2_items = use_signal(Vec::<ColumnItem>::new);
    let mut col2_selected = use_signal(|| None::<usize>);
    let mut col3_items = use_signal(Vec::<ColumnItem>::new);
    let mut col3_selected = use_signal(|| None::<usize>);
    let mut col4_items = use_signal(Vec::<ColumnItem>::new);
    let mut col4_selected = use_signal(|| None::<usize>);

    // Cache of raw Preset objects from the last Blocks col3 fetch.
    // Used by col4 to look up snapshots without re-querying the DB.
    let mut block_presets_cache = use_signal(Vec::<Preset>::new);

    // Search / sort / filter state
    let mut search_text = use_signal(String::new);
    let mut sort_mode = use_signal(|| SortMode::Name);
    let mut active_tag_filters = use_signal(Vec::<String>::new);
    let mut show_tag_panel = use_signal(|| false);

    let nav_memo = use_memo(move || nav());

    // Auto-fetch col2 when nav or rig_type changes.
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            let category = nav_memo();
            let rt = rig_type();
            col2_selected.set(None);
            col3_items.set(Vec::new());
            col3_selected.set(None);
            col4_items.set(Vec::new());
            col4_selected.set(None);
            block_presets_cache.set(Vec::new());
            search_text.set(String::new());
            active_tag_filters.set(Vec::new());
            spawn(async move {
                let items = fetch_col2(&controller, category, rt).await;
                // Auto-select the first item so detail panel is populated on load.
                if !items.is_empty() && category == NavCategory::Presets {
                    let first_id = items[0].id.clone();
                    let first_tag = items[0].tag;
                    col2_selected.set(Some(0));
                    let (v, presets) =
                        fetch_col3(&controller, category, &first_id, first_tag).await;
                    // Auto-select first scene too
                    if !v.is_empty() {
                        col3_selected.set(Some(0));
                    }
                    col3_items.set(v);
                    block_presets_cache.set(presets);
                }
                col2_items.set(items);
            });
        });
    }

    let current_nav = nav_memo();
    let current_rt = rig_type();

    // Pre-clone for rsx branches
    let ctrl_c2 = controller.clone();

    // Apply search + tag filter + sort to col2 items.
    let all_col2 = filter_and_sort(
        &col2_items(),
        &search_text(),
        &active_tag_filters(),
        sort_mode(),
    );
    let all_col3 = col3_items();
    let all_col4 = col4_items();

    let has_col4 = current_nav == NavCategory::Blocks;

    // Detail panel: deepest selection
    let (detail_name, detail_meta, detail_data) = find_detail(
        &all_col4,
        col4_selected(),
        &all_col3,
        col3_selected(),
        &all_col2,
        col2_selected(),
    );

    let col2_header = match current_nav {
        NavCategory::Presets => "Presets",
        NavCategory::Engines => "Engines",
        NavCategory::Modules => "Module Presets",
        NavCategory::Blocks => "Block Types",
    };
    let col3_header = match current_nav {
        NavCategory::Presets => "Scenes",
        NavCategory::Engines => "Layers",
        NavCategory::Modules => "Snapshots",
        NavCategory::Blocks => "Presets",
    };

    let accent = current_nav.accent();
    let show_block_dots = current_nav == NavCategory::Blocks;

    // Compute available tags from the unfiltered col2 items for the tag panel.
    let available_tags = collect_available_tags(&col2_items());
    let current_sort = sort_mode();
    let current_search = search_text();
    let tag_panel_open = show_tag_panel();
    let current_filters = active_tag_filters();
    let has_active_filters = !current_search.is_empty() || !current_filters.is_empty();

    rsx! {
        div { class: "h-full w-full flex flex-col overflow-hidden",
            div { class: "h-[2px] w-full bg-gradient-to-r {accent} flex-shrink-0" }

            // ── Toolbar: search + sort + filter ──
            div { class: "px-3 py-1.5 border-b border-zinc-800 flex items-center gap-2 flex-shrink-0 bg-zinc-950/50",
                // Search input
                div { class: "flex items-center gap-1.5 flex-1 min-w-0",
                    span { class: "text-zinc-500 text-xs flex-shrink-0", ">" }
                    input {
                        class: "bg-transparent text-xs text-zinc-200 outline-none flex-1 min-w-0 placeholder-zinc-600",
                        r#type: "text",
                        placeholder: "Search {current_nav.label().to_ascii_lowercase()}...",
                        value: "{current_search}",
                        oninput: move |evt: Event<FormData>| {
                            search_text.set(evt.value().clone());
                        },
                    }
                    if has_active_filters {
                        button {
                            class: "text-[10px] text-zinc-500 hover:text-zinc-300 px-1",
                            onclick: move |_| {
                                search_text.set(String::new());
                                active_tag_filters.set(Vec::new());
                            },
                            "Clear"
                        }
                    }
                }
                // Sort dropdown
                select {
                    class: "px-1.5 py-0.5 text-[10px] rounded bg-zinc-800 text-zinc-300 border border-zinc-700 outline-none cursor-pointer flex-shrink-0",
                    value: "{current_sort.value()}",
                    onchange: move |evt: Event<FormData>| {
                        sort_mode.set(SortMode::from_value(&evt.value()));
                    },
                    for sm in SortMode::ALL.iter() {
                        {
                            let s = *sm;
                            rsx! {
                                option {
                                    value: "{s.value()}",
                                    selected: current_sort == s,
                                    "{s.label()}"
                                }
                            }
                        }
                    }
                }
                // Tag filter toggle
                button {
                    class: if tag_panel_open {
                        "px-2 py-0.5 text-[10px] rounded bg-zinc-600 text-zinc-100 flex-shrink-0"
                    } else {
                        "px-2 py-0.5 text-[10px] rounded bg-zinc-800 text-zinc-400 hover:text-zinc-200 hover:bg-zinc-700 flex-shrink-0"
                    },
                    onclick: move |_| show_tag_panel.set(!tag_panel_open),
                    if current_filters.is_empty() {
                        "Tags"
                    } else {
                        "Tags ({current_filters.len()})"
                    }
                }
            }

            // ── Active tag chips ──
            if !current_filters.is_empty() {
                div { class: "px-3 py-1 border-b border-zinc-800 flex items-center gap-1 flex-shrink-0 flex-wrap bg-zinc-950/40",
                    for filter_key in current_filters.iter() {
                        {
                            let key = filter_key.clone();
                            let display = tag_display_value(&key).to_string();
                            rsx! {
                                button {
                                    key: "{key}",
                                    class: "inline-flex items-center gap-1 px-1.5 py-0.5 text-[10px] rounded bg-zinc-700 text-zinc-200 hover:bg-zinc-600",
                                    onclick: move |_| {
                                        let mut filters = active_tag_filters();
                                        filters.retain(|f| f != &key);
                                        active_tag_filters.set(filters);
                                    },
                                    "{display}"
                                    span { class: "text-zinc-400", "x" }
                                }
                            }
                        }
                    }
                }
            }

            // ── Tag filter panel (collapsible) ──
            if tag_panel_open {
                div { class: "px-3 py-2 border-b border-zinc-800 flex-shrink-0 bg-zinc-900/60 max-h-40 overflow-y-auto",
                    if available_tags.is_empty() {
                        div { class: "text-xs text-zinc-600 italic", "No tags available" }
                    }
                    for (cat, keys) in available_tags.iter() {
                        {
                            let cat_label = tag_category_label(*cat);
                            rsx! {
                                div { class: "mb-1.5",
                                    h4 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider mb-0.5", "{cat_label}" }
                                    div { class: "flex flex-wrap gap-1",
                                        for key in keys.iter() {
                                            {
                                                let k = key.clone();
                                                let display = tag_display_value(key).to_string();
                                                let is_active = current_filters.contains(key);
                                                rsx! {
                                                    button {
                                                        key: "{k}",
                                                        class: if is_active {
                                                            "px-1.5 py-0.5 text-[10px] rounded bg-zinc-500 text-zinc-100"
                                                        } else {
                                                            "px-1.5 py-0.5 text-[10px] rounded bg-zinc-800 text-zinc-400 hover:bg-zinc-700 hover:text-zinc-200"
                                                        },
                                                        onclick: move |_| {
                                                            let mut filters = active_tag_filters();
                                                            if is_active {
                                                                filters.retain(|f| f != &k);
                                                            } else {
                                                                filters.push(k.clone());
                                                            }
                                                            active_tag_filters.set(filters);
                                                        },
                                                        "{display}"
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

            div { class: "flex-1 flex min-h-0 overflow-hidden",

                // ── Col 1: Nav ──
                div { class: "w-36 flex-shrink-0 border-r border-zinc-800 flex flex-col min-h-0 bg-zinc-950/60",
                    div { class: "px-3 py-2 border-b border-zinc-800",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Browse" }
                    }
                    div { class: "flex-1 overflow-y-auto py-1",
                        for cat in NavCategory::ALL.iter() {
                            {
                                let c = *cat;
                                let is_active = current_nav == c;
                                rsx! {
                                    button {
                                        key: "{c.label()}",
                                        class: if is_active {
                                            "w-full text-left px-3 py-2 text-sm font-medium bg-zinc-700/70 text-zinc-100"
                                        } else {
                                            "w-full text-left px-3 py-2 text-sm text-zinc-400 hover:bg-zinc-800/60 hover:text-zinc-200"
                                        },
                                        onclick: move |_| nav.set(c),
                                        "{c.label()}"
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Col 2: Items (auto-fetched) ──
                div { class: "w-52 flex-shrink-0 border-r border-zinc-800 flex flex-col min-h-0 bg-zinc-950/50",
                    div { class: "px-3 py-2 border-b border-zinc-800",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "{col2_header}" }
                    }
                    div { class: "flex-1 overflow-y-auto",
                        if all_col2.is_empty() {
                            div { class: "text-xs text-zinc-600 text-center py-6", "No items" }
                        }
                        for (idx, item) in all_col2.iter().enumerate() {
                            {
                                let is_sel = col2_selected() == Some(idx);
                                let name = item.name.clone();
                                let subtitle = item.subtitle.clone();
                                let badge = item.badge.clone();
                                let color_bg = if show_block_dots {
                                    item.tag.and_then(|t| ALL_BLOCK_TYPES.get(t)).map(|bt| bt.color().bg.to_string())
                                } else {
                                    None
                                };
                                let controller = ctrl_c2.clone();
                                let item_id = item.id.clone();
                                let item_tag = item.tag;
                                rsx! {
                                    button {
                                        key: "{item_id}",
                                        class: if is_sel {
                                            "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                        } else {
                                            "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                        },
                                        onclick: move |_| {
                                            col2_selected.set(Some(idx));
                                            col3_selected.set(None);
                                            col4_items.set(Vec::new());
                                            col4_selected.set(None);
                                            block_presets_cache.set(Vec::new());
                                            let controller = controller.clone();
                                            let nav = nav();
                                            let id = item_id.clone();
                                            let tag = item_tag;
                                            spawn(async move {
                                                let (v, presets) = fetch_col3(&controller, nav, &id, tag).await;
                                                col3_items.set(v);
                                                block_presets_cache.set(presets);
                                            });
                                        },
                                        div { class: "flex items-center gap-1.5",
                                            if let Some(ref bg) = color_bg {
                                                span {
                                                    class: "w-2 h-2 rounded-full flex-shrink-0",
                                                    style: "background-color: {bg}",
                                                }
                                            }
                                            span { class: "text-sm text-zinc-200 truncate flex-1", "{name}" }
                                            if let Some(ref b) = badge {
                                                span { class: "text-[10px] text-zinc-500 flex-shrink-0", "{b}" }
                                            }
                                        }
                                        if let Some(ref sub) = subtitle {
                                            div { class: "text-xs text-zinc-500 truncate", "{sub}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    div { class: "px-3 py-1 border-t border-zinc-800 flex-shrink-0",
                        if has_active_filters {
                            span { class: "text-[10px] text-zinc-600",
                                "{all_col2.len()} / {col2_items().len()}"
                            }
                        } else {
                            span { class: "text-[10px] text-zinc-600", "{all_col2.len()}" }
                        }
                    }
                }

                // ── Col 3: Children (on col2 click) ──
                div { class: "w-52 flex-shrink-0 border-r border-zinc-800 flex flex-col min-h-0 bg-zinc-950/40",
                    div { class: "px-3 py-2 border-b border-zinc-800",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                            {if col2_selected().is_some() { col3_header } else { "—" }}
                        }
                    }
                    div { class: "flex-1 overflow-y-auto",
                        if all_col3.is_empty() {
                            div { class: "text-xs text-zinc-600 text-center py-6",
                                {if col2_selected().is_some() { "No items" } else { "Select from left" }}
                            }
                        }
                        for (cidx, child) in all_col3.iter().enumerate() {
                            {
                                let is_sel = col3_selected() == Some(cidx);
                                let name = child.name.clone();
                                let subtitle = child.subtitle.clone();
                                let badge = child.badge.clone();
                                rsx! {
                                    button {
                                        key: "{child.id}",
                                        class: if is_sel {
                                            "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                        } else {
                                            "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                        },
                                        onclick: move |_| {
                                            col3_selected.set(Some(cidx));
                                            col4_selected.set(None);
                                            if has_col4 {
                                                let items = col3_items();
                                                if let Some(item) = items.get(cidx) {
                                                    let item_id = &item.id;
                                                    // Look up snapshots directly from the cached presets
                                                    let cached = block_presets_cache();
                                                    let snap_items = cached.iter()
                                                        .find(|p| p.id().to_string() == *item_id)
                                                        .map(|preset| {
                                                            preset.snapshots().iter().map(|s| ColumnItem {
                                                                id: s.id().to_string(),
                                                                name: s.name().to_string(),
                                                                subtitle: Some(format!("{} param(s)", s.block().parameters().len())),
                                                                badge: None,
                                                                metadata: None,
                                                                structured_tags: TagSet::default(),
                                                                detail: DetailData {
                                                                    params: s.block().parameters().iter().map(|p| DetailParam {
                                                                        name: p.name().to_string(),
                                                                        value: p.value().get(),
                                                                    }).collect(),
                                                                    ..Default::default()
                                                                },
                                                                tag: None,
                                                            }).collect::<Vec<_>>()
                                                        })
                                                        .unwrap_or_default();
                                                    col4_items.set(snap_items);
                                                }
                                            }
                                        },
                                        div { class: "flex items-center gap-1.5",
                                            span { class: "text-sm text-zinc-200 truncate flex-1", "{name}" }
                                            if let Some(ref b) = badge {
                                                span { class: "text-[10px] text-zinc-500 flex-shrink-0", "{b}" }
                                            }
                                        }
                                        if let Some(ref sub) = subtitle {
                                            div { class: "text-xs text-zinc-500 truncate", "{sub}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    div { class: "px-3 py-1 border-t border-zinc-800 flex-shrink-0",
                        span { class: "text-[10px] text-zinc-600", "{all_col3.len()}" }
                    }
                }

                // ── Col 4: Snapshots (only for Blocks) ──
                if has_col4 {
                    div { class: "w-52 flex-shrink-0 border-r border-zinc-800 flex flex-col min-h-0 bg-zinc-950/30",
                        div { class: "px-3 py-2 border-b border-zinc-800",
                            h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                {if col3_selected().is_some() { "Snapshots" } else { "—" }}
                            }
                        }
                        div { class: "flex-1 overflow-y-auto",
                            if all_col4.is_empty() {
                                div { class: "text-xs text-zinc-600 text-center py-6",
                                    {if col3_selected().is_some() { "No items" } else { "Select from left" }}
                                }
                            }
                            for (didx, item) in all_col4.iter().enumerate() {
                                {
                                    let is_sel = col4_selected() == Some(didx);
                                    let name = item.name.clone();
                                    let subtitle = item.subtitle.clone();
                                    rsx! {
                                        button {
                                            key: "{item.id}",
                                            class: if is_sel {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                            } else {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                            },
                                            onclick: move |_| {
                                                col4_selected.set(Some(didx));
                                            },
                                            span { class: "text-sm text-zinc-200 truncate", "{name}" }
                                            if let Some(ref sub) = subtitle {
                                                div { class: "text-xs text-zinc-500 truncate", "{sub}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        div { class: "px-3 py-1 border-t border-zinc-800 flex-shrink-0",
                            span { class: "text-[10px] text-zinc-600", "{all_col4.len()}" }
                        }
                    }
                }

                // ── Detail ──
                div { class: "flex-1 min-w-0 flex flex-col min-h-0 bg-zinc-950/20",
                    div { class: "px-4 py-2 border-b border-zinc-800",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Detail" }
                    }
                    div { class: "flex-1 overflow-y-auto p-4",
                        if let Some(ref name) = detail_name {
                            div { class: "mb-4",
                                h2 { class: "text-base font-semibold text-zinc-200", "{name}" }
                            }
                        }
                        if let Some(meta) = detail_meta {
                            MetadataDisplay {
                                tags: meta.tags.as_slice().to_vec(),
                                description: meta.description.clone(),
                                notes: meta.notes.clone(),
                            }
                        }
                        if let Some(data) = detail_data {
                            // Rig-level: interactive DynamicGridView
                            if !data.engines.is_empty() {
                                {
                                    let grid_slots = engines_to_grid_slots(&data.engines);
                                    rsx! {
                                        RigGridPanel { initial_slots: grid_slots }
                                    }
                                }
                            }
                            // Module chains (layer/engine detail) — unified grid
                            if !data.module_chains.is_empty() {
                                {
                                    let (flow_blocks, total_cols, total_lanes) =
                                        layout_module_chains(&data.module_chains);
                                    rsx! {
                                        div { class: "mt-3",
                                            SignalChainGrid {
                                                blocks: flow_blocks,
                                                total_columns: total_cols,
                                                total_lanes: total_lanes,
                                            }
                                        }
                                    }
                                }
                            }
                            // Signal chain grid (module snapshot detail)
                            if let Some(ref chain) = data.chain {
                                {
                                    let (flow_blocks, total_cols, total_lanes) =
                                        layout_signal_chain(chain);
                                    rsx! {
                                        div { class: "mt-3",
                                            h4 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider mb-2", "Signal Chain" }
                                            SignalChainGrid {
                                                blocks: flow_blocks,
                                                total_columns: total_cols,
                                                total_lanes: total_lanes,
                                            }
                                        }
                                    }
                                }
                            }
                            // Flat params (block snapshot detail)
                            if !data.params.is_empty() {
                                div { class: "mt-3 space-y-2",
                                    h4 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider mb-2", "Parameters" }
                                    { render_param_bars(&data.params) }
                                }
                            }
                        }
                        if detail_name.is_none() {
                            div { class: "text-xs text-zinc-600 italic", "Select an item to see details" }
                        }
                    }
                }
            }

            // Status bar
            div { class: "px-4 py-1.5 border-t border-zinc-800 flex items-center gap-3 flex-shrink-0 bg-zinc-950/60",
                div { class: "w-1.5 h-1.5 rounded-full bg-green-500" }
                span { class: "text-[10px] text-zinc-500", "{current_nav.label()}" }
                div { class: "flex-1" }
                span { class: "text-[10px] text-zinc-600 mr-1", "Rig:" }
                for rt in RIG_TYPES.iter() {
                    {
                        let t = *rt;
                        let is_active = current_rt == t;
                        rsx! {
                            button {
                                key: "{t.as_str()}",
                                class: if is_active {
                                    "px-1.5 py-0.5 text-[10px] rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-1.5 py-0.5 text-[10px] rounded text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800"
                                },
                                onclick: move |_| rig_type.set(t),
                                "{rig_type_display(t)}"
                            }
                        }
                    }
                }
            }
        }
    }
}

// endregion: --- CollectionBrowser

// region: --- Helpers

fn render_param_bars(params: &[DetailParam]) -> Element {
    rsx! {
        for param in params.iter() {
            {
                let pct = (param.value * 100.0).round() as u32;
                let width_pct = format!("{}%", pct);
                let name = param.name.clone();
                rsx! {
                    div { class: "flex items-center gap-2",
                        span { class: "text-xs text-zinc-400 w-24 truncate flex-shrink-0", "{name}" }
                        div { class: "flex-1 h-1.5 bg-zinc-800 rounded-full overflow-hidden",
                            div {
                                class: "h-full bg-zinc-500 rounded-full",
                                style: "width: {width_pct}",
                            }
                        }
                        span { class: "text-[10px] text-zinc-600 w-8 text-right flex-shrink-0", "{pct}%" }
                    }
                }
            }
        }
    }
}

fn find_detail<'a>(
    col4: &'a [ColumnItem],
    col4_sel: Option<usize>,
    col3: &'a [ColumnItem],
    col3_sel: Option<usize>,
    col2: &'a [ColumnItem],
    col2_sel: Option<usize>,
) -> (
    Option<String>,
    Option<&'a MetadataModel>,
    Option<&'a DetailData>,
) {
    if let Some(item) = col4_sel.and_then(|i| col4.get(i)) {
        return (
            Some(item.name.clone()),
            item.metadata.as_ref(),
            Some(&item.detail),
        );
    }
    if let Some(item) = col3_sel.and_then(|i| col3.get(i)) {
        return (
            Some(item.name.clone()),
            item.metadata.as_ref(),
            Some(&item.detail),
        );
    }
    if let Some(item) = col2_sel.and_then(|i| col2.get(i)) {
        return (
            Some(item.name.clone()),
            item.metadata.as_ref(),
            Some(&item.detail),
        );
    }
    (None, None, None)
}

/// Filter items by text search + tag keys, then sort.
fn filter_and_sort(
    items: &[ColumnItem],
    search: &str,
    tag_filters: &[String],
    sort: SortMode,
) -> Vec<ColumnItem> {
    let needle = search.trim().to_ascii_lowercase();
    let mut out: Vec<ColumnItem> = items
        .iter()
        .filter(|item| {
            // Text search: match name or subtitle
            if !needle.is_empty() {
                let name_match = item.name.to_ascii_lowercase().contains(&needle);
                let sub_match = item
                    .subtitle
                    .as_ref()
                    .map_or(false, |s| s.to_ascii_lowercase().contains(&needle));
                let tag_match = item
                    .structured_tags
                    .values()
                    .any(|t| t.value.contains(&needle));
                if !name_match && !sub_match && !tag_match {
                    return false;
                }
            }
            // Tag filters: item must have ALL active filter tags
            for key in tag_filters {
                if !item.structured_tags.contains_key(key) {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect();

    match sort {
        SortMode::Name => out.sort_by(|a, b| {
            a.name
                .to_ascii_lowercase()
                .cmp(&b.name.to_ascii_lowercase())
        }),
        SortMode::NameDesc => out.sort_by(|a, b| {
            b.name
                .to_ascii_lowercase()
                .cmp(&a.name.to_ascii_lowercase())
        }),
        SortMode::Variants => out.sort_by(|a, b| {
            let va = a
                .badge
                .as_ref()
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(0);
            let vb = b
                .badge
                .as_ref()
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(0);
            vb.cmp(&va)
        }),
        SortMode::BlockType => out.sort_by(|a, b| {
            let sa = a.subtitle.as_deref().unwrap_or("");
            let sb = b.subtitle.as_deref().unwrap_or("");
            sa.cmp(sb).then_with(|| a.name.cmp(&b.name))
        }),
    }
    out
}

/// Collect all unique tag keys from a set of items, grouped by category.
fn collect_available_tags(items: &[ColumnItem]) -> Vec<(TagCategory, Vec<String>)> {
    use std::collections::BTreeMap;
    let mut by_cat: BTreeMap<TagCategory, Vec<String>> = BTreeMap::new();
    for item in items {
        for tag in item.structured_tags.values() {
            let entry = by_cat.entry(tag.category).or_default();
            let key = tag.key();
            if !entry.contains(&key) {
                entry.push(key);
            }
        }
    }
    // Return only the categories we want to expose as filters, in display order.
    FILTER_CATEGORIES
        .iter()
        .filter_map(|cat| {
            by_cat.remove(cat).map(|mut vals| {
                vals.sort();
                (*cat, vals)
            })
        })
        .collect()
}

/// Display name for a tag category.
fn tag_category_label(cat: TagCategory) -> &'static str {
    match cat {
        TagCategory::Tone => "Tone",
        TagCategory::Character => "Character",
        TagCategory::Genre => "Genre",
        TagCategory::Vendor => "Vendor",
        TagCategory::Plugin => "Plugin",
        TagCategory::Context => "Context",
        TagCategory::Instrument => "Instrument",
        TagCategory::Block => "Block",
        TagCategory::Module => "Module",
        TagCategory::RigType => "Rig Type",
        TagCategory::EngineType => "Engine Type",
        TagCategory::DomainLevel => "Level",
        TagCategory::Workflow => "Workflow",
        TagCategory::Custom => "Custom",
    }
}

/// Extract just the value portion from a `category:value` tag key.
fn tag_display_value(key: &str) -> &str {
    key.split_once(':').map_or(key, |(_, v)| v)
}

fn rig_type_to_engine_type(rig_type: RigType) -> signal::EngineType {
    match rig_type {
        RigType::Guitar => signal::EngineType::Guitar,
        RigType::Bass => signal::EngineType::Bass,
        RigType::Keys => signal::EngineType::Keys,
        RigType::Drums | RigType::DrumReplacement => signal::EngineType::Guitar,
        RigType::Vocals => signal::EngineType::Vocal,
    }
}

fn rig_type_display(rt: RigType) -> &'static str {
    match rt {
        RigType::Guitar => "Guitar",
        RigType::Bass => "Bass",
        RigType::Keys => "Keys",
        RigType::Drums => "Drums",
        RigType::DrumReplacement => "Drum Repl.",
        RigType::Vocals => "Vocals",
    }
}

// endregion: --- Helpers

// region: --- Data Fetching

async fn fetch_col2(
    controller: &SignalController,
    nav: NavCategory,
    rig_type: RigType,
) -> Vec<ColumnItem> {
    match nav {
        NavCategory::Presets => {
            let rigs = controller.list_rig_collections().await;
            rigs.into_iter()
                .filter(|r| r.rig_type.map_or(false, |rt| rt == rig_type))
                .map(|r| {
                    let meta = r.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    ColumnItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        subtitle: None,
                        badge: Some(format!("{}", r.variants.len())),
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
                    }
                })
                .collect()
        }
        NavCategory::Engines => {
            let et = rig_type_to_engine_type(rig_type);
            let engines = controller.list_engines().await;
            engines
                .into_iter()
                .filter(|e| e.engine_type == et)
                .map(|e| {
                    let meta = e.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    ColumnItem {
                        id: e.id.to_string(),
                        name: e.name.clone(),
                        subtitle: Some(format!("{} layer(s)", e.layer_ids.len())),
                        badge: Some(format!("{}", e.variants.len())),
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
                    }
                })
                .collect()
        }
        NavCategory::Modules => {
            let presets = controller.list_module_collections().await;
            presets
                .into_iter()
                .map(|p| {
                    let mut tags = TagSet::default();
                    tags.insert(StructuredTag::new(
                        TagCategory::Module,
                        p.module_type().as_str(),
                    ));
                    ColumnItem {
                        id: p.id().to_string(),
                        name: p.name().to_string(),
                        subtitle: Some(p.module_type().display_name().to_string()),
                        badge: Some(format!("{}", p.snapshots().len())),
                        metadata: None,
                        structured_tags: tags,
                        detail: DetailData::default(),
                        tag: None,
                    }
                })
                .collect()
        }
        NavCategory::Blocks => ALL_BLOCK_TYPES
            .iter()
            .enumerate()
            .map(|(idx, bt)| {
                let mut tags = TagSet::default();
                tags.insert(StructuredTag::new(TagCategory::Block, bt.as_str()));
                ColumnItem {
                    id: bt.as_str().to_string(),
                    name: bt.display_name().to_string(),
                    subtitle: Some(bt.category().display_name().to_string()),
                    badge: None,
                    metadata: None,
                    structured_tags: tags,
                    detail: DetailData::default(),
                    tag: Some(idx),
                }
            })
            .collect(),
    }
}

/// Returns (column items, block presets cache).
/// The cache is non-empty only for `NavCategory::Blocks` — it holds the raw
/// `Preset` objects so col4 can extract snapshots without re-querying.
async fn fetch_col3(
    controller: &SignalController,
    nav: NavCategory,
    col2_id: &str,
    col2_tag: Option<usize>,
) -> (Vec<ColumnItem>, Vec<Preset>) {
    match nav {
        NavCategory::Presets => {
            let items = if let Some(rig) = controller.load_rig_collection(col2_id).await {
                let mut out = Vec::new();
                for v in &rig.variants {
                    let engines = resolve_rig_scene_engines(controller, v).await;
                    let meta = v.metadata().clone();
                    let tags = TagSet::from_tags(&meta.tags);
                    out.push(ColumnItem {
                        id: v.id.to_string(),
                        name: v.name.clone(),
                        subtitle: Some(format!("{} engine(s)", v.engine_selections.len())),
                        badge: None,
                        metadata: Some(meta),
                        structured_tags: tags,
                        detail: DetailData {
                            engines,
                            ..Default::default()
                        },
                        tag: None,
                    });
                }
                out
            } else {
                Vec::new()
            };
            (items, Vec::new())
        }
        NavCategory::Engines => {
            let items = if let Some(engine) = controller.load_engine(col2_id).await {
                let mut items = Vec::new();
                for layer_id in &engine.layer_ids {
                    if let Some(layer) = controller.load_layer(layer_id.as_str()).await {
                        let module_chains = resolve_layer_module_chains(controller, &layer).await;
                        let meta = layer.metadata().clone();
                        let tags = TagSet::from_tags(&meta.tags);
                        items.push(ColumnItem {
                            id: layer.id.to_string(),
                            name: layer.name.clone(),
                            subtitle: Some(format!("{} variant(s)", layer.variants.len())),
                            badge: None,
                            metadata: Some(meta),
                            structured_tags: tags,
                            detail: DetailData {
                                module_chains,
                                ..Default::default()
                            },
                            tag: None,
                        });
                    }
                }
                items
            } else {
                Vec::new()
            };
            (items, Vec::new())
        }
        NavCategory::Modules => {
            let presets = controller.list_module_collections().await;
            let items = if let Some(preset) = presets.iter().find(|p| p.id().to_string() == col2_id)
            {
                let mut out = Vec::new();
                for s in preset.snapshots() {
                    let block_count = s.module().blocks().len();
                    let chain = s.module().chain().clone();
                    out.push(ColumnItem {
                        id: s.id().to_string(),
                        name: s.name().to_string(),
                        subtitle: Some(format!("{block_count} block(s)")),
                        badge: None,
                        metadata: None,
                        structured_tags: TagSet::default(),
                        detail: DetailData {
                            chain: Some(chain),
                            ..Default::default()
                        },
                        tag: None,
                    });
                }
                out
            } else {
                Vec::new()
            };
            (items, Vec::new())
        }
        NavCategory::Blocks => {
            if let Some(idx) = col2_tag {
                if let Some(&bt) = ALL_BLOCK_TYPES.get(idx) {
                    let presets = controller.list_collections(bt).await;
                    let items = presets
                        .iter()
                        .map(|p| {
                            let tags = signal::tagging::infer_tags_from_name(p.name());
                            ColumnItem {
                                id: p.id().to_string(),
                                name: p.name().to_string(),
                                subtitle: None,
                                badge: Some(format!("{}", p.snapshots().len())),
                                metadata: None,
                                structured_tags: tags,
                                detail: DetailData::default(),
                                tag: col2_tag,
                            }
                        })
                        .collect();
                    return (items, presets);
                }
            }
            (Vec::new(), Vec::new())
        }
    }
}

// region: --- Detail resolution helpers

/// Resolve a layer's default variant module refs into `ModuleChainData` for grid rendering.
async fn resolve_layer_module_chains(
    controller: &SignalController,
    layer: &Layer,
) -> Vec<ModuleChainData> {
    let variant = match layer.default_variant() {
        Some(v) => v,
        None => return Vec::new(),
    };
    // Pre-fetch all module presets to look up module types for colors.
    let all_module_presets = controller.list_module_collections().await;
    let mut out = Vec::new();
    for mr in &variant.module_refs {
        let collection_id_str = mr.collection_id.to_string();
        let module_preset = all_module_presets
            .iter()
            .find(|p| p.id().to_string() == collection_id_str);
        let mt = module_preset.map(|p| p.module_type());
        let mc = mt
            .map(|m| m.color())
            .unwrap_or(signal::ModuleType::Drive.color());
        let module_name;
        let chain;
        if let Some(snapshot) = controller
            .load_module_collection_default(collection_id_str)
            .await
        {
            module_name = snapshot.name().to_string();
            chain = snapshot.module().chain().clone();
        } else {
            module_name = module_preset
                .map(|p| p.name().to_string())
                .unwrap_or_else(|| format!("Module {}", mr.collection_id));
            chain = SignalChain::new(vec![]);
        }
        out.push(ModuleChainData {
            name: module_name,
            color_bg: mc.bg.to_string(),
            color_fg: mc.fg.to_string(),
            color_border: mc.border.to_string(),
            chain,
            module_type: mt,
        });
    }
    out
}

/// Resolve a rig scene's full hierarchy into `EngineFlowData` for grid rendering.
///
/// Walks: `RigScene.engine_selections → Engine → EngineScene.layer_selections → Layer → modules`
async fn resolve_rig_scene_engines(
    controller: &SignalController,
    scene: &signal::rig::RigScene,
) -> Vec<EngineFlowData> {
    let mut engines = Vec::new();
    for es in &scene.engine_selections {
        let engine_id_str = es.engine_id.as_str();
        let engine = match controller.load_engine(engine_id_str).await {
            Some(e) => e,
            None => continue,
        };
        // Find the selected engine variant, fall back to default
        let engine_variant = engine
            .variant(&es.variant_id)
            .or_else(|| engine.default_variant());
        let engine_variant = match engine_variant {
            Some(v) => v,
            None => continue,
        };
        let mut layers = Vec::new();
        for ls in &engine_variant.layer_selections {
            let layer_id_str = ls.layer_id.as_str();
            let layer = match controller.load_layer(layer_id_str).await {
                Some(l) => l,
                None => continue,
            };
            let module_chains = resolve_layer_module_chains(controller, &layer).await;
            layers.push(LayerFlowData {
                name: layer.name.clone(),
                module_chains,
            });
        }
        engines.push(EngineFlowData {
            name: engine.name.clone(),
            layers,
        });
    }
    engines
}

// endregion: --- Detail resolution helpers

// region: --- Grid slot conversion

/// Preferred max columns before wrapping a module to the next row band.
const SOFT_MAX_COLS: usize = 14;

/// Gap between row bands — 2 empty rows for cable routing + split fan-out.
const ROW_BAND_STRIDE: usize = 3;

/// Flatten the full rig hierarchy (engines → layers → modules → blocks)
/// into a single `Vec<GridSlot>` for the interactive `DynamicGridView`.
///
/// Layout strategy (matching legacy `unified_grid_editor`):
///  - Modules flow left-to-right across the row band
///  - A module is **never split** across rows — if it won't fit in the
///    remaining columns, the entire module wraps to the next row band
///  - Row bands are separated by `ROW_BAND_STRIDE` rows (2 empty gap rows)
///  - Split nodes fan out vertically within the module's row band
fn engines_to_grid_slots(engines: &[EngineFlowData]) -> Vec<GridSlot> {
    let mut slots = Vec::new();
    let mut col: usize = 0;
    let mut row: usize = 0;

    for engine in engines {
        let engine_key = engine.name.clone();
        for layer in &engine.layers {
            let layer_key = format!("{}/{}", engine.name, layer.name);
            for mc in &layer.module_chains {
                let module_key = format!("{}/{}/{}", engine.name, layer.name, mc.name);
                let mt = mc.module_type;

                // Count how many columns this module needs
                let module_width = count_chain_width(mc.chain.nodes());

                // Wrap to next row band if module won't fit (never split a module)
                if col > 0 && col + module_width > SOFT_MAX_COLS {
                    col = 0;
                    row += ROW_BAND_STRIDE;
                }

                let mut col_cursor = col;
                flatten_chain_nodes(
                    mc.chain.nodes(),
                    &module_key,
                    &layer_key,
                    &engine_key,
                    mt,
                    &mut col_cursor,
                    row,
                    &mut slots,
                );

                col = col_cursor;
            }
        }
    }

    slots
}

/// Count the number of columns a chain of nodes needs (for wrapping decisions).
fn count_chain_width(nodes: &[signal::SignalNode]) -> usize {
    let mut width = 0;
    for node in nodes {
        match node {
            signal::SignalNode::Block(_) => width += 1,
            signal::SignalNode::Split { lanes } => {
                // A split's width is the max width among its lanes
                let max_lane_width = lanes
                    .iter()
                    .map(|lane| count_chain_width(lane.nodes()))
                    .max()
                    .unwrap_or(0);
                width += max_lane_width;
            }
        }
    }
    width
}

/// Recursively flatten SignalNodes into GridSlots, handling splits.
fn flatten_chain_nodes(
    nodes: &[signal::SignalNode],
    module_key: &str,
    layer_key: &str,
    engine_key: &str,
    module_type: Option<signal::ModuleType>,
    col_cursor: &mut usize,
    base_row: usize,
    slots: &mut Vec<GridSlot>,
) {
    for node in nodes {
        match node {
            signal::SignalNode::Block(mb) => {
                slots.push(GridSlot {
                    id: uuid::Uuid::new_v4(),
                    block_type: mb.block_type(),
                    block_preset_name: Some(mb.label().to_string()),
                    plugin_name: None,
                    col: *col_cursor,
                    row: base_row,
                    module_group: Some(module_key.to_string()),
                    module_type,
                    layer_group: Some(layer_key.to_string()),
                    engine_group: Some(engine_key.to_string()),
                    is_template: false,
                    bypassed: false,
                });
                *col_cursor += 1;
            }
            signal::SignalNode::Split { lanes } => {
                // Fan-out: each lane gets its own row, all starting at the same col
                let split_start_col = *col_cursor;
                let mut max_col = split_start_col;
                for (i, lane) in lanes.iter().enumerate() {
                    let mut lane_col = split_start_col;
                    flatten_chain_nodes(
                        lane.nodes(),
                        module_key,
                        layer_key,
                        engine_key,
                        module_type,
                        &mut lane_col,
                        base_row + i,
                        slots,
                    );
                    if lane_col > max_col {
                        max_col = lane_col;
                    }
                }
                *col_cursor = max_col;
            }
        }
    }
}

// endregion: --- Grid slot conversion

// region: --- RigGridPanel (stateful DynamicGridView wrapper)

#[derive(Props, Clone, PartialEq)]
struct RigGridPanelProps {
    initial_slots: Vec<GridSlot>,
}

/// Stateful wrapper around `DynamicGridView` + `BlockPickerDropdown`.
///
/// Owns local signals for chain, selection, and connections so the
/// detail panel can render an interactive grid without lifting state further.
#[component]
fn RigGridPanel(props: RigGridPanelProps) -> Element {
    let mut chain = use_signal(|| props.initial_slots.clone());
    let mut selection = use_signal(|| Option::<GridSelection>::None);
    let mut connections = use_signal(Vec::<DynGridConnection>::new);

    // Sync when the parent passes new data (e.g. user selects a different preset)
    use_effect(move || {
        chain.set(props.initial_slots.clone());
        selection.set(None);
        connections.set(Vec::new());
    });

    let picker_cell = PICKER_CELL();
    let picker_pos = PICKER_CLICK_POS();

    rsx! {
        div {
            class: "mt-3",
            style: "height: 480px;",
            DynamicGridView {
                chain: chain(),
                selection: selection(),
                connections: connections(),
                on_chain_change: move |new_chain: Vec<GridSlot>| {
                    chain.set(new_chain);
                },
                on_connections_change: move |new_conns: Vec<DynGridConnection>| {
                    connections.set(new_conns);
                },
                on_select: move |sel: Option<GridSelection>| {
                    selection.set(sel);
                },
            }
        }
        // Block picker rendered outside the transform context
        if let Some((col, row)) = picker_cell {
            BlockPickerDropdown {
                col: col,
                row: row,
                click_x: picker_pos.0,
                click_y: picker_pos.1,
                on_add_slot: move |slot: GridSlot| {
                    let mut current = chain();
                    current.push(slot);
                    chain.set(current);
                    *PICKER_CELL.write() = None;
                },
                on_close: move |_| {
                    *PICKER_CELL.write() = None;
                },
            }
        }
    }
}

// endregion: --- RigGridPanel

// endregion: --- Data Fetching

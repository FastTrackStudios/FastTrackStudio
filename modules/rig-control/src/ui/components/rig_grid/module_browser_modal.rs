//! Module browser modal for adding blocks to the signal flow grid.
//!
//! Modal overlay with:
//! - Search input for filtering modules
//! - Category filter tabs
//! - Grid of available modules with add buttons

use dioxus::prelude::*;
use crate::block::BlockType;

/// Props for the module browser modal.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleBrowserModalProps {
    /// Whether the modal is open.
    pub is_open: bool,
    /// Callback to close the modal.
    pub on_close: Callback<()>,
    /// Callback when a module type is selected to add.
    pub on_add_module: Callback<BlockType>,
}

/// Module browser modal for adding new blocks to the grid.
///
/// Features:
/// - Overlay backdrop (click to close)
/// - Search input for filtering
/// - Category tabs (All, Drive, Amp, Modulation, Time, Utility)
/// - Grid of modules with descriptions
#[component]
pub fn ModuleBrowserModal(props: ModuleBrowserModalProps) -> Element {
    if !props.is_open {
        return rsx! {};
    }

    let mut search_query = use_signal(String::new);
    let mut selected_category = use_signal(|| ModuleCategory::All);

    // Get modules filtered by search and category
    let filtered_modules = get_filtered_modules(search_query(), selected_category());

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/70 backdrop-blur-sm",
            onclick: move |_| props.on_close.call(()),

            // Modal container
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[600px] max-h-[80vh] \
                        flex flex-col overflow-hidden",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-4 py-3 border-b border-zinc-800",
                    h2 { class: "text-lg font-semibold text-zinc-200", "Add Module" }
                    button {
                        class: "p-1 rounded hover:bg-zinc-800 text-zinc-400 hover:text-zinc-200 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        // Close icon
                        svg {
                            class: "w-5 h-5",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            view_box: "0 0 24 24",
                            path {
                                stroke_linecap: "round",
                                stroke_linejoin: "round",
                                d: "M6 18L18 6M6 6l12 12",
                            }
                        }
                    }
                }

                // Search input
                div { class: "px-4 py-3 border-b border-zinc-800",
                    div { class: "relative",
                        input {
                            class: "w-full px-3 py-2 pl-9 text-sm bg-zinc-800 border border-zinc-700 rounded-lg \
                                    placeholder:text-zinc-500 focus:outline-none focus:ring-1 focus:ring-zinc-600 text-zinc-300",
                            r#type: "text",
                            placeholder: "Search modules...",
                            value: "{search_query}",
                            oninput: move |e| search_query.set(e.value().clone()),
                        }
                        // Search icon
                        span { class: "absolute left-3 top-2.5 text-zinc-500", "🔍" }
                    }
                }

                // Category tabs
                div { class: "px-4 py-2 border-b border-zinc-800 overflow-x-auto",
                    div { class: "flex gap-1",
                        for category in ModuleCategory::all() {
                            CategoryTab {
                                key: "{category.display_name()}",
                                category: *category,
                                is_active: *category == selected_category(),
                                on_click: move |cat| selected_category.set(cat),
                            }
                        }
                    }
                }

                // Module grid
                div { class: "flex-1 overflow-y-auto p-4",
                    if filtered_modules.is_empty() {
                        div { class: "flex items-center justify-center h-32 text-zinc-500",
                            "No modules found"
                        }
                    } else {
                        div { class: "grid grid-cols-2 gap-3",
                            for module in filtered_modules {
                                ModuleCard {
                                    key: "{module.name}",
                                    module: module.clone(),
                                    on_add: props.on_add_module.clone(),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Module category for filtering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ModuleCategory {
    #[default]
    All,
    Drive,
    Amp,
    Modulation,
    Time,
    Utility,
}

impl ModuleCategory {
    /// Get all categories.
    pub const fn all() -> &'static [ModuleCategory] {
        &[
            ModuleCategory::All,
            ModuleCategory::Drive,
            ModuleCategory::Amp,
            ModuleCategory::Modulation,
            ModuleCategory::Time,
            ModuleCategory::Utility,
        ]
    }

    /// Get display name.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::All => "All",
            Self::Drive => "Drive",
            Self::Amp => "Amp & Cab",
            Self::Modulation => "Modulation",
            Self::Time => "Time",
            Self::Utility => "Utility",
        }
    }

    /// Check if a block type belongs to this category.
    pub fn matches(&self, block_type: BlockType) -> bool {
        match self {
            Self::All => true,
            Self::Drive => matches!(block_type, BlockType::Drive | BlockType::Saturator),
            Self::Amp => matches!(block_type, BlockType::Amp | BlockType::Cabinet),
            Self::Modulation => matches!(
                block_type,
                BlockType::Modulation | BlockType::Pitch | BlockType::Tremolo
            ),
            Self::Time => matches!(block_type, BlockType::Delay | BlockType::Reverb | BlockType::Freeze),
            Self::Utility => matches!(
                block_type,
                BlockType::Compressor
                    | BlockType::Eq
                    | BlockType::Gate
                    | BlockType::Limiter
                    | BlockType::Volume
                    | BlockType::DeEsser
                    | BlockType::Tuner
                    | BlockType::Send
            ),
        }
    }
}

/// Module info for display.
#[derive(Debug, Clone, PartialEq)]
struct ModuleInfo {
    name: &'static str,
    description: &'static str,
    block_type: BlockType,
    icon: &'static str,
}

/// Get the list of available modules.
fn get_all_modules() -> Vec<ModuleInfo> {
    vec![
        // Drive
        ModuleInfo {
            name: "Overdrive",
            description: "Classic tube-style overdrive",
            block_type: BlockType::Drive,
            icon: "🔥",
        },
        ModuleInfo {
            name: "Distortion",
            description: "High-gain distortion pedal",
            block_type: BlockType::Drive,
            icon: "⚡",
        },
        ModuleInfo {
            name: "Fuzz",
            description: "Vintage fuzz tones",
            block_type: BlockType::Drive,
            icon: "💥",
        },
        ModuleInfo {
            name: "Saturator",
            description: "Tape-style saturation",
            block_type: BlockType::Saturator,
            icon: "📼",
        },
        // Amp & Cab
        ModuleInfo {
            name: "Amp",
            description: "Guitar amplifier simulation",
            block_type: BlockType::Amp,
            icon: "🎸",
        },
        ModuleInfo {
            name: "Cabinet",
            description: "Speaker cabinet IR",
            block_type: BlockType::Cabinet,
            icon: "📦",
        },
        // Modulation
        ModuleInfo {
            name: "Chorus",
            description: "Lush chorus effect",
            block_type: BlockType::Modulation,
            icon: "🌊",
        },
        ModuleInfo {
            name: "Phaser",
            description: "Classic phaser sweeps",
            block_type: BlockType::Modulation,
            icon: "🔄",
        },
        ModuleInfo {
            name: "Flanger",
            description: "Jet-like flanging",
            block_type: BlockType::Modulation,
            icon: "✈️",
        },
        ModuleInfo {
            name: "Tremolo",
            description: "Amplitude modulation",
            block_type: BlockType::Tremolo,
            icon: "〰️",
        },
        ModuleInfo {
            name: "Pitch Shift",
            description: "Pitch shifting and harmony",
            block_type: BlockType::Pitch,
            icon: "🎵",
        },
        // Time
        ModuleInfo {
            name: "Delay",
            description: "Digital delay with tap tempo",
            block_type: BlockType::Delay,
            icon: "⏱️",
        },
        ModuleInfo {
            name: "Reverb",
            description: "Room, hall, and plate reverbs",
            block_type: BlockType::Reverb,
            icon: "🏛️",
        },
        ModuleInfo {
            name: "Freeze",
            description: "Infinite sustain/freeze effect",
            block_type: BlockType::Freeze,
            icon: "❄️",
        },
        // Utility
        ModuleInfo {
            name: "Compressor",
            description: "Dynamic range compression",
            block_type: BlockType::Compressor,
            icon: "📊",
        },
        ModuleInfo {
            name: "EQ",
            description: "Parametric equalizer",
            block_type: BlockType::Eq,
            icon: "📈",
        },
        ModuleInfo {
            name: "Gate",
            description: "Noise gate",
            block_type: BlockType::Gate,
            icon: "🚪",
        },
        ModuleInfo {
            name: "Limiter",
            description: "Brick-wall limiter",
            block_type: BlockType::Limiter,
            icon: "🧱",
        },
        ModuleInfo {
            name: "Volume",
            description: "Volume/gain control",
            block_type: BlockType::Volume,
            icon: "🔊",
        },
        ModuleInfo {
            name: "De-Esser",
            description: "Sibilance reduction",
            block_type: BlockType::DeEsser,
            icon: "🔇",
        },
        ModuleInfo {
            name: "Tuner",
            description: "Chromatic tuner",
            block_type: BlockType::Tuner,
            icon: "🎯",
        },
        ModuleInfo {
            name: "Send",
            description: "Parallel routing send",
            block_type: BlockType::Send,
            icon: "↗️",
        },
    ]
}

/// Filter modules by search query and category.
fn get_filtered_modules(query: String, category: ModuleCategory) -> Vec<ModuleInfo> {
    let all_modules = get_all_modules();
    let query_lower = query.to_lowercase();

    all_modules
        .into_iter()
        .filter(|m| {
            // Filter by category
            if !category.matches(m.block_type) {
                return false;
            }

            // Filter by search query
            if !query_lower.is_empty() {
                let name_lower = m.name.to_lowercase();
                let desc_lower = m.description.to_lowercase();
                if !name_lower.contains(&query_lower) && !desc_lower.contains(&query_lower) {
                    return false;
                }
            }

            true
        })
        .collect()
}

/// Props for category tab.
#[derive(Props, Clone, PartialEq)]
struct CategoryTabProps {
    category: ModuleCategory,
    is_active: bool,
    on_click: EventHandler<ModuleCategory>,
}

/// Category filter tab.
#[component]
fn CategoryTab(props: CategoryTabProps) -> Element {
    let category = props.category;

    rsx! {
        button {
            class: if props.is_active {
                "px-3 py-1.5 rounded-lg text-sm font-medium bg-zinc-700 text-zinc-200 transition-colors"
            } else {
                "px-3 py-1.5 rounded-lg text-sm font-medium text-zinc-400 hover:text-zinc-200 \
                 hover:bg-zinc-800 transition-colors"
            },
            onclick: move |_| props.on_click.call(category),
            "{props.category.display_name()}"
        }
    }
}

/// Props for module card.
#[derive(Props, Clone, PartialEq)]
struct ModuleCardProps {
    module: ModuleInfo,
    on_add: Callback<BlockType>,
}

/// Module card in the browser grid.
#[component]
fn ModuleCard(props: ModuleCardProps) -> Element {
    let block_type = props.module.block_type;

    rsx! {
        div {
            class: "flex items-center gap-3 p-3 rounded-lg bg-zinc-800 hover:bg-zinc-750 \
                    border border-zinc-700 hover:border-zinc-600 transition-all cursor-pointer group",
            onclick: move |_| props.on_add.call(block_type),

            // Icon
            div { class: "text-2xl", "{props.module.icon}" }

            // Info
            div { class: "flex-1 min-w-0",
                h3 { class: "font-medium text-sm text-zinc-200 truncate",
                    "{props.module.name}"
                }
                p { class: "text-xs text-zinc-500 truncate",
                    "{props.module.description}"
                }
            }

            // Add button (appears on hover)
            div {
                class: "opacity-0 group-hover:opacity-100 transition-opacity",
                button {
                    class: "p-1.5 rounded bg-green-600 hover:bg-green-500 text-white transition-colors",
                    onclick: move |e| {
                        e.stop_propagation();
                        props.on_add.call(block_type);
                    },
                    // Plus icon
                    svg {
                        class: "w-4 h-4",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: "M12 4v16m8-8H4",
                        }
                    }
                }
            }
        }
    }
}

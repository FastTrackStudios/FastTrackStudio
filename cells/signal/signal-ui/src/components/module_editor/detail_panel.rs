//! Detail Panel — 3-column layout for editing the selected block/module.
//!
//! Layout:
//! ┌──────────────┬─────────────────────────────────────┬──────────────┐
//! │  Left 15%    │           Middle 65%                │  Right 15%   │
//! │  Preset /    │  Tabs: Macro | Detail | Advanced    │  Reserved    │
//! │  Snapshot    │                                     │  (future)    │
//! │  Selector    │                                     │              │
//! └──────────────┴─────────────────────────────────────┴──────────────┘

use super::module_editor_view::CompositionSlot;
use crate::components::rig_grid::block_colors::block_type_color;
use crate::prelude::*;

// ─────────────────────────────────────────────────────────────────────────────
// Detail tabs
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum DetailTab {
    #[default]
    Macro,
    Detail,
    Advanced,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum AdvancedSubTab {
    #[default]
    Parameters,
    Chunk,
}

// ─────────────────────────────────────────────────────────────────────────────
// DetailPanel
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DetailPanelProps {
    /// The currently selected slot (None = nothing selected).
    pub selected_slot: Option<CompositionSlot>,
}

/// 3-column detail panel for editing the selected block or module.
#[component]
pub fn DetailPanel(props: DetailPanelProps) -> Element {
    let mut active_tab = use_signal(|| DetailTab::Macro);
    let mut advanced_sub = use_signal(|| AdvancedSubTab::Parameters);
    let tab = active_tab();

    rsx! {
        div { class: "h-full w-full flex overflow-hidden",
            // ── Left column (15%): Preset / Snapshot selector ──
            div { class: "w-[15%] flex-shrink-0 border-r border-zinc-800/40 flex flex-col min-h-0 overflow-hidden",
                div { class: "px-3 py-2 border-b border-zinc-800/30 flex-shrink-0",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                        "Presets"
                    }
                }
                div { class: "flex-1 overflow-y-auto min-h-0 px-2 py-1.5",
                    if let Some(ref slot) = props.selected_slot {
                        {
                            let color = block_type_color(slot.block_type);
                            let dot_style = format!("background-color: {};", color.bg);
                            let name = slot.block_preset_name.as_deref()
                                .unwrap_or(slot.block_type.display_name());
                            rsx! {
                                div { class: "flex flex-col gap-2",
                                    // Current assignment
                                    div { class: "flex items-center gap-2 px-2 py-1.5 rounded-md bg-zinc-800/40 border border-zinc-700/30",
                                        div {
                                            class: "w-2 h-2 rounded-full flex-shrink-0",
                                            style: "{dot_style}",
                                        }
                                        div { class: "min-w-0",
                                            span { class: "text-[10px] font-medium text-zinc-200 block truncate", "{name}" }
                                            span { class: "text-[9px] text-zinc-500", "{slot.block_type.display_name()}" }
                                        }
                                    }
                                    // Preset list placeholder
                                    div { class: "px-2",
                                        p { class: "text-[9px] text-zinc-600 italic",
                                            "Saved presets will appear here"
                                        }
                                    }
                                    // Snapshot section
                                    div { class: "pt-2 border-t border-zinc-800/30",
                                        span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-[0.1em] px-2",
                                            "Snapshots"
                                        }
                                        div { class: "px-2 pt-1",
                                            p { class: "text-[9px] text-zinc-600 italic",
                                                "Snapshots will appear here"
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-[9px] text-zinc-600", "Select a block" }
                        }
                    }
                }
            }

            // ── Middle column (65%): Tabbed content ──
            div { class: "flex-1 flex flex-col min-h-0 min-w-0 overflow-hidden",
                // Tab bar
                div { class: "px-3 py-1.5 border-b border-zinc-800/30 flex items-center gap-1 flex-shrink-0",
                    {
                        let tabs = [
                            (DetailTab::Macro, "Macro"),
                            (DetailTab::Detail, "Detail"),
                            (DetailTab::Advanced, "Advanced"),
                        ];
                        rsx! {
                            for (t, label) in tabs {
                                button {
                                    key: "{label}",
                                    class: if tab == t {
                                        "px-3 py-1 rounded-md text-[10px] font-semibold text-zinc-200 bg-zinc-700/60 transition-colors"
                                    } else {
                                        "px-3 py-1 rounded-md text-[10px] font-medium text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800/40 transition-colors"
                                    },
                                    onclick: move |_| active_tab.set(t),
                                    "{label}"
                                }
                            }
                        }
                    }
                    // Advanced sub-tabs (when Advanced is active)
                    if tab == DetailTab::Advanced {
                        div { class: "ml-3 pl-3 border-l border-zinc-800/30 flex items-center gap-0.5",
                            {
                                let sub_tabs = [
                                    (AdvancedSubTab::Parameters, "Parameters"),
                                    (AdvancedSubTab::Chunk, "Chunk"),
                                ];
                                rsx! {
                                    for (st, label) in sub_tabs {
                                        button {
                                            key: "adv-{label}",
                                            class: if advanced_sub() == st {
                                                "px-2 py-0.5 rounded text-[9px] font-semibold text-zinc-300 bg-zinc-800/60 transition-colors"
                                            } else {
                                                "px-2 py-0.5 rounded text-[9px] font-medium text-zinc-600 hover:text-zinc-400 hover:bg-zinc-800/30 transition-colors"
                                            },
                                            onclick: move |_| advanced_sub.set(st),
                                            "{label}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Tab content
                div { class: "flex-1 overflow-y-auto min-h-0 px-3 py-2",
                    if props.selected_slot.is_none() {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-xs text-zinc-600",
                                "Select a block or module in the grid to edit"
                            }
                        }
                    } else {
                        match tab {
                            DetailTab::Macro => rsx! {
                                MacroTabContent { slot: props.selected_slot.clone().unwrap() }
                            },
                            DetailTab::Detail => rsx! {
                                DetailTabContent { slot: props.selected_slot.clone().unwrap() }
                            },
                            DetailTab::Advanced => rsx! {
                                AdvancedTabContent {
                                    slot: props.selected_slot.clone().unwrap(),
                                    sub_tab: advanced_sub(),
                                }
                            },
                        }
                    }
                }
            }

            // ── Right column (15%): Reserved ──
            div { class: "w-[15%] flex-shrink-0 border-l border-zinc-800/40 flex flex-col min-h-0 overflow-hidden",
                div { class: "px-3 py-2 border-b border-zinc-800/30 flex-shrink-0",
                    span { class: "text-[9px] font-bold text-zinc-500 uppercase tracking-[0.1em]",
                        "Controls"
                    }
                }
                div { class: "flex-1 flex items-center justify-center",
                    p { class: "text-[9px] text-zinc-700 italic text-center px-3",
                        "Reserved for routing, MIDI learn, automation"
                    }
                }
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tab content components
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct SlotProps {
    slot: CompositionSlot,
}

/// Macro tab: predefined "big knob" controls for the selected block/module.
#[component]
fn MacroTabContent(props: SlotProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500", "Macro Controls" }
            }
            // Placeholder macro knobs
            div { class: "grid grid-cols-4 gap-3",
                for label in ["Gain", "Tone", "Level", "Mix"] {
                    div { class: "flex flex-col items-center gap-1.5",
                        div {
                            class: "w-10 h-10 rounded-full border-2 border-zinc-700/50 \
                                    bg-zinc-800/30 flex items-center justify-center",
                            span { class: "text-[10px] text-zinc-500 font-mono", "0" }
                        }
                        span { class: "text-[9px] text-zinc-500 font-medium", "{label}" }
                    }
                }
            }
            p { class: "text-[9px] text-zinc-600 italic mt-2",
                "Macro controls will be populated from block parameter definitions"
            }
        }
    }
}

/// Detail tab: full parameter list with inline sliders.
#[component]
fn DetailTabContent(props: SlotProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500", "All Parameters" }
            }
            // Placeholder parameter list
            div { class: "flex flex-col gap-1.5",
                for (i, param_name) in ["Parameter 1", "Parameter 2", "Parameter 3", "Parameter 4"].iter().enumerate() {
                    div { class: "flex items-center gap-3 px-2 py-1 rounded hover:bg-zinc-800/30",
                        span { class: "text-[10px] text-zinc-400 w-24 flex-shrink-0", "{param_name}" }
                        div { class: "flex-1 h-1.5 bg-zinc-800 rounded-full overflow-hidden",
                            div {
                                class: "h-full rounded-full",
                                style: "width: {(i + 1) * 20}%; background-color: {dot_bg};",
                            }
                        }
                        span { class: "text-[9px] text-zinc-500 font-mono w-8 text-right",
                            "{(i + 1) * 20}%"
                        }
                    }
                }
            }
            p { class: "text-[9px] text-zinc-600 italic mt-2",
                "Full parameter list will be populated from the plugin's exposed parameters"
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct AdvancedTabProps {
    slot: CompositionSlot,
    sub_tab: AdvancedSubTab,
}

/// Advanced tab: raw parameter table or chunk data.
#[component]
fn AdvancedTabContent(props: AdvancedTabProps) -> Element {
    let color = block_type_color(props.slot.block_type);
    let dot_bg = color.bg;
    let name = props
        .slot
        .block_preset_name
        .as_deref()
        .unwrap_or(props.slot.block_type.display_name());

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                div {
                    class: "w-3 h-3 rounded-full flex-shrink-0",
                    style: "background-color: {dot_bg};",
                }
                span { class: "text-xs font-semibold text-zinc-200", "{name}" }
                span { class: "text-[10px] text-zinc-500",
                    if props.sub_tab == AdvancedSubTab::Parameters {
                        "Raw Parameters"
                    } else {
                        "State Chunk"
                    }
                }
            }
            match props.sub_tab {
                AdvancedSubTab::Parameters => rsx! {
                    // Raw parameter table
                    div { class: "border border-zinc-800/50 rounded-lg overflow-hidden",
                        // Header
                        div { class: "grid grid-cols-4 gap-px bg-zinc-800/30 px-3 py-1.5",
                            span { class: "text-[9px] font-semibold text-zinc-500", "Index" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Name" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Value" }
                            span { class: "text-[9px] font-semibold text-zinc-500", "Range" }
                        }
                        // Placeholder rows
                        for i in 0..4u32 {
                            div { class: "grid grid-cols-4 gap-px px-3 py-1 border-t border-zinc-800/30 hover:bg-zinc-800/20",
                                span { class: "text-[9px] font-mono text-zinc-500", "{i}" }
                                span { class: "text-[9px] text-zinc-400", "Param {i}" }
                                span { class: "text-[9px] font-mono text-zinc-300", "0.50" }
                                span { class: "text-[9px] font-mono text-zinc-600", "0.0 - 1.0" }
                            }
                        }
                    }
                    p { class: "text-[9px] text-zinc-600 italic",
                        "Raw parameter table will show all plugin parameters with their current values"
                    }
                },
                AdvancedSubTab::Chunk => rsx! {
                    // Chunk data display
                    div { class: "border border-zinc-800/50 rounded-lg p-3 bg-zinc-950/50",
                        pre { class: "text-[9px] font-mono text-zinc-500 whitespace-pre-wrap break-all leading-relaxed",
                            "// State chunk data will be displayed here\n\
                             // when a plugin is loaded into this block.\n\
                             //\n\
                             // This shows the raw REAPER FX state chunk,\n\
                             // useful for debugging and advanced editing."
                        }
                    }
                },
            }
        }
    }
}

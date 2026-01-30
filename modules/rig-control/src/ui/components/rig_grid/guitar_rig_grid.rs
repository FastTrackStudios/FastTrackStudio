//! Guitar rig grid - main entry component.
//!
//! This component displays either:
//! - A 14x6 signal flow grid (Grid mode) - Quad Cortex style
//! - A horizontal scrollable layout of module groups (Macro/Detail modes)

use std::collections::HashMap;

use dioxus::prelude::*;
use crate::id::BlockId;
use crate::module::Module;
use uuid::Uuid;

use super::grid_model::SignalFlowGrid;
use super::module_group::ModuleGroup;
use super::module_header::ModulePresetOption;
// TODO: Re-enable when signal_flow_grid is ported without audio_controls dependency
// use super::signal_flow_grid::SignalFlowGridView;
use super::view_mode::{GlobalViewOverride, ModuleViewMode};

/// Props for the guitar rig grid.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigGridProps {
    /// Modules to display in the module-based views (Macro/Detail).
    #[props(default)]
    pub modules: Vec<Module>,
    /// Signal flow grid data for Grid mode.
    #[props(default)]
    pub signal_flow: Option<SignalFlowGrid>,
    /// Callback when a module's bypass is toggled.
    pub on_toggle_module_bypass: Callback<Uuid>,
    /// Callback when a block's bypass is toggled.
    pub on_toggle_block_bypass: Callback<Uuid>,
    /// Callback when a macro value changes: (block_id, param_index, value).
    pub on_macro_change: Callback<(BlockId, u32, f64)>,
    /// Callback when a block parameter changes: (block_id, param_index, value).
    pub on_param_change: Callback<(Uuid, u32, f64)>,
    /// Available presets per module type (keyed by module ID).
    #[props(default)]
    pub module_presets: HashMap<Uuid, Vec<ModulePresetOption>>,
    /// Callback when a module preset is selected: (module_id, preset_id).
    #[props(default)]
    pub on_preset_select: Option<Callback<(Uuid, Uuid)>>,
    /// Callback when a block is clicked in grid mode.
    #[props(default)]
    pub on_block_click: Option<Callback<Uuid>>,
}

/// Guitar rig grid with view mode switching.
///
/// Features:
/// - Grid mode: 14x6 signal flow grid (Quad Cortex style)
/// - Macro mode: Quick-access knob layout
/// - Detail mode: Full parameter editing
/// - Global view override toggle
#[component]
pub fn GuitarRigGrid(props: GuitarRigGridProps) -> Element {
    // State: current view mode
    let mut current_mode = use_signal(|| ModuleViewMode::Grid);

    // State: per-module view mode preferences (for Macro/Detail modes)
    let mut module_prefs = use_signal(HashMap::<Uuid, ModuleViewMode>::new);

    // Get signal flow grid (use provided or sample)
    let grid = props.signal_flow.clone().unwrap_or_else(SignalFlowGrid::sample_guitar_rig);

    rsx! {
        div { class: "flex flex-col h-full bg-background",
            // Header with view mode controls
            GridHeader {
                current_mode: current_mode(),
                on_mode_change: move |mode| current_mode.set(mode),
            }

            // Content based on view mode
            div { class: "flex-1 overflow-auto",
                match current_mode() {
                    ModuleViewMode::Grid => rsx! {
                        // TODO: Re-enable when SignalFlowGridView is ported without audio_controls dependency
                        div { class: "p-4 flex items-center justify-center text-muted-foreground",
                            "Grid view temporarily unavailable"
                        }
                    },
                    ModuleViewMode::Macro | ModuleViewMode::Detail => rsx! {
                        // Module-based view
                        ModuleGridContent {
                            modules: props.modules.clone(),
                            view_mode: current_mode(),
                            module_prefs: module_prefs,
                            on_toggle_module_bypass: props.on_toggle_module_bypass.clone(),
                            on_toggle_block_bypass: props.on_toggle_block_bypass.clone(),
                            on_macro_change: props.on_macro_change.clone(),
                            on_param_change: props.on_param_change.clone(),
                            module_presets: props.module_presets.clone(),
                            on_preset_select: props.on_preset_select.clone(),
                        }
                    },
                }
            }
        }
    }
}

/// Props for module grid content.
#[derive(Props, Clone, PartialEq)]
struct ModuleGridContentProps {
    modules: Vec<Module>,
    view_mode: ModuleViewMode,
    module_prefs: Signal<HashMap<Uuid, ModuleViewMode>>,
    on_toggle_module_bypass: Callback<Uuid>,
    on_toggle_block_bypass: Callback<Uuid>,
    on_macro_change: Callback<(BlockId, u32, f64)>,
    on_param_change: Callback<(Uuid, u32, f64)>,
    module_presets: HashMap<Uuid, Vec<ModulePresetOption>>,
    on_preset_select: Option<Callback<(Uuid, Uuid)>>,
}

/// Module-based grid content (for Macro/Detail modes).
#[component]
fn ModuleGridContent(props: ModuleGridContentProps) -> Element {
    if props.modules.is_empty() {
        return rsx! {
            div { class: "flex items-center justify-center h-64 text-muted-foreground",
                div { class: "text-center",
                    p { class: "text-lg font-medium", "No modules in rig" }
                    p { class: "text-sm mt-1", "Add modules to see them here" }
                }
            }
        };
    }

    rsx! {
        div {
            class: "p-4 overflow-x-auto scrollbar-thin scrollbar-track-muted \
                    scrollbar-thumb-muted-foreground/30 hover:scrollbar-thumb-muted-foreground/50",
            style: "scrollbar-width: thin;",

            div { class: "flex gap-4",
                for module in &props.modules {
                    {
                        let module_id = module.id.as_uuid();
                        let view_mode = props.view_mode;
                        let presets = props.module_presets.get(&module_id)
                            .cloned()
                            .unwrap_or_default();

                        let on_preset_select = props.on_preset_select.clone();
                        let mut module_prefs = props.module_prefs;

                        rsx! {
                            ModuleGroup {
                                key: "{module_id}",
                                module: module.clone(),
                                view_mode,
                                on_toggle_module_bypass: props.on_toggle_module_bypass.clone(),
                                on_toggle_block_bypass: props.on_toggle_block_bypass.clone(),
                                on_macro_change: props.on_macro_change.clone(),
                                on_param_change: props.on_param_change.clone(),
                                on_view_mode_change: move |new_mode| {
                                    let mut prefs = module_prefs();
                                    prefs.insert(module_id, new_mode);
                                    module_prefs.set(prefs);
                                },
                                available_presets: presets,
                                on_preset_select: on_preset_select.clone().map(|cb| {
                                    Callback::new(move |preset_id| {
                                        cb.call((module_id, preset_id));
                                    })
                                }),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for the grid header.
#[derive(Props, Clone, PartialEq)]
struct GridHeaderProps {
    current_mode: ModuleViewMode,
    on_mode_change: EventHandler<ModuleViewMode>,
}

/// Grid header with title and view mode controls.
#[component]
fn GridHeader(props: GridHeaderProps) -> Element {
    rsx! {
        div { class: "flex items-center justify-between px-4 py-3 border-b border-border bg-card",
            // Title with preset indicator
            div { class: "flex items-center gap-3",
                h2 { class: "text-lg font-semibold text-green-500", "1A" }
                h2 { class: "text-lg font-medium", "Quad Cortex mini" }
            }

            // View mode controls
            div { class: "flex items-center gap-4",
                // View mode buttons
                div { class: "flex items-center gap-1 bg-muted rounded-lg p-1",
                    for mode in ModuleViewMode::all() {
                        {
                            let is_active = *mode == props.current_mode;
                            let mode_copy = *mode;
                            rsx! {
                                button {
                                    key: "{mode.display_name()}",
                                    class: if is_active {
                                        "px-3 py-1.5 rounded text-xs font-medium \
                                         bg-primary text-primary-foreground transition-colors"
                                    } else {
                                        "px-3 py-1.5 rounded text-xs font-medium \
                                         text-muted-foreground hover:text-foreground transition-colors"
                                    },
                                    title: "{mode.display_name()} view",
                                    onclick: move |_| props.on_mode_change.call(mode_copy),
                                    span { class: "mr-1", "{mode.icon()}" }
                                    span { "{mode.display_name()}" }
                                }
                            }
                        }
                    }
                }

                // Additional controls (stomp mode indicator from Quad Cortex)
                div { class: "flex items-center gap-2 text-xs text-muted-foreground",
                    span { "⚡ STOMP" }
                }
            }
        }
    }
}

//! Module header component.
//!
//! The header displays the module name, preset dropdown, bypass toggle,
//! and view mode selector.

use dioxus::prelude::*;
use rig_control::module::{Module, ModuleType};
use uuid::Uuid;

use super::view_mode::ModuleViewMode;

/// Props for the module header.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleHeaderProps {
    /// The module being displayed.
    pub module: Module,
    /// Currently selected view mode for this module.
    pub view_mode: ModuleViewMode,
    /// Callback when the module bypass is toggled.
    pub on_toggle_bypass: Callback<Uuid>,
    /// Callback when the view mode changes.
    pub on_view_mode_change: Callback<ModuleViewMode>,
    /// Available presets for this module type (optional).
    #[props(default)]
    pub available_presets: Vec<ModulePresetOption>,
    /// Callback when a preset is selected (optional).
    #[props(default)]
    pub on_preset_select: Option<Callback<Uuid>>,
}

/// A preset option for the dropdown.
#[derive(Debug, Clone, PartialEq)]
pub struct ModulePresetOption {
    pub id: Uuid,
    pub name: String,
}

/// Module header with name, controls, and view mode selector.
#[component]
pub fn ModuleHeader(props: ModuleHeaderProps) -> Element {
    let module = &props.module;
    let module_id = module.id.as_uuid();
    let is_enabled = module.enabled;

    let on_toggle = props.on_toggle_bypass.clone();

    rsx! {
        div { class: "flex items-center justify-between px-3 py-2 border-b border-border bg-card/50",
            // Left side: Module name and type
            div { class: "flex items-center gap-2 min-w-0 flex-1",
                // Module type indicator
                span { class: "text-xs text-muted-foreground font-medium uppercase tracking-wider",
                    "{module.module_type.display_name()}"
                }
                // Module name
                span { class: "font-semibold text-sm truncate", "{module.name}" }
            }

            // Right side: Controls
            div { class: "flex items-center gap-2 flex-shrink-0",
                // Preset dropdown (if presets available)
                if !props.available_presets.is_empty() {
                    PresetDropdown {
                        presets: props.available_presets.clone(),
                        on_select: props.on_preset_select.clone(),
                    }
                }

                // Bypass toggle
                button {
                    class: if is_enabled {
                        "w-8 h-8 rounded flex items-center justify-center \
                         bg-green-500/20 text-green-400 hover:bg-green-500/30 transition-colors"
                    } else {
                        "w-8 h-8 rounded flex items-center justify-center \
                         bg-red-500/20 text-red-400 hover:bg-red-500/30 transition-colors"
                    },
                    title: if is_enabled { "Click to bypass" } else { "Click to enable" },
                    onclick: move |_| on_toggle.call(module_id),
                    // Power icon
                    svg {
                        class: "w-4 h-4",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        path {
                            d: "M18.36 6.64a9 9 0 1 1-12.73 0",
                        }
                        line {
                            x1: "12",
                            y1: "2",
                            x2: "12",
                            y2: "12",
                        }
                    }
                }

                // View mode selector
                ViewModeSelector {
                    current_mode: props.view_mode,
                    on_change: props.on_view_mode_change.clone(),
                }
            }
        }
    }
}

/// Props for the preset dropdown.
#[derive(Props, Clone, PartialEq)]
struct PresetDropdownProps {
    presets: Vec<ModulePresetOption>,
    on_select: Option<Callback<Uuid>>,
}

/// Dropdown for selecting module presets.
#[component]
fn PresetDropdown(props: PresetDropdownProps) -> Element {
    let mut is_open = use_signal(|| false);

    rsx! {
        div { class: "relative",
            // Dropdown button
            button {
                class: "px-2 py-1 text-xs bg-muted hover:bg-muted/80 rounded transition-colors",
                onclick: move |_| is_open.set(!is_open()),
                "Presets ▼"
            }

            // Dropdown menu
            if is_open() {
                div {
                    class: "absolute right-0 top-full mt-1 min-w-32 bg-popover border border-border \
                            rounded-md shadow-lg z-50 max-h-48 overflow-y-auto",
                    for preset in &props.presets {
                        {
                            let preset_id = preset.id;
                            let preset_name = preset.name.clone();
                            let on_select = props.on_select.clone();
                            let mut is_open = is_open.clone();
                            rsx! {
                                button {
                                    key: "{preset_id}",
                                    class: "w-full px-3 py-2 text-left text-xs hover:bg-muted \
                                            transition-colors",
                                    onclick: move |_| {
                                        if let Some(callback) = on_select.clone() {
                                            callback.call(preset_id);
                                        }
                                        is_open.set(false);
                                    },
                                    "{preset_name}"
                                }
                            }
                        }
                    }
                }

                // Click-outside handler
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| is_open.set(false),
                }
            }
        }
    }
}

/// Props for the view mode selector.
#[derive(Props, Clone, PartialEq)]
struct ViewModeSelectorProps {
    current_mode: ModuleViewMode,
    on_change: Callback<ModuleViewMode>,
}

/// Button group for selecting view mode.
#[component]
fn ViewModeSelector(props: ViewModeSelectorProps) -> Element {
    rsx! {
        div { class: "flex items-center bg-muted rounded p-0.5",
            for mode in ModuleViewMode::all() {
                {
                    let is_active = *mode == props.current_mode;
                    let mode_copy = *mode;
                    let on_change = props.on_change.clone();
                    rsx! {
                        button {
                            key: "{mode.display_name()}",
                            class: if is_active {
                                "w-6 h-6 rounded flex items-center justify-center \
                                 bg-primary text-primary-foreground text-xs transition-colors"
                            } else {
                                "w-6 h-6 rounded flex items-center justify-center \
                                 text-muted-foreground hover:text-foreground text-xs transition-colors"
                            },
                            title: "{mode.display_name()}",
                            onclick: move |_| on_change.call(mode_copy),
                            "{mode.icon()}"
                        }
                    }
                }
            }
        }
    }
}

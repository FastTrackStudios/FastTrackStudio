//! Module group container component.
//!
//! A module group wraps a module's header and view content in a styled
//! container that adapts its width based on the current view mode.

use crate::callback_types::{MacroChange, SetParameter};
use crate::prelude::*;
use signal_control::id::BlockId;
use signal_control::module::Module;
use uuid::Uuid;

use super::module_header::{ModuleHeader, ModulePresetOption};
use super::view_mode::ModuleViewMode;

/// Props for the module group.
#[derive(Props, Clone, PartialEq)]
pub struct ModuleGroupProps {
    /// The module to display.
    pub module: Module,
    /// Current view mode for this module.
    pub view_mode: ModuleViewMode,
    /// Callback when the module bypass is toggled.
    pub on_toggle_module_bypass: Callback<Uuid>,
    /// Callback when a block's bypass is toggled.
    pub on_toggle_block_bypass: Callback<Uuid>,
    /// Callback when a macro value changes.
    pub on_macro_change: Callback<MacroChange>,
    /// Callback when a block parameter changes.
    pub on_param_change: Callback<SetParameter>,
    /// Callback when the view mode changes.
    pub on_view_mode_change: Callback<ModuleViewMode>,
    /// Available presets for this module (optional).
    #[props(default)]
    pub available_presets: Vec<ModulePresetOption>,
    /// Callback when a preset is selected (optional).
    #[props(default)]
    pub on_preset_select: Option<Callback<Uuid>>,
}

/// A module group container with header and view-mode-dependent content.
///
/// The width adapts based on view mode:
/// - Detail: ~320px
/// - Macro: ~192px
/// - Compact: ~128px
#[component]
pub fn ModuleGroup(props: ModuleGroupProps) -> Element {
    let module = &props.module;
    let is_enabled = module.enabled;

    // Calculate width class based on view mode
    let width_class = match props.view_mode {
        ModuleViewMode::Flow | ModuleViewMode::FlowCompact | ModuleViewMode::Grid => "w-full",
    };

    // Container styling based on enabled state
    let container_class = if is_enabled {
        format!(
            "{} border border-border rounded-lg overflow-hidden bg-card flex-shrink-0 \
             shadow-sm hover:shadow-md transition-shadow",
            width_class
        )
    } else {
        format!(
            "{} border border-border/50 rounded-lg overflow-hidden bg-card/50 flex-shrink-0 \
             opacity-60",
            width_class
        )
    };

    rsx! {
        div { class: "{container_class}",
            // Header
            ModuleHeader {
                module: module.clone(),
                view_mode: props.view_mode,
                on_toggle_bypass: props.on_toggle_module_bypass.clone(),
                on_view_mode_change: props.on_view_mode_change.clone(),
                available_presets: props.available_presets.clone(),
                on_preset_select: props.on_preset_select.clone(),
            }

            // Flow modes are handled at the GuitarRigGrid level
            // ModuleGroup is retained for potential future use but renders no content
        }
    }
}

/// Props for the level control.
#[derive(Props, Clone, PartialEq)]
struct ModuleLevelControlProps {
    level: f64,
    on_change: EventHandler<f64>,
}

/// Simple level slider for module output.
#[component]
fn ModuleLevelControl(props: ModuleLevelControlProps) -> Element {
    let level_percent = (props.level * 100.0) as i32;

    rsx! {
        div { class: "flex items-center gap-2 px-3 py-2 border-t border-border bg-muted/30",
            span { class: "text-xs text-muted-foreground w-10", "Level" }
            div { class: "flex-1 h-1 bg-muted rounded-full overflow-hidden",
                div {
                    class: "h-full bg-primary rounded-full transition-all",
                    style: "width: {level_percent}%;",
                }
            }
            span { class: "text-xs font-mono w-10 text-right", "{level_percent}%" }
        }
    }
}

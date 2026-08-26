//! Modes — the shared workflow configs (`config/workflows/*.styx`) parsed
//! into a display model for the site. A mode layers keybind overlays
//! (`config/overlays/*.styx`, resolved by id) plus its inline bindings on
//! top of whatever profile is active; the `mode-*` files are the modal
//! states (Record, Mix, Organize, …), the rest are toggleable workflows.
//! One mode is active at a time.

use input_config_proto::{
    ArmedActionDef, KeybindDef, OverlayConfig, ReaperSettingDef, WorkflowConfig, kebab_to_title,
};

use super::colors::category_color;
use super::input_tutorial::embedded::{OVERLAYS, WORKFLOWS};

/// One selectable mode/workflow, with its overlay bindings resolved.
#[derive(Clone, PartialEq)]
pub struct Mode {
    /// Workflow id — the filename stem, e.g. `mode-record`, `quick-edit`.
    pub id: String,
    /// Display name (file `name` override, else kebab→Title of the id).
    pub name: String,
    pub description: String,
    /// `mode-*` files are the modal states; the rest are workflows.
    pub is_modal: bool,
    /// All keyboard bindings the mode layers on the base profile:
    /// inline bindings first, then each `keybind_overlays` entry's
    /// bindings (resolved from the shared overlays, priority order).
    pub bindings: Vec<KeybindDef>,
    /// REAPER settings applied while the mode is active.
    pub settings: Vec<ReaperSettingDef>,
    /// Action armed while the mode is active.
    pub armed_action: Option<ArmedActionDef>,
}

impl Mode {
    /// The mode's accent color — same palette as categories, keyed on
    /// the workflow id so it's stable.
    pub fn color(&self) -> &'static str {
        category_color(&self.id)
    }
}

/// Accent color for a mode id (usable without a loaded [`Mode`]).
pub fn mode_color(id: &str) -> &'static str {
    category_color(id)
}

/// Parse every embedded workflow into a [`Mode`], overlays resolved.
/// Modal states (`mode-*`) sort before plain workflows; each group keeps
/// its file order.
pub fn load_modes() -> Vec<Mode> {
    let mut modes: Vec<Mode> = WORKFLOWS
        .iter()
        .filter_map(|w| {
            let config: WorkflowConfig = facet_styx::from_str(w.styx).ok()?;

            let name = config.name.clone().unwrap_or_else(|| kebab_to_title(w.id));
            let description = config.description.clone().unwrap_or_default();

            // Inline bindings are an implicit overlay; explicit overlays
            // resolve by id from the shared overlay files, stacked by
            // priority (higher priority later = closer to the top).
            let mut bindings: Vec<KeybindDef> = config.bindings().to_vec();
            let mut overlays: Vec<OverlayConfig> = config
                .keybind_overlays()
                .iter()
                .filter_map(|oid| {
                    let o = OVERLAYS.iter().find(|o| o.id == oid.as_str())?;
                    facet_styx::from_str::<OverlayConfig>(o.styx).ok()
                })
                .collect();
            overlays.sort_by_key(|o| o.priority);
            for o in &overlays {
                bindings.extend(o.bindings().iter().cloned());
            }

            Some(Mode {
                id: w.id.to_string(),
                name,
                description,
                is_modal: w.id.starts_with("mode-"),
                bindings,
                settings: config.settings().to_vec(),
                armed_action: config.armed_action().cloned(),
            })
        })
        .collect();
    modes.sort_by_key(|m| !m.is_modal);
    modes
}

/// Look up one mode by workflow id.
pub fn find_mode(id: &str) -> Option<Mode> {
    load_modes().into_iter().find(|m| m.id == id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_embedded_workflows_parse_into_modes() {
        let modes = load_modes();
        assert_eq!(
            modes.len(),
            WORKFLOWS.len(),
            "some workflow file failed to parse into a Mode"
        );
        // The modal states exist and are grouped first.
        let first_workflow = modes.iter().position(|m| !m.is_modal);
        if let Some(pos) = first_workflow {
            assert!(
                modes[pos..].iter().all(|m| !m.is_modal),
                "modal states must sort before plain workflows"
            );
        }
        let record = modes
            .iter()
            .find(|m| m.id == "mode-record")
            .expect("mode-record");
        assert_eq!(record.name, "Record");
        assert!(!record.bindings.is_empty());
    }

    #[test]
    fn overlay_references_resolve() {
        // tempo-mapping pulls the tempo-map overlay's bindings in.
        let m = find_mode("tempo-mapping").expect("tempo-mapping workflow");
        assert!(
            m.bindings.iter().any(|b| b.keys == "g"),
            "tempo-map overlay bindings should be resolved into the mode"
        );
    }
}

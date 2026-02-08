//! Dock presets — named layout snapshots (screensets).
//!
//! Equivalent to REAPER screensets: save/load/cycle complete layout configurations.

use serde::{Deserialize, Serialize};

use crate::id::PresetId;
use crate::layout::DockLayout;

/// A named dock layout preset (equivalent to a REAPER screenset).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DockPreset {
    pub id: PresetId,
    pub name: String,
    pub layout: DockLayout,
    /// Optional hotkey hint (e.g. "F5", "Ctrl+1") for display in the preset bar.
    pub hotkey_hint: Option<String>,
    /// Whether this preset was created by the user or is a built-in default.
    pub is_builtin: bool,
}

impl DockPreset {
    pub fn new(name: impl Into<String>, layout: DockLayout) -> Self {
        Self {
            id: PresetId::new(),
            name: name.into(),
            layout,
            hotkey_hint: None,
            is_builtin: false,
        }
    }

    pub fn builtin(name: impl Into<String>, layout: DockLayout) -> Self {
        Self {
            id: PresetId::new(),
            name: name.into(),
            layout,
            hotkey_hint: None,
            is_builtin: true,
        }
    }

    pub fn with_hotkey(mut self, hotkey: impl Into<String>) -> Self {
        self.hotkey_hint = Some(hotkey.into());
        self
    }
}

/// Collection of dock presets with an active selection.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PresetCollection {
    pub presets: Vec<DockPreset>,
    pub active_index: usize,
}

impl PresetCollection {
    pub fn new(presets: Vec<DockPreset>) -> Self {
        Self {
            presets,
            active_index: 0,
        }
    }

    pub fn active_preset(&self) -> Option<&DockPreset> {
        self.presets.get(self.active_index)
    }

    pub fn active_layout(&self) -> Option<&DockLayout> {
        self.active_preset().map(|p| &p.layout)
    }

    pub fn set_active(&mut self, index: usize) {
        if index < self.presets.len() {
            self.active_index = index;
        }
    }

    pub fn cycle_next(&mut self) {
        if !self.presets.is_empty() {
            self.active_index = (self.active_index + 1) % self.presets.len();
        }
    }

    pub fn cycle_prev(&mut self) {
        if !self.presets.is_empty() {
            self.active_index = if self.active_index == 0 {
                self.presets.len() - 1
            } else {
                self.active_index - 1
            };
        }
    }

    pub fn add_preset(&mut self, preset: DockPreset) {
        self.presets.push(preset);
    }

    pub fn remove_preset(&mut self, index: usize) -> Option<DockPreset> {
        if index < self.presets.len() {
            let preset = self.presets.remove(index);
            if self.active_index >= self.presets.len() && self.active_index > 0 {
                self.active_index -= 1;
            }
            Some(preset)
        } else {
            None
        }
    }

    /// Save the current layout back into the active preset.
    pub fn save_active(&mut self, layout: DockLayout) {
        if let Some(preset) = self.presets.get_mut(self.active_index) {
            preset.layout = layout;
        }
    }
}

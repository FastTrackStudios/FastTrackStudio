//! REAPER-specific InputProcessor wrapper.
//!
//! Bridges the gap between REAPER's raw VK-code keyboard API and the
//! unified `input::InputProcessor` state machine.

use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

use enumflags2::BitFlags;
use input::command::InputCommand;
use input::config::KeymapConfig;
use input::context::ActionContext;
use input::key::KeyChord;
use input::processor::InputProcessor;
use input::trie::KeyTrie;
use reaper_medium::{AcceleratorBehavior, AcceleratorKeyCode};
use tracing::{debug, info};

use super::keybinds::bridge;
use super::keybinds::which_key::WhichKeyEntry;
use super::keybinds::{KeybindContext, KeybindOverride, KeybindPreset};
use super::keybinds::defaults::{ALL_OVERRIDES, ALL_PRESETS};

// ---------------------------------------------------------------------------
// ReaperInputProcessor
// ---------------------------------------------------------------------------

/// Wraps `InputProcessor` for REAPER-specific usage.
///
/// Handles VK code → InputEvent conversion, context management,
/// override layers, and preset switching.
pub struct ReaperInputProcessor {
    processor: InputProcessor,
    context: ActionContext,

    /// The base KeymapConfig (from preset + which-key trees).
    base_config: KeymapConfig,

    /// Active override configs, keyed by name.
    active_overrides: Vec<(String, KeymapConfig)>,

    /// All available presets.
    available_presets: HashMap<String, (KeybindPreset, Vec<(String, String, Vec<WhichKeyEntry>)>)>,

    /// All available override layers.
    available_overrides: HashMap<String, KeybindOverride>,

    /// Name of the currently active preset.
    active_preset_name: String,

    /// Which-key trees for the current preset (kept for introspection).
    current_trees: Vec<(String, String, Vec<WhichKeyEntry>)>,
}

impl ReaperInputProcessor {
    /// Create from a preset and its which-key trees.
    pub fn new(
        preset: &KeybindPreset,
        trees: &[(String, String, Vec<WhichKeyEntry>)],
    ) -> Self {
        let config = bridge::preset_to_keymap_config(preset, trees);
        let processor = InputProcessor::from_config(config.clone())
            .unwrap_or_else(|e| {
                tracing::error!("Failed to build InputProcessor from config: {}", e);
                InputProcessor::new()
            });

        Self {
            processor,
            context: ActionContext::new(),
            base_config: config,
            active_overrides: Vec::new(),
            available_presets: HashMap::new(),
            available_overrides: HashMap::new(),
            active_preset_name: preset.name.clone(),
            current_trees: trees.to_vec(),
        }
    }

    /// Load all default presets and overrides.
    pub fn load_defaults(&mut self) {
        use super::keybinds::sections::which_key_fts::fts_which_key_trees;

        // Load presets
        for preset_fn in ALL_PRESETS.iter() {
            let preset = preset_fn();
            let name = preset.name.clone();

            // FTS preset includes which-key trees
            let trees = if name == "fastrackstudio" {
                fts_which_key_trees()
            } else {
                Vec::new()
            };

            self.available_presets.insert(name, (preset, trees));
        }

        // Load overrides
        for override_fn in ALL_OVERRIDES.iter() {
            let overlay = override_fn();
            self.available_overrides
                .insert(overlay.name.clone(), overlay);
        }

        info!(
            preset_count = self.available_presets.len(),
            override_count = self.available_overrides.len(),
            "ReaperInputProcessor defaults loaded"
        );
    }

    /// Process a REAPER key event, returning commands to execute.
    pub fn process_key(
        &mut self,
        key: AcceleratorKeyCode,
        behavior: &BitFlags<AcceleratorBehavior>,
    ) -> Vec<InputCommand> {
        let Some(event) = bridge::vk_to_input_event(key, behavior) else {
            return Vec::new(); // Pure modifier key, skip
        };

        self.processor.process(event, &self.context)
    }

    /// Update the REAPER context (e.g., when focus changes to MIDI editor).
    pub fn set_reaper_context(&mut self, ctx: KeybindContext) {
        // Remove all context tags first
        self.context.remove_tag("context:main");
        self.context.remove_tag("context:midi");
        self.context.remove_tag("context:midi_inline");
        self.context.remove_tag("context:media_explorer");
        self.context.remove_tag("context:global");

        // Set the active context tag
        match ctx {
            KeybindContext::Global => self.context.set_tag("context:global"),
            KeybindContext::Main => self.context.set_tag("context:main"),
            KeybindContext::Midi => self.context.set_tag("context:midi"),
            KeybindContext::MidiInline => self.context.set_tag("context:midi_inline"),
            KeybindContext::MediaExplorer => self.context.set_tag("context:media_explorer"),
        }
    }

    /// Enable an override layer by name.
    pub fn enable_override(&mut self, name: &str) {
        if self.active_overrides.iter().any(|(n, _)| n == name) {
            return; // Already active
        }

        let Some(overlay) = self.available_overrides.get(name) else {
            tracing::warn!(name = %name, "Attempted to enable unknown override layer");
            return;
        };

        let config = bridge::override_to_keymap_config(overlay);
        self.active_overrides
            .push((name.to_string(), config));

        // Sort by priority (highest last so they override)
        self.active_overrides.sort_by(|a, b| {
            let a_pri = self
                .available_overrides
                .get(&a.0)
                .map(|o| o.priority)
                .unwrap_or(0);
            let b_pri = self
                .available_overrides
                .get(&b.0)
                .map(|o| o.priority)
                .unwrap_or(0);
            a_pri.cmp(&b_pri)
        });

        self.rebuild_processor();
        info!(name = %name, "Override layer enabled");
    }

    /// Disable an override layer by name.
    pub fn disable_override(&mut self, name: &str) {
        let before = self.active_overrides.len();
        self.active_overrides.retain(|(n, _)| n != name);

        if self.active_overrides.len() != before {
            self.rebuild_processor();
            info!(name = %name, "Override layer disabled");
        }
    }

    /// Toggle an override layer, returns new active state.
    pub fn toggle_override(&mut self, name: &str) -> bool {
        if self.is_override_active(name) {
            self.disable_override(name);
            false
        } else {
            self.enable_override(name);
            true
        }
    }

    /// Check if an override is active.
    pub fn is_override_active(&self, name: &str) -> bool {
        self.active_overrides.iter().any(|(n, _)| n == name)
    }

    /// Disable all active overrides.
    pub fn disable_all_overrides(&mut self) {
        if !self.active_overrides.is_empty() {
            let count = self.active_overrides.len();
            self.active_overrides.clear();
            self.rebuild_processor();
            info!(count = count, "Disabled all overrides");
        }
    }

    /// Set the active preset by name.
    pub fn set_preset(&mut self, name: &str) -> bool {
        let Some((preset, trees)) = self.available_presets.get(name).cloned() else {
            tracing::warn!(name = %name, "Attempted to set unknown preset");
            return false;
        };

        self.base_config = bridge::preset_to_keymap_config(&preset, &trees);
        self.active_preset_name = name.to_string();
        self.current_trees = trees;
        self.active_overrides.clear();
        self.rebuild_processor();
        info!(name = %name, "Preset changed");
        true
    }

    /// Get the display string for pending keys (for which-key overlay).
    pub fn pending_display(&self) -> Option<String> {
        self.processor.pending_display()
    }

    /// Check if the processor is waiting for more keys.
    pub fn needs_timeout(&self) -> bool {
        self.processor.needs_timeout()
    }

    /// Handle timeout expiration for pending key sequences.
    pub fn timeout_expired(&mut self) -> Vec<InputCommand> {
        self.processor.timeout_expired()
    }

    /// Access the underlying processor for introspection (visualizer).
    pub fn processor(&self) -> &InputProcessor {
        &self.processor
    }

    /// Get the currently active preset name.
    pub fn active_preset_name(&self) -> &str {
        &self.active_preset_name
    }

    /// Get names of all available presets.
    pub fn available_presets(&self) -> Vec<String> {
        self.available_presets.keys().cloned().collect()
    }

    /// Get names of all available override layers.
    pub fn available_overrides(&self) -> Vec<String> {
        self.available_overrides.keys().cloned().collect()
    }

    /// Get the names of currently active override layers.
    pub fn active_override_names(&self) -> Vec<&str> {
        self.active_overrides.iter().map(|(n, _)| n.as_str()).collect()
    }

    /// Get the keytrie for the normal mode (for which-key introspection).
    pub fn normal_keytrie(&self) -> Option<&KeyTrie> {
        self.processor
            .keymaps()
            .get(&input::mode::ModeId::normal())
    }

    /// Get continuations at a given prefix path (for which-key overlay).
    pub fn continuations_at(&self, prefix: &[KeyChord]) -> Vec<(String, String, bool)> {
        if let Some(trie) = self.normal_keytrie() {
            bridge::trie_continuations_at(trie, prefix)
        } else {
            Vec::new()
        }
    }

    /// Get all root-level prefix keys with labels (for which-key cheat sheet).
    pub fn all_root_prefixes(&self) -> Vec<(String, String, bool)> {
        self.continuations_at(&[])
    }

    /// Get the which-key trees for the current preset.
    pub fn current_trees(&self) -> &[(String, String, Vec<WhichKeyEntry>)] {
        &self.current_trees
    }

    /// Rebuild the InputProcessor from base config + active overrides.
    fn rebuild_processor(&mut self) {
        let mut merged = self.base_config.clone();

        for (_, override_config) in &self.active_overrides {
            merged = KeymapConfig::merge(merged, override_config.clone());
        }

        match InputProcessor::from_config(merged) {
            Ok(processor) => {
                self.processor = processor;
                debug!("InputProcessor rebuilt");
            }
            Err(e) => {
                tracing::error!("Failed to rebuild InputProcessor: {}", e);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Global state
// ---------------------------------------------------------------------------

/// Get the global ReaperInputProcessor instance.
pub fn get_processor() -> &'static RwLock<ReaperInputProcessor> {
    static PROCESSOR: OnceLock<RwLock<ReaperInputProcessor>> = OnceLock::new();
    PROCESSOR.get_or_init(|| {
        let mut proc = init_processor();
        proc.load_defaults();

        // Set FTS as default preset
        if !proc.set_preset("fastrackstudio") {
            tracing::warn!("Failed to set fastrackstudio as default preset");
        }

        info!("ReaperInputProcessor lazily initialized with defaults");
        RwLock::new(proc)
    })
}

/// Create the initial processor from the default FTS preset.
fn init_processor() -> ReaperInputProcessor {
    use super::keybinds::sections::which_key_fts::fts_which_key_trees;
    use super::keybinds::defaults::fastrackstudio_preset;

    let preset = fastrackstudio_preset();
    let trees = fts_which_key_trees();
    ReaperInputProcessor::new(&preset, &trees)
}

/// Initialize the processor (explicit trigger for lazy init).
pub fn init() {
    let _ = get_processor();
    info!("ReaperInputProcessor initialized");
}

/// Resolve a key event through the global processor, returning commands.
pub fn process_key(
    key: AcceleratorKeyCode,
    behavior: &BitFlags<AcceleratorBehavior>,
) -> Vec<InputCommand> {
    let mut proc = get_processor().write().unwrap();
    proc.process_key(key, behavior)
}

/// Update the global processor's REAPER context.
pub fn set_context(ctx: KeybindContext) {
    let mut proc = get_processor().write().unwrap();
    proc.set_reaper_context(ctx);
}

/// Enable an override layer globally.
pub fn enable_override(name: &str) {
    let mut proc = get_processor().write().unwrap();
    proc.enable_override(name);
}

/// Disable an override layer globally.
pub fn disable_override(name: &str) {
    let mut proc = get_processor().write().unwrap();
    proc.disable_override(name);
}

/// Toggle an override layer globally, returns new state.
pub fn toggle_override(name: &str) -> bool {
    let mut proc = get_processor().write().unwrap();
    proc.toggle_override(name)
}

/// Check if an override is active.
pub fn is_override_active(name: &str) -> bool {
    let proc = get_processor().read().unwrap();
    proc.is_override_active(name)
}

/// Disable all overrides.
pub fn disable_all_overrides() {
    let mut proc = get_processor().write().unwrap();
    proc.disable_all_overrides();
}

/// Set the active preset.
pub fn set_preset(name: &str) -> bool {
    let mut proc = get_processor().write().unwrap();
    proc.set_preset(name)
}

/// Get the active preset name.
pub fn active_preset_name() -> String {
    let proc = get_processor().read().unwrap();
    proc.active_preset_name().to_string()
}

/// Get all available preset names.
pub fn available_presets() -> Vec<String> {
    let proc = get_processor().read().unwrap();
    proc.available_presets()
}

/// Get all available override names.
pub fn available_overrides() -> Vec<String> {
    let proc = get_processor().read().unwrap();
    proc.available_overrides()
}

/// Check if the processor has a pending sequence.
pub fn needs_timeout() -> bool {
    let proc = get_processor().read().unwrap();
    proc.needs_timeout()
}

/// Get the pending display string.
pub fn pending_display() -> Option<String> {
    let proc = get_processor().read().unwrap();
    proc.pending_display()
}

/// Handle timeout expiration.
pub fn timeout_expired() -> Vec<InputCommand> {
    let mut proc = get_processor().write().unwrap();
    proc.timeout_expired()
}

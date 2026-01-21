//! Keybind Resolver
//!
//! Handles resolution of key sequences through the layer stack:
//! Override Layers → Base Preset → (fallback to REAPER)

use super::defaults::{self, ALL_OVERRIDES, ALL_PRESETS};
use super::{Keybind, KeybindContext, KeybindOverride, KeybindPreset, WheelBind, WheelDirection};
use std::collections::HashMap;
use tracing::{debug, info};

/// Key for wheel binding cache lookups
/// (context, modifiers_normalized, is_horizontal)
type WheelCacheKey = (KeybindContext, String, bool);

/// Value for wheel binding cache
/// (action_id, direction)
type WheelCacheValue = (String, WheelDirection);

/// The keybind resolver handles layer-based keybind resolution
pub struct KeybindResolver {
    /// Currently active base preset
    base_preset: KeybindPreset,

    /// All available presets (loaded from defaults + user files)
    available_presets: HashMap<String, KeybindPreset>,

    /// All available override layers
    available_overrides: HashMap<String, KeybindOverride>,

    /// Currently enabled override layers (sorted by priority, highest first)
    active_overrides: Vec<String>,

    /// Cached resolution map: (context, keys) -> action
    cache: HashMap<(KeybindContext, String), String>,

    /// Cached wheel bindings: (context, modifiers, horizontal) -> (action, direction)
    wheel_cache: HashMap<WheelCacheKey, WheelCacheValue>,
}

impl KeybindResolver {
    /// Create a new resolver with empty configuration
    pub fn new() -> Self {
        Self {
            base_preset: KeybindPreset::new("empty", "Empty preset"),
            available_presets: HashMap::new(),
            available_overrides: HashMap::new(),
            active_overrides: Vec::new(),
            cache: HashMap::new(),
            wheel_cache: HashMap::new(),
        }
    }

    /// Load default presets and overrides
    pub fn load_defaults(&mut self) {
        // Load all built-in presets
        for preset in ALL_PRESETS.iter() {
            let preset_instance = preset();
            self.available_presets
                .insert(preset_instance.name.clone(), preset_instance);
        }

        // Load all built-in overrides
        for overlay in ALL_OVERRIDES.iter() {
            let overlay_instance = overlay();
            self.available_overrides
                .insert(overlay_instance.name.clone(), overlay_instance);
        }

        // Set FastTrackStudio as default preset
        if let Some(fts) = self.available_presets.get("fastrackstudio").cloned() {
            self.base_preset = fts;
        }

        self.rebuild_cache();

        info!(
            preset = %self.base_preset.name,
            preset_count = self.available_presets.len(),
            override_count = self.available_overrides.len(),
            "Keybind resolver initialized"
        );
    }

    /// Resolve a key sequence to an action
    ///
    /// Resolution order:
    /// 1. Active override layers (highest priority first)
    /// 2. Base preset
    /// 3. Returns None (let REAPER handle it)
    pub fn resolve(&self, context: KeybindContext, keys: &str) -> Option<&str> {
        // Normalize the key sequence
        let normalized = keys.to_lowercase();

        // Check cache first
        if let Some(action) = self.cache.get(&(context, normalized.clone())) {
            debug!(keys = %keys, action = %action, "Keybind resolved from cache");
            return Some(action.as_str());
        }

        // Try global context if specific context didn't match
        if context != KeybindContext::Global {
            if let Some(action) = self.cache.get(&(KeybindContext::Global, normalized)) {
                debug!(keys = %keys, action = %action, "Keybind resolved from global context");
                return Some(action.as_str());
            }
        }

        None
    }

    /// Resolve a wheel event to an action
    ///
    /// # Arguments
    /// * `context` - The keybind context (Main, Midi, etc.)
    /// * `modifiers` - Modifier string (e.g., "<C-S->" or "")
    /// * `horizontal` - Whether this is a horizontal wheel event
    /// * `delta` - Wheel delta (positive = up, negative = down)
    ///
    /// # Returns
    /// The action ID if a matching wheel binding is found
    pub fn resolve_wheel(
        &self,
        context: KeybindContext,
        modifiers: &str,
        horizontal: bool,
        delta: i16,
    ) -> Option<&str> {
        // Normalize the modifiers
        let normalized_modifiers = modifiers.to_lowercase();

        // Look up in wheel cache
        let cache_key = (context, normalized_modifiers.clone(), horizontal);

        if let Some((action, direction)) = self.wheel_cache.get(&cache_key) {
            // Check if direction matches
            let matches = match direction {
                WheelDirection::Both => true,
                WheelDirection::Up => delta > 0,
                WheelDirection::Down => delta < 0,
            };

            if matches {
                debug!(
                    modifiers = %modifiers,
                    horizontal = horizontal,
                    delta = delta,
                    action = %action,
                    "Wheel binding resolved from cache"
                );
                return Some(action.as_str());
            }
        }

        // Try global context if specific context didn't match
        if context != KeybindContext::Global {
            let global_key = (KeybindContext::Global, normalized_modifiers, horizontal);
            if let Some((action, direction)) = self.wheel_cache.get(&global_key) {
                let matches = match direction {
                    WheelDirection::Both => true,
                    WheelDirection::Up => delta > 0,
                    WheelDirection::Down => delta < 0,
                };

                if matches {
                    debug!(
                        modifiers = %modifiers,
                        action = %action,
                        "Wheel binding resolved from global context"
                    );
                    return Some(action.as_str());
                }
            }
        }

        None
    }

    /// Enable an override layer by name
    pub fn enable_override(&mut self, name: &str) {
        if !self.available_overrides.contains_key(name) {
            tracing::warn!(name = %name, "Attempted to enable unknown override layer");
            return;
        }

        if self.active_overrides.contains(&name.to_string()) {
            return; // Already enabled
        }

        self.active_overrides.push(name.to_string());
        self.sort_overrides();
        self.rebuild_cache();

        info!(name = %name, "Override layer enabled");
    }

    /// Disable an override layer by name
    pub fn disable_override(&mut self, name: &str) {
        let before_len = self.active_overrides.len();
        self.active_overrides.retain(|n| n != name);

        if self.active_overrides.len() != before_len {
            self.rebuild_cache();
            info!(name = %name, "Override layer disabled");
        }
    }

    /// Toggle an override layer, returns new state
    pub fn toggle_override(&mut self, name: &str) -> bool {
        if self.is_override_active(name) {
            self.disable_override(name);
            false
        } else {
            self.enable_override(name);
            true
        }
    }

    /// Check if an override layer is active
    pub fn is_override_active(&self, name: &str) -> bool {
        self.active_overrides.contains(&name.to_string())
    }

    /// Disable all active override layers
    pub fn disable_all_overrides(&mut self) {
        if !self.active_overrides.is_empty() {
            let count = self.active_overrides.len();
            self.active_overrides.clear();
            self.rebuild_cache();
            info!(count = count, "Disabled all keybind overrides");
        }
    }

    /// Set the base preset by name
    pub fn set_preset_by_name(&mut self, name: &str) -> bool {
        if let Some(preset) = self.available_presets.get(name).cloned() {
            self.base_preset = preset;
            self.rebuild_cache();
            info!(name = %name, "Base preset changed");
            true
        } else {
            tracing::warn!(name = %name, "Attempted to set unknown preset");
            false
        }
    }

    /// Set the base preset directly
    pub fn set_preset(&mut self, preset: KeybindPreset) {
        let name = preset.name.clone();
        self.base_preset = preset;
        self.rebuild_cache();
        info!(name = %name, "Base preset changed");
    }

    /// Get the name of the currently active preset
    pub fn active_preset_name(&self) -> &str {
        &self.base_preset.name
    }

    /// Get names of all available presets
    pub fn available_presets(&self) -> Vec<String> {
        self.available_presets.keys().cloned().collect()
    }

    /// Get names of all available override layers
    pub fn available_overrides(&self) -> Vec<String> {
        self.available_overrides.keys().cloned().collect()
    }

    /// Get names of currently active override layers
    pub fn active_override_names(&self) -> &[String] {
        &self.active_overrides
    }

    /// Sort active overrides by priority (highest first)
    fn sort_overrides(&mut self) {
        self.active_overrides.sort_by(|a, b| {
            let a_priority = self
                .available_overrides
                .get(a)
                .map(|o| o.priority)
                .unwrap_or(0);
            let b_priority = self
                .available_overrides
                .get(b)
                .map(|o| o.priority)
                .unwrap_or(0);
            b_priority.cmp(&a_priority) // Descending order
        });
    }

    /// Rebuild the resolution cache
    fn rebuild_cache(&mut self) {
        self.cache.clear();
        self.wheel_cache.clear();

        // Collect all bindings to add (base preset first, then overrides in priority order)
        let mut bindings_to_add: Vec<(KeybindContext, String, String)> = Vec::new();
        let mut wheel_bindings_to_add: Vec<(WheelCacheKey, WheelCacheValue)> = Vec::new();

        // Add base preset bindings first (lowest priority)
        for binding in &self.base_preset.bindings {
            let context = binding.effective_context();
            let normalized_keys = binding.keys.to_lowercase();
            bindings_to_add.push((context, normalized_keys, binding.action.clone()));
        }

        // Add base preset wheel bindings
        for wheel_bind in &self.base_preset.wheel_bindings {
            let context = wheel_bind.effective_context();
            let normalized_modifiers = wheel_bind.modifiers.to_lowercase();
            let cache_key = (context, normalized_modifiers, wheel_bind.horizontal);
            let cache_value = (wheel_bind.action.clone(), wheel_bind.direction);
            wheel_bindings_to_add.push((cache_key, cache_value));
        }

        // Add override bindings in priority order (so higher priority overwrites lower)
        let mut sorted_override_names: Vec<_> = self.active_overrides.clone();
        sorted_override_names.sort_by(|a, b| {
            let a_priority = self
                .available_overrides
                .get(a)
                .map(|o| o.priority)
                .unwrap_or(0);
            let b_priority = self
                .available_overrides
                .get(b)
                .map(|o| o.priority)
                .unwrap_or(0);
            a_priority.cmp(&b_priority) // Ascending order (lower priority first)
        });

        for name in &sorted_override_names {
            if let Some(overlay) = self.available_overrides.get(name) {
                // Add key bindings
                for binding in &overlay.bindings {
                    let context = binding.effective_context();
                    let normalized_keys = binding.keys.to_lowercase();
                    bindings_to_add.push((context, normalized_keys, binding.action.clone()));
                }

                // Add wheel bindings
                for wheel_bind in &overlay.wheel_bindings {
                    let context = wheel_bind.effective_context();
                    let normalized_modifiers = wheel_bind.modifiers.to_lowercase();
                    let cache_key = (context, normalized_modifiers, wheel_bind.horizontal);
                    let cache_value = (wheel_bind.action.clone(), wheel_bind.direction);
                    wheel_bindings_to_add.push((cache_key, cache_value));
                }
            }
        }

        // Now add all bindings to cache (later entries overwrite earlier)
        for (context, keys, action) in bindings_to_add {
            self.cache.insert((context, keys), action);
        }

        // Add wheel bindings to wheel cache
        for (key, value) in wheel_bindings_to_add {
            self.wheel_cache.insert(key, value);
        }

        debug!(
            cache_size = self.cache.len(),
            wheel_cache_size = self.wheel_cache.len(),
            active_overrides = ?self.active_overrides,
            "Keybind cache rebuilt"
        );
    }
}

impl Default for KeybindResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_preset() -> KeybindPreset {
        KeybindPreset::new("test", "Test preset").with_bindings(vec![
            Keybind::new("a", "action_a"),
            Keybind::new("b", "action_b"),
            Keybind::new("<C-s>", "action_save"),
        ])
    }

    fn test_override() -> KeybindOverride {
        KeybindOverride::new("test_override", "Test override")
            .with_priority(100)
            .with_bindings(vec![
                Keybind::new("a", "override_action_a"), // Override 'a'
                Keybind::new("c", "action_c"),          // New binding
            ])
    }

    #[test]
    fn test_basic_resolution() {
        let mut resolver = KeybindResolver::new();
        resolver
            .available_presets
            .insert("test".to_string(), test_preset());
        resolver.set_preset_by_name("test");

        assert_eq!(
            resolver.resolve(KeybindContext::Global, "a"),
            Some("action_a")
        );
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "b"),
            Some("action_b")
        );
        assert_eq!(resolver.resolve(KeybindContext::Global, "x"), None);
    }

    #[test]
    fn test_override_resolution() {
        let mut resolver = KeybindResolver::new();
        resolver
            .available_presets
            .insert("test".to_string(), test_preset());
        resolver
            .available_overrides
            .insert("test_override".to_string(), test_override());
        resolver.set_preset_by_name("test");

        // Before enabling override
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "a"),
            Some("action_a")
        );
        assert_eq!(resolver.resolve(KeybindContext::Global, "c"), None);

        // Enable override
        resolver.enable_override("test_override");

        // After enabling override - 'a' should be overridden
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "a"),
            Some("override_action_a")
        );
        // 'c' should now be available
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "c"),
            Some("action_c")
        );
        // 'b' should still work from base preset
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "b"),
            Some("action_b")
        );

        // Disable override
        resolver.disable_override("test_override");
        assert_eq!(
            resolver.resolve(KeybindContext::Global, "a"),
            Some("action_a")
        );
    }

    #[test]
    fn test_toggle_override() {
        let mut resolver = KeybindResolver::new();
        resolver
            .available_overrides
            .insert("test_override".to_string(), test_override());

        assert!(!resolver.is_override_active("test_override"));
        assert!(resolver.toggle_override("test_override"));
        assert!(resolver.is_override_active("test_override"));
        assert!(!resolver.toggle_override("test_override"));
        assert!(!resolver.is_override_active("test_override"));
    }
}

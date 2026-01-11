//! Mouse Modifier Manager
//!
//! Provides a layered system for mouse modifier profiles with stackable overrides.
//! Similar to the keybind overlay system, this allows temporary overrides
//! (e.g., for tempo mapping workflow) that can be reverted.
//!
//! ## Architecture
//!
//! - **Base Profile**: Set by the active keybind preset (FTS, Logic, Reaper, etc.)
//! - **Override Layers**: Stackable, temporary changes (e.g., tempo_map overlay)
//! - **Resolution Order**: Override Layer (highest priority first) → Base Profile
//!
//! When an override is disabled, the original values are restored.

use super::behaviors::media_item::{
    MediaItemCrossfadeLeftDragBehavior, MediaItemEdgeLeftDragBehavior,
    MediaItemLowerLeftDragBehavior,
};
use super::behaviors::shared::traits::BehaviorDisplay;
use super::core::{get_mouse_modifier, set_mouse_modifier, MouseModifierFlag};
use reaper_high::Reaper;
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};
use tracing::{debug, info};

/// A single mouse modifier setting
#[derive(Debug, Clone)]
pub struct MouseModifierSetting {
    /// REAPER context string (e.g., "MM_CTX_ITEMEDGE")
    pub context: &'static str,
    /// Modifier flags (shift, ctrl, alt, win)
    pub flags: MouseModifierFlag,
    /// Behavior ID with " m" suffix (e.g., "2 m" for Stretch item)
    pub behavior: &'static str,
    /// Human-readable description
    pub description: Option<&'static str>,
}

impl MouseModifierSetting {
    pub fn new(context: &'static str, flags: MouseModifierFlag, behavior: &'static str) -> Self {
        Self {
            context,
            flags,
            behavior,
            description: None,
        }
    }

    /// Create a setting from a behavior enum that implements BehaviorDisplay
    pub fn from_behavior<B: BehaviorDisplay>(
        context: &'static str,
        flags: MouseModifierFlag,
        behavior: B,
    ) -> Self {
        // Leak the string to get a 'static str - this is fine for long-lived config
        let behavior_str: &'static str = Box::leak(behavior.behavior_id_string().into_boxed_str());
        let desc: &'static str = Box::leak(behavior.display_name().to_string().into_boxed_str());
        Self {
            context,
            flags,
            behavior: behavior_str,
            description: Some(desc),
        }
    }

    pub fn with_description(mut self, desc: &'static str) -> Self {
        self.description = Some(desc);
        self
    }

    /// Create a setting for the default modifier (no keys held) from a behavior enum
    pub fn default_behavior<B: BehaviorDisplay>(context: &'static str, behavior: B) -> Self {
        Self::from_behavior(context, MouseModifierFlag::none(), behavior)
    }

    /// Create a setting with Shift modifier from a behavior enum
    pub fn shift_behavior<B: BehaviorDisplay>(context: &'static str, behavior: B) -> Self {
        Self::from_behavior(
            context,
            MouseModifierFlag::new(true, false, false, false),
            behavior,
        )
    }

    /// Create a setting with Cmd/Ctrl modifier from a behavior enum
    pub fn cmd_behavior<B: BehaviorDisplay>(context: &'static str, behavior: B) -> Self {
        Self::from_behavior(
            context,
            MouseModifierFlag::new(false, true, false, false),
            behavior,
        )
    }

    /// Create a setting with Alt/Opt modifier from a behavior enum
    pub fn alt_behavior<B: BehaviorDisplay>(context: &'static str, behavior: B) -> Self {
        Self::from_behavior(
            context,
            MouseModifierFlag::new(false, false, true, false),
            behavior,
        )
    }

    // Legacy string-based methods (for backwards compatibility)

    /// Create a setting for the default modifier (no keys held)
    pub fn default_action(context: &'static str, behavior: &'static str) -> Self {
        Self::new(context, MouseModifierFlag::none(), behavior)
    }

    /// Create a setting with Shift modifier
    pub fn with_shift(context: &'static str, behavior: &'static str) -> Self {
        Self::new(
            context,
            MouseModifierFlag::new(true, false, false, false),
            behavior,
        )
    }

    /// Create a setting with Cmd/Ctrl modifier
    pub fn with_cmd(context: &'static str, behavior: &'static str) -> Self {
        Self::new(
            context,
            MouseModifierFlag::new(false, true, false, false),
            behavior,
        )
    }

    /// Create a setting with Alt/Opt modifier
    pub fn with_alt(context: &'static str, behavior: &'static str) -> Self {
        Self::new(
            context,
            MouseModifierFlag::new(false, false, true, false),
            behavior,
        )
    }
}

/// A base mouse modifier profile (tied to a keybind preset)
#[derive(Debug, Clone)]
pub struct MouseModifierProfile {
    /// Profile name (matches keybind preset name)
    pub name: &'static str,
    /// Human-readable description
    pub description: &'static str,
    /// Mouse modifier settings for this profile
    pub settings: Vec<MouseModifierSetting>,
}

impl MouseModifierProfile {
    pub fn new(name: &'static str, description: &'static str) -> Self {
        Self {
            name,
            description,
            settings: Vec::new(),
        }
    }

    pub fn with_setting(mut self, setting: MouseModifierSetting) -> Self {
        self.settings.push(setting);
        self
    }

    pub fn with_settings(mut self, settings: impl IntoIterator<Item = MouseModifierSetting>) -> Self {
        self.settings.extend(settings);
        self
    }
}

/// A stackable mouse modifier override layer
#[derive(Debug, Clone)]
pub struct MouseModifierOverride {
    /// Override name (e.g., "tempo_map")
    pub name: &'static str,
    /// Human-readable description
    pub description: &'static str,
    /// Priority for stacking (higher = applied later, wins conflicts)
    pub priority: i32,
    /// Mouse modifier settings to apply
    pub settings: Vec<MouseModifierSetting>,
}

impl MouseModifierOverride {
    pub fn new(name: &'static str, description: &'static str) -> Self {
        Self {
            name,
            description,
            priority: 0,
            settings: Vec::new(),
        }
    }

    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_setting(mut self, setting: MouseModifierSetting) -> Self {
        self.settings.push(setting);
        self
    }

    pub fn with_settings(mut self, settings: impl IntoIterator<Item = MouseModifierSetting>) -> Self {
        self.settings.extend(settings);
        self
    }
}

/// Key for storing/restoring mouse modifier values
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ModifierKey {
    context: String,
    flag: i32,
}

/// Manages mouse modifier profiles and overlays
pub struct MouseModifierManager {
    /// Currently active base profile name
    active_profile: String,

    /// All available base profiles
    profiles: HashMap<String, MouseModifierProfile>,

    /// All available override layers
    overrides: HashMap<String, MouseModifierOverride>,

    /// Currently active override layers (sorted by priority)
    active_overrides: Vec<String>,

    /// Stored original values for restoration when overrides are disabled
    /// Key: (context, flag), Value: original behavior string
    stored_values: HashMap<ModifierKey, String>,
}

impl MouseModifierManager {
    pub fn new() -> Self {
        Self {
            active_profile: "reaper".to_string(),
            profiles: HashMap::new(),
            overrides: HashMap::new(),
            active_overrides: Vec::new(),
            stored_values: HashMap::new(),
        }
    }

    /// Register a base profile
    pub fn register_profile(&mut self, profile: MouseModifierProfile) {
        self.profiles.insert(profile.name.to_string(), profile);
    }

    /// Register an override layer
    pub fn register_override(&mut self, override_layer: MouseModifierOverride) {
        self.overrides
            .insert(override_layer.name.to_string(), override_layer);
    }

    /// Set the active base profile and apply its settings
    pub fn set_profile(&mut self, name: &str) -> bool {
        if !self.profiles.contains_key(name) {
            tracing::warn!(name = %name, "Attempted to set unknown mouse modifier profile");
            return false;
        }

        // Log state BEFORE changing
        info!(name = %name, "Applying mouse modifier profile...");
        self.log_before_after("BEFORE");

        self.active_profile = name.to_string();
        self.apply_current_state();

        // Log state AFTER changing
        self.log_before_after("AFTER");
        info!(name = %name, "Mouse modifier profile applied");
        true
    }

    /// Log current REAPER mouse modifier state (helper for before/after comparison)
    fn log_before_after(&self, label: &str) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        let contexts = [
            ("MM_CTX_ITEMEDGE", "Item Edge"),
        ];

        let default_flag = MouseModifierFlag::none();
        for (ctx, display_name) in contexts {
            if let Some(value) = get_mouse_modifier(ctx, default_flag, medium_reaper) {
                let behavior_name = super::contexts::get_behavior_display_name(ctx, &value);
                info!("  [{}] {}: {} ({})", label, display_name, behavior_name, value);
            }
        }
    }

    /// Enable an override layer
    pub fn enable_override(&mut self, name: &str) -> bool {
        if !self.overrides.contains_key(name) {
            tracing::warn!(name = %name, "Attempted to enable unknown mouse modifier override");
            return false;
        }

        if self.active_overrides.contains(&name.to_string()) {
            return true; // Already enabled
        }

        // Clone settings to avoid borrow issues
        let settings = self
            .overrides
            .get(name)
            .map(|o| o.settings.clone())
            .unwrap_or_default();

        // Store current values before applying override
        self.store_current_values(&settings);

        self.active_overrides.push(name.to_string());
        self.sort_overrides();
        self.apply_current_state();

        info!(name = %name, "Mouse modifier override enabled");
        true
    }

    /// Disable an override layer and restore previous values
    pub fn disable_override(&mut self, name: &str) -> bool {
        let before_len = self.active_overrides.len();
        self.active_overrides.retain(|n| n != name);

        if self.active_overrides.len() != before_len {
            // Clone settings to avoid borrow issues
            let settings = self
                .overrides
                .get(name)
                .map(|o| o.settings.clone())
                .unwrap_or_default();

            // Restore stored values for settings that were overridden
            self.restore_stored_values(&settings);

            self.apply_current_state();
            info!(name = %name, "Mouse modifier override disabled");
            true
        } else {
            false
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

    /// Check if an override is active
    pub fn is_override_active(&self, name: &str) -> bool {
        self.active_overrides.contains(&name.to_string())
    }

    /// Disable all active override layers
    pub fn disable_all_overrides(&mut self) {
        if self.active_overrides.is_empty() {
            return;
        }

        let count = self.active_overrides.len();

        // Collect all settings from active overrides to restore
        let mut all_settings: Vec<MouseModifierSetting> = Vec::new();
        for name in &self.active_overrides {
            if let Some(override_layer) = self.overrides.get(name) {
                all_settings.extend(override_layer.settings.iter().cloned());
            }
        }

        // Clear active overrides
        self.active_overrides.clear();

        // Restore stored values for all affected settings
        self.restore_stored_values(&all_settings);

        // Re-apply base profile state
        self.apply_current_state();

        info!(count = count, "Disabled all mouse modifier overrides");
    }

    /// Get the active profile name
    pub fn active_profile_name(&self) -> &str {
        &self.active_profile
    }

    /// Sort active overrides by priority (lowest first, so highest wins when applied)
    fn sort_overrides(&mut self) {
        self.active_overrides.sort_by(|a, b| {
            let a_priority = self.overrides.get(a).map(|o| o.priority).unwrap_or(0);
            let b_priority = self.overrides.get(b).map(|o| o.priority).unwrap_or(0);
            a_priority.cmp(&b_priority)
        });
    }

    /// Store current REAPER values for the given settings
    fn store_current_values(&mut self, settings: &[MouseModifierSetting]) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        for setting in settings {
            let key = ModifierKey {
                context: setting.context.to_string(),
                flag: setting.flags.to_flag(),
            };

            // Only store if we don't already have a stored value (preserve original)
            if !self.stored_values.contains_key(&key) {
                if let Some(current) = get_mouse_modifier(setting.context, setting.flags, medium_reaper)
                {
                    debug!(
                        context = %setting.context,
                        flag = setting.flags.to_flag(),
                        value = %current,
                        "Storing mouse modifier value for restoration"
                    );
                    self.stored_values.insert(key, current);
                }
            }
        }
    }

    /// Restore stored values for the given settings
    fn restore_stored_values(&mut self, settings: &[MouseModifierSetting]) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        for setting in settings {
            let key = ModifierKey {
                context: setting.context.to_string(),
                flag: setting.flags.to_flag(),
            };

            if let Some(original_value) = self.stored_values.remove(&key) {
                debug!(
                    context = %setting.context,
                    flag = setting.flags.to_flag(),
                    value = %original_value,
                    "Restoring mouse modifier value"
                );
                let _ = set_mouse_modifier(setting.context, setting.flags, &original_value, medium_reaper);
            }
        }
    }

    /// Apply the current state (base profile + active overrides) to REAPER
    fn apply_current_state(&self) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        // Collect all settings to apply (profile first, then overrides in priority order)
        let mut settings_to_apply: Vec<&MouseModifierSetting> = Vec::new();

        // Add base profile settings
        if let Some(profile) = self.profiles.get(&self.active_profile) {
            settings_to_apply.extend(profile.settings.iter());
        }

        // Add override settings (in priority order, so higher priority wins)
        for name in &self.active_overrides {
            if let Some(override_layer) = self.overrides.get(name) {
                settings_to_apply.extend(override_layer.settings.iter());
            }
        }

        // Apply all settings
        for setting in settings_to_apply {
            if let Err(e) = set_mouse_modifier(
                setting.context,
                setting.flags,
                setting.behavior,
                medium_reaper,
            ) {
                tracing::warn!(
                    context = %setting.context,
                    behavior = %setting.behavior,
                    error = %e,
                    "Failed to apply mouse modifier setting"
                );
            } else {
                debug!(
                    context = %setting.context,
                    flag = setting.flags.to_flag(),
                    behavior = %setting.behavior,
                    description = ?setting.description,
                    "Applied mouse modifier setting"
                );
            }
        }
    }

    /// Log all current mouse modifier settings (for debugging)
    pub fn log_current_state(&self) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();

        info!(
            profile = %self.active_profile,
            overrides = ?self.active_overrides,
            "Current mouse modifier state"
        );

        // Log key contexts with human-readable behavior names
        let contexts = [
            ("MM_CTX_ITEM", "Media Item"),
            ("MM_CTX_ITEMEDGE", "Media Item Edge"),
            ("MM_CTX_ITEMFADE", "Media Item Fade"),
        ];

        let default_flag = MouseModifierFlag::none();
        for (ctx, display_name) in contexts {
            if let Some(value) = get_mouse_modifier(ctx, default_flag, medium_reaper) {
                // Get human-readable behavior name
                let behavior_name = super::contexts::get_behavior_display_name(ctx, &value);
                info!("  {}: {} ({})", display_name, behavior_name, value);
            }
        }
    }
}

impl Default for MouseModifierManager {
    fn default() -> Self {
        Self::new()
    }
}

// === Global Manager Instance ===

static MANAGER: OnceLock<RwLock<MouseModifierManager>> = OnceLock::new();

/// Get the global mouse modifier manager instance (lazily initialized with defaults)
pub fn get_manager() -> &'static RwLock<MouseModifierManager> {
    MANAGER.get_or_init(|| {
        let mut manager = MouseModifierManager::new();

        // Register base profiles
        manager.register_profile(fts_profile());
        manager.register_profile(reaper_profile());
        manager.register_profile(logic_profile());

        // Register override layers
        manager.register_override(tempo_map_override());
        manager.register_override(quick_edit_override());

        info!("Mouse modifier manager initialized with {} profiles", manager.profiles.len());

        RwLock::new(manager)
    })
}

/// Initialize the mouse modifier manager (for explicit early initialization)
/// Note: The manager is also lazily initialized on first access via get_manager()
pub fn init() {
    // Just access the manager to trigger lazy initialization
    let _ = get_manager();
}

/// Set the active profile by name
pub fn set_profile(name: &str) -> bool {
    let mut manager = get_manager().write().unwrap();
    manager.set_profile(name)
}

/// Enable an override layer
pub fn enable_override(name: &str) -> bool {
    let mut manager = get_manager().write().unwrap();
    manager.enable_override(name)
}

/// Disable an override layer
pub fn disable_override(name: &str) -> bool {
    let mut manager = get_manager().write().unwrap();
    manager.disable_override(name)
}

/// Toggle an override layer, returns new state
pub fn toggle_override(name: &str) -> bool {
    let mut manager = get_manager().write().unwrap();
    manager.toggle_override(name)
}

/// Check if an override is active
pub fn is_override_active(name: &str) -> bool {
    let manager = get_manager().read().unwrap();
    manager.is_override_active(name)
}

/// Disable all active override layers
pub fn disable_all_overrides() {
    let mut manager = get_manager().write().unwrap();
    manager.disable_all_overrides();
}

/// Log current state for debugging
pub fn log_state() {
    let manager = get_manager().read().unwrap();
    manager.log_current_state();
}

// === Built-in Profiles ===

/// FastTrackStudio profile - optimized for creative workflow
fn fts_profile() -> MouseModifierProfile {
    MouseModifierProfile::new("fastrackstudio", "FastTrackStudio optimized mouse modifiers")
        .with_settings([
            // Item edge: Stretch by default (instead of Move edge)
            // This makes time-stretching items easier without holding a modifier
            MouseModifierSetting::default_behavior(
                "MM_CTX_ITEMEDGE",
                MediaItemEdgeLeftDragBehavior::StretchItem,
            ),
            // Shift+edge: Move edge (swap with default)
            MouseModifierSetting::shift_behavior(
                "MM_CTX_ITEMEDGE",
                MediaItemEdgeLeftDragBehavior::MoveEdge,
            ),
            // Item lower half: Move item (just move) by default
            MouseModifierSetting::default_behavior(
                "MM_CTX_ITEMLOWER",
                MediaItemLowerLeftDragBehavior::MoveItem,
            ),
        ])
}

/// Default REAPER profile
fn reaper_profile() -> MouseModifierProfile {
    MouseModifierProfile::new("reaper", "Default REAPER mouse modifiers")
        .with_settings([
            // Item edge: Move edge by default (REAPER default)
            MouseModifierSetting::default_behavior(
                "MM_CTX_ITEMEDGE",
                MediaItemEdgeLeftDragBehavior::MoveEdge,
            ),
        ])
}

/// Logic Pro style profile
fn logic_profile() -> MouseModifierProfile {
    MouseModifierProfile::new("logic", "Logic Pro style mouse modifiers")
        .with_settings([
            // Item edge: Move edge by default (similar to Logic)
            MouseModifierSetting::default_behavior(
                "MM_CTX_ITEMEDGE",
                MediaItemEdgeLeftDragBehavior::MoveEdge,
            ),
        ])
}

// === Built-in Overrides ===

/// Tempo Map override - for tempo mapping workflow
fn tempo_map_override() -> MouseModifierOverride {
    MouseModifierOverride::new("tempo_map", "Tempo mapping workflow mouse modifiers")
        .with_priority(100)
        .with_settings([
            // During tempo mapping, we might want different behaviors
            // For example, item edge could do something special
            // This is a placeholder - customize based on workflow needs
        ])
}

/// Quick Edit override - for fast slip editing workflow
fn quick_edit_override() -> MouseModifierOverride {
    MouseModifierOverride::new("quick_edit", "Quick slip editing workflow mouse modifiers")
        .with_priority(50)
        .with_settings([
            // Media Item Bottom Half Left Drag → "Move Item Contents"
            MouseModifierSetting::default_behavior(
                "MM_CTX_ITEMLOWER",
                MediaItemLowerLeftDragBehavior::MoveItemContents,
            ),
            // Media Item Fade Intersection Left Drag → "Move both fades ignoring snap"
            MouseModifierSetting::default_behavior(
                "MM_CTX_CROSSFADE",
                MediaItemCrossfadeLeftDragBehavior::MoveBothFadesIgnoringSnap,
            ),
        ])
}

//! Layered Keybinding System
//!
//! A keybinding system with base presets and overlay overrides for contextual keyboard control.
//!
//! ## Architecture
//!
//! **Base Presets** (mutually exclusive, user chooses one):
//! - `Reaper` - Default REAPER-style keybinds
//! - `Logic` - Logic Pro-style keybinds
//! - `FastTrackStudio` - FTS-optimized keybinds
//! - `ReaVim` - Vim-style modal keybinds (placeholder for future)
//!
//! **Override Layers** (stackable, enabled by state):
//! - `TempoMap` - Active during tempo mapping workflow
//! - (Future: `Recording`, `Mixing`, etc.)
//!
//! **Resolution Order**: Override Layer → Base Preset → REAPER Default

mod config;
pub mod defaults;
mod parser;
mod resolver;
pub mod sections;

pub use config::KeybindConfig;
pub use parser::{KeySequence, Modifiers, ParsedKey};
pub use resolver::KeybindResolver;

use facet::Facet;

/// Context where keybinds apply
#[derive(Debug, Clone, Copy, Facet, PartialEq, Eq, Hash, Default)]
#[repr(C)]
pub enum KeybindContext {
    /// All contexts (default)
    #[default]
    Global,
    /// Arrange view (main window)
    Main,
    /// MIDI editor window
    Midi,
    /// Inline MIDI editor
    MidiInline,
    /// Media explorer
    MediaExplorer,
}

impl KeybindContext {
    /// Check if this context matches another (Global matches everything)
    pub fn matches(&self, other: &KeybindContext) -> bool {
        *self == KeybindContext::Global || *other == KeybindContext::Global || *self == *other
    }
}

/// A single keybinding definition
#[derive(Debug, Clone, Facet)]
pub struct Keybind {
    /// Key sequence (e.g., "g", "gg", "<C-s>", "<S-Tab>")
    pub keys: String,

    /// Action command ID to execute (e.g., "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE" or "40044")
    pub action: String,

    /// Optional description for UI/docs
    pub description: Option<String>,

    /// Context where this binding is active (None = Global)
    pub context: Option<KeybindContext>,
}

impl Keybind {
    /// Create a new keybind
    pub fn new(keys: impl Into<String>, action: impl Into<String>) -> Self {
        Self {
            keys: keys.into(),
            action: action.into(),
            description: None,
            context: None,
        }
    }

    /// Builder: add description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Builder: set context
    pub fn with_context(mut self, ctx: KeybindContext) -> Self {
        self.context = Some(ctx);
        self
    }

    /// Get the effective context (defaults to Global)
    pub fn effective_context(&self) -> KeybindContext {
        self.context.unwrap_or(KeybindContext::Global)
    }
}

/// Mouse modifier context - where mouse modifiers apply
#[derive(Debug, Clone, Copy, Facet, PartialEq, Eq, Hash, Default)]
#[repr(C)]
pub enum MouseModifierContext {
    /// Media item left edge
    #[default]
    MediaItemLeftEdge,
    /// Media item right edge
    MediaItemRightEdge,
    /// Media item bottom half
    MediaItemBottomHalf,
    /// Media item fade/autocrossfade
    MediaItemFade,
    /// Envelope point
    EnvelopePoint,
    /// Envelope segment
    EnvelopeSegment,
    /// Track control panel
    TrackControlPanel,
    /// Arrange view (empty area)
    ArrangeView,
    /// MIDI note
    MidiNote,
    /// MIDI CC lane
    MidiCCLane,
}

/// A mouse modifier binding (for click+drag behaviors)
#[derive(Debug, Clone, Facet)]
pub struct MouseModifier {
    /// The context where this modifier applies
    pub context: MouseModifierContext,

    /// Modifiers required (e.g., "<C->" for Ctrl+click, "" for no modifiers)
    pub modifiers: String,

    /// Action command ID or behavior to execute
    pub action: String,

    /// Optional description for UI/docs
    pub description: Option<String>,
}

impl MouseModifier {
    /// Create a new mouse modifier binding
    pub fn new(
        context: MouseModifierContext,
        modifiers: impl Into<String>,
        action: impl Into<String>,
    ) -> Self {
        Self {
            context,
            modifiers: modifiers.into(),
            action: action.into(),
            description: None,
        }
    }

    /// Builder: add description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

/// Direction for wheel bindings
#[derive(Debug, Clone, Copy, Facet, PartialEq, Eq, Hash, Default)]
#[repr(C)]
pub enum WheelDirection {
    /// Wheel scrolled up
    Up,
    /// Wheel scrolled down
    Down,
    /// Either direction (action receives delta value via relative mode)
    #[default]
    Both,
}

/// A mouse wheel binding
#[derive(Debug, Clone, Facet)]
pub struct WheelBind {
    /// Modifiers required (e.g., "<C->" for Ctrl+wheel, "" for no modifiers)
    pub modifiers: String,

    /// Action command ID to execute
    pub action: String,

    /// Direction: up, down, or both (action called with delta)
    pub direction: WheelDirection,

    /// Whether this is for horizontal wheel
    pub horizontal: bool,

    /// Optional description for UI/docs
    pub description: Option<String>,

    /// Context where this binding is active (None = Global)
    pub context: Option<KeybindContext>,
}

impl WheelBind {
    /// Create a new wheel binding
    pub fn new(modifiers: impl Into<String>, action: impl Into<String>) -> Self {
        Self {
            modifiers: modifiers.into(),
            action: action.into(),
            direction: WheelDirection::Both,
            horizontal: false,
            description: None,
            context: None,
        }
    }

    /// Builder: set direction
    pub fn with_direction(mut self, direction: WheelDirection) -> Self {
        self.direction = direction;
        self
    }

    /// Builder: set as horizontal wheel
    pub fn with_horizontal(mut self) -> Self {
        self.horizontal = true;
        self
    }

    /// Builder: add description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Builder: set context
    pub fn with_context(mut self, ctx: KeybindContext) -> Self {
        self.context = Some(ctx);
        self
    }

    /// Get the effective context (defaults to Global)
    pub fn effective_context(&self) -> KeybindContext {
        self.context.unwrap_or(KeybindContext::Global)
    }

    /// Check if this binding matches the given direction
    pub fn matches_direction(&self, delta: i16) -> bool {
        match self.direction {
            WheelDirection::Both => true,
            WheelDirection::Up => delta > 0,
            WheelDirection::Down => delta < 0,
        }
    }
}

/// A complete keybind preset (base layer)
#[derive(Debug, Clone, Facet)]
pub struct KeybindPreset {
    /// Unique name for this preset (e.g., "fastrackstudio", "reaper", "logic")
    pub name: String,

    /// Human-readable description
    pub description: String,

    /// Version string for compatibility tracking
    pub version: String,

    /// The keybindings in this preset
    pub bindings: Vec<Keybind>,

    /// Wheel bindings for mouse wheel actions
    pub wheel_bindings: Vec<WheelBind>,

    /// Mouse modifier bindings (click+drag behaviors)
    pub mouse_modifiers: Vec<MouseModifier>,
}

fn default_version() -> String {
    "1.0.0".to_string()
}

impl KeybindPreset {
    /// Create a new preset
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            version: default_version(),
            bindings: Vec::new(),
            wheel_bindings: Vec::new(),
            mouse_modifiers: Vec::new(),
        }
    }

    /// Builder: add a binding
    pub fn with_binding(mut self, binding: Keybind) -> Self {
        self.bindings.push(binding);
        self
    }

    /// Builder: add multiple bindings
    pub fn with_bindings(mut self, bindings: impl IntoIterator<Item = Keybind>) -> Self {
        self.bindings.extend(bindings);
        self
    }

    /// Builder: add a wheel binding
    pub fn with_wheel_binding(mut self, binding: WheelBind) -> Self {
        self.wheel_bindings.push(binding);
        self
    }

    /// Builder: add multiple wheel bindings
    pub fn with_wheel_bindings(mut self, bindings: impl IntoIterator<Item = WheelBind>) -> Self {
        self.wheel_bindings.extend(bindings);
        self
    }

    /// Builder: add a mouse modifier
    pub fn with_mouse_modifier(mut self, modifier: MouseModifier) -> Self {
        self.mouse_modifiers.push(modifier);
        self
    }

    /// Builder: add multiple mouse modifiers
    pub fn with_mouse_modifiers(
        mut self,
        modifiers: impl IntoIterator<Item = MouseModifier>,
    ) -> Self {
        self.mouse_modifiers.extend(modifiers);
        self
    }

    /// Builder: set version
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = version.into();
        self
    }

    /// Compose this preset with an action set, adding its bindings
    pub fn with_section<S: ActionSet>(mut self, section: S) -> Self {
        self.bindings.extend(section.keybinds());
        self.wheel_bindings.extend(section.wheel_binds());
        self.mouse_modifiers.extend(section.mouse_modifiers());
        self
    }
}

/// Trait for composable action sets (sections of keybindings)
pub trait ActionSet {
    /// Get the keybindings provided by this action set
    fn keybinds(&self) -> Vec<Keybind> {
        Vec::new()
    }

    /// Get the wheel bindings provided by this action set
    fn wheel_binds(&self) -> Vec<WheelBind> {
        Vec::new()
    }

    /// Get the mouse modifiers provided by this action set
    fn mouse_modifiers(&self) -> Vec<MouseModifier> {
        Vec::new()
    }

    /// Name of this action set for conflict reporting
    fn name(&self) -> &'static str;
}

/// A conflict detected during preset building
#[derive(Debug, Clone)]
pub struct BindingConflict {
    /// The key/binding that conflicts
    pub binding: String,
    /// Name of the first section that defined it
    pub first_section: String,
    /// Name of the conflicting section
    pub second_section: String,
    /// Type of conflict
    pub conflict_type: ConflictType,
}

/// Type of binding conflict
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConflictType {
    /// Two sections define the same keybind
    KeyConflict,
    /// Two sections define the same wheel binding
    WheelConflict,
    /// Two sections define the same mouse modifier
    MouseModifierConflict,
}

/// Builder for composing presets from action sets with conflict detection
pub struct PresetBuilder {
    name: String,
    description: String,
    version: String,
    bindings: Vec<(Keybind, &'static str)>, // (binding, section_name)
    wheel_bindings: Vec<(WheelBind, &'static str)>,
    mouse_modifiers: Vec<(MouseModifier, &'static str)>,
    conflicts: Vec<BindingConflict>,
    allow_override: bool,
}

impl PresetBuilder {
    /// Create a new preset builder
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            version: "1.0.0".to_string(),
            bindings: Vec::new(),
            wheel_bindings: Vec::new(),
            mouse_modifiers: Vec::new(),
            conflicts: Vec::new(),
            allow_override: false,
        }
    }

    /// Set the version
    pub fn version(mut self, version: impl Into<String>) -> Self {
        self.version = version.into();
        self
    }

    /// Allow later sections to override earlier ones (no conflict warnings)
    pub fn allow_override(mut self, allow: bool) -> Self {
        self.allow_override = allow;
        self
    }

    /// Add an action set section
    pub fn with_section<S: ActionSet>(mut self, section: S) -> Self {
        let section_name = section.name();

        // Check for keybind conflicts
        for keybind in section.keybinds() {
            let key = keybind.keys.to_lowercase();
            if !self.allow_override {
                if let Some((_, existing_section)) = self
                    .bindings
                    .iter()
                    .find(|(b, _)| b.keys.to_lowercase() == key)
                {
                    self.conflicts.push(BindingConflict {
                        binding: key.clone(),
                        first_section: (*existing_section).to_string(),
                        second_section: section_name.to_string(),
                        conflict_type: ConflictType::KeyConflict,
                    });
                }
            }
            self.bindings.push((keybind, section_name));
        }

        // Check for wheel binding conflicts
        for wheel in section.wheel_binds() {
            let key = format!(
                "{}:{}:{}",
                wheel.modifiers.to_lowercase(),
                wheel.horizontal,
                wheel.direction as u8
            );
            if !self.allow_override {
                if let Some((_, existing_section)) = self.wheel_bindings.iter().find(|(w, _)| {
                    format!(
                        "{}:{}:{}",
                        w.modifiers.to_lowercase(),
                        w.horizontal,
                        w.direction as u8
                    ) == key
                }) {
                    self.conflicts.push(BindingConflict {
                        binding: format!("wheel({})", wheel.modifiers),
                        first_section: (*existing_section).to_string(),
                        second_section: section_name.to_string(),
                        conflict_type: ConflictType::WheelConflict,
                    });
                }
            }
            self.wheel_bindings.push((wheel, section_name));
        }

        // Check for mouse modifier conflicts
        for modifier in section.mouse_modifiers() {
            let key = format!(
                "{:?}:{}",
                modifier.context,
                modifier.modifiers.to_lowercase()
            );
            if !self.allow_override {
                if let Some((_, existing_section)) = self
                    .mouse_modifiers
                    .iter()
                    .find(|(m, _)| format!("{:?}:{}", m.context, m.modifiers.to_lowercase()) == key)
                {
                    self.conflicts.push(BindingConflict {
                        binding: format!("{:?}({})", modifier.context, modifier.modifiers),
                        first_section: (*existing_section).to_string(),
                        second_section: section_name.to_string(),
                        conflict_type: ConflictType::MouseModifierConflict,
                    });
                }
            }
            self.mouse_modifiers.push((modifier, section_name));
        }

        self
    }

    /// Get any conflicts detected during building
    pub fn conflicts(&self) -> &[BindingConflict] {
        &self.conflicts
    }

    /// Check if there are any conflicts
    pub fn has_conflicts(&self) -> bool {
        !self.conflicts.is_empty()
    }

    /// Log any conflicts as warnings
    pub fn log_conflicts(&self) {
        for conflict in &self.conflicts {
            tracing::warn!(
                binding = %conflict.binding,
                first = %conflict.first_section,
                second = %conflict.second_section,
                conflict_type = ?conflict.conflict_type,
                "Keybind conflict detected"
            );
        }
    }

    /// Build the preset (later sections override earlier ones)
    pub fn build(self) -> KeybindPreset {
        // Log conflicts before building
        if self.has_conflicts() {
            self.log_conflicts();
        }

        // Build with last-wins semantics for conflicts
        let mut bindings_map: std::collections::HashMap<String, Keybind> =
            std::collections::HashMap::new();
        for (keybind, _) in self.bindings {
            let key = keybind.keys.to_lowercase();
            bindings_map.insert(key, keybind);
        }

        let mut wheel_map: std::collections::HashMap<String, WheelBind> =
            std::collections::HashMap::new();
        for (wheel, _) in self.wheel_bindings {
            let key = format!(
                "{}:{}:{}",
                wheel.modifiers.to_lowercase(),
                wheel.horizontal,
                wheel.direction as u8
            );
            wheel_map.insert(key, wheel);
        }

        let mut mouse_map: std::collections::HashMap<String, MouseModifier> =
            std::collections::HashMap::new();
        for (modifier, _) in self.mouse_modifiers {
            let key = format!(
                "{:?}:{}",
                modifier.context,
                modifier.modifiers.to_lowercase()
            );
            mouse_map.insert(key, modifier);
        }

        KeybindPreset {
            name: self.name,
            description: self.description,
            version: self.version,
            bindings: bindings_map.into_values().collect(),
            wheel_bindings: wheel_map.into_values().collect(),
            mouse_modifiers: mouse_map.into_values().collect(),
        }
    }
}

/// An override layer (stackable on top of base preset)
#[derive(Debug, Clone, Facet)]
pub struct KeybindOverride {
    /// Unique name for this override (e.g., "tempo_map", "recording")
    pub name: String,

    /// Human-readable description
    pub description: String,

    /// Priority for stacking (higher = checked first)
    pub priority: i32,

    /// Key bindings that override/add to base preset
    pub bindings: Vec<Keybind>,

    /// Wheel bindings for mouse wheel actions
    pub wheel_bindings: Vec<WheelBind>,
}

impl KeybindOverride {
    /// Create a new override layer
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            priority: 0,
            bindings: Vec::new(),
            wheel_bindings: Vec::new(),
        }
    }

    /// Builder: add a binding
    pub fn with_binding(mut self, binding: Keybind) -> Self {
        self.bindings.push(binding);
        self
    }

    /// Builder: add multiple bindings
    pub fn with_bindings(mut self, bindings: impl IntoIterator<Item = Keybind>) -> Self {
        self.bindings.extend(bindings);
        self
    }

    /// Builder: add a wheel binding
    pub fn with_wheel_binding(mut self, binding: WheelBind) -> Self {
        self.wheel_bindings.push(binding);
        self
    }

    /// Builder: add multiple wheel bindings
    pub fn with_wheel_bindings(mut self, bindings: impl IntoIterator<Item = WheelBind>) -> Self {
        self.wheel_bindings.extend(bindings);
        self
    }

    /// Builder: set priority
    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }
}

/// Get the global keybind resolver instance (lazily initialized with defaults)
pub fn get_resolver() -> &'static std::sync::RwLock<KeybindResolver> {
    static RESOLVER: std::sync::OnceLock<std::sync::RwLock<KeybindResolver>> =
        std::sync::OnceLock::new();
    RESOLVER.get_or_init(|| {
        let mut resolver = KeybindResolver::new();
        resolver.load_defaults();
        tracing::info!("Keybind resolver lazily initialized with defaults");
        std::sync::RwLock::new(resolver)
    })
}

/// Initialize the keybind system with defaults (explicit initialization)
/// Note: The resolver is also lazily initialized on first access via get_resolver()
pub fn init() {
    let _ = get_resolver(); // Trigger lazy initialization
    tracing::info!("Keybind system initialized");
}

/// Resolve a key sequence to an action in the current context
pub fn resolve(context: KeybindContext, keys: &str) -> Option<String> {
    let resolver = get_resolver().read().unwrap();
    resolver.resolve(context, keys).map(|s| s.to_string())
}

/// Enable an override layer by name
pub fn enable_override(name: &str) {
    let mut resolver = get_resolver().write().unwrap();
    resolver.enable_override(name);
}

/// Disable an override layer by name
pub fn disable_override(name: &str) {
    let mut resolver = get_resolver().write().unwrap();
    resolver.disable_override(name);
}

/// Toggle an override layer by name, returns new state
pub fn toggle_override(name: &str) -> bool {
    let mut resolver = get_resolver().write().unwrap();
    resolver.toggle_override(name)
}

/// Check if an override layer is active
pub fn is_override_active(name: &str) -> bool {
    let resolver = get_resolver().read().unwrap();
    resolver.is_override_active(name)
}

/// Disable all active override layers
pub fn disable_all_overrides() {
    let mut resolver = get_resolver().write().unwrap();
    resolver.disable_all_overrides();
}

/// Set the active base preset by name
pub fn set_preset(name: &str) -> bool {
    let mut resolver = get_resolver().write().unwrap();
    resolver.set_preset_by_name(name)
}

/// Get the name of the currently active preset
pub fn active_preset_name() -> String {
    let resolver = get_resolver().read().unwrap();
    resolver.active_preset_name().to_string()
}

/// Get names of all available presets
pub fn available_presets() -> Vec<String> {
    let resolver = get_resolver().read().unwrap();
    resolver.available_presets()
}

/// Get names of all available override layers
pub fn available_overrides() -> Vec<String> {
    let resolver = get_resolver().read().unwrap();
    resolver.available_overrides()
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
    context: KeybindContext,
    modifiers: &str,
    horizontal: bool,
    delta: i16,
) -> Option<String> {
    let resolver = get_resolver().read().unwrap();
    resolver
        .resolve_wheel(context, modifiers, horizontal, delta)
        .map(|s| s.to_string())
}

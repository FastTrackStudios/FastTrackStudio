//! Source types — sound source generators.
//!
//! A [`Source`] represents a sound generator: audio input, instrument plugin,
//! sampler, or internal generator. Sources feed into sections and layers
//! for processing.

use facet::Facet;
use uuid::Uuid;

use crate::block::PluginId;
use crate::normalized::{NormalizedF64, Pan};

// ─────────────────────────────────────────────────────────────────────────────
// SourceType
// ─────────────────────────────────────────────────────────────────────────────

/// The kind of sound generator backing a source.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum SourceType {
    /// A hardware audio input (mic, DI, line in).
    AudioInput { input_index: u8 } = 0,
    /// A virtual instrument plugin (synth, piano, etc.).
    Instrument { plugin_id: PluginId } = 1,
    /// A sample-based plugin (drum machine, sample player).
    Sampler { plugin_id: PluginId } = 2,
    /// An internal signal generator (test tone, noise, etc.).
    Generator = 3,
}

impl SourceType {
    /// Human-readable display name for this source type.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::AudioInput { .. } => "Audio Input",
            Self::Instrument { .. } => "Instrument",
            Self::Sampler { .. } => "Sampler",
            Self::Generator => "Generator",
        }
    }

    /// Whether this source is an audio input.
    pub fn is_audio_input(&self) -> bool {
        matches!(self, Self::AudioInput { .. })
    }

    /// Whether this source is backed by a plugin (instrument or sampler).
    pub fn is_plugin_based(&self) -> bool {
        matches!(self, Self::Instrument { .. } | Self::Sampler { .. })
    }

    /// Get the plugin ID if this source is plugin-based.
    pub fn plugin_id(&self) -> Option<&PluginId> {
        match self {
            Self::Instrument { plugin_id } | Self::Sampler { plugin_id } => Some(plugin_id),
            _ => None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Source
// ─────────────────────────────────────────────────────────────────────────────

/// A sound source generator.
///
/// Each source has a type (audio input, instrument, sampler, or generator),
/// an enable toggle, and level/pan controls.
#[derive(Debug, Clone, Facet)]
pub struct Source {
    pub id: Uuid,
    pub name: String,
    pub source_type: SourceType,
    pub enabled: bool,
    pub level: NormalizedF64,
    pub pan: Pan,
}

impl Source {
    /// Create a source backed by an audio input.
    pub fn audio_input(name: impl Into<String>, input_index: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::AudioInput { input_index },
            enabled: true,
            level: NormalizedF64::ONE,
            pan: Pan::CENTER,
        }
    }

    /// Create a source backed by an instrument plugin.
    pub fn instrument(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Instrument { plugin_id },
            enabled: true,
            level: NormalizedF64::ONE,
            pan: Pan::CENTER,
        }
    }

    /// Create a source backed by a sampler plugin.
    pub fn sampler(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Sampler { plugin_id },
            enabled: true,
            level: NormalizedF64::ONE,
            pan: Pan::CENTER,
        }
    }

    /// Create an internal generator source.
    pub fn generator(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Generator,
            enabled: true,
            level: NormalizedF64::ONE,
            pan: Pan::CENTER,
        }
    }

    /// Toggle the enabled state, returning the new state.
    pub fn toggle(&mut self) -> bool {
        self.enabled = !self.enabled;
        self.enabled
    }

    /// Set the output level.
    pub fn set_level(&mut self, level: NormalizedF64) {
        self.level = level;
    }

    /// Set the pan position.
    pub fn set_pan(&mut self, pan: Pan) {
        self.pan = pan;
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_plugin() -> PluginId {
        PluginId::vst3("com.example.synth", "Example Synth")
    }

    #[test]
    fn audio_input_creation() {
        let source = Source::audio_input("Guitar DI", 0);
        assert_eq!(source.name, "Guitar DI");
        assert!(source.source_type.is_audio_input());
        assert!(!source.source_type.is_plugin_based());
        assert!(source.source_type.plugin_id().is_none());
        assert_eq!(source.source_type.display_name(), "Audio Input");
        assert!(source.enabled);
        assert_eq!(source.level.get(), 1.0);
        assert_eq!(source.pan.get(), 0.0);
    }

    #[test]
    fn instrument_creation() {
        let source = Source::instrument("Lead Synth", test_plugin());
        assert_eq!(source.name, "Lead Synth");
        assert!(source.source_type.is_plugin_based());
        assert!(!source.source_type.is_audio_input());
        assert!(source.source_type.plugin_id().is_some());
        assert_eq!(source.source_type.display_name(), "Instrument");
    }

    #[test]
    fn sampler_creation() {
        let source = Source::sampler("Drum Kit", test_plugin());
        assert_eq!(source.name, "Drum Kit");
        assert!(source.source_type.is_plugin_based());
        assert!(source.source_type.plugin_id().is_some());
        assert_eq!(source.source_type.display_name(), "Sampler");
    }

    #[test]
    fn generator_creation() {
        let source = Source::generator("Test Tone");
        assert_eq!(source.name, "Test Tone");
        assert!(!source.source_type.is_audio_input());
        assert!(!source.source_type.is_plugin_based());
        assert!(source.source_type.plugin_id().is_none());
        assert_eq!(source.source_type.display_name(), "Generator");
    }

    #[test]
    fn toggle() {
        let mut source = Source::audio_input("Mic", 1);
        assert!(source.enabled);

        let new_state = source.toggle();
        assert!(!new_state);
        assert!(!source.enabled);

        let new_state = source.toggle();
        assert!(new_state);
        assert!(source.enabled);
    }

    #[test]
    fn set_level_and_pan() {
        let mut source = Source::audio_input("DI", 0);

        source.set_level(NormalizedF64::new(0.75));
        assert!((source.level.get() - 0.75).abs() < f64::EPSILON);

        source.set_pan(Pan::new(-0.5));
        assert!((source.pan.get() - (-0.5)).abs() < f64::EPSILON);
    }

}

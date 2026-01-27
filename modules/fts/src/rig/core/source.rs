//! Source - Sound source generators
//!
//! Sources represent everything that generates audio in the Sound Manager system.
//! They are the originators of audio that then flows through Modules (processing).

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::PluginId;

/// A sound source generator.
///
/// Sources produce audio - they are instrument inputs, synth engines,
/// sample players, etc. Everything that generates audio is a Source.
///
/// Sources are distinct from Modules: sources GENERATE audio, modules PROCESS it.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Source {
    /// Unique identifier
    pub id: Uuid,
    /// Display name (e.g., "Vocal Input", "Keys Synth", "Sample Player")
    pub name: String,
    /// Type of source
    pub source_type: SourceType,
    /// Whether this source is enabled
    pub enabled: bool,
    /// Output level (0.0 - 1.0)
    pub level: f64,
    /// Pan position (-1.0 to 1.0, 0.0 = center)
    pub pan: f64,
}

/// Type of sound source.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum SourceType {
    /// Audio input (mic, DI, line)
    ///
    /// The input_index refers to the physical input on the audio interface
    AudioInput { input_index: u8 } = 0,

    /// Virtual instrument / synthesizer engine
    ///
    /// A plugin-based instrument that generates audio algorithmically
    Instrument { plugin_id: PluginId } = 1,

    /// Sample player
    ///
    /// Plays back recorded audio samples
    Sampler { plugin_id: PluginId } = 2,

    /// Internal generator (test tone, noise, etc.)
    ///
    /// Useful for testing and diagnostics
    Generator = 3,
}

impl Source {
    /// Create a new audio input source
    pub fn audio_input(name: impl Into<String>, input_index: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::AudioInput { input_index },
            enabled: true,
            level: 1.0,
            pan: 0.0,
        }
    }

    /// Create a new instrument source
    pub fn instrument(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Instrument { plugin_id },
            enabled: true,
            level: 1.0,
            pan: 0.0,
        }
    }

    /// Create a new sampler source
    pub fn sampler(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Sampler { plugin_id },
            enabled: true,
            level: 1.0,
            pan: 0.0,
        }
    }

    /// Create a new generator source
    pub fn generator(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            source_type: SourceType::Generator,
            enabled: true,
            level: 1.0,
            pan: 0.0,
        }
    }

    /// Toggle this source on/off
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
    }

    /// Set the level (clamps to 0.0-1.0)
    pub fn set_level(&mut self, level: f64) {
        self.level = level.clamp(0.0, 1.0);
    }

    /// Set the pan (clamps to -1.0 to 1.0)
    pub fn set_pan(&mut self, pan: f64) {
        self.pan = pan.clamp(-1.0, 1.0);
    }
}

impl SourceType {
    /// Get the display name for this source type
    pub fn display_name(&self) -> &'static str {
        match self {
            SourceType::AudioInput { .. } => "Audio Input",
            SourceType::Instrument { .. } => "Instrument",
            SourceType::Sampler { .. } => "Sampler",
            SourceType::Generator => "Generator",
        }
    }

    /// Check if this is an audio input source
    pub fn is_audio_input(&self) -> bool {
        matches!(self, SourceType::AudioInput { .. })
    }

    /// Check if this is a plugin-based source
    pub fn is_plugin_based(&self) -> bool {
        matches!(self, SourceType::Instrument { .. } | SourceType::Sampler { .. })
    }

    /// Get the plugin ID if this is a plugin-based source
    pub fn plugin_id(&self) -> Option<&PluginId> {
        match self {
            SourceType::Instrument { plugin_id } | SourceType::Sampler { plugin_id } => {
                Some(plugin_id)
            }
            _ => None,
        }
    }
}

//! Layer - A slot within a section for a sound source
//!
//! Layers can hold patches (sound configurations) and have their own
//! local blocks and send levels to global effects.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::Block;

/// A slot within a section that can hold a sound source.
///
/// Each layer can have its own chain of local blocks, send levels to
/// global effects, and load different patches.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Layer {
    /// Unique identifier
    pub id: Uuid,
    /// Layer index within the section (0-based)
    pub index: u8,
    /// Layer name (e.g., "Layer 1", "Main")
    pub name: String,
    /// Local DSP blocks dedicated to this layer
    pub local_blocks: Vec<Block>,
    /// Send levels to global blocks
    pub global_sends: Vec<SendLevel>,
    /// Currently loaded patch ID
    pub current_patch_id: Option<Uuid>,
    /// Currently active patch variation ID
    pub current_variation_id: Option<Uuid>,
    /// Whether this layer is enabled
    pub enabled: bool,
    /// Layer volume (0.0 - 1.0)
    pub volume: f64,
    /// Layer pan (-1.0 to 1.0)
    pub pan: f64,
    /// Optional key range for splits
    pub key_range: Option<KeyRange>,
    /// Optional MIDI channel filter (None = omni)
    pub midi_channel: Option<u8>,
}

/// Send level from a layer to a global block.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SendLevel {
    /// ID of the global block to send to
    pub global_block_id: Uuid,
    /// Send level (0.0 - 1.0)
    pub level: f64,
    /// Whether this is a pre-fader send (before layer volume)
    pub pre_fader: bool,
}

/// Key range for keyboard splits.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct KeyRange {
    /// Lowest MIDI note (0-127)
    pub low: u8,
    /// Highest MIDI note (0-127)
    pub high: u8,
}

impl Layer {
    /// Create a new layer
    pub fn new(name: impl Into<String>, index: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            index,
            name: name.into(),
            local_blocks: Vec::new(),
            global_sends: Vec::new(),
            current_patch_id: None,
            current_variation_id: None,
            enabled: true,
            volume: 1.0,
            pan: 0.0,
            key_range: None,
            midi_channel: None,
        }
    }

    /// Add a local block to this layer
    pub fn add_block(&mut self, block: Block) {
        self.local_blocks.push(block);
    }

    /// Set a send level to a global block
    pub fn set_send(&mut self, global_block_id: Uuid, level: f64, pre_fader: bool) {
        if let Some(send) = self.global_sends.iter_mut().find(|s| s.global_block_id == global_block_id) {
            send.level = level;
            send.pre_fader = pre_fader;
        } else {
            self.global_sends.push(SendLevel {
                global_block_id,
                level,
                pre_fader,
            });
        }
    }

    /// Get send level to a global block
    pub fn get_send(&self, global_block_id: Uuid) -> Option<&SendLevel> {
        self.global_sends.iter().find(|s| s.global_block_id == global_block_id)
    }

    /// Set the key range for this layer
    pub fn set_key_range(&mut self, low: u8, high: u8) {
        self.key_range = Some(KeyRange {
            low: low.min(127),
            high: high.min(127),
        });
    }
}

impl KeyRange {
    /// Create a new key range
    pub fn new(low: u8, high: u8) -> Self {
        Self {
            low: low.min(127),
            high: high.min(127),
        }
    }

    /// Check if a MIDI note is within this range
    pub fn contains(&self, note: u8) -> bool {
        note >= self.low && note <= self.high
    }

    /// Full keyboard range (all 128 notes)
    pub fn full() -> Self {
        Self { low: 0, high: 127 }
    }
}

impl SendLevel {
    /// Create a new send level
    pub fn new(global_block_id: Uuid, level: f64) -> Self {
        Self {
            global_block_id,
            level: level.clamp(0.0, 1.0),
            pre_fader: false,
        }
    }

    /// Create a pre-fader send
    pub fn pre_fader(global_block_id: Uuid, level: f64) -> Self {
        Self {
            global_block_id,
            level: level.clamp(0.0, 1.0),
            pre_fader: true,
        }
    }
}

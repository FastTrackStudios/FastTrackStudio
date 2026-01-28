//! Layer — a slot within a section for a sound source.
//!
//! Layers hold local blocks, send levels to global effects, and can
//! optionally be key-split across the MIDI range.

use facet::Facet;

use crate::block::Block;
use crate::id::{BlockId, LayerId, PatchId, VariationId};
use crate::normalized::{MidiNote, NormalizedF64, Pan};

// ─────────────────────────────────────────────────────────────────────────────
// KeyRange — enforces low ≤ high
// ─────────────────────────────────────────────────────────────────────────────

/// MIDI key range with the invariant that `low <= high`.
///
/// Construction returns `None` if the range is inverted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub struct KeyRange {
    low: MidiNote,
    high: MidiNote,
}

impl KeyRange {
    /// Create a key range. Returns `None` if `low > high`.
    pub fn new(low: MidiNote, high: MidiNote) -> Option<Self> {
        if low.get() <= high.get() {
            Some(Self { low, high })
        } else {
            None
        }
    }

    /// Full keyboard range: MIDI notes 0–127.
    pub fn full() -> Self {
        Self {
            low: MidiNote::MIN,
            high: MidiNote::MAX,
        }
    }

    pub fn low(&self) -> MidiNote {
        self.low
    }

    pub fn high(&self) -> MidiNote {
        self.high
    }

    /// Check if a MIDI note falls within this range (inclusive).
    pub fn contains(&self, note: MidiNote) -> bool {
        note.get() >= self.low.get() && note.get() <= self.high.get()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SendLevel — send from a layer to a global block
// ─────────────────────────────────────────────────────────────────────────────

/// Send level from a layer to a global block.
#[derive(Debug, Clone, Facet)]
pub struct SendLevel {
    /// ID of the global block to send to
    pub global_block_id: BlockId,
    /// Send level (0.0–1.0)
    pub level: NormalizedF64,
    /// Whether this is a pre-fader send (before layer volume)
    pub pre_fader: bool,
}

impl SendLevel {
    /// Post-fader send at the given level.
    pub fn new(global_block_id: BlockId, level: NormalizedF64) -> Self {
        Self {
            global_block_id,
            level,
            pre_fader: false,
        }
    }

    /// Pre-fader send at the given level.
    pub fn pre_fader(global_block_id: BlockId, level: NormalizedF64) -> Self {
        Self {
            global_block_id,
            level,
            pre_fader: true,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Layer
// ─────────────────────────────────────────────────────────────────────────────

/// A slot within a section that can hold a sound source.
#[derive(Debug, Clone, Facet)]
pub struct Layer {
    pub id: LayerId,
    /// Layer index within the section (0-based)
    pub index: u8,
    pub name: String,
    /// Local DSP blocks dedicated to this layer
    pub local_blocks: Vec<Block>,
    /// Send levels to global blocks
    pub global_sends: Vec<SendLevel>,
    /// Currently loaded patch ID
    pub current_patch_id: Option<PatchId>,
    /// Currently active patch variation ID
    pub current_variation_id: Option<VariationId>,
    pub enabled: bool,
    pub volume: NormalizedF64,
    pub pan: Pan,
    /// Optional key range for keyboard splits
    pub key_range: Option<KeyRange>,
    /// Optional MIDI channel filter (None = omni)
    pub midi_channel: Option<u8>,
}

impl Layer {
    pub fn new(name: impl Into<String>, index: u8) -> Self {
        Self {
            id: LayerId::new(),
            index,
            name: name.into(),
            local_blocks: Vec::new(),
            global_sends: Vec::new(),
            current_patch_id: None,
            current_variation_id: None,
            enabled: true,
            volume: NormalizedF64::ONE,
            pan: Pan::CENTER,
            key_range: None,
            midi_channel: None,
        }
    }

    pub fn add_block(&mut self, block: Block) {
        self.local_blocks.push(block);
    }

    pub fn set_send(&mut self, global_block_id: BlockId, level: NormalizedF64, pre_fader: bool) {
        if let Some(send) = self
            .global_sends
            .iter_mut()
            .find(|s| s.global_block_id == global_block_id)
        {
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

    pub fn get_send(&self, global_block_id: BlockId) -> Option<&SendLevel> {
        self.global_sends
            .iter()
            .find(|s| s.global_block_id == global_block_id)
    }

    pub fn set_key_range(&mut self, range: KeyRange) {
        self.key_range = Some(range);
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn key_range_valid() {
        let range = KeyRange::new(MidiNote::new(36), MidiNote::new(84));
        assert!(range.is_some());
        let range = range.unwrap();
        assert_eq!(range.low().get(), 36);
        assert_eq!(range.high().get(), 84);
    }

    #[test]
    fn key_range_equal_bounds_valid() {
        let range = KeyRange::new(MidiNote::new(60), MidiNote::new(60));
        assert!(range.is_some());
    }

    #[test]
    fn key_range_inverted_returns_none() {
        let range = KeyRange::new(MidiNote::new(100), MidiNote::new(50));
        assert!(range.is_none());
    }

    #[test]
    fn key_range_contains() {
        let range = KeyRange::new(MidiNote::new(36), MidiNote::new(84)).unwrap();
        assert!(range.contains(MidiNote::new(36)));
        assert!(range.contains(MidiNote::new(60)));
        assert!(range.contains(MidiNote::new(84)));
        assert!(!range.contains(MidiNote::new(35)));
        assert!(!range.contains(MidiNote::new(85)));
    }

    #[test]
    fn key_range_full() {
        let range = KeyRange::full();
        assert_eq!(range.low().get(), 0);
        assert_eq!(range.high().get(), 127);
        assert!(range.contains(MidiNote::new(0)));
        assert!(range.contains(MidiNote::new(127)));
    }

    #[test]
    fn layer_defaults() {
        let layer = Layer::new("Main", 0);
        assert!(layer.enabled);
        assert_eq!(layer.volume.get(), 1.0);
        assert_eq!(layer.pan.get(), 0.0);
        assert!(layer.key_range.is_none());
        assert!(layer.local_blocks.is_empty());
    }

    #[test]
    fn layer_send_upsert() {
        let mut layer = Layer::new("Main", 0);
        let block_id = BlockId::new();

        // Add new send
        layer.set_send(block_id, NormalizedF64::new(0.5), false);
        assert_eq!(layer.global_sends.len(), 1);
        assert_eq!(layer.get_send(block_id).unwrap().level.get(), 0.5);

        // Update existing send
        layer.set_send(block_id, NormalizedF64::new(0.8), true);
        assert_eq!(layer.global_sends.len(), 1);
        assert_eq!(layer.get_send(block_id).unwrap().level.get(), 0.8);
        assert!(layer.get_send(block_id).unwrap().pre_fader);
    }
}

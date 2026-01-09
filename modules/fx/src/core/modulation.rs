//! Modulation source trait and routing.
//!
//! This module defines the interface for modulation sources (LFOs, envelopes, etc.)
//! and how they connect to parameters.
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::core::{ModSource, ModSlot, ModAmount};
//! use fts_fx::primitives::Lfo;
//!
//! // LFO implements ModSource
//! let lfo = Lfo::new(44100.0, 2.0, LfoWaveform::Sine);
//!
//! // Create a modulation slot with 50% depth
//! let slot = ModSlot::new(Box::new(lfo), ModAmount::bipolar(0.5));
//! ```

/// Trait for modulation sources.
///
/// Implemented by LFO, ADSR, envelope followers, and any other
/// source that can modulate parameters.
pub trait ModSource: Send {
    /// Generate the next modulation value.
    ///
    /// Returns a value in the range -1.0 to 1.0 (bipolar) or 0.0 to 1.0 (unipolar),
    /// depending on the source type.
    fn next(&mut self) -> f32;

    /// Get the current value without advancing.
    fn current(&self) -> f32;

    /// Reset the modulation source to its initial state.
    fn reset(&mut self);

    /// Update the sample rate.
    fn set_sample_rate(&mut self, sample_rate: f32);

    /// Whether this source outputs bipolar (-1 to 1) or unipolar (0 to 1) values.
    fn is_bipolar(&self) -> bool {
        true // Most sources are bipolar by default
    }
}

/// Boxed modulation source for dynamic dispatch.
pub type ModSourceBox = Box<dyn ModSource>;

/// Modulation amount/depth configuration.
#[derive(Clone, Copy, Debug)]
pub struct ModAmount {
    /// Modulation depth (-1.0 to 1.0 for bipolar, 0.0 to 1.0 for unipolar destinations)
    pub depth: f32,
    /// Whether the modulation is bipolar (centered around current value)
    /// or unipolar (adds on top of current value)
    pub bipolar: bool,
}

impl ModAmount {
    /// Create a bipolar modulation amount.
    ///
    /// Bipolar modulation swings both above and below the base value.
    /// A depth of 1.0 means full range modulation.
    #[must_use]
    pub fn bipolar(depth: f32) -> Self {
        Self {
            depth,
            bipolar: true,
        }
    }

    /// Create a unipolar modulation amount.
    ///
    /// Unipolar modulation only adds to (or subtracts from) the base value.
    /// Useful for things like envelope-controlled filter cutoff.
    #[must_use]
    pub fn unipolar(depth: f32) -> Self {
        Self {
            depth,
            bipolar: false,
        }
    }

    /// Apply this modulation amount to a source value.
    ///
    /// # Arguments
    /// * `source_value` - The raw modulation source output (-1 to 1 or 0 to 1)
    /// * `source_is_bipolar` - Whether the source outputs bipolar values
    ///
    /// # Returns
    /// The scaled modulation value to add to the base parameter value (normalized 0-1)
    #[must_use]
    pub fn apply(&self, source_value: f32, source_is_bipolar: bool) -> f32 {
        let normalized = if source_is_bipolar && !self.bipolar {
            // Convert bipolar source to unipolar for unipolar destination
            (source_value + 1.0) * 0.5
        } else if !source_is_bipolar && self.bipolar {
            // Convert unipolar source to bipolar for bipolar destination
            source_value * 2.0 - 1.0
        } else {
            source_value
        };

        normalized * self.depth
    }
}

impl Default for ModAmount {
    fn default() -> Self {
        Self::bipolar(0.0)
    }
}

/// A modulation slot connecting a source to a parameter.
pub struct ModSlot {
    source: ModSourceBox,
    amount: ModAmount,
    enabled: bool,
}

impl ModSlot {
    /// Create a new modulation slot.
    #[must_use]
    pub fn new(source: ModSourceBox, amount: ModAmount) -> Self {
        Self {
            source,
            amount,
            enabled: true,
        }
    }

    /// Create a bypassed modulation slot.
    #[must_use]
    pub fn bypassed(source: ModSourceBox, amount: ModAmount) -> Self {
        Self {
            source,
            amount,
            enabled: false,
        }
    }

    /// Set the modulation amount.
    pub fn set_amount(&mut self, amount: ModAmount) {
        self.amount = amount;
    }

    /// Set just the depth.
    pub fn set_depth(&mut self, depth: f32) {
        self.amount.depth = depth;
    }

    /// Enable or disable this slot.
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Check if the slot is enabled.
    #[must_use]
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Get the modulation amount.
    #[must_use]
    pub fn amount(&self) -> ModAmount {
        self.amount
    }

    /// Get a reference to the source.
    #[must_use]
    pub fn source(&self) -> &dyn ModSource {
        self.source.as_ref()
    }

    /// Get a mutable reference to the source.
    pub fn source_mut(&mut self) -> &mut dyn ModSource {
        self.source.as_mut()
    }

    /// Generate the next modulation value (scaled by amount).
    ///
    /// Returns the value to add to the normalized parameter (0-1 range).
    #[must_use]
    pub fn next(&mut self) -> f32 {
        if !self.enabled {
            return 0.0;
        }
        let raw = self.source.next();
        self.amount.apply(raw, self.source.is_bipolar())
    }

    /// Get the current modulation value without advancing.
    #[must_use]
    pub fn current(&self) -> f32 {
        if !self.enabled {
            return 0.0;
        }
        let raw = self.source.current();
        self.amount.apply(raw, self.source.is_bipolar())
    }

    /// Reset the modulation source.
    pub fn reset(&mut self) {
        self.source.reset();
    }

    /// Update the sample rate.
    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        self.source.set_sample_rate(sample_rate);
    }
}

/// A collection of modulation slots for a single parameter.
///
/// Allows multiple sources to modulate the same parameter,
/// with their contributions summed together.
#[derive(Default)]
pub struct ModSlots {
    slots: Vec<ModSlot>,
}

impl ModSlots {
    /// Create an empty modulation slot collection.
    #[must_use]
    pub fn new() -> Self {
        Self { slots: Vec::new() }
    }

    /// Add a modulation slot.
    pub fn add(&mut self, slot: ModSlot) {
        self.slots.push(slot);
    }

    /// Remove all modulation slots.
    pub fn clear(&mut self) {
        self.slots.clear();
    }

    /// Get the number of slots.
    #[must_use]
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Check if there are no slots.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Get the sum of all modulation values.
    ///
    /// Call this once per sample and add to the normalized parameter value.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        self.slots.iter_mut().map(ModSlot::next).sum()
    }

    /// Get the current sum without advancing.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.slots.iter().map(ModSlot::current).sum()
    }

    /// Reset all modulation sources.
    pub fn reset(&mut self) {
        for slot in &mut self.slots {
            slot.reset();
        }
    }

    /// Update sample rate for all sources.
    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        for slot in &mut self.slots {
            slot.set_sample_rate(sample_rate);
        }
    }

    /// Iterate over slots.
    pub fn iter(&self) -> impl Iterator<Item = &ModSlot> {
        self.slots.iter()
    }

    /// Iterate over slots mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ModSlot> {
        self.slots.iter_mut()
    }
}

//! Refined numeric types — make out-of-range values unrepresentable.
//!
//! Each type clamps on construction so the inner value is always valid.
//! Private inner fields prevent direct mutation.

// ─────────────────────────────────────────────────────────────────────────────
// NormalizedF64: [0.0, 1.0]
// ─────────────────────────────────────────────────────────────────────────────

/// A floating-point value clamped to the range [0.0, 1.0].
///
/// Used for volume, send levels, parameter values, and any 0–1 normalized quantity.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, ::facet::Facet)]
pub struct NormalizedF64(f64);

impl NormalizedF64 {
    pub const ZERO: Self = Self(0.0);
    pub const HALF: Self = Self(0.5);
    pub const ONE: Self = Self(1.0);

    /// Create a new normalized value, clamping to [0.0, 1.0].
    pub fn new(value: f64) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    /// Get the inner value. Always in [0.0, 1.0].
    pub fn get(self) -> f64 {
        self.0
    }
}

impl Default for NormalizedF64 {
    fn default() -> Self {
        Self::ZERO
    }
}

impl std::fmt::Display for NormalizedF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.2}", self.0)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Pan: [-1.0, 1.0]
// ─────────────────────────────────────────────────────────────────────────────

/// A pan value clamped to [-1.0, 1.0].
///
/// -1.0 = hard left, 0.0 = center, 1.0 = hard right.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, ::facet::Facet)]
pub struct Pan(f64);

impl Pan {
    pub const LEFT: Self = Self(-1.0);
    pub const CENTER: Self = Self(0.0);
    pub const RIGHT: Self = Self(1.0);

    /// Create a new pan value, clamping to [-1.0, 1.0].
    pub fn new(value: f64) -> Self {
        Self(value.clamp(-1.0, 1.0))
    }

    /// Get the inner value. Always in [-1.0, 1.0].
    pub fn get(self) -> f64 {
        self.0
    }
}

impl Default for Pan {
    fn default() -> Self {
        Self::CENTER
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Rating: [0, 5]
// ─────────────────────────────────────────────────────────────────────────────

/// A star rating clamped to [0, 5].
///
/// 0 = unrated, 1–5 = star rating.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet)]
pub struct Rating(u8);

impl Rating {
    pub const UNRATED: Self = Self(0);

    /// Create a new rating, clamping to [0, 5].
    pub fn new(value: u8) -> Self {
        Self(value.min(5))
    }

    /// Get the inner value. Always in [0, 5].
    pub fn get(self) -> u8 {
        self.0
    }

    /// Whether a rating has been given (non-zero).
    pub fn is_rated(self) -> bool {
        self.0 > 0
    }
}

impl Default for Rating {
    fn default() -> Self {
        Self::UNRATED
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiNote: [0, 127]
// ─────────────────────────────────────────────────────────────────────────────

/// A MIDI note number clamped to [0, 127].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet)]
pub struct MidiNote(u8);

impl MidiNote {
    pub const MIN: Self = Self(0);
    pub const MAX: Self = Self(127);
    pub const MIDDLE_C: Self = Self(60);

    /// Create a new MIDI note, clamping to [0, 127].
    pub fn new(value: u8) -> Self {
        Self(value.min(127))
    }

    /// Get the inner value. Always in [0, 127].
    pub fn get(self) -> u8 {
        self.0
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiChannel: [1, 16]
// ─────────────────────────────────────────────────────────────────────────────

/// A MIDI channel clamped to [1, 16].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet)]
pub struct MidiChannel(u8);

impl MidiChannel {
    /// Create a new MIDI channel, clamping to [1, 16].
    pub fn new(value: u8) -> Self {
        Self(value.clamp(1, 16))
    }

    /// Get the inner value. Always in [1, 16].
    pub fn get(self) -> u8 {
        self.0
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Order: signal chain position
// ─────────────────────────────────────────────────────────────────────────────

/// Signal chain position. Lower values are earlier in the chain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::facet::Facet, Default)]
pub struct Order(u8);

impl Order {
    /// Create a new order value.
    pub fn new(value: u8) -> Self {
        Self(value)
    }

    /// Get the inner value.
    pub fn get(self) -> u8 {
        self.0
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // NormalizedF64

    #[test]
    fn normalized_clamps_above_one() {
        assert_eq!(NormalizedF64::new(2.0).get(), 1.0);
    }

    #[test]
    fn normalized_clamps_below_zero() {
        assert_eq!(NormalizedF64::new(-0.5).get(), 0.0);
    }

    #[test]
    fn normalized_preserves_valid() {
        assert!((NormalizedF64::new(0.75).get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn normalized_constants() {
        assert_eq!(NormalizedF64::ZERO.get(), 0.0);
        assert_eq!(NormalizedF64::HALF.get(), 0.5);
        assert_eq!(NormalizedF64::ONE.get(), 1.0);
    }

    #[test]
    fn normalized_default_is_zero() {
        assert_eq!(NormalizedF64::default().get(), 0.0);
    }

    // Pan

    #[test]
    fn pan_clamps_range() {
        assert_eq!(Pan::new(-5.0).get(), -1.0);
        assert_eq!(Pan::new(5.0).get(), 1.0);
    }

    #[test]
    fn pan_preserves_valid() {
        assert!((Pan::new(-0.3).get() - (-0.3)).abs() < f64::EPSILON);
    }

    #[test]
    fn pan_default_is_center() {
        assert_eq!(Pan::default().get(), 0.0);
    }

    // Rating

    #[test]
    fn rating_clamps_above_five() {
        assert_eq!(Rating::new(10).get(), 5);
        assert_eq!(Rating::new(255).get(), 5);
    }

    #[test]
    fn rating_preserves_valid() {
        assert_eq!(Rating::new(3).get(), 3);
    }

    #[test]
    fn rating_unrated() {
        assert!(!Rating::UNRATED.is_rated());
        assert!(Rating::new(1).is_rated());
    }

    // MidiNote

    #[test]
    fn midi_note_clamps() {
        assert_eq!(MidiNote::new(200).get(), 127);
    }

    #[test]
    fn midi_note_preserves_valid() {
        assert_eq!(MidiNote::new(60).get(), 60);
    }

    // MidiChannel

    #[test]
    fn midi_channel_clamps() {
        assert_eq!(MidiChannel::new(0).get(), 1);
        assert_eq!(MidiChannel::new(20).get(), 16);
    }

    #[test]
    fn midi_channel_preserves_valid() {
        assert_eq!(MidiChannel::new(10).get(), 10);
    }

    // Order

    #[test]
    fn order_default() {
        assert_eq!(Order::default().get(), 0);
    }
}

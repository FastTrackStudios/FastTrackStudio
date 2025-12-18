pub mod naming;
pub mod template;

/// Snare instrument
pub struct Snare {}

impl Snare {
    /// Create a new Snare instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Snare {
    fn default() -> Self {
        Self::new()
    }
}

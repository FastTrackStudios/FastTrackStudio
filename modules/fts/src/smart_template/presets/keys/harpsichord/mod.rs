pub mod naming;
pub mod template;

/// Harpsichord instrument
pub struct Harpsichord {}

impl Harpsichord {
    /// Create a new Harpsichord instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Harpsichord {
    fn default() -> Self {
        Self::new()
    }
}

pub mod naming;
pub mod template;

/// Clavichord instrument
pub struct Clavichord {}

impl Clavichord {
    /// Create a new Clavichord instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Clavichord {
    fn default() -> Self {
        Self::new()
    }
}

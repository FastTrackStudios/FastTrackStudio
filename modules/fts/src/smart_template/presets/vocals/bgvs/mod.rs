pub mod naming;
pub mod template;

/// Backing Vocals instrument
pub struct BGVs {}

impl BGVs {
    /// Create a new BGVs instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for BGVs {
    fn default() -> Self {
        Self::new()
    }
}

pub mod naming;
pub mod template;

/// Bass instrument
pub struct Bass {}

impl Bass {
    /// Create a new Bass instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Bass {
    fn default() -> Self {
        Self::new()
    }
}

pub mod naming;
pub mod template;

/// Kick instrument
pub struct Kick {}

impl Kick {
    /// Create a new Kick instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Kick {
    fn default() -> Self {
        Self::new()
    }
}

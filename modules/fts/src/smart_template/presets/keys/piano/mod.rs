pub mod naming;
pub mod template;

/// Piano instrument
pub struct Piano {}

impl Piano {
    /// Create a new Piano instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Piano {
    fn default() -> Self {
        Self::new()
    }
}

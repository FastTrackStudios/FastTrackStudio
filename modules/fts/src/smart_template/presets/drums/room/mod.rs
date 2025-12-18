pub mod naming;
pub mod template;

pub struct Room {}

impl Room {
    /// Create a new Room instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Room {
    fn default() -> Self {
        Self::new()
    }
}

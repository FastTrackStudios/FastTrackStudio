pub mod naming;
pub mod template;
pub mod tom_mapper;

pub use tom_mapper::TomMapper;

/// Tom instrument
pub struct Tom {}

impl Tom {
    /// Create a new Tom instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Tom {
    fn default() -> Self {
        Self::new()
    }
}

pub mod bgvs;
pub mod naming;
pub mod template;

pub use bgvs::BGVs;

/// Vocals instrument
pub struct Vocals {}

impl Vocals {
    /// Create a new Vocals instrument with default config and template
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Vocals {
    fn default() -> Self {
        Self::new()
    }
}

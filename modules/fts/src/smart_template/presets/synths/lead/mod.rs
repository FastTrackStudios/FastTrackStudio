pub mod template;
pub mod naming;

pub use naming::*;
pub use template::*;

/// Synth Lead instrument
pub struct Lead {}

impl Lead {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Lead {
    fn default() -> Self {
        Self::new()
    }
}

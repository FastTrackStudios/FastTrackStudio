pub mod template;
pub mod naming;

pub use naming::*;
pub use template::*;

/// Synth Pad instrument
pub struct Pad {}

impl Pad {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Pad {
    fn default() -> Self {
        Self::new()
    }
}

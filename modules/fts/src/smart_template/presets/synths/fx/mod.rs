pub mod template;
pub mod naming;

pub use naming::*;
pub use template::*;

/// Synth FX instrument
pub struct FX {}

impl FX {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for FX {
    fn default() -> Self {
        Self::new()
    }
}

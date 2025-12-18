pub mod template;
pub mod naming;

pub use naming::*;
pub use template::*;

/// Synth Chord instrument
pub struct Chord {}

impl Chord {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Chord {
    fn default() -> Self {
        Self::new()
    }
}

pub mod template;
pub mod naming;

pub use naming::*;
pub use template::*;

/// Synth Arp instrument
pub struct Arp {}

impl Arp {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Arp {
    fn default() -> Self {
        Self::new()
    }
}

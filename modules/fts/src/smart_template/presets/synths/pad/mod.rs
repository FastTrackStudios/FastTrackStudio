pub mod template;
pub mod naming;


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

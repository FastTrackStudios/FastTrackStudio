pub mod template;
pub mod naming;


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

pub mod template;
pub mod naming;


/// Synth Keys instrument
pub struct Keys {}

impl Keys {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Keys {
    fn default() -> Self {
        Self::new()
    }
}

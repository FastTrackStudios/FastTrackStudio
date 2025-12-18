pub mod template;
pub mod naming;

/// Organ instrument
pub struct Organ {}

impl Organ {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Organ {
    fn default() -> Self {
        Self::new()
    }
}

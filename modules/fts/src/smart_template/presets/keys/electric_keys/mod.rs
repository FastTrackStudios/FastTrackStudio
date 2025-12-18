pub mod template;
pub mod naming;

/// Electric Keys instrument
pub struct ElectricKeys {}

impl ElectricKeys {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for ElectricKeys {
    fn default() -> Self {
        Self::new()
    }
}

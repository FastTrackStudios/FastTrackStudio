use crate::smart_template::core::models::organization::OrganizationMode;

pub mod naming;
pub mod template;


/// Guitar Electric instrument
pub struct GuitarElectric {
    pub mode: OrganizationMode,
}

impl GuitarElectric {
    /// Create a new Guitar Electric instrument
    pub fn new() -> Self {
        Self::with_mode(OrganizationMode::ByPerformer)
    }

    /// Create a new Guitar Electric instrument with a specific organization mode
    pub fn with_mode(mode: OrganizationMode) -> Self {
        Self { mode }
    }
}

impl Default for GuitarElectric {
    fn default() -> Self {
        Self::new()
    }
}


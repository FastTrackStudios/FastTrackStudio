use crate::smart_template::core::models::organization::OrganizationMode;

pub mod naming;
pub mod template;


/// Guitar Acoustic instrument
pub struct GuitarAcoustic {
    pub mode: OrganizationMode,
}

impl GuitarAcoustic {
    /// Create a new Guitar Acoustic instrument
    pub fn new() -> Self {
        Self::with_mode(OrganizationMode::ByPerformer)
    }

    /// Create a new Guitar Acoustic instrument with a specific organization mode
    pub fn with_mode(mode: OrganizationMode) -> Self {
        Self { mode }
    }
}

impl Default for GuitarAcoustic {
    fn default() -> Self {
        Self::new()
    }
}

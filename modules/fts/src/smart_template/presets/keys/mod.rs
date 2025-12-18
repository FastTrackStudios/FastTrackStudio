pub mod piano;
pub mod electric_keys;
pub mod organ;
pub mod harpsichord;
pub mod clavichord;
pub mod naming;
pub mod template;

pub use piano::Piano;
pub use electric_keys::ElectricKeys;
pub use organ::Organ;
pub use harpsichord::Harpsichord;
pub use clavichord::Clavichord;
pub use naming::*;
pub use template::*;

/// Keys instrument consolidated struct
pub struct Keys {}

impl Keys {
    /// Create a new Keys instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Keys {
    fn default() -> Self {
        Self::new()
    }
}

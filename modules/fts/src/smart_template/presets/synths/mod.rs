pub mod lead;
pub mod chord;
pub mod pad;
pub mod arp;
pub mod fx;
pub mod keys;
pub mod naming;
pub mod template;

pub use lead::Lead;
pub use chord::Chord;
pub use pad::Pad;
pub use arp::Arp;
pub use fx::FX;
pub use keys::Keys;

/// Synths instrument consolidated struct
pub struct Synths {}

impl Synths {
    /// Create a new Synths instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for Synths {
    fn default() -> Self {
        Self::new()
    }
}

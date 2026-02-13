//! Markers and Regions Action Sets
//!
//! Keeps marker/region workflow bindings in a dedicated section.

use crate::input::keybinds::{ActionSet, Keybind};

/// FastTrackStudio markers/regions bindings
pub struct FtsMarkersRegions;

impl ActionSet for FtsMarkersRegions {
    fn name(&self) -> &'static str {
        "FtsMarkersRegions"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            Keybind::new("<S-r>", "40306")
                .with_description("Insert region from time selection and edit"),
            Keybind::new("<S-m>", "40171")
                .with_description("Insert and/or edit marker at current position"),
        ]
    }
}


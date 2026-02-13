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
            Keybind::new("<S-r>", "FTS_DAW_MARKERS_REGIONS_INSERT_REGION_AND_EDIT")
                .with_description("FTS: insert region from time selection and edit"),
            Keybind::new("<S-m>", "FTS_DAW_MARKERS_REGIONS_INSERT_MARKER_AND_EDIT")
                .with_description("FTS: insert marker at cursor and edit"),
        ]
    }
}

//! Distortion effect group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Distortion effect group
pub struct Distortion;

impl From<Distortion> for Group<ItemMetadata> {
    fn from(_val: Distortion) -> Self {
        Group::builder("Distortion")
            .patterns(vec![
                "distortion",
                "dist_",
                "overdrive",
                "drive_",
                "fuzz",
                "saturation",
                "saturator",
                "bitcrusher",
                "bitcrush",
                "waveshaper",
                "tube_",
                "tape_",
                "analog_",
                "crunch",
                "grit",
                "dirt",
            ])
            // Exclude single letters to avoid conflicts with channels
            .exclude(vec!["L", "C", "R", "l", "c", "r"])
            .build()
    }
}

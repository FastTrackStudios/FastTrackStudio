//! EQ effect group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// EQ effect group
pub struct EQ;

impl From<EQ> for Group<ItemMetadata> {
    fn from(_val: EQ) -> Self {
        Group::builder("EQ")
            .patterns(vec![
                "equalizer",
                "eq_",
                "parametric",
                "graphic",
                "filter",
                "highpass",
                "high_pass",
                "lowpass",
                "low_pass",
                "bandpass",
                "band_pass",
                "notch",
                "shelf",
                "bell",
            ])
            // Exclude single letters to avoid conflicts with channels
            .exclude(vec!["L", "C", "R", "l", "c", "r"])
            .build()
    }
}

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
            .priority(-1000) // Very low priority - only match when nothing else matches
            .build()
    }
}

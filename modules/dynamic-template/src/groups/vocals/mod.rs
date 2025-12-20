//! Vocal group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod background_vocals;
pub mod lead_vocals;

pub use background_vocals::BackgroundVocals;
pub use lead_vocals::LeadVocals;

/// Top-level vocals group containing lead and background vocals
/// This is transparent so LeadVocals and BackgroundVocals appear at top level
pub struct Vocals;

impl From<Vocals> for Group<ItemMetadata> {
    fn from(_val: Vocals) -> Self {
        Group::builder("Vocals")
            .prefix("Vox")
            .patterns(vec!["vocal", "vocals", "vox", "voice"])
            // Make transparent so Lead Vocals and Background Vocals appear at top level
            .transparent()
            .group(LeadVocals)
            .group(BackgroundVocals)
            .build()
    }
}

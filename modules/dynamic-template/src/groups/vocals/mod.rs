//! Vocal group definitions

use crate::item_metadata::prelude::*;

pub mod background_vocals;
pub mod lead_vocals;

pub use background_vocals::BackgroundVocals;
pub use lead_vocals::LeadVocals;

/// Top-level vocals group containing lead and background vocals
/// This is transparent so Lead Vocals and BGVs appear at top level
pub struct Vocals;

impl From<Vocals> for ItemMetadataGroup {
    fn from(_val: Vocals) -> Self {
        ItemMetadataGroup::builder("Vocals")
            .prefix("Vox")
            .patterns(["vocal", "vocals", "vox", "voc", "voca", "voice"])
            // Exclude non-vocal voice effects (these should go to SFX)
            .exclude(["robot", "vocoder", "talkbox"])
            // Make transparent so Lead Vocals and BGVs appear at top level
            .transparent()
            .group(LeadVocals)
            .group(BackgroundVocals)
            .build()
    }
}

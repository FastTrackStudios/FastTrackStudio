//! Sound Effects (SFX) group definitions
//!
//! This group captures non-musical audio elements like:
//! - Sound effects (whoosh, impact, etc.)
//! - Voice effects (robot, processed, etc.)
//! - Foley and ambient sounds
//! - Count-ins, clicks, and cue tracks
//! - Generic "FX" tracks that aren't tied to a specific instrument

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Top-level SFX group containing sound effects and non-musical audio
pub struct SFX;

impl From<SFX> for Group<ItemMetadata> {
    fn from(_val: SFX) -> Self {
        Group::builder("SFX")
            .prefix("SFX")
            .patterns(vec![
                // Generic FX patterns
                "fx",
                "sfx",
                "effect",
                "effects",
                // Sound design
                "whoosh",
                "impact",
                "riser",
                "sweep",
                "hit",
                "boom",
                "explosion",
                "transition",
                // Voice effects
                "robot",
                "vocoder",
                "talkbox",
                // Foley and ambient
                "foley",
                "ambient",
                "atmo",
                "atmosphere",
                "room tone",
                "noise",
                // Cue tracks
                "count",
                "click",
                "cue",
                "slate",
                "beep",
                "tone",
            ])
            .build()
    }
}

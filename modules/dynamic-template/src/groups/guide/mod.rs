//! Guide track group definitions
//!
//! This group captures timing and cue tracks for performers:
//! - Click tracks (metronome)
//! - Count-ins
//! - Cue tracks for performers' in-ears

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Top-level Guide group containing timing and cue tracks
pub struct Guide;

impl From<Guide> for Group<ItemMetadata> {
    fn from(_val: Guide) -> Self {
        Group::builder("Guide")
            .prefix("GDE")
            .patterns(vec![
                // Generic patterns (matched by subgroups)
                "click",
                "metronome",
                "count",
                "guide",
                "cue",
            ])
            .group(Click)
            .group(Count)
            .group(Cues)
            .build()
    }
}

/// Click/Metronome subgroup for timing tracks
pub struct Click;

impl From<Click> for Group<ItemMetadata> {
    fn from(_val: Click) -> Self {
        Group::builder("Click")
            .patterns(vec![
                "click",
                "click track",
                "metronome",
                "met",
                "tempo",
                "tempo track",
            ])
            .build()
    }
}

/// Count subgroup for count-ins (not part of final song)
pub struct Count;

impl From<Count> for Group<ItemMetadata> {
    fn from(_val: Count) -> Self {
        Group::builder("Count")
            .patterns(vec![
                "count",
                "count in",
                "count-in",
                "countin",
                "count off",
                "1234",
                "one two three four",
                "intro count",
            ])
            .build()
    }
}

/// Cues subgroup for performer cue tracks (in-ears)
pub struct Cues;

impl From<Cues> for Group<ItemMetadata> {
    fn from(_val: Cues) -> Self {
        Group::builder("Cues")
            .patterns(vec![
                // Guide patterns
                "guide",
                "guide track",
                "guide vox",
                "guide vocal",
                // Section guides
                "sections",
                "sections guide",
                "section guide",
                "tracks guide",
                // Cue/callout patterns
                "cue",
                "cue track",
                "callout",
                "callouts",
                "call out",
                "call outs",
                // IEM/monitor patterns
                "iem",
                "in ear",
                "in-ear",
                "monitor cue",
                "ear cue",
            ])
            .build()
    }
}

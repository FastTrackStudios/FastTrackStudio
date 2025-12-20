//! Percussion group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Top-level percussion group for non-drum kit percussion instruments
pub struct Percussion;

impl From<Percussion> for Group<ItemMetadata> {
    fn from(_val: Percussion) -> Self {
        Group::builder("Percussion")
            .prefix("Perc")
            .patterns(vec![
                "percussion",
                "perc",
                "shaker",
                "tambourine",
                "conga",
                "bongo",
                "cowbell",
                "woodblock",
                "clap",
                "handclap",
                "triangle",
                "cabasa",
                "guiro",
                "maracas",
                "cajon",
                "djembe",
                "timbales",
                "vibraslap",
                "chimes",
                "bells",
            ])
            // Exclude drum kit items
            .exclude(vec!["drum", "kick", "snare", "hihat", "cymbal", "tom"])
            .build()
    }
}

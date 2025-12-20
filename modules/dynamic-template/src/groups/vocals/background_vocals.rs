//! Background vocals group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Background vocals group
pub struct BackgroundVocals;

impl From<BackgroundVocals> for Group<ItemMetadata> {
    fn from(_val: BackgroundVocals) -> Self {
        Group::builder("Background Vocals")
            .prefix("BGV")
            .patterns(vec!["bgv", "background", "backing", "harmony", "choir"])
            .build()
    }
}

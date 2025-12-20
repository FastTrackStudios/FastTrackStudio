//! Lead vocals group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Lead vocals group
pub struct LeadVocals;

impl From<LeadVocals> for Group<ItemMetadata> {
    fn from(_val: LeadVocals) -> Self {
        Group::builder("Lead Vocals")
            .prefix("LV")
            .patterns(vec!["lead", "main", "solo"])
            .build()
    }
}

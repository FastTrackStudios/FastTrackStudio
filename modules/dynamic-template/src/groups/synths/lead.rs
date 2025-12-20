//! Synth lead group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Lead synth group
pub struct Lead;

impl From<Lead> for Group<ItemMetadata> {
    fn from(_val: Lead) -> Self {
        Group::builder("Lead").patterns(vec!["lead"]).build()
    }
}

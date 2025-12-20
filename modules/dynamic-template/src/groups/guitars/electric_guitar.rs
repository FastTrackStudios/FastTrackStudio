//! Electric guitar group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Electric guitar group
pub struct ElectricGuitar;

impl From<ElectricGuitar> for Group<ItemMetadata> {
    fn from(_val: ElectricGuitar) -> Self {
        Group::builder("Electric Guitar")
            .prefix("EG")
            .patterns(vec!["electric"])
            .build()
    }
}

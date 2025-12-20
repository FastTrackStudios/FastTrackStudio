//! Electric keys group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Electric keys group (Rhodes, Wurlitzer, etc.)
pub struct ElectricKeys;

impl From<ElectricKeys> for Group<ItemMetadata> {
    fn from(_val: ElectricKeys) -> Self {
        Group::builder("Electric Keys")
            .patterns(vec![
                "rhodes",
                "wurlitzer",
                "electric_piano",
                "ep",
                "fender_rhodes",
            ])
            .build()
    }
}

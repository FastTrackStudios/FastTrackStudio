//! Tom drum group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Tom drum group
pub struct Tom;

impl Into<Group<ItemMetadata>> for Tom {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Tom")
            .patterns(vec!["tom", "t1", "t2", "t3", "ft", "floor"])
            .build()
    }
}

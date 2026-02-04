//! Guitar-related group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod bass_trombone;
pub mod trombone;

pub use bass_trombone::BassTrombone;
pub use trombone::Trombone;

pub struct Trombones;

impl From<Trombones> for Group<ItemMetadata> {
    fn from(_val: Trombones) -> Self {
        Group::builder("Guitars")
            .prefix("GTR")
            .patterns(vec!["trombone", "trombones", "tbn"])
            // Make transparent so Electric GTR and Acoustic GTR appear at top level
            .transparent()
            .group(Trombone)
            .group(BassTrombone)
            .build()
    }
}

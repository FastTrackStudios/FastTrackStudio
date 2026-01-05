//! Electric keys group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Electric keys group (Rhodes, Wurlitzer, DX7, Mellotron, etc.)
pub struct ElectricKeys;

impl From<ElectricKeys> for Group<ItemMetadata> {
    fn from(_val: ElectricKeys) -> Self {
        // Subgroup for Rhodes/Wurlitzer to preserve the instrument name in display names
        let rhodes = Group::builder("Rhodes")
            .patterns(vec!["rhodes", "wurlitzer", "fender_rhodes"])
            .build();

        Group::builder("Electric Keys")
            .group(rhodes)
            .patterns(vec![
                // Electric pianos
                "rhodes",
                "wurlitzer",
                "electric_piano",
                "ep",
                "fender_rhodes",
                // FM synthesizers (keyboard-based)
                "dx7",
                "dx-7",
                "fm piano",
                "fm_piano",
                // Clavinet
                "clav",
                "clavinet",
                // Hohner
                "hohner",
                "pianet",
                // Tape-based keyboards
                "mellotron",
                "chamberlin",
            ])
            .build()
    }
}

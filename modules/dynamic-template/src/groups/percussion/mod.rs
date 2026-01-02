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
            ])
            // Exclude drum kit items
            .exclude(vec!["drum", "kick", "snare", "hihat", "cymbal", "tom"])
            // Add subgroups for specific percussion instruments
            .group(Shaker)
            .group(Tambourine)
            .group(Cabasa)
            .group(Guiro)
            .group(Clave)
            .group(Vibraslap)
            .group(Conga)
            .group(Bongo)
            .group(Cowbell)
            .group(Woodblock)
            .group(Clap)
            .group(Triangle)
            .group(Maracas)
            .group(Cajon)
            .group(Djembe)
            .group(Timbales)
            .group(Chimes)
            .group(Agogo)
            .group(Castanets)
            .build()
    }
}

// Individual percussion instrument groups

pub struct Shaker;
impl From<Shaker> for Group<ItemMetadata> {
    fn from(_val: Shaker) -> Self {
        Group::builder("Shaker").patterns(vec!["shaker"]).build()
    }
}

pub struct Tambourine;
impl From<Tambourine> for Group<ItemMetadata> {
    fn from(_val: Tambourine) -> Self {
        Group::builder("Tambourine").patterns(vec!["tambourine", "tamb"]).build()
    }
}

pub struct Cabasa;
impl From<Cabasa> for Group<ItemMetadata> {
    fn from(_val: Cabasa) -> Self {
        Group::builder("Cabasa").patterns(vec!["cabasa"]).build()
    }
}

pub struct Guiro;
impl From<Guiro> for Group<ItemMetadata> {
    fn from(_val: Guiro) -> Self {
        Group::builder("Guiro").patterns(vec!["guiro"]).build()
    }
}

pub struct Clave;
impl From<Clave> for Group<ItemMetadata> {
    fn from(_val: Clave) -> Self {
        Group::builder("Clave").patterns(vec!["clave"]).build()
    }
}

pub struct Vibraslap;
impl From<Vibraslap> for Group<ItemMetadata> {
    fn from(_val: Vibraslap) -> Self {
        Group::builder("Vibraslap").patterns(vec!["vibraslap"]).build()
    }
}

pub struct Conga;
impl From<Conga> for Group<ItemMetadata> {
    fn from(_val: Conga) -> Self {
        Group::builder("Conga").patterns(vec!["conga"]).build()
    }
}

pub struct Bongo;
impl From<Bongo> for Group<ItemMetadata> {
    fn from(_val: Bongo) -> Self {
        Group::builder("Bongo").patterns(vec!["bongo"]).build()
    }
}

pub struct Cowbell;
impl From<Cowbell> for Group<ItemMetadata> {
    fn from(_val: Cowbell) -> Self {
        Group::builder("Cowbell").patterns(vec!["cowbell"]).build()
    }
}

pub struct Woodblock;
impl From<Woodblock> for Group<ItemMetadata> {
    fn from(_val: Woodblock) -> Self {
        Group::builder("Woodblock").patterns(vec!["woodblock", "wood block"]).build()
    }
}

pub struct Clap;
impl From<Clap> for Group<ItemMetadata> {
    fn from(_val: Clap) -> Self {
        Group::builder("Clap").patterns(vec!["clap", "handclap"]).build()
    }
}

pub struct Triangle;
impl From<Triangle> for Group<ItemMetadata> {
    fn from(_val: Triangle) -> Self {
        Group::builder("Triangle").patterns(vec!["triangle"]).build()
    }
}

pub struct Maracas;
impl From<Maracas> for Group<ItemMetadata> {
    fn from(_val: Maracas) -> Self {
        Group::builder("Maracas").patterns(vec!["maracas", "maraca"]).build()
    }
}

pub struct Cajon;
impl From<Cajon> for Group<ItemMetadata> {
    fn from(_val: Cajon) -> Self {
        Group::builder("Cajon").patterns(vec!["cajon"]).build()
    }
}

pub struct Djembe;
impl From<Djembe> for Group<ItemMetadata> {
    fn from(_val: Djembe) -> Self {
        Group::builder("Djembe").patterns(vec!["djembe"]).build()
    }
}

pub struct Timbales;
impl From<Timbales> for Group<ItemMetadata> {
    fn from(_val: Timbales) -> Self {
        Group::builder("Timbales").patterns(vec!["timbales", "timbale"]).build()
    }
}

pub struct Chimes;
impl From<Chimes> for Group<ItemMetadata> {
    fn from(_val: Chimes) -> Self {
        Group::builder("Chimes").patterns(vec!["chimes", "chime"]).build()
    }
}

pub struct Agogo;
impl From<Agogo> for Group<ItemMetadata> {
    fn from(_val: Agogo) -> Self {
        Group::builder("Agogo").patterns(vec!["agogo"]).build()
    }
}

pub struct Castanets;
impl From<Castanets> for Group<ItemMetadata> {
    fn from(_val: Castanets) -> Self {
        Group::builder("Castanets").patterns(vec!["castanets", "castanet"]).build()
    }
}

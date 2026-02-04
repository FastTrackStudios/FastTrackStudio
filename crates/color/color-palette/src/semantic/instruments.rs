//! Semantic color mappings for instrument groups.
//!
//! This module provides consistent color assignments for audio tracks
//! based on instrument classification. Colors are chosen to be:
//! - Visually distinct between major categories
//! - Intuitive (warm colors for rhythm section, cool for melodic)
//! - Consistent with audio engineering conventions

use crate::{palette, Color};

// ============================================================================
// Top-Level Group Colors
// ============================================================================

/// Color definitions for top-level instrument groups
pub mod groups {
    use super::*;

    pub const DRUMS: Color = palette::red::S500;
    pub const PERCUSSION: Color = palette::orange::S500;
    pub const BASS: Color = palette::amber::S500;
    /// Guitars defaults to electric guitar color since most "Guitars" folders contain electric
    pub const GUITARS: Color = palette::sky::S600;
    pub const KEYS: Color = palette::green::S500;
    pub const SYNTHS: Color = palette::violet::S500;
    pub const HORNS: Color = palette::amber::S400;
    pub const HARMONICA: Color = palette::stone::S500;
    pub const VOCALS: Color = palette::pink::S500;
    pub const CHOIR: Color = palette::purple::S300;
    pub const ORCHESTRA: Color = palette::purple::S600;
    pub const SFX: Color = palette::teal::S500;
    pub const GUIDE: Color = palette::slate::S400;
    pub const REFERENCE: Color = palette::slate::S500;
}

// ============================================================================
// Sub-Group Colors
// ============================================================================

/// Color definitions for drum sub-groups
pub mod drums {
    use super::*;

    pub const DRUM_KIT: Color = palette::red::S500;
    pub const ELECTRONIC: Color = palette::red::S600;
    pub const KICK: Color = palette::red::S700;
    pub const SNARE: Color = palette::orange::S600;
    pub const TOM: Color = palette::red::S500;
    pub const CYMBALS: Color = palette::amber::S400;
    pub const HI_HAT: Color = palette::yellow::S300;
    pub const ROOM: Color = palette::red::S400;
    pub const OVERHEAD: Color = palette::red::S300;
}

/// Color definitions for bass sub-groups
pub mod bass {
    use super::*;

    pub const GUITAR: Color = palette::amber::S500;
    pub const SYNTH: Color = palette::amber::S600;
    pub const UPRIGHT: Color = palette::amber::S700;
    pub const DI: Color = palette::amber::S400;
    pub const AMP: Color = palette::amber::S600;
}

/// Color definitions for guitar sub-groups
pub mod guitars {
    use super::*;

    pub const ELECTRIC: Color = palette::sky::S600;
    pub const ACOUSTIC: Color = palette::emerald::S400;
    pub const STEEL: Color = palette::sky::S400;
    pub const BANJO: Color = palette::amber::S600;
    pub const DI: Color = palette::sky::S400;
    pub const AMP: Color = palette::sky::S700;
}

/// Color definitions for keys sub-groups
pub mod keys {
    use super::*;

    pub const PIANO: Color = palette::green::S500;
    pub const ELECTRIC: Color = palette::green::S600;
    pub const ORGAN: Color = palette::green::S700;
    pub const HARPSICHORD: Color = palette::green::S300;
    pub const CLAVICHORD: Color = palette::green::S400;
}

/// Color definitions for synth sub-groups
pub mod synths {
    use super::*;

    pub const LEAD: Color = palette::violet::S500;
    pub const PAD: Color = palette::violet::S400;
    pub const ARP: Color = palette::violet::S600;
    pub const CHORD: Color = palette::violet::S300;
    pub const KEYS: Color = palette::violet::S700;
    pub const FX: Color = palette::violet::S800;
    pub const BASS: Color = palette::indigo::S600;
}

/// Color definitions for vocal sub-groups
pub mod vocals {
    use super::*;

    pub const LEAD: Color = palette::pink::S500;
    pub const BACKGROUND: Color = palette::pink::S300;
    pub const HARMONY: Color = palette::pink::S400;
    pub const DOUBLE: Color = palette::pink::S600;
}

/// Color definitions for orchestra sub-groups
pub mod orchestra {
    use super::*;

    pub const STRINGS: Color = palette::rose::S600;
    pub const WOODWINDS: Color = palette::emerald::S600;
    pub const BRASS: Color = palette::amber::S500;
    pub const HARP: Color = palette::purple::S400;
    pub const PERCUSSION: Color = palette::orange::S600;

    /// String section colors
    pub mod strings {
        use super::*;

        pub const VIOLINS: Color = palette::rose::S500;
        pub const FIRST_VIOLIN: Color = palette::rose::S400;
        pub const SECOND_VIOLIN: Color = palette::rose::S500;
        pub const VIOLA: Color = palette::rose::S600;
        pub const CELLO: Color = palette::rose::S700;
        pub const CONTRABASS: Color = palette::rose::S800;
    }

    /// Woodwind section colors
    pub mod woodwinds {
        use super::*;

        pub const FLUTES: Color = palette::emerald::S400;
        pub const PICCOLO: Color = palette::emerald::S300;
        pub const OBOES: Color = palette::emerald::S500;
        pub const CLARINETS: Color = palette::emerald::S600;
        pub const BASSOONS: Color = palette::emerald::S700;
    }

    /// Brass section colors
    pub mod brass {
        use super::*;

        pub const TRUMPETS: Color = palette::amber::S400;
        pub const HORNS: Color = palette::amber::S500;
        pub const TROMBONES: Color = palette::orange::S500;
        pub const BASS_TROMBONE: Color = palette::orange::S600;
        pub const TUBA: Color = palette::orange::S700;
    }
}

/// Color definitions for guide tracks
pub mod guide {
    use super::*;

    pub const CLICK: Color = palette::pink::S500;
    pub const SOLO: Color = palette::amber::S500;
    pub const SECTIONS: Color = palette::slate::S400;
}

// ============================================================================
// Lookup Functions
// ============================================================================

/// Get the color for a top-level instrument group.
///
/// # Arguments
/// * `group` - Group name (case-insensitive)
///
/// # Returns
/// The color for the group, or None if not found.
pub fn color_for_group(group: &str) -> Option<Color> {
    match group.to_lowercase().as_str() {
        "drums" | "drum" => Some(groups::DRUMS),
        "percussion" | "perc" => Some(groups::PERCUSSION),
        "bass" => Some(groups::BASS),
        "guitars" | "guitar" | "gtr" => Some(groups::GUITARS),
        "keys" | "keyboards" | "keyboard" => Some(groups::KEYS),
        "synths" | "synth" | "synthesizer" | "synthesizers" => Some(groups::SYNTHS),
        "horns" | "horn" => Some(groups::HORNS),
        "harmonica" => Some(groups::HARMONICA),
        "vocals" | "vocal" | "vox" => Some(groups::VOCALS),
        "choir" | "choirs" => Some(groups::CHOIR),
        "orchestra" | "orch" | "orchestral" => Some(groups::ORCHESTRA),
        "sfx" | "effects" | "fx" => Some(groups::SFX),
        "guide" | "click" => Some(groups::GUIDE),
        "reference" | "ref" | "mix" => Some(groups::REFERENCE),
        _ => None,
    }
}

/// Get the color for a specific instrument or sub-group.
///
/// # Arguments
/// * `instrument` - Instrument name (case-insensitive)
///
/// # Returns
/// The color for the instrument, or None if not found.
pub fn color_for_instrument(instrument: &str) -> Option<Color> {
    match instrument.to_lowercase().as_str() {
        // Drums
        "kick" | "kick drum" | "bd" => Some(drums::KICK),
        "snare" | "snr" | "sd" => Some(drums::SNARE),
        "tom" | "toms" => Some(drums::TOM),
        "hi-hat" | "hihat" | "hi hat" | "hh" => Some(drums::HI_HAT),
        "cymbals" | "cymbal" | "cym" => Some(drums::CYMBALS),
        "overhead" | "oh" | "overheads" => Some(drums::OVERHEAD),
        "room" | "rooms" => Some(drums::ROOM),

        // Bass
        "bass guitar" | "bass gtr" | "electric bass" => Some(bass::GUITAR),
        "synth bass" | "bass synth" => Some(bass::SYNTH),
        "upright bass" | "upright" | "double bass" | "acoustic bass" => Some(bass::UPRIGHT),

        // Guitars
        "electric guitar" | "electric" | "elec gtr" => Some(guitars::ELECTRIC),
        "acoustic guitar" | "acoustic" | "ac gtr" => Some(guitars::ACOUSTIC),
        "steel guitar" | "pedal steel" | "lap steel" => Some(guitars::STEEL),
        "banjo" => Some(guitars::BANJO),

        // Keys
        "piano" | "grand piano" | "upright piano" => Some(keys::PIANO),
        "electric piano" | "rhodes" | "wurlitzer" | "ep" => Some(keys::ELECTRIC),
        "organ" | "b3" | "hammond" => Some(keys::ORGAN),
        "harpsichord" => Some(keys::HARPSICHORD),
        "clavichord" | "clav" | "clavinet" => Some(keys::CLAVICHORD),

        // Synths
        "synth lead" | "lead synth" => Some(synths::LEAD),
        "synth pad" | "pad" | "pads" => Some(synths::PAD),
        "arp" | "arpeggio" | "arpeggios" => Some(synths::ARP),

        // Vocals
        "lead vocal" | "lead vox" | "lead" => Some(vocals::LEAD),
        "bgv" | "bgvs" | "background vocals" | "backing vocals" => Some(vocals::BACKGROUND),
        "harmony" | "harmonies" => Some(vocals::HARMONY),
        "double" | "dbl" | "doubles" => Some(vocals::DOUBLE),

        // Orchestra - Strings
        "violin" | "violins" | "vln" => Some(orchestra::strings::VIOLINS),
        "first violin" | "violin 1" | "vln 1" => Some(orchestra::strings::FIRST_VIOLIN),
        "second violin" | "violin 2" | "vln 2" => Some(orchestra::strings::SECOND_VIOLIN),
        "viola" | "violas" | "vla" => Some(orchestra::strings::VIOLA),
        "cello" | "cellos" | "vc" | "vcl" => Some(orchestra::strings::CELLO),
        "contrabass" | "cb" => Some(orchestra::strings::CONTRABASS),
        "strings" => Some(orchestra::STRINGS),

        // Orchestra - Woodwinds
        "flute" | "flutes" | "fl" => Some(orchestra::woodwinds::FLUTES),
        "piccolo" | "picc" => Some(orchestra::woodwinds::PICCOLO),
        "oboe" | "oboes" | "ob" => Some(orchestra::woodwinds::OBOES),
        "clarinet" | "clarinets" | "cl" => Some(orchestra::woodwinds::CLARINETS),
        "bassoon" | "bassoons" | "bsn" => Some(orchestra::woodwinds::BASSOONS),
        "woodwinds" | "woods" => Some(orchestra::WOODWINDS),

        // Orchestra - Brass
        "trumpet" | "trumpets" | "tpt" | "tr" => Some(orchestra::brass::TRUMPETS),
        "french horn" | "horn" | "horns" | "hn" => Some(orchestra::brass::HORNS),
        "trombone" | "trombones" | "tbn" => Some(orchestra::brass::TROMBONES),
        "bass trombone" | "bass tbn" => Some(orchestra::brass::BASS_TROMBONE),
        "tuba" | "tba" => Some(orchestra::brass::TUBA),
        "brass" => Some(orchestra::BRASS),

        // Other
        "harp" => Some(orchestra::HARP),

        _ => None,
    }
}

/// Get the color for a hierarchical path of groups/instruments.
///
/// This tries to find the most specific color for a path like
/// `["Drums", "Kick", "In"]` by checking from most to least specific.
///
/// # Arguments
/// * `path` - Slice of group/instrument names from root to leaf
///
/// # Returns
/// The most specific color found, or None if no match.
pub fn color_for_path(path: &[&str]) -> Option<Color> {
    // Try from most specific to least specific
    for i in (0..path.len()).rev() {
        if let Some(color) = color_for_instrument(path[i]) {
            return Some(color);
        }
        if let Some(color) = color_for_group(path[i]) {
            return Some(color);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_for_group() {
        assert_eq!(color_for_group("Drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("DRUMS"), Some(groups::DRUMS));
        assert_eq!(color_for_group("Bass"), Some(groups::BASS));
        assert_eq!(color_for_group("Vocals"), Some(groups::VOCALS));
        assert_eq!(color_for_group("Unknown"), None);
    }

    #[test]
    fn test_color_for_instrument() {
        assert_eq!(color_for_instrument("kick"), Some(drums::KICK));
        assert_eq!(color_for_instrument("Snare"), Some(drums::SNARE));
        assert_eq!(color_for_instrument("lead vocal"), Some(vocals::LEAD));
        assert_eq!(color_for_instrument("piano"), Some(keys::PIANO));
    }

    #[test]
    fn test_color_for_path() {
        // Most specific wins
        assert_eq!(color_for_path(&["Drums", "Kick", "In"]), Some(drums::KICK));
        // Falls back to group
        assert_eq!(color_for_path(&["Drums", "Unknown"]), Some(groups::DRUMS));
        // Unknown path
        assert_eq!(color_for_path(&["Totally", "Unknown"]), None);
    }
}

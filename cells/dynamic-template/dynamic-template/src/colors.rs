//! Track colors based on instrument classification
//!
//! This module provides color definitions for instrument groups using
//! the shared color-palette crate's Tailwind-style palette system.
//!
//! # Usage
//!
//! ```rust
//! use dynamic_template::colors::{color_for_group, groups, drums};
//!
//! // Get color for a top-level group
//! let drum_color = groups::DRUMS;
//!
//! // Get color for a sub-group
//! let kick_color = drums::KICK;
//!
//! // Look up by name
//! if let Some(color) = color_for_group("Electric Guitar") {
//!     println!("Color: {}", color);
//! }
//! ```

use std::collections::HashMap;
use std::sync::LazyLock;

// Re-export the Color type and palette from color-palette
pub use color_palette::palette;
pub use color_palette::Color;

// ============================================================================
// Group Color Assignments (using palette)
// ============================================================================

/// Color definitions for top-level instrument groups
pub mod groups {
    use super::palette;
    use color_palette::Color;

    pub const DRUMS: Color = palette::red::S500;
    pub const PERCUSSION: Color = palette::orange::S500;
    pub const BASS: Color = palette::amber::S500;
    /// Guitars defaults to electric guitar color (sky-600) since most "Guitars" folders contain electric
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

/// Color definitions for guitar sub-groups
pub mod guitars {
    use super::palette;
    use color_palette::Color;

    pub const ELECTRIC: Color = palette::sky::S600;
    pub const ACOUSTIC: Color = palette::emerald::S400;
    pub const STEEL: Color = palette::sky::S400;
    pub const BANJO: Color = palette::amber::S600;
}

/// Color definitions for vocal sub-groups
pub mod vocals {
    use super::palette;
    use color_palette::Color;

    pub const LEAD: Color = palette::pink::S500;
    pub const BACKGROUND: Color = palette::pink::S300;
}

/// Color definitions for drum sub-groups
pub mod drums {
    use super::palette;
    use color_palette::Color;

    pub const DRUM_KIT: Color = palette::red::S500;
    pub const ELECTRONIC: Color = palette::red::S600;
    pub const KICK: Color = palette::red::S700;
    pub const SNARE: Color = palette::orange::S600;
    pub const TOM: Color = palette::red::S500;
    pub const CYMBALS: Color = palette::amber::S400;
    pub const HI_HAT: Color = palette::yellow::S300;
    pub const ROOM: Color = palette::red::S400;
}

/// Color definitions for bass sub-groups
pub mod bass {
    use super::palette;
    use color_palette::Color;

    pub const GUITAR: Color = palette::amber::S500;
    pub const SYNTH: Color = palette::amber::S600;
    pub const UPRIGHT: Color = palette::amber::S700;
}

/// Color definitions for keys sub-groups
pub mod keys {
    use super::palette;
    use color_palette::Color;

    pub const PIANO: Color = palette::green::S500;
    pub const ELECTRIC: Color = palette::green::S600;
    pub const ORGAN: Color = palette::green::S700;
    pub const HARPSICHORD: Color = palette::green::S300;
    pub const CLAVICHORD: Color = palette::green::S400;
}

/// Color definitions for synth sub-groups
pub mod synths {
    use super::palette;
    use color_palette::Color;

    pub const LEAD: Color = palette::violet::S500;
    pub const PAD: Color = palette::violet::S400;
    pub const ARP: Color = palette::violet::S600;
    pub const CHORD: Color = palette::violet::S300;
    pub const KEYS: Color = palette::violet::S700;
    pub const FX: Color = palette::violet::S800;
}

/// Color definitions for orchestra sub-groups
pub mod orchestra {
    use super::palette;
    use color_palette::Color;

    pub const STRINGS: Color = palette::rose::S600;
    pub const WOODWINDS: Color = palette::emerald::S600;
    pub const BRASS: Color = palette::amber::S500;
    pub const HARP: Color = palette::purple::S400;
    pub const PERCUSSION: Color = palette::orange::S600;

    pub mod strings {
        use super::palette;
        use color_palette::Color;

        pub const VIOLINS: Color = palette::rose::S500;
        pub const VIOLA: Color = palette::rose::S600;
        pub const CELLO: Color = palette::rose::S700;
        pub const CONTRABASS: Color = palette::rose::S800;
    }

    pub mod woodwinds {
        use super::palette;
        use color_palette::Color;

        pub const FLUTES: Color = palette::emerald::S400;
        pub const OBOES: Color = palette::emerald::S500;
        pub const CLARINETS: Color = palette::emerald::S600;
        pub const BASSOONS: Color = palette::emerald::S700;
        pub const PICCOLO: Color = palette::emerald::S300;
    }

    pub mod brass {
        use super::palette;
        use color_palette::Color;

        pub const TRUMPETS: Color = palette::amber::S400;
        pub const HORNS: Color = palette::amber::S500;
        pub const TROMBONES: Color = palette::amber::S600;
        pub const TUBA: Color = palette::amber::S700;
    }
}

/// Color definitions for percussion sub-groups
pub mod percussion {
    use super::palette;
    use color_palette::Color;

    pub const SHAKER: Color = palette::orange::S500;
    pub const TAMBOURINE: Color = palette::orange::S600;
    pub const CABASA: Color = palette::orange::S500;
    pub const CONGA: Color = palette::orange::S700;
    pub const BONGO: Color = palette::orange::S600;
    pub const COWBELL: Color = palette::amber::S400;
    pub const CLAP: Color = palette::orange::S400;
    pub const TRIANGLE: Color = palette::yellow::S300;
}

/// Color definitions for guide/click tracks and song sections
pub mod guide {
    use super::palette;
    use color_palette::Color;

    // Main guide track types
    pub const CLICK: Color = palette::slate::S600;
    pub const GUIDE_TRACK: Color = palette::slate::S400;
    pub const CUE: Color = palette::slate::S500;

    // Song sections - using semantic colors
    pub const INTRO: Color = palette::sky::S400;
    pub const VERSE: Color = palette::green::S400;
    pub const PRE_CHORUS: Color = palette::yellow::S400;
    pub const CHORUS: Color = palette::red::S500;
    pub const POST_CHORUS: Color = palette::orange::S400;
    pub const BRIDGE: Color = palette::purple::S400;
    pub const BREAKDOWN: Color = palette::blue::S400;
    pub const INTERLUDE: Color = palette::teal::S400;
    pub const INSTRUMENTAL: Color = palette::emerald::S400;
    pub const SOLO: Color = palette::amber::S500;
    pub const OUTRO: Color = palette::slate::S500;
    pub const ENDING: Color = palette::slate::S600;
    pub const TAG: Color = palette::purple::S300;
    pub const VAMP: Color = palette::green::S300;
    pub const TURNAROUND: Color = palette::slate::S300;
    pub const REFRAIN: Color = palette::red::S500;
    pub const ACAPELLA: Color = palette::pink::S500;
    pub const RAP: Color = palette::violet::S500;
    pub const EXHORTATION: Color = palette::amber::S400;

    // Dynamic cues
    pub const BUILD: Color = palette::amber::S500;
    pub const SLOWLY_BUILD: Color = palette::amber::S600;
    pub const SWELL: Color = palette::orange::S500;
    pub const ALL_IN: Color = palette::red::S500;
    pub const SOFTLY: Color = palette::sky::S300;
    pub const HOLD: Color = palette::slate::S300;
    pub const BREAK: Color = palette::slate::S600;
    pub const HITS: Color = palette::red::S600;
    pub const DRUMS_IN: Color = palette::red::S500;
    pub const BIG_ENDING: Color = palette::red::S700;
    pub const LAST_TIME: Color = palette::red::S800;
    pub const KEY_CHANGE_UP: Color = palette::yellow::S500;
    pub const KEY_CHANGE_DOWN: Color = palette::blue::S500;
    pub const AD_LIB: Color = palette::purple::S300;
    pub const WORSHIP_FREELY: Color = palette::purple::S300;
}

/// Static color lookup table for hierarchical group paths
static COLOR_MAP: LazyLock<HashMap<&'static str, Color>> = LazyLock::new(|| {
    let mut m = HashMap::new();

    // Top-level groups (lowercase for case-insensitive matching)
    m.insert("drums", groups::DRUMS);
    m.insert("percussion", groups::PERCUSSION);
    m.insert("bass", groups::BASS);
    m.insert("guitars", groups::GUITARS);
    m.insert("keys", groups::KEYS);
    m.insert("synths", groups::SYNTHS);
    m.insert("horns", groups::HORNS);
    m.insert("harmonica", groups::HARMONICA);
    m.insert("vocals", groups::VOCALS);
    m.insert("choir", groups::CHOIR);
    m.insert("orchestra", groups::ORCHESTRA);
    m.insert("sfx", groups::SFX);
    m.insert("guide", groups::GUIDE);
    m.insert("reference", groups::REFERENCE);

    // Guitar sub-groups
    m.insert("guitars/electric", guitars::ELECTRIC);
    m.insert("guitars/acoustic", guitars::ACOUSTIC);
    m.insert("guitars/steel", guitars::STEEL);
    m.insert("guitars/banjo", guitars::BANJO);
    m.insert("electric guitar", guitars::ELECTRIC);
    m.insert("electric_guitar", guitars::ELECTRIC);
    m.insert("acoustic guitar", guitars::ACOUSTIC);
    m.insert("acoustic_guitar", guitars::ACOUSTIC);
    m.insert("steel guitar", guitars::STEEL);
    m.insert("steel_guitar", guitars::STEEL);

    // Vocal sub-groups
    m.insert("vocals/lead", vocals::LEAD);
    m.insert("vocals/background", vocals::BACKGROUND);
    m.insert("vocals/bgvs", vocals::BACKGROUND);
    m.insert("lead vocals", vocals::LEAD);
    m.insert("lead_vocals", vocals::LEAD);
    m.insert("background vocals", vocals::BACKGROUND);
    m.insert("background_vocals", vocals::BACKGROUND);
    m.insert("bgvs", vocals::BACKGROUND);
    m.insert("bgv", vocals::BACKGROUND);
    m.insert("bvs", vocals::BACKGROUND);

    // Drum sub-groups
    m.insert("drums/drum_kit", drums::DRUM_KIT);
    m.insert("drums/electronic", drums::ELECTRONIC);
    m.insert("drums/kick", drums::KICK);
    m.insert("drums/snare", drums::SNARE);
    m.insert("drums/tom", drums::TOM);
    m.insert("drums/cymbals", drums::CYMBALS);
    m.insert("drums/hi_hat", drums::HI_HAT);
    m.insert("drums/room", drums::ROOM);
    m.insert("drum kit", drums::DRUM_KIT);
    m.insert("electronic kit", drums::ELECTRONIC);
    m.insert("kick", drums::KICK);
    m.insert("snare", drums::SNARE);
    m.insert("tom", drums::TOM);
    m.insert("cymbals", drums::CYMBALS);
    m.insert("hi hat", drums::HI_HAT);
    m.insert("hi-hat", drums::HI_HAT);
    m.insert("hihat", drums::HI_HAT);
    m.insert("room", drums::ROOM);

    // Bass sub-groups
    m.insert("bass/guitar", bass::GUITAR);
    m.insert("bass/synth", bass::SYNTH);
    m.insert("bass/upright", bass::UPRIGHT);
    m.insert("bass guitar", bass::GUITAR);
    m.insert("bass_guitar", bass::GUITAR);
    m.insert("synth bass", bass::SYNTH);
    m.insert("synth_bass", bass::SYNTH);
    m.insert("upright bass", bass::UPRIGHT);
    m.insert("upright_bass", bass::UPRIGHT);

    // Keys sub-groups
    m.insert("keys/piano", keys::PIANO);
    m.insert("keys/electric", keys::ELECTRIC);
    m.insert("keys/organ", keys::ORGAN);
    m.insert("keys/harpsichord", keys::HARPSICHORD);
    m.insert("keys/clavichord", keys::CLAVICHORD);
    m.insert("piano", keys::PIANO);
    m.insert("electric keys", keys::ELECTRIC);
    m.insert("electric_keys", keys::ELECTRIC);
    m.insert("organ", keys::ORGAN);
    m.insert("harpsichord", keys::HARPSICHORD);
    m.insert("clavichord", keys::CLAVICHORD);

    // Synth sub-groups
    m.insert("synths/lead", synths::LEAD);
    m.insert("synths/pad", synths::PAD);
    m.insert("synths/arp", synths::ARP);
    m.insert("synths/chord", synths::CHORD);
    m.insert("synths/keys", synths::KEYS);
    m.insert("synths/fx", synths::FX);
    m.insert("lead synth", synths::LEAD);
    m.insert("pad", synths::PAD);
    m.insert("arp", synths::ARP);
    m.insert("synth arp", synths::ARP);

    // Orchestra sub-groups
    m.insert("orchestra/strings", orchestra::STRINGS);
    m.insert("orchestra/woodwinds", orchestra::WOODWINDS);
    m.insert("orchestra/brass", orchestra::BRASS);
    m.insert("orchestra/harp", orchestra::HARP);
    m.insert("orchestra/percussion", orchestra::PERCUSSION);
    m.insert("strings", orchestra::STRINGS);
    m.insert("woodwinds", orchestra::WOODWINDS);
    m.insert("brass", orchestra::BRASS);
    m.insert("harp", orchestra::HARP);

    // String section
    m.insert("violins", orchestra::strings::VIOLINS);
    m.insert("viola", orchestra::strings::VIOLA);
    m.insert("cello", orchestra::strings::CELLO);
    m.insert("contrabass", orchestra::strings::CONTRABASS);

    // Woodwind section
    m.insert("flutes", orchestra::woodwinds::FLUTES);
    m.insert("flute", orchestra::woodwinds::FLUTES);
    m.insert("oboes", orchestra::woodwinds::OBOES);
    m.insert("oboe", orchestra::woodwinds::OBOES);
    m.insert("clarinets", orchestra::woodwinds::CLARINETS);
    m.insert("clarinet", orchestra::woodwinds::CLARINETS);
    m.insert("bassoons", orchestra::woodwinds::BASSOONS);
    m.insert("bassoon", orchestra::woodwinds::BASSOONS);
    m.insert("piccolo", orchestra::woodwinds::PICCOLO);

    // Brass section
    m.insert("trumpets", orchestra::brass::TRUMPETS);
    m.insert("trumpet", orchestra::brass::TRUMPETS);
    m.insert("french horns", orchestra::brass::HORNS);
    m.insert("french horn", orchestra::brass::HORNS);
    m.insert("trombones", orchestra::brass::TROMBONES);
    m.insert("trombone", orchestra::brass::TROMBONES);
    m.insert("tuba", orchestra::brass::TUBA);

    // Percussion sub-groups
    m.insert("shaker", percussion::SHAKER);
    m.insert("tambourine", percussion::TAMBOURINE);
    m.insert("cabasa", percussion::CABASA);
    m.insert("conga", percussion::CONGA);
    m.insert("bongo", percussion::BONGO);
    m.insert("cowbell", percussion::COWBELL);
    m.insert("clap", percussion::CLAP);
    m.insert("triangle", percussion::TRIANGLE);

    // Guide/Click track types
    m.insert("click", guide::CLICK);
    m.insert("click track", guide::CLICK);
    m.insert("guide track", guide::GUIDE_TRACK);
    m.insert("cue", guide::CUE);
    m.insert("cue track", guide::CUE);

    // Song sections - full names
    m.insert("intro", guide::INTRO);
    m.insert("verse", guide::VERSE);
    m.insert("verse 1", guide::VERSE);
    m.insert("verse 2", guide::VERSE);
    m.insert("verse 3", guide::VERSE);
    m.insert("verse 4", guide::VERSE);
    m.insert("verse 5", guide::VERSE);
    m.insert("verse 6", guide::VERSE);
    m.insert("pre chorus", guide::PRE_CHORUS);
    m.insert("pre-chorus", guide::PRE_CHORUS);
    m.insert("prechorus", guide::PRE_CHORUS);
    m.insert("pre chorus 1", guide::PRE_CHORUS);
    m.insert("pre chorus 2", guide::PRE_CHORUS);
    m.insert("pre chorus 3", guide::PRE_CHORUS);
    m.insert("pre chorus 4", guide::PRE_CHORUS);
    m.insert("chorus", guide::CHORUS);
    m.insert("chorus 1", guide::CHORUS);
    m.insert("chorus 2", guide::CHORUS);
    m.insert("chorus 3", guide::CHORUS);
    m.insert("chorus 4", guide::CHORUS);
    m.insert("post chorus", guide::POST_CHORUS);
    m.insert("post-chorus", guide::POST_CHORUS);
    m.insert("postchorus", guide::POST_CHORUS);
    m.insert("bridge", guide::BRIDGE);
    m.insert("bridge 1", guide::BRIDGE);
    m.insert("bridge 2", guide::BRIDGE);
    m.insert("bridge 3", guide::BRIDGE);
    m.insert("bridge 4", guide::BRIDGE);
    m.insert("breakdown", guide::BREAKDOWN);
    m.insert("interlude", guide::INTERLUDE);
    m.insert("instrumental", guide::INSTRUMENTAL);
    m.insert("solo", guide::SOLO);
    m.insert("outro", guide::OUTRO);
    m.insert("ending", guide::ENDING);
    m.insert("tag", guide::TAG);
    m.insert("vamp", guide::VAMP);
    m.insert("turnaround", guide::TURNAROUND);
    m.insert("refrain", guide::REFRAIN);
    m.insert("acapella", guide::ACAPELLA);
    m.insert("a capella", guide::ACAPELLA);
    m.insert("rap", guide::RAP);
    m.insert("exhortation", guide::EXHORTATION);

    // Song section abbreviations/prefixes (common in DAW regions)
    // Intro
    m.insert("in", guide::INTRO);
    m.insert("int", guide::INTRO);
    m.insert("i", guide::INTRO);
    // Verse
    m.insert("v", guide::VERSE);
    m.insert("vs", guide::VERSE);
    m.insert("v1", guide::VERSE);
    m.insert("v2", guide::VERSE);
    m.insert("v3", guide::VERSE);
    m.insert("v4", guide::VERSE);
    m.insert("v5", guide::VERSE);
    m.insert("v6", guide::VERSE);
    m.insert("vs1", guide::VERSE);
    m.insert("vs2", guide::VERSE);
    m.insert("vs3", guide::VERSE);
    m.insert("vs4", guide::VERSE);
    // Pre-chorus
    m.insert("pc", guide::PRE_CHORUS);
    m.insert("pre", guide::PRE_CHORUS);
    m.insert("pc1", guide::PRE_CHORUS);
    m.insert("pc2", guide::PRE_CHORUS);
    m.insert("pc3", guide::PRE_CHORUS);
    m.insert("pre1", guide::PRE_CHORUS);
    m.insert("pre2", guide::PRE_CHORUS);
    // Chorus
    m.insert("c", guide::CHORUS);
    m.insert("ch", guide::CHORUS);
    m.insert("cho", guide::CHORUS);
    m.insert("c1", guide::CHORUS);
    m.insert("c2", guide::CHORUS);
    m.insert("c3", guide::CHORUS);
    m.insert("c4", guide::CHORUS);
    m.insert("ch1", guide::CHORUS);
    m.insert("ch2", guide::CHORUS);
    m.insert("ch3", guide::CHORUS);
    m.insert("ch4", guide::CHORUS);
    // Post-chorus
    m.insert("poc", guide::POST_CHORUS);
    m.insert("post", guide::POST_CHORUS);
    // Bridge
    m.insert("b", guide::BRIDGE);
    m.insert("br", guide::BRIDGE);
    m.insert("brg", guide::BRIDGE);
    m.insert("b1", guide::BRIDGE);
    m.insert("b2", guide::BRIDGE);
    m.insert("br1", guide::BRIDGE);
    m.insert("br2", guide::BRIDGE);
    // Breakdown
    m.insert("bd", guide::BREAKDOWN);
    m.insert("brkdn", guide::BREAKDOWN);
    m.insert("brkdwn", guide::BREAKDOWN);
    // Interlude
    m.insert("inter", guide::INTERLUDE);
    m.insert("intl", guide::INTERLUDE);
    m.insert("il", guide::INTERLUDE);
    // Instrumental
    m.insert("inst", guide::INSTRUMENTAL);
    m.insert("instr", guide::INSTRUMENTAL);
    // Solo
    m.insert("s", guide::SOLO);
    m.insert("sol", guide::SOLO);
    // Outro
    m.insert("o", guide::OUTRO);
    m.insert("out", guide::OUTRO);
    m.insert("end", guide::ENDING);
    // Tag
    m.insert("t", guide::TAG);
    m.insert("tg", guide::TAG);
    // Turnaround
    m.insert("ta", guide::TURNAROUND);
    m.insert("turn", guide::TURNAROUND);
    // Vamp
    m.insert("vp", guide::VAMP);
    // Refrain
    m.insert("ref", guide::REFRAIN);
    m.insert("rf", guide::REFRAIN);

    // Dynamic cues
    m.insert("build", guide::BUILD);
    m.insert("slowly build", guide::SLOWLY_BUILD);
    m.insert("swell", guide::SWELL);
    m.insert("all in", guide::ALL_IN);
    m.insert("all-in", guide::ALL_IN);
    m.insert("softly", guide::SOFTLY);
    m.insert("hold", guide::HOLD);
    m.insert("break", guide::BREAK);
    m.insert("hits", guide::HITS);
    m.insert("drums in", guide::DRUMS_IN);
    m.insert("big ending", guide::BIG_ENDING);
    m.insert("last time", guide::LAST_TIME);
    m.insert("key change up", guide::KEY_CHANGE_UP);
    m.insert("key change down", guide::KEY_CHANGE_DOWN);
    m.insert("ad lib", guide::AD_LIB);
    m.insert("ad-lib", guide::AD_LIB);
    m.insert("adlib", guide::AD_LIB);
    m.insert("worship freely", guide::WORSHIP_FREELY);

    m
});

/// Get the color for a group by name or path
///
/// Supports various lookup formats:
/// - Top-level: "Drums", "guitars", "VOCALS"
/// - Path-based: "Guitars/Electric", "vocals/bgvs"
/// - Space-separated: "Electric Guitar", "Lead Vocals"
/// - Underscore-separated: "electric_guitar", "lead_vocals"
///
/// Returns `None` if no color is defined for the given group.
///
/// # Example
/// ```
/// use dynamic_template::colors::{color_for_group, groups, guitars};
///
/// assert_eq!(color_for_group("Drums"), Some(groups::DRUMS));
/// assert_eq!(color_for_group("Electric Guitar"), Some(guitars::ELECTRIC));
/// ```
pub fn color_for_group(group: &str) -> Option<Color> {
    let normalized = group.to_lowercase();
    COLOR_MAP.get(normalized.as_str()).copied()
}

/// Get the color for a region/marker name, with smart parsing for common formats
///
/// This function is specifically designed for parsing DAW region names which
/// often have various formats like:
/// - "V1", "VS2", "CH", "BR"
/// - "1. Verse", "2. Chorus"
/// - "INTRO - 8 bars"
///
/// Returns `None` if no matching section color is found.
pub fn color_for_region(name: &str) -> Option<Color> {
    // Try exact match first
    if let Some(color) = color_for_group(name) {
        return Some(color);
    }

    // Normalize: lowercase, trim, remove common prefixes/suffixes
    let normalized = name
        .to_lowercase()
        .trim()
        .trim_start_matches(|c: char| c.is_ascii_digit() || c == '.' || c == ' ')
        .trim_end_matches(|c: char| c.is_ascii_digit() || c == ' ')
        .split(&['-', '–', '—', '|', ':'][..])
        .next()
        .unwrap_or("")
        .trim()
        .to_string();

    color_for_group(&normalized)
}

/// Get the color for a hierarchical path of groups
///
/// Tries to find the most specific color for a path like
/// `["Drums", "Kick", "In"]` by checking from most to least specific.
///
/// # Example
/// ```
/// use dynamic_template::colors::{color_for_path, drums, groups};
///
/// // Most specific match wins
/// assert_eq!(color_for_path(&["Drums", "Kick"]), Some(drums::KICK));
/// // Falls back to parent if no specific match
/// assert_eq!(color_for_path(&["Drums", "Unknown"]), Some(groups::DRUMS));
/// ```
pub fn color_for_path(path: &[&str]) -> Option<Color> {
    // Try full path first (joined with /)
    let full_path = path.join("/").to_lowercase();
    if let Some(color) = COLOR_MAP.get(full_path.as_str()) {
        return Some(*color);
    }

    // Try from most specific to least specific
    for i in (0..path.len()).rev() {
        if let Some(color) = color_for_group(path[i]) {
            return Some(color);
        }
    }

    None
}

// ============================================================================
// HSL Color Utilities (for generating shades/gradients)
// ============================================================================

/// Convert RGB color to HSL
pub fn rgb_to_hsl(color: Color) -> (f32, f32, f32) {
    color.to_hsl()
}

/// Convert HSL to RGB color
pub fn hsl_to_rgb(h: f32, s: f32, l: f32) -> Color {
    Color::from_hsl(h, s, l)
}

/// Adjust the lightness of a color
pub fn adjust_lightness(color: Color, delta: f32) -> Color {
    let (h, s, l) = color.to_hsl();
    Color::from_hsl(h, s, (l + delta).clamp(0.0, 1.0))
}

/// Get a shade of a color (0-9 scale, 4 = base)
pub fn get_shade(color: Color, shade: u8) -> Color {
    color.shade(shade)
}

/// Calculate a color on a gradient between two colors
pub fn calc_gradient(start: Color, end: Color, position: f32) -> Color {
    start.mix(end, position)
}

/// Generate a full gradient between two colors
pub fn generate_gradient(start: Color, end: Color, steps: usize) -> Vec<Color> {
    Color::gradient(start, end, steps)
}

/// Convert a hex color to REAPER's native format
pub fn to_reaper_color(color: Color) -> i32 {
    color.to_reaper_native()
}

/// Convert REAPER's native color format to hex
pub fn from_reaper_color(native: i32) -> Color {
    Color::from_reaper_native(native)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_for_group_top_level() {
        assert_eq!(color_for_group("Drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("DRUMS"), Some(groups::DRUMS));
        assert_eq!(color_for_group("Bass"), Some(groups::BASS));
        assert_eq!(color_for_group("Vocals"), Some(groups::VOCALS));
    }

    #[test]
    fn test_color_for_group_subgroups() {
        assert_eq!(color_for_group("Electric Guitar"), Some(guitars::ELECTRIC));
        assert_eq!(color_for_group("Kick"), Some(drums::KICK));
        assert_eq!(color_for_group("BGVs"), Some(vocals::BACKGROUND));
    }

    #[test]
    fn test_color_for_path() {
        assert_eq!(color_for_path(&["Drums", "Kick"]), Some(drums::KICK));
        assert_eq!(color_for_path(&["Drums"]), Some(groups::DRUMS));
    }

    #[test]
    fn test_color_for_region() {
        assert_eq!(color_for_region("Verse"), Some(guide::VERSE));
        assert_eq!(color_for_region("1. Verse"), Some(guide::VERSE));
        assert_eq!(color_for_region("V1"), Some(guide::VERSE));
        assert_eq!(color_for_region("CHORUS - 8 bars"), Some(guide::CHORUS));
    }

    #[test]
    fn test_hsl_roundtrip() {
        let color = palette::blue::S500;
        let (h, s, l) = rgb_to_hsl(color);
        let restored = hsl_to_rgb(h, s, l);
        // Allow for small rounding errors
        let (r1, g1, b1) = color.to_rgb();
        let (r2, g2, b2) = restored.to_rgb();
        assert!((r1 as i16 - r2 as i16).abs() <= 1);
        assert!((g1 as i16 - g2 as i16).abs() <= 1);
        assert!((b1 as i16 - b2 as i16).abs() <= 1);
    }

    #[test]
    fn test_adjust_lightness() {
        let color = palette::blue::S500;
        let lighter = adjust_lightness(color, 0.2);
        let darker = adjust_lightness(color, -0.2);
        let (_, _, l_orig) = color.to_hsl();
        let (_, _, l_light) = lighter.to_hsl();
        let (_, _, l_dark) = darker.to_hsl();
        assert!(l_light > l_orig);
        assert!(l_dark < l_orig);
    }

    #[test]
    fn test_generate_gradient() {
        let gradient = generate_gradient(Color::BLACK, Color::WHITE, 5);
        assert_eq!(gradient.len(), 5);
        assert_eq!(gradient[0], Color::BLACK);
        assert_eq!(gradient[4], Color::WHITE);
    }
}

//! Track colors based on instrument classification
//!
//! This module provides color definitions for instrument groups using a
//! Tailwind CSS-like color palette system with named colors and shades.
//!
//! Colors are represented as RGB values in 0xRRGGBB format.
//!
//! # Shade Scale
//! - 50: Lightest
//! - 100-400: Light shades
//! - 500: Base/default shade
//! - 600-900: Dark shades
//! - 950: Darkest

use std::collections::HashMap;
use std::sync::LazyLock;

/// RGB color type (0xRRGGBB format)
pub type Color = u32;

// ============================================================================
// Tailwind-style Color Palette
// ============================================================================

/// Tailwind-style color palette with shades from 50 (lightest) to 950 (darkest)
pub mod palette {
    use super::Color;

    /// Red shades (Drums)
    pub mod red {
        use super::Color;
        pub const S50: Color = 0xFEF2F2;
        pub const S100: Color = 0xFEE2E2;
        pub const S200: Color = 0xFECACA;
        pub const S300: Color = 0xFCA5A5;
        pub const S400: Color = 0xF87171;
        pub const S500: Color = 0xEF4444;
        pub const S600: Color = 0xDC2626;
        pub const S700: Color = 0xB91C1C;
        pub const S800: Color = 0x991B1B;
        pub const S900: Color = 0x7F1D1D;
        pub const S950: Color = 0x450A0A;
    }

    /// Orange shades (Percussion)
    pub mod orange {
        use super::Color;
        pub const S50: Color = 0xFFF7ED;
        pub const S100: Color = 0xFFEDD5;
        pub const S200: Color = 0xFED7AA;
        pub const S300: Color = 0xFDBA74;
        pub const S400: Color = 0xFB923C;
        pub const S500: Color = 0xF97316;
        pub const S600: Color = 0xEA580C;
        pub const S700: Color = 0xC2410C;
        pub const S800: Color = 0x9A3412;
        pub const S900: Color = 0x7C2D12;
        pub const S950: Color = 0x431407;
    }

    /// Amber shades (Bass, Horns)
    pub mod amber {
        use super::Color;
        pub const S50: Color = 0xFFFBEB;
        pub const S100: Color = 0xFEF3C7;
        pub const S200: Color = 0xFDE68A;
        pub const S300: Color = 0xFCD34D;
        pub const S400: Color = 0xFBBF24;
        pub const S500: Color = 0xF59E0B;
        pub const S600: Color = 0xD97706;
        pub const S700: Color = 0xB45309;
        pub const S800: Color = 0x92400E;
        pub const S900: Color = 0x78350F;
        pub const S950: Color = 0x451A03;
    }

    /// Yellow shades
    pub mod yellow {
        use super::Color;
        pub const S50: Color = 0xFEFCE8;
        pub const S100: Color = 0xFEF9C3;
        pub const S200: Color = 0xFEF08A;
        pub const S300: Color = 0xFDE047;
        pub const S400: Color = 0xFACC15;
        pub const S500: Color = 0xEAB308;
        pub const S600: Color = 0xCA8A04;
        pub const S700: Color = 0xA16207;
        pub const S800: Color = 0x854D0E;
        pub const S900: Color = 0x713F12;
        pub const S950: Color = 0x422006;
    }

    /// Lime shades
    pub mod lime {
        use super::Color;
        pub const S50: Color = 0xF7FEE7;
        pub const S100: Color = 0xECFCCB;
        pub const S200: Color = 0xD9F99D;
        pub const S300: Color = 0xBEF264;
        pub const S400: Color = 0xA3E635;
        pub const S500: Color = 0x84CC16;
        pub const S600: Color = 0x65A30D;
        pub const S700: Color = 0x4D7C0F;
        pub const S800: Color = 0x3F6212;
        pub const S900: Color = 0x365314;
        pub const S950: Color = 0x1A2E05;
    }

    /// Green shades (Keys)
    pub mod green {
        use super::Color;
        pub const S50: Color = 0xF0FDF4;
        pub const S100: Color = 0xDCFCE7;
        pub const S200: Color = 0xBBF7D0;
        pub const S300: Color = 0x86EFAC;
        pub const S400: Color = 0x4ADE80;
        pub const S500: Color = 0x22C55E;
        pub const S600: Color = 0x16A34A;
        pub const S700: Color = 0x15803D;
        pub const S800: Color = 0x166534;
        pub const S900: Color = 0x14532D;
        pub const S950: Color = 0x052E16;
    }

    /// Emerald shades
    pub mod emerald {
        use super::Color;
        pub const S50: Color = 0xECFDF5;
        pub const S100: Color = 0xD1FAE5;
        pub const S200: Color = 0xA7F3D0;
        pub const S300: Color = 0x6EE7B7;
        pub const S400: Color = 0x34D399;
        pub const S500: Color = 0x10B981;
        pub const S600: Color = 0x059669;
        pub const S700: Color = 0x047857;
        pub const S800: Color = 0x065F46;
        pub const S900: Color = 0x064E3B;
        pub const S950: Color = 0x022C22;
    }

    /// Teal shades (SFX)
    pub mod teal {
        use super::Color;
        pub const S50: Color = 0xF0FDFA;
        pub const S100: Color = 0xCCFBF1;
        pub const S200: Color = 0x99F6E4;
        pub const S300: Color = 0x5EEAD4;
        pub const S400: Color = 0x2DD4BF;
        pub const S500: Color = 0x14B8A6;
        pub const S600: Color = 0x0D9488;
        pub const S700: Color = 0x0F766E;
        pub const S800: Color = 0x115E59;
        pub const S900: Color = 0x134E4A;
        pub const S950: Color = 0x042F2E;
    }

    /// Cyan shades
    pub mod cyan {
        use super::Color;
        pub const S50: Color = 0xECFEFF;
        pub const S100: Color = 0xCFFAFE;
        pub const S200: Color = 0xA5F3FC;
        pub const S300: Color = 0x67E8F9;
        pub const S400: Color = 0x22D3EE;
        pub const S500: Color = 0x06B6D4;
        pub const S600: Color = 0x0891B2;
        pub const S700: Color = 0x0E7490;
        pub const S800: Color = 0x155E75;
        pub const S900: Color = 0x164E63;
        pub const S950: Color = 0x083344;
    }

    /// Sky shades (Guitars)
    pub mod sky {
        use super::Color;
        pub const S50: Color = 0xF0F9FF;
        pub const S100: Color = 0xE0F2FE;
        pub const S200: Color = 0xBAE6FD;
        pub const S300: Color = 0x7DD3FC;
        pub const S400: Color = 0x38BDF8;
        pub const S500: Color = 0x0EA5E9;
        pub const S600: Color = 0x0284C7;
        pub const S700: Color = 0x0369A1;
        pub const S800: Color = 0x075985;
        pub const S900: Color = 0x0C4A6E;
        pub const S950: Color = 0x082F49;
    }

    /// Blue shades
    pub mod blue {
        use super::Color;
        pub const S50: Color = 0xEFF6FF;
        pub const S100: Color = 0xDBEAFE;
        pub const S200: Color = 0xBFDBFE;
        pub const S300: Color = 0x93C5FD;
        pub const S400: Color = 0x60A5FA;
        pub const S500: Color = 0x3B82F6;
        pub const S600: Color = 0x2563EB;
        pub const S700: Color = 0x1D4ED8;
        pub const S800: Color = 0x1E40AF;
        pub const S900: Color = 0x1E3A8A;
        pub const S950: Color = 0x172554;
    }

    /// Indigo shades
    pub mod indigo {
        use super::Color;
        pub const S50: Color = 0xEEF2FF;
        pub const S100: Color = 0xE0E7FF;
        pub const S200: Color = 0xC7D2FE;
        pub const S300: Color = 0xA5B4FC;
        pub const S400: Color = 0x818CF8;
        pub const S500: Color = 0x6366F1;
        pub const S600: Color = 0x4F46E5;
        pub const S700: Color = 0x4338CA;
        pub const S800: Color = 0x3730A3;
        pub const S900: Color = 0x312E81;
        pub const S950: Color = 0x1E1B4B;
    }

    /// Violet shades (Synths)
    pub mod violet {
        use super::Color;
        pub const S50: Color = 0xF5F3FF;
        pub const S100: Color = 0xEDE9FE;
        pub const S200: Color = 0xDDD6FE;
        pub const S300: Color = 0xC4B5FD;
        pub const S400: Color = 0xA78BFA;
        pub const S500: Color = 0x8B5CF6;
        pub const S600: Color = 0x7C3AED;
        pub const S700: Color = 0x6D28D9;
        pub const S800: Color = 0x5B21B6;
        pub const S900: Color = 0x4C1D95;
        pub const S950: Color = 0x2E1065;
    }

    /// Purple shades (Orchestra)
    pub mod purple {
        use super::Color;
        pub const S50: Color = 0xFAF5FF;
        pub const S100: Color = 0xF3E8FF;
        pub const S200: Color = 0xE9D5FF;
        pub const S300: Color = 0xD8B4FE;
        pub const S400: Color = 0xC084FC;
        pub const S500: Color = 0xA855F7;
        pub const S600: Color = 0x9333EA;
        pub const S700: Color = 0x7E22CE;
        pub const S800: Color = 0x6B21A8;
        pub const S900: Color = 0x581C87;
        pub const S950: Color = 0x3B0764;
    }

    /// Fuchsia shades
    pub mod fuchsia {
        use super::Color;
        pub const S50: Color = 0xFDF4FF;
        pub const S100: Color = 0xFAE8FF;
        pub const S200: Color = 0xF5D0FE;
        pub const S300: Color = 0xF0ABFC;
        pub const S400: Color = 0xE879F9;
        pub const S500: Color = 0xD946EF;
        pub const S600: Color = 0xC026D3;
        pub const S700: Color = 0xA21CAF;
        pub const S800: Color = 0x86198F;
        pub const S900: Color = 0x701A75;
        pub const S950: Color = 0x4A044E;
    }

    /// Pink shades (Vocals)
    pub mod pink {
        use super::Color;
        pub const S50: Color = 0xFDF2F8;
        pub const S100: Color = 0xFCE7F3;
        pub const S200: Color = 0xFBCFE8;
        pub const S300: Color = 0xF9A8D4;
        pub const S400: Color = 0xF472B6;
        pub const S500: Color = 0xEC4899;
        pub const S600: Color = 0xDB2777;
        pub const S700: Color = 0xBE185D;
        pub const S800: Color = 0x9D174D;
        pub const S900: Color = 0x831843;
        pub const S950: Color = 0x500724;
    }

    /// Rose shades
    pub mod rose {
        use super::Color;
        pub const S50: Color = 0xFFF1F2;
        pub const S100: Color = 0xFFE4E6;
        pub const S200: Color = 0xFECDD3;
        pub const S300: Color = 0xFDA4AF;
        pub const S400: Color = 0xFB7185;
        pub const S500: Color = 0xF43F5E;
        pub const S600: Color = 0xE11D48;
        pub const S700: Color = 0xBE123C;
        pub const S800: Color = 0x9F1239;
        pub const S900: Color = 0x881337;
        pub const S950: Color = 0x4C0519;
    }

    /// Slate shades (Guide/Reference)
    pub mod slate {
        use super::Color;
        pub const S50: Color = 0xF8FAFC;
        pub const S100: Color = 0xF1F5F9;
        pub const S200: Color = 0xE2E8F0;
        pub const S300: Color = 0xCBD5E1;
        pub const S400: Color = 0x94A3B8;
        pub const S500: Color = 0x64748B;
        pub const S600: Color = 0x475569;
        pub const S700: Color = 0x334155;
        pub const S800: Color = 0x1E293B;
        pub const S900: Color = 0x0F172A;
        pub const S950: Color = 0x020617;
    }

    /// Gray shades
    pub mod gray {
        use super::Color;
        pub const S50: Color = 0xF9FAFB;
        pub const S100: Color = 0xF3F4F6;
        pub const S200: Color = 0xE5E7EB;
        pub const S300: Color = 0xD1D5DB;
        pub const S400: Color = 0x9CA3AF;
        pub const S500: Color = 0x6B7280;
        pub const S600: Color = 0x4B5563;
        pub const S700: Color = 0x374151;
        pub const S800: Color = 0x1F2937;
        pub const S900: Color = 0x111827;
        pub const S950: Color = 0x030712;
    }

    /// Zinc shades
    pub mod zinc {
        use super::Color;
        pub const S50: Color = 0xFAFAFA;
        pub const S100: Color = 0xF4F4F5;
        pub const S200: Color = 0xE4E4E7;
        pub const S300: Color = 0xD4D4D8;
        pub const S400: Color = 0xA1A1AA;
        pub const S500: Color = 0x71717A;
        pub const S600: Color = 0x52525B;
        pub const S700: Color = 0x3F3F46;
        pub const S800: Color = 0x27272A;
        pub const S900: Color = 0x18181B;
        pub const S950: Color = 0x09090B;
    }

    /// Stone shades (Harmonica)
    pub mod stone {
        use super::Color;
        pub const S50: Color = 0xFAFAF9;
        pub const S100: Color = 0xF5F5F4;
        pub const S200: Color = 0xE7E5E4;
        pub const S300: Color = 0xD6D3D1;
        pub const S400: Color = 0xA8A29E;
        pub const S500: Color = 0x78716C;
        pub const S600: Color = 0x57534E;
        pub const S700: Color = 0x44403C;
        pub const S800: Color = 0x292524;
        pub const S900: Color = 0x1C1917;
        pub const S950: Color = 0x0C0A09;
    }
}

// ============================================================================
// Group Color Assignments (using palette)
// ============================================================================

/// Color definitions for top-level instrument groups
pub mod groups {
    use super::Color;
    use super::palette;

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
    use super::Color;
    use super::palette;

    pub const ELECTRIC: Color = palette::sky::S600;
    pub const ACOUSTIC: Color = palette::emerald::S400;
    pub const STEEL: Color = palette::sky::S400;
    pub const BANJO: Color = palette::amber::S600;
}

/// Color definitions for vocal sub-groups
pub mod vocals {
    use super::Color;
    use super::palette;

    pub const LEAD: Color = palette::pink::S500;
    pub const BACKGROUND: Color = palette::pink::S300;
}

/// Color definitions for drum sub-groups
pub mod drums {
    use super::Color;
    use super::palette;

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
    use super::Color;
    use super::palette;

    pub const GUITAR: Color = palette::amber::S500;
    pub const SYNTH: Color = palette::amber::S600;
    pub const UPRIGHT: Color = palette::amber::S700;
}

/// Color definitions for keys sub-groups
pub mod keys {
    use super::Color;
    use super::palette;

    pub const PIANO: Color = palette::green::S500;
    pub const ELECTRIC: Color = palette::green::S600;
    pub const ORGAN: Color = palette::green::S700;
    pub const HARPSICHORD: Color = palette::green::S300;
    pub const CLAVICHORD: Color = palette::green::S400;
}

/// Color definitions for synth sub-groups
pub mod synths {
    use super::Color;
    use super::palette;

    pub const LEAD: Color = palette::violet::S500;
    pub const PAD: Color = palette::violet::S400;
    pub const ARP: Color = palette::violet::S600;
    pub const CHORD: Color = palette::violet::S300;
    pub const KEYS: Color = palette::violet::S700;
    pub const FX: Color = palette::violet::S800;
}

/// Color definitions for orchestra sub-groups
pub mod orchestra {
    use super::Color;
    use super::palette;

    pub const STRINGS: Color = palette::rose::S600;
    pub const WOODWINDS: Color = palette::emerald::S600;
    pub const BRASS: Color = palette::amber::S500;
    pub const HARP: Color = palette::purple::S400;
    pub const PERCUSSION: Color = palette::orange::S600;

    pub mod strings {
        use super::super::Color;
        use super::super::palette;

        pub const VIOLINS: Color = palette::rose::S500;
        pub const VIOLA: Color = palette::rose::S600;
        pub const CELLO: Color = palette::rose::S700;
        pub const CONTRABASS: Color = palette::rose::S800;
    }

    pub mod woodwinds {
        use super::super::Color;
        use super::super::palette;

        pub const FLUTES: Color = palette::emerald::S400;
        pub const OBOES: Color = palette::emerald::S500;
        pub const CLARINETS: Color = palette::emerald::S600;
        pub const BASSOONS: Color = palette::emerald::S700;
        pub const PICCOLO: Color = palette::emerald::S300;
    }

    pub mod brass {
        use super::super::Color;
        use super::super::palette;

        pub const TRUMPETS: Color = palette::amber::S400;
        pub const HORNS: Color = palette::amber::S500;
        pub const TROMBONES: Color = palette::amber::S600;
        pub const TUBA: Color = palette::amber::S700;
    }
}

/// Color definitions for percussion sub-groups
pub mod percussion {
    use super::Color;
    use super::palette;

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
    use super::Color;
    use super::palette;

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
    // Alternative naming
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
/// use dynamic_template::colors::{color_for_group, palette, groups};
///
/// assert_eq!(color_for_group("Drums"), Some(groups::DRUMS));
/// assert_eq!(color_for_group("Electric Guitar"), Some(palette::sky::S600));
/// assert_eq!(color_for_group("BGVs"), Some(palette::pink::S300));
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
/// - "Chorus (x2)"
/// - "V1 - Build"
///
/// # Example
/// ```
/// use dynamic_template::colors::{color_for_region, guide};
///
/// assert_eq!(color_for_region("V1"), Some(guide::VERSE));
/// assert_eq!(color_for_region("CH 2"), Some(guide::CHORUS));
/// assert_eq!(color_for_region("INTRO - 8 bars"), Some(guide::INTRO));
/// assert_eq!(color_for_region("1. Bridge"), Some(guide::BRIDGE));
/// ```
pub fn color_for_region(region_name: &str) -> Option<Color> {
    let normalized = region_name.to_lowercase().trim().to_string();

    // Try exact match first
    if let Some(&color) = COLOR_MAP.get(normalized.as_str()) {
        return Some(color);
    }

    // Try extracting the section identifier using various patterns

    // Pattern 1: Strip leading numbers with dots/dashes (e.g., "1. Verse", "2 - Chorus")
    let without_leading_number = normalized
        .trim_start_matches(|c: char| c.is_ascii_digit() || c == '.')
        .trim_start_matches(|c: char| c == '-' || c == ' ' || c == '.')
        .trim();
    if !without_leading_number.is_empty() && without_leading_number != normalized {
        if let Some(&color) = COLOR_MAP.get(without_leading_number) {
            return Some(color);
        }
    }

    // Pattern 2: Take first word/token before space, dash, parenthesis, or other separator
    let first_token = normalized
        .split(|c: char| c == ' ' || c == '-' || c == '_' || c == '(' || c == '[' || c == ':')
        .next()
        .unwrap_or("")
        .trim();
    if !first_token.is_empty() {
        if let Some(&color) = COLOR_MAP.get(first_token) {
            return Some(color);
        }
    }

    // Pattern 3: Handle abbreviated forms with numbers (e.g., "V1" -> "v1", "CH2" -> try "ch" then "ch2")
    // First try the token as-is (already done above), then try stripping trailing numbers
    let without_trailing_number = first_token.trim_end_matches(|c: char| c.is_ascii_digit());
    if !without_trailing_number.is_empty() && without_trailing_number != first_token {
        if let Some(&color) = COLOR_MAP.get(without_trailing_number) {
            return Some(color);
        }
    }

    // Pattern 4: Try first two words together (e.g., "pre chorus", "post chorus")
    let words: Vec<&str> = normalized
        .split(|c: char| c == ' ' || c == '-' || c == '_')
        .filter(|s| !s.is_empty())
        .take(2)
        .collect();
    if words.len() == 2 {
        let two_words = format!("{} {}", words[0], words[1]);
        if let Some(&color) = COLOR_MAP.get(two_words.as_str()) {
            return Some(color);
        }
    }

    None
}

/// Get the color for a group by its classification path
///
/// The path should be a list of group names from root to leaf,
/// e.g., `["Guitars", "Electric"]` or `["Vocals", "Background"]`.
///
/// Tries to find the most specific color first, falling back to parent colors.
///
/// # Example
/// ```
/// use dynamic_template::colors::{color_for_path, palette, groups, guitars};
///
/// // Returns Electric Guitar color (sky-600)
/// assert_eq!(color_for_path(&["Guitars", "Electric"]), Some(guitars::ELECTRIC));
///
/// // Falls back to Guitars color (sky-500) for unknown sub-group
/// assert_eq!(color_for_path(&["Guitars", "Unknown"]), Some(groups::GUITARS));
/// ```
pub fn color_for_path(path: &[&str]) -> Option<Color> {
    if path.is_empty() {
        return None;
    }

    // Try the full path first (joined with "/")
    let full_path = path.join("/").to_lowercase();
    if let Some(&color) = COLOR_MAP.get(full_path.as_str()) {
        return Some(color);
    }

    // Try the last element (leaf group)
    let leaf = path.last()?.to_lowercase();
    if let Some(&color) = COLOR_MAP.get(leaf.as_str()) {
        return Some(color);
    }

    // Fall back through parent groups
    for i in (0..path.len()).rev() {
        let parent_path = path[..=i].join("/").to_lowercase();
        if let Some(&color) = COLOR_MAP.get(parent_path.as_str()) {
            return Some(color);
        }

        // Also try just the group name at this level
        let group_name = path[i].to_lowercase();
        if let Some(&color) = COLOR_MAP.get(group_name.as_str()) {
            return Some(color);
        }
    }

    None
}

/// Convert a color to REAPER's native format
///
/// REAPER color format varies by platform:
/// - Windows: BGR (0x00BBGGRR)
/// - macOS: RGB (0x00RRGGBB)
pub fn to_reaper_color(color: Color) -> u32 {
    #[cfg(target_os = "windows")]
    {
        // Windows uses BGR
        let r = (color >> 16) & 0xFF;
        let g = (color >> 8) & 0xFF;
        let b = color & 0xFF;
        (b << 16) | (g << 8) | r
    }
    #[cfg(not(target_os = "windows"))]
    {
        // macOS and Linux use RGB
        color
    }
}

/// Convert a color to REAPER's native format with the "custom color" flag set
///
/// REAPER requires bit 24 (0x01000000) to be set for custom colors.
pub fn to_reaper_custom_color(color: Color) -> u32 {
    to_reaper_color(color) | 0x01000000
}

/// Convert a REAPER custom color back to RGB format
///
/// Strips the custom color flag and converts from platform-native format to RGB.
pub fn from_reaper_custom_color(reaper_color: u32) -> Option<Color> {
    // Check if custom color flag is set
    if reaper_color & 0x01000000 == 0 {
        return None;
    }

    // Strip the flag
    let color = reaper_color & 0x00FFFFFF;

    #[cfg(target_os = "windows")]
    {
        // Windows uses BGR, convert to RGB
        let b = (color >> 16) & 0xFF;
        let g = (color >> 8) & 0xFF;
        let r = color & 0xFF;
        Some((r << 16) | (g << 8) | b)
    }
    #[cfg(not(target_os = "windows"))]
    {
        // macOS and Linux use RGB
        Some(color)
    }
}

/// Convert RGB components to a Color
pub fn rgb(r: u8, g: u8, b: u8) -> Color {
    ((r as u32) << 16) | ((g as u32) << 8) | (b as u32)
}

/// Convert a hex string to a Color
///
/// Accepts formats: "#RRGGBB", "RRGGBB", "#RGB", "RGB"
pub fn from_hex(hex: &str) -> Option<Color> {
    let hex = hex.trim_start_matches('#');
    match hex.len() {
        6 => u32::from_str_radix(hex, 16).ok(),
        3 => {
            // Expand shorthand (e.g., "F00" -> "FF0000")
            let chars: Vec<char> = hex.chars().collect();
            let expanded = format!(
                "{}{}{}{}{}{}",
                chars[0], chars[0], chars[1], chars[1], chars[2], chars[2]
            );
            u32::from_str_radix(&expanded, 16).ok()
        }
        _ => None,
    }
}

/// Convert a Color to a hex string (without #)
pub fn to_hex(color: Color) -> String {
    format!("{:06X}", color)
}

/// Parse a Tailwind-style color string like "red-500", "blue-300", etc.
///
/// Supports all palette colors with shades from 50 to 950.
///
/// # Arguments
/// * `color_str` - A string in format "color-shade" (e.g., "red-500", "sky-300")
///
/// # Example
/// ```
/// use dynamic_template::colors::parse_tailwind_color;
///
/// assert_eq!(parse_tailwind_color("red-500"), Some(0xEF4444));
/// assert_eq!(parse_tailwind_color("sky-300"), Some(0x7DD3FC));
/// assert_eq!(parse_tailwind_color("invalid"), None);
/// ```
pub fn parse_tailwind_color(color_str: &str) -> Option<Color> {
    let parts: Vec<&str> = color_str.split('-').collect();
    if parts.len() != 2 {
        return None;
    }

    let color_name = parts[0].to_lowercase();
    let shade: u16 = parts[1].parse().ok()?;

    match color_name.as_str() {
        "red" => match shade {
            50 => Some(palette::red::S50),
            100 => Some(palette::red::S100),
            200 => Some(palette::red::S200),
            300 => Some(palette::red::S300),
            400 => Some(palette::red::S400),
            500 => Some(palette::red::S500),
            600 => Some(palette::red::S600),
            700 => Some(palette::red::S700),
            800 => Some(palette::red::S800),
            900 => Some(palette::red::S900),
            950 => Some(palette::red::S950),
            _ => None,
        },
        "orange" => match shade {
            50 => Some(palette::orange::S50),
            100 => Some(palette::orange::S100),
            200 => Some(palette::orange::S200),
            300 => Some(palette::orange::S300),
            400 => Some(palette::orange::S400),
            500 => Some(palette::orange::S500),
            600 => Some(palette::orange::S600),
            700 => Some(palette::orange::S700),
            800 => Some(palette::orange::S800),
            900 => Some(palette::orange::S900),
            950 => Some(palette::orange::S950),
            _ => None,
        },
        "amber" => match shade {
            50 => Some(palette::amber::S50),
            100 => Some(palette::amber::S100),
            200 => Some(palette::amber::S200),
            300 => Some(palette::amber::S300),
            400 => Some(palette::amber::S400),
            500 => Some(palette::amber::S500),
            600 => Some(palette::amber::S600),
            700 => Some(palette::amber::S700),
            800 => Some(palette::amber::S800),
            900 => Some(palette::amber::S900),
            950 => Some(palette::amber::S950),
            _ => None,
        },
        "yellow" => match shade {
            50 => Some(palette::yellow::S50),
            100 => Some(palette::yellow::S100),
            200 => Some(palette::yellow::S200),
            300 => Some(palette::yellow::S300),
            400 => Some(palette::yellow::S400),
            500 => Some(palette::yellow::S500),
            600 => Some(palette::yellow::S600),
            700 => Some(palette::yellow::S700),
            800 => Some(palette::yellow::S800),
            900 => Some(palette::yellow::S900),
            950 => Some(palette::yellow::S950),
            _ => None,
        },
        "lime" => match shade {
            50 => Some(palette::lime::S50),
            100 => Some(palette::lime::S100),
            200 => Some(palette::lime::S200),
            300 => Some(palette::lime::S300),
            400 => Some(palette::lime::S400),
            500 => Some(palette::lime::S500),
            600 => Some(palette::lime::S600),
            700 => Some(palette::lime::S700),
            800 => Some(palette::lime::S800),
            900 => Some(palette::lime::S900),
            950 => Some(palette::lime::S950),
            _ => None,
        },
        "green" => match shade {
            50 => Some(palette::green::S50),
            100 => Some(palette::green::S100),
            200 => Some(palette::green::S200),
            300 => Some(palette::green::S300),
            400 => Some(palette::green::S400),
            500 => Some(palette::green::S500),
            600 => Some(palette::green::S600),
            700 => Some(palette::green::S700),
            800 => Some(palette::green::S800),
            900 => Some(palette::green::S900),
            950 => Some(palette::green::S950),
            _ => None,
        },
        "emerald" => match shade {
            50 => Some(palette::emerald::S50),
            100 => Some(palette::emerald::S100),
            200 => Some(palette::emerald::S200),
            300 => Some(palette::emerald::S300),
            400 => Some(palette::emerald::S400),
            500 => Some(palette::emerald::S500),
            600 => Some(palette::emerald::S600),
            700 => Some(palette::emerald::S700),
            800 => Some(palette::emerald::S800),
            900 => Some(palette::emerald::S900),
            950 => Some(palette::emerald::S950),
            _ => None,
        },
        "teal" => match shade {
            50 => Some(palette::teal::S50),
            100 => Some(palette::teal::S100),
            200 => Some(palette::teal::S200),
            300 => Some(palette::teal::S300),
            400 => Some(palette::teal::S400),
            500 => Some(palette::teal::S500),
            600 => Some(palette::teal::S600),
            700 => Some(palette::teal::S700),
            800 => Some(palette::teal::S800),
            900 => Some(palette::teal::S900),
            950 => Some(palette::teal::S950),
            _ => None,
        },
        "cyan" => match shade {
            50 => Some(palette::cyan::S50),
            100 => Some(palette::cyan::S100),
            200 => Some(palette::cyan::S200),
            300 => Some(palette::cyan::S300),
            400 => Some(palette::cyan::S400),
            500 => Some(palette::cyan::S500),
            600 => Some(palette::cyan::S600),
            700 => Some(palette::cyan::S700),
            800 => Some(palette::cyan::S800),
            900 => Some(palette::cyan::S900),
            950 => Some(palette::cyan::S950),
            _ => None,
        },
        "sky" => match shade {
            50 => Some(palette::sky::S50),
            100 => Some(palette::sky::S100),
            200 => Some(palette::sky::S200),
            300 => Some(palette::sky::S300),
            400 => Some(palette::sky::S400),
            500 => Some(palette::sky::S500),
            600 => Some(palette::sky::S600),
            700 => Some(palette::sky::S700),
            800 => Some(palette::sky::S800),
            900 => Some(palette::sky::S900),
            950 => Some(palette::sky::S950),
            _ => None,
        },
        "blue" => match shade {
            50 => Some(palette::blue::S50),
            100 => Some(palette::blue::S100),
            200 => Some(palette::blue::S200),
            300 => Some(palette::blue::S300),
            400 => Some(palette::blue::S400),
            500 => Some(palette::blue::S500),
            600 => Some(palette::blue::S600),
            700 => Some(palette::blue::S700),
            800 => Some(palette::blue::S800),
            900 => Some(palette::blue::S900),
            950 => Some(palette::blue::S950),
            _ => None,
        },
        "indigo" => match shade {
            50 => Some(palette::indigo::S50),
            100 => Some(palette::indigo::S100),
            200 => Some(palette::indigo::S200),
            300 => Some(palette::indigo::S300),
            400 => Some(palette::indigo::S400),
            500 => Some(palette::indigo::S500),
            600 => Some(palette::indigo::S600),
            700 => Some(palette::indigo::S700),
            800 => Some(palette::indigo::S800),
            900 => Some(palette::indigo::S900),
            950 => Some(palette::indigo::S950),
            _ => None,
        },
        "violet" => match shade {
            50 => Some(palette::violet::S50),
            100 => Some(palette::violet::S100),
            200 => Some(palette::violet::S200),
            300 => Some(palette::violet::S300),
            400 => Some(palette::violet::S400),
            500 => Some(palette::violet::S500),
            600 => Some(palette::violet::S600),
            700 => Some(palette::violet::S700),
            800 => Some(palette::violet::S800),
            900 => Some(palette::violet::S900),
            950 => Some(palette::violet::S950),
            _ => None,
        },
        "purple" => match shade {
            50 => Some(palette::purple::S50),
            100 => Some(palette::purple::S100),
            200 => Some(palette::purple::S200),
            300 => Some(palette::purple::S300),
            400 => Some(palette::purple::S400),
            500 => Some(palette::purple::S500),
            600 => Some(palette::purple::S600),
            700 => Some(palette::purple::S700),
            800 => Some(palette::purple::S800),
            900 => Some(palette::purple::S900),
            950 => Some(palette::purple::S950),
            _ => None,
        },
        "fuchsia" => match shade {
            50 => Some(palette::fuchsia::S50),
            100 => Some(palette::fuchsia::S100),
            200 => Some(palette::fuchsia::S200),
            300 => Some(palette::fuchsia::S300),
            400 => Some(palette::fuchsia::S400),
            500 => Some(palette::fuchsia::S500),
            600 => Some(palette::fuchsia::S600),
            700 => Some(palette::fuchsia::S700),
            800 => Some(palette::fuchsia::S800),
            900 => Some(palette::fuchsia::S900),
            950 => Some(palette::fuchsia::S950),
            _ => None,
        },
        "pink" => match shade {
            50 => Some(palette::pink::S50),
            100 => Some(palette::pink::S100),
            200 => Some(palette::pink::S200),
            300 => Some(palette::pink::S300),
            400 => Some(palette::pink::S400),
            500 => Some(palette::pink::S500),
            600 => Some(palette::pink::S600),
            700 => Some(palette::pink::S700),
            800 => Some(palette::pink::S800),
            900 => Some(palette::pink::S900),
            950 => Some(palette::pink::S950),
            _ => None,
        },
        "rose" => match shade {
            50 => Some(palette::rose::S50),
            100 => Some(palette::rose::S100),
            200 => Some(palette::rose::S200),
            300 => Some(palette::rose::S300),
            400 => Some(palette::rose::S400),
            500 => Some(palette::rose::S500),
            600 => Some(palette::rose::S600),
            700 => Some(palette::rose::S700),
            800 => Some(palette::rose::S800),
            900 => Some(palette::rose::S900),
            950 => Some(palette::rose::S950),
            _ => None,
        },
        "slate" => match shade {
            50 => Some(palette::slate::S50),
            100 => Some(palette::slate::S100),
            200 => Some(palette::slate::S200),
            300 => Some(palette::slate::S300),
            400 => Some(palette::slate::S400),
            500 => Some(palette::slate::S500),
            600 => Some(palette::slate::S600),
            700 => Some(palette::slate::S700),
            800 => Some(palette::slate::S800),
            900 => Some(palette::slate::S900),
            950 => Some(palette::slate::S950),
            _ => None,
        },
        "gray" => match shade {
            50 => Some(palette::gray::S50),
            100 => Some(palette::gray::S100),
            200 => Some(palette::gray::S200),
            300 => Some(palette::gray::S300),
            400 => Some(palette::gray::S400),
            500 => Some(palette::gray::S500),
            600 => Some(palette::gray::S600),
            700 => Some(palette::gray::S700),
            800 => Some(palette::gray::S800),
            900 => Some(palette::gray::S900),
            950 => Some(palette::gray::S950),
            _ => None,
        },
        "zinc" => match shade {
            50 => Some(palette::zinc::S50),
            100 => Some(palette::zinc::S100),
            200 => Some(palette::zinc::S200),
            300 => Some(palette::zinc::S300),
            400 => Some(palette::zinc::S400),
            500 => Some(palette::zinc::S500),
            600 => Some(palette::zinc::S600),
            700 => Some(palette::zinc::S700),
            800 => Some(palette::zinc::S800),
            900 => Some(palette::zinc::S900),
            950 => Some(palette::zinc::S950),
            _ => None,
        },
        "stone" => match shade {
            50 => Some(palette::stone::S50),
            100 => Some(palette::stone::S100),
            200 => Some(palette::stone::S200),
            300 => Some(palette::stone::S300),
            400 => Some(palette::stone::S400),
            500 => Some(palette::stone::S500),
            600 => Some(palette::stone::S600),
            700 => Some(palette::stone::S700),
            800 => Some(palette::stone::S800),
            900 => Some(palette::stone::S900),
            950 => Some(palette::stone::S950),
            _ => None,
        },
        _ => None,
    }
}

/// Get all available shade values for the palette
pub const SHADES: [u16; 11] = [50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950];

/// Get all available color names in the palette
pub const COLOR_NAMES: [&str; 22] = [
    "red", "orange", "amber", "yellow", "lime", "green", "emerald", "teal", "cyan", "sky", "blue",
    "indigo", "violet", "purple", "fuchsia", "pink", "rose", "slate", "gray", "zinc", "stone",
    "neutral",
];

/// Get a shade from a color by index (0-10, where 0=50, 5=500, 10=950)
///
/// This is useful for generating related shades programmatically.
///
/// # Example
/// ```
/// use dynamic_template::colors::get_shade;
///
/// // Get the 500 shade (index 5) of red
/// let red_500 = get_shade("red", 5);
/// assert_eq!(red_500, Some(0xEF4444));
/// ```
pub fn get_shade(color_name: &str, shade_index: usize) -> Option<Color> {
    if shade_index >= SHADES.len() {
        return None;
    }
    let shade = SHADES[shade_index];
    parse_tailwind_color(&format!("{}-{}", color_name, shade))
}

// === Gradient Functions ===

/// Direction for gradient generation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GradientDirection {
    /// Gradient goes from base color toward white (lighter)
    Lighten,
    /// Gradient goes from base color toward black (darker)
    Darken,
    /// Gradient goes from lighter to darker
    LightToDark,
    /// Gradient goes from darker to lighter
    DarkToLight,
}

impl Default for GradientDirection {
    fn default() -> Self {
        Self::Darken
    }
}

/// Calculate a gradient color between two colors at a given position.
///
/// This uses linear RGB interpolation (same as SWS CalcGradient).
///
/// # Arguments
/// * `start` - Starting color (position 0.0)
/// * `end` - Ending color (position 1.0)
/// * `position` - Position in gradient (0.0 to 1.0)
///
/// # Example
/// ```
/// use dynamic_template::colors::calc_gradient;
///
/// let red = 0xFF0000;
/// let blue = 0x0000FF;
/// let purple = calc_gradient(red, blue, 0.5); // Mid-point between red and blue
/// ```
pub fn calc_gradient(start: Color, end: Color, position: f64) -> Color {
    let position = position.clamp(0.0, 1.0);

    let start_r = ((start >> 16) & 0xFF) as f64;
    let start_g = ((start >> 8) & 0xFF) as f64;
    let start_b = (start & 0xFF) as f64;

    let end_r = ((end >> 16) & 0xFF) as f64;
    let end_g = ((end >> 8) & 0xFF) as f64;
    let end_b = (end & 0xFF) as f64;

    let r = (start_r + (end_r - start_r) * position) as u32;
    let g = (start_g + (end_g - start_g) * position) as u32;
    let b = (start_b + (end_b - start_b) * position) as u32;

    (r << 16) | (g << 8) | b
}

/// Generate a gradient of colors for child tracks based on a parent color.
///
/// This is similar to how SWS handles gradient children.
///
/// # Arguments
/// * `base` - The parent/base color
/// * `count` - Number of gradient colors to generate
/// * `direction` - Direction of the gradient
/// * `strength` - How much to vary from base (0.0 to 1.0, default 0.5)
///
/// # Example
/// ```
/// use dynamic_template::colors::{generate_gradient, GradientDirection};
///
/// let drums_color = 0xE74C3C; // Red
/// let child_colors = generate_gradient(drums_color, 5, GradientDirection::Darken, 0.5);
/// // Returns 5 colors from drums_color progressively darker
/// ```
pub fn generate_gradient(
    base: Color,
    count: usize,
    direction: GradientDirection,
    strength: f64,
) -> Vec<Color> {
    if count == 0 {
        return vec![];
    }
    if count == 1 {
        return vec![base];
    }

    let strength = strength.clamp(0.0, 1.0);

    // Calculate end color based on direction
    let (start, end) = match direction {
        GradientDirection::Lighten => {
            // Start at base, end at lightened version
            let white = 0xFFFFFF;
            (base, calc_gradient(base, white, strength))
        }
        GradientDirection::Darken => {
            // Start at base, end at darkened version
            let black = 0x000000;
            (base, calc_gradient(base, black, strength))
        }
        GradientDirection::LightToDark => {
            // Start at lightened, end at darkened
            let white = 0xFFFFFF;
            let black = 0x000000;
            (
                calc_gradient(base, white, strength * 0.5),
                calc_gradient(base, black, strength * 0.5),
            )
        }
        GradientDirection::DarkToLight => {
            // Start at darkened, end at lightened
            let white = 0xFFFFFF;
            let black = 0x000000;
            (
                calc_gradient(base, black, strength * 0.5),
                calc_gradient(base, white, strength * 0.5),
            )
        }
    };

    // Generate gradient colors
    (0..count)
        .map(|i| {
            let position = i as f64 / (count - 1) as f64;
            calc_gradient(start, end, position)
        })
        .collect()
}

/// Generate gradient colors for folder children, where the first child
/// matches the parent and subsequent children get progressively different.
///
/// This is useful for auto-coloring where the folder track gets a base color
/// and all child tracks get gradient variations.
///
/// # Arguments
/// * `parent_color` - The folder/parent track color
/// * `child_count` - Number of child tracks
/// * `direction` - Direction of the gradient
///
/// # Returns
/// A vector of colors, one for each child track
pub fn generate_child_gradient(
    parent_color: Color,
    child_count: usize,
    direction: GradientDirection,
) -> Vec<Color> {
    if child_count == 0 {
        return vec![];
    }
    if child_count == 1 {
        // Single child gets same color as parent
        return vec![parent_color];
    }

    // Use moderate strength (0.4) so children are related but distinct
    generate_gradient(parent_color, child_count, direction, 0.4)
}

/// Adjust a color's brightness (simple lightness adjustment)
///
/// # Arguments
/// * `color` - The color to adjust
/// * `factor` - Brightness factor (< 1.0 darkens, > 1.0 lightens, 1.0 = no change)
pub fn adjust_brightness(color: Color, factor: f64) -> Color {
    let r = ((color >> 16) & 0xFF) as f64;
    let g = ((color >> 8) & 0xFF) as f64;
    let b = (color & 0xFF) as f64;

    let r = (r * factor).clamp(0.0, 255.0) as u32;
    let g = (g * factor).clamp(0.0, 255.0) as u32;
    let b = (b * factor).clamp(0.0, 255.0) as u32;

    (r << 16) | (g << 8) | b
}

/// Convert RGB to HSL (Hue, Saturation, Lightness)
///
/// Returns (H, S, L) where:
/// - H is in degrees (0-360)
/// - S is 0.0-1.0
/// - L is 0.0-1.0
pub fn rgb_to_hsl(color: Color) -> (f64, f64, f64) {
    let r = ((color >> 16) & 0xFF) as f64 / 255.0;
    let g = ((color >> 8) & 0xFF) as f64 / 255.0;
    let b = (color & 0xFF) as f64 / 255.0;

    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let l = (max + min) / 2.0;

    if (max - min).abs() < f64::EPSILON {
        // Achromatic (gray)
        return (0.0, 0.0, l);
    }

    let d = max - min;
    let s = if l > 0.5 {
        d / (2.0 - max - min)
    } else {
        d / (max + min)
    };

    let h = if (max - r).abs() < f64::EPSILON {
        let h = (g - b) / d;
        if g < b { h + 6.0 } else { h }
    } else if (max - g).abs() < f64::EPSILON {
        (b - r) / d + 2.0
    } else {
        (r - g) / d + 4.0
    };

    (h * 60.0, s, l)
}

/// Convert HSL to RGB
///
/// # Arguments
/// * `h` - Hue in degrees (0-360)
/// * `s` - Saturation (0.0-1.0)
/// * `l` - Lightness (0.0-1.0)
pub fn hsl_to_rgb(h: f64, s: f64, l: f64) -> Color {
    if s.abs() < f64::EPSILON {
        // Achromatic (gray)
        let v = (l * 255.0) as u32;
        return (v << 16) | (v << 8) | v;
    }

    let q = if l < 0.5 {
        l * (1.0 + s)
    } else {
        l + s - l * s
    };
    let p = 2.0 * l - q;

    let h = h / 360.0;

    fn hue_to_rgb(p: f64, q: f64, mut t: f64) -> f64 {
        if t < 0.0 {
            t += 1.0;
        }
        if t > 1.0 {
            t -= 1.0;
        }
        if t < 1.0 / 6.0 {
            return p + (q - p) * 6.0 * t;
        }
        if t < 0.5 {
            return q;
        }
        if t < 2.0 / 3.0 {
            return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
        }
        p
    }

    let r = (hue_to_rgb(p, q, h + 1.0 / 3.0) * 255.0) as u32;
    let g = (hue_to_rgb(p, q, h) * 255.0) as u32;
    let b = (hue_to_rgb(p, q, h - 1.0 / 3.0) * 255.0) as u32;

    (r << 16) | (g << 8) | b
}

/// Adjust a color's lightness using HSL color space
///
/// This provides better perceptual results than simple RGB brightness adjustment.
///
/// # Arguments
/// * `color` - The color to adjust
/// * `delta` - Lightness delta (-1.0 to 1.0, negative darkens, positive lightens)
pub fn adjust_lightness(color: Color, delta: f64) -> Color {
    let (h, s, l) = rgb_to_hsl(color);
    let new_l = (l + delta).clamp(0.0, 1.0);
    hsl_to_rgb(h, s, new_l)
}

/// Generate a gradient using HSL interpolation for more natural color transitions
pub fn generate_hsl_gradient(base: Color, count: usize, lightness_range: (f64, f64)) -> Vec<Color> {
    if count == 0 {
        return vec![];
    }
    if count == 1 {
        return vec![base];
    }

    let (h, s, _) = rgb_to_hsl(base);
    let (start_l, end_l) = lightness_range;

    (0..count)
        .map(|i| {
            let position = i as f64 / (count - 1) as f64;
            let l = start_l + (end_l - start_l) * position;
            hsl_to_rgb(h, s, l.clamp(0.0, 1.0))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_for_group_top_level() {
        assert_eq!(color_for_group("Drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("drums"), Some(groups::DRUMS));
        assert_eq!(color_for_group("DRUMS"), Some(groups::DRUMS));
        assert_eq!(color_for_group("Guitars"), Some(groups::GUITARS));
        assert_eq!(color_for_group("Vocals"), Some(groups::VOCALS));
    }

    #[test]
    fn test_color_for_group_subgroups() {
        assert_eq!(color_for_group("Electric Guitar"), Some(guitars::ELECTRIC));
        assert_eq!(color_for_group("electric_guitar"), Some(guitars::ELECTRIC));
        assert_eq!(color_for_group("Acoustic Guitar"), Some(guitars::ACOUSTIC));
        assert_eq!(color_for_group("BGVs"), Some(vocals::BACKGROUND));
        assert_eq!(color_for_group("Lead Vocals"), Some(vocals::LEAD));
    }

    #[test]
    fn test_color_for_path() {
        // Full path
        assert_eq!(
            color_for_path(&["Guitars", "Electric"]),
            Some(guitars::ELECTRIC)
        );

        // Falls back to parent
        assert_eq!(
            color_for_path(&["Guitars", "SomeUnknownType"]),
            Some(groups::GUITARS)
        );

        // Top level only
        assert_eq!(color_for_path(&["Drums"]), Some(groups::DRUMS));

        // Empty path
        assert_eq!(color_for_path(&[]), None);
    }

    #[test]
    fn test_reaper_color_conversion() {
        let red = 0xFF0000;
        let blue = 0x0000FF;

        #[cfg(target_os = "windows")]
        {
            // Windows: RGB -> BGR swap
            assert_eq!(to_reaper_color(red), 0x0000FF);
            assert_eq!(to_reaper_color(blue), 0xFF0000);
        }
        #[cfg(not(target_os = "windows"))]
        {
            // macOS/Linux: RGB stays RGB
            assert_eq!(to_reaper_color(red), 0xFF0000);
            assert_eq!(to_reaper_color(blue), 0x0000FF);
        }

        // Custom color flag
        let color = 0x123456;
        let reaper_color = to_reaper_custom_color(color);
        assert!(reaper_color & 0x01000000 != 0);

        // Round-trip conversion
        let original = 0xABCDEF;
        let reaper = to_reaper_custom_color(original);
        let back = from_reaper_custom_color(reaper);
        assert_eq!(back, Some(original));
    }

    #[test]
    fn test_hex_conversion() {
        assert_eq!(from_hex("#FF0000"), Some(0xFF0000));
        assert_eq!(from_hex("FF0000"), Some(0xFF0000));
        assert_eq!(from_hex("#F00"), Some(0xFF0000));
        assert_eq!(from_hex("F00"), Some(0xFF0000));
        assert_eq!(to_hex(0xFF0000), "FF0000");
    }

    #[test]
    fn test_rgb() {
        assert_eq!(rgb(255, 0, 0), 0xFF0000);
        assert_eq!(rgb(0, 255, 0), 0x00FF00);
        assert_eq!(rgb(0, 0, 255), 0x0000FF);
        assert_eq!(rgb(18, 52, 86), 0x123456);
    }

    #[test]
    fn test_calc_gradient() {
        let red = 0xFF0000;
        let blue = 0x0000FF;

        // Start color
        assert_eq!(calc_gradient(red, blue, 0.0), red);

        // End color
        assert_eq!(calc_gradient(red, blue, 1.0), blue);

        // Mid-point (should be purple-ish)
        let mid = calc_gradient(red, blue, 0.5);
        let mid_r = (mid >> 16) & 0xFF;
        let mid_b = mid & 0xFF;
        assert!(mid_r > 100 && mid_r < 150); // ~127
        assert!(mid_b > 100 && mid_b < 150); // ~127
    }

    #[test]
    fn test_generate_gradient() {
        let red = 0xFF0000;

        // Single color
        let single = generate_gradient(red, 1, GradientDirection::Darken, 0.5);
        assert_eq!(single.len(), 1);
        assert_eq!(single[0], red);

        // Multiple colors
        let colors = generate_gradient(red, 5, GradientDirection::Darken, 0.5);
        assert_eq!(colors.len(), 5);
        // First should be base color
        assert_eq!(colors[0], red);
        // Last should be darker
        let last = colors[4];
        let last_r = (last >> 16) & 0xFF;
        let base_r = (red >> 16) & 0xFF;
        assert!(last_r < base_r);
    }

    #[test]
    fn test_hsl_roundtrip() {
        let colors = [0xFF0000, 0x00FF00, 0x0000FF, 0xFFFF00, 0xFF00FF, 0x808080];

        for &color in &colors {
            let (h, s, l) = rgb_to_hsl(color);
            let back = hsl_to_rgb(h, s, l);

            // Allow small rounding errors
            let r1 = (color >> 16) & 0xFF;
            let r2 = (back >> 16) & 0xFF;
            let g1 = (color >> 8) & 0xFF;
            let g2 = (back >> 8) & 0xFF;
            let b1 = color & 0xFF;
            let b2 = back & 0xFF;

            assert!((r1 as i32 - r2 as i32).abs() <= 1);
            assert!((g1 as i32 - g2 as i32).abs() <= 1);
            assert!((b1 as i32 - b2 as i32).abs() <= 1);
        }
    }

    #[test]
    fn test_adjust_lightness() {
        let red = 0xFF0000;

        // Lighten
        let lighter = adjust_lightness(red, 0.2);
        let (_, _, l_lighter) = rgb_to_hsl(lighter);
        let (_, _, l_original) = rgb_to_hsl(red);
        assert!(l_lighter > l_original);

        // Darken
        let darker = adjust_lightness(red, -0.2);
        let (_, _, l_darker) = rgb_to_hsl(darker);
        assert!(l_darker < l_original);
    }

    #[test]
    fn test_parse_tailwind_color() {
        // Valid colors
        assert_eq!(parse_tailwind_color("red-500"), Some(palette::red::S500));
        assert_eq!(parse_tailwind_color("blue-300"), Some(palette::blue::S300));
        assert_eq!(parse_tailwind_color("sky-400"), Some(palette::sky::S400));
        assert_eq!(parse_tailwind_color("slate-50"), Some(palette::slate::S50));
        assert_eq!(
            parse_tailwind_color("stone-950"),
            Some(palette::stone::S950)
        );

        // Case insensitive
        assert_eq!(parse_tailwind_color("RED-500"), Some(palette::red::S500));
        assert_eq!(parse_tailwind_color("Sky-300"), Some(palette::sky::S300));

        // Invalid formats
        assert_eq!(parse_tailwind_color("red"), None);
        assert_eq!(parse_tailwind_color("red-"), None);
        assert_eq!(parse_tailwind_color("-500"), None);
        assert_eq!(parse_tailwind_color("invalid-500"), None);
        assert_eq!(parse_tailwind_color("red-999"), None); // Invalid shade
        assert_eq!(parse_tailwind_color("red-500-extra"), None);
    }

    #[test]
    fn test_get_shade() {
        // Valid shade indices
        assert_eq!(get_shade("red", 0), Some(palette::red::S50));
        assert_eq!(get_shade("red", 5), Some(palette::red::S500));
        assert_eq!(get_shade("red", 10), Some(palette::red::S950));

        // Invalid index
        assert_eq!(get_shade("red", 11), None);
        assert_eq!(get_shade("red", 100), None);

        // Invalid color name
        assert_eq!(get_shade("invalid", 5), None);
    }

    #[test]
    fn test_color_for_region() {
        // Exact matches
        assert_eq!(color_for_region("verse"), Some(guide::VERSE));
        assert_eq!(color_for_region("chorus"), Some(guide::CHORUS));
        assert_eq!(color_for_region("bridge"), Some(guide::BRIDGE));

        // Abbreviations
        assert_eq!(color_for_region("v1"), Some(guide::VERSE));
        assert_eq!(color_for_region("vs2"), Some(guide::VERSE));
        assert_eq!(color_for_region("ch"), Some(guide::CHORUS));
        assert_eq!(color_for_region("br"), Some(guide::BRIDGE));
        assert_eq!(color_for_region("inst"), Some(guide::INSTRUMENTAL));

        // With separators and extra info
        assert_eq!(color_for_region("V1 - Build"), Some(guide::VERSE));
        assert_eq!(color_for_region("CH 2"), Some(guide::CHORUS));
        assert_eq!(color_for_region("INTRO - 8 bars"), Some(guide::INTRO));
        assert_eq!(color_for_region("Chorus (x2)"), Some(guide::CHORUS));

        // Leading numbers
        assert_eq!(color_for_region("1. Verse"), Some(guide::VERSE));
        assert_eq!(color_for_region("2 - Chorus"), Some(guide::CHORUS));
        assert_eq!(color_for_region("3. Bridge"), Some(guide::BRIDGE));

        // Case insensitivity
        assert_eq!(color_for_region("VERSE"), Some(guide::VERSE));
        assert_eq!(color_for_region("Chorus"), Some(guide::CHORUS));
        assert_eq!(color_for_region("BR"), Some(guide::BRIDGE));

        // Pre-chorus and post-chorus
        assert_eq!(color_for_region("pre chorus"), Some(guide::PRE_CHORUS));
        assert_eq!(color_for_region("pre-chorus"), Some(guide::PRE_CHORUS));
        assert_eq!(color_for_region("pc"), Some(guide::PRE_CHORUS));
        assert_eq!(color_for_region("post chorus"), Some(guide::POST_CHORUS));

        // Unknown regions
        assert_eq!(color_for_region("random text"), None);
        assert_eq!(color_for_region("xyz123"), None);
    }
}

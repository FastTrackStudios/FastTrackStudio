//! Default top-level guitar presets.
//!
//! Each function returns a [`Preset`] with module assignments pointing to
//! module presets from [`super::modules`]. These are the "starting point"
//! presets that profiles and songs reference.

use crate::category::{BaseTone, Genre, PresetCategory};
use crate::module::ModuleType;
use crate::module_preset::{ModuleAssignment, ModulePreset};
use crate::normalized::Order;
use crate::preset::{Credit, Inspiration, Preset};

/// Helper to build a module assignment from a preset reference.
fn assign(module_preset: &ModulePreset, order: u8) -> ModuleAssignment {
    ModuleAssignment::new(module_preset.module_type, module_preset.id, Order::new(order))
}

/// Helper to build a module assignment with a named snapshot.
fn assign_snap(module_preset: &ModulePreset, order: u8, snap_name: &str) -> ModuleAssignment {
    let snap_id = module_preset
        .snapshot_by_name(snap_name)
        .expect("snapshot not found")
        .id;
    ModuleAssignment::new(module_preset.module_type, module_preset.id, Order::new(order))
        .with_snapshot(snap_id)
}

/// All module presets needed for the default guitar rig, bundled together.
///
/// Callers create this once and pass references when building presets,
/// profiles, and songs so they all share the same IDs.
pub struct GuitarModulePresets {
    pub source: ModulePreset,
    pub dynamics: ModulePreset,
    pub envelope_filter: ModulePreset,
    pub wah_pedal: ModulePreset,
    pub pitch_octave: ModulePreset,
    pub doubler: ModulePreset,
    pub blues_stack: ModulePreset,
    pub protein_kilt: ModulePreset,
    pub gravity_tank: ModulePreset,
    pub dream_ruby: ModulePreset,
    pub deluxe_ac30: ModulePreset,
    pub dumble_two_rock: ModulePreset,
    pub marshall_stack: ModulePreset,
    pub chorus: ModulePreset,
    pub flanger: ModulePreset,
    pub phaser: ModulePreset,
    pub delay: ModulePreset,
    pub reverb: ModulePreset,
    pub freeze: ModulePreset,
    pub tremolo_8th: ModulePreset,
    pub tremolo_16th: ModulePreset,
    pub vibrato: ModulePreset,
    pub rotary: ModulePreset,
    pub too_much_to_drink: ModulePreset,
    pub master: ModulePreset,
}

impl Default for GuitarModulePresets {
    fn default() -> Self {
        Self::new()
    }
}

impl GuitarModulePresets {
    /// Build all default guitar module presets.
    pub fn new() -> Self {
        use super::modules::{source, dynamics, special, drive, prefx, amp, modulation, time, motion, master};

        Self {
            source: source::guitar_input(),
            dynamics: dynamics::compressor(),
            envelope_filter: special::envelope_filter(),
            wah_pedal: special::wah_pedal(),
            pitch_octave: special::pitch_octave(),
            doubler: special::doubler(),
            blues_stack: drive::blues_stack(),
            protein_kilt: drive::protein_kilt(),
            gravity_tank: prefx::gravity_tank(),
            dream_ruby: amp::dream_and_ruby(),
            deluxe_ac30: amp::deluxe_and_ac30(),
            dumble_two_rock: amp::dumble_and_two_rock(),
            marshall_stack: amp::marshall_stack(),
            chorus: modulation::chorus(),
            flanger: modulation::flanger(),
            phaser: modulation::phaser(),
            delay: time::delay(),
            reverb: time::reverb(),
            freeze: time::freeze(),
            tremolo_8th: motion::tremolo_8th(),
            tremolo_16th: motion::tremolo_16th(),
            vibrato: motion::vibrato(),
            rotary: motion::rotary(),
            too_much_to_drink: motion::too_much_to_drink(),
            master: master::master(),
        }
    }

    /// Collect all module presets into a Vec (for adding to a Rig).
    pub fn all(&self) -> Vec<&ModulePreset> {
        vec![
            &self.source,
            &self.dynamics,
            &self.envelope_filter,
            &self.wah_pedal,
            &self.pitch_octave,
            &self.doubler,
            &self.blues_stack,
            &self.protein_kilt,
            &self.gravity_tank,
            &self.dream_ruby,
            &self.deluxe_ac30,
            &self.dumble_two_rock,
            &self.marshall_stack,
            &self.chorus,
            &self.flanger,
            &self.phaser,
            &self.delay,
            &self.reverb,
            &self.freeze,
            &self.tremolo_8th,
            &self.tremolo_16th,
            &self.vibrato,
            &self.rotary,
            &self.too_much_to_drink,
            &self.master,
        ]
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Top-level Presets
// ─────────────────────────────────────────────────────────────────────────────

/// "AC30 Ambient Clean" — Amp=Dream+Ruby/Clean, Time=Reverb, Mod=Chorus.
pub fn ac30_ambient_clean(m: &GuitarModulePresets) -> Preset {
    let mut preset = Preset::new(
        "AC30 Ambient Clean",
        PresetCategory::Generic { base_tone: BaseTone::Clean },
    );
    preset.description = Some("Lush clean tone with reverb and chorus shimmer".into());

    preset.add_module_assignment(assign(&m.source, 0));
    preset.add_module_assignment(assign(&m.dynamics, 1));
    preset.add_module_assignment(assign_snap(&m.dream_ruby, 6, "Clean"));
    preset.add_module_assignment(assign(&m.chorus, 8));
    preset.add_module_assignment(assign(&m.reverb, 9));
    preset.add_module_assignment(assign(&m.master, 11));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Ambient Worship Tones")
            .with_source("Hillsong, Bethel")
            .with_notes("AC30 clean with lush reverb and subtle chorus"),
    );

    preset
}

/// "Tremolo Swells" — Amp=Dream+Ruby/Clean, Motion=Tremolo 8th.
pub fn tremolo_swells(m: &GuitarModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Tremolo Swells",
        PresetCategory::Generic { base_tone: BaseTone::Ambient },
    );
    preset.description = Some("Clean amp with rhythmic tremolo pulsing".into());

    preset.add_module_assignment(assign(&m.source, 0));
    preset.add_module_assignment(assign(&m.dynamics, 1));
    preset.add_module_assignment(assign_snap(&m.dream_ruby, 6, "Clean"));
    preset.add_module_assignment(assign(&m.tremolo_8th, 10));
    preset.add_module_assignment(assign(&m.master, 11));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Vintage Tremolo")
            .with_notes("Fender-style harmonic tremolo swells"),
    );

    preset
}

/// "80's Drive" — Drive=Blues Stack/Halfman+Teal, Amp=Deluxe+AC30/Breakup.
pub fn eighties_drive(m: &GuitarModulePresets) -> Preset {
    let mut preset = Preset::new(
        "80's Drive",
        PresetCategory::Genre {
            base_tone: BaseTone::Drive,
            genre: Genre::Rock,
        },
    );
    preset.description = Some("Classic 80s drive with stacked overdrives into AC30 breakup".into());

    preset.add_module_assignment(assign(&m.source, 0));
    preset.add_module_assignment(assign(&m.dynamics, 1));
    preset.add_module_assignment(assign_snap(&m.blues_stack, 4, "Halfman + Teal"));
    preset.add_module_assignment(assign_snap(&m.deluxe_ac30, 6, "Breakup"));
    preset.add_module_assignment(assign(&m.delay, 9));
    preset.add_module_assignment(assign(&m.master, 11));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("80s Rock Tones")
            .with_source("Van Halen, Def Leppard")
            .with_notes("Stacked Klon + TS into an edge-of-breakup AC30"),
    );

    preset
}

/// "Stank" — Drive=Protein+Kilt/Blue+Green, Amp=Marshall/Drive.
pub fn stank(m: &GuitarModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Stank",
        PresetCategory::Genre {
            base_tone: BaseTone::Drive,
            genre: Genre::Rock,
        },
    );
    preset.description = Some("Nasty high-gain tone with Protein and Kilt into Marshall".into());

    preset.add_module_assignment(assign(&m.source, 0));
    preset.add_module_assignment(assign(&m.dynamics, 1));
    preset.add_module_assignment(assign_snap(&m.protein_kilt, 4, "Blue + Green"));
    preset.add_module_assignment(assign_snap(&m.marshall_stack, 6, "Drive"));
    preset.add_module_assignment(assign(&m.master, 11));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("High-Gain Marshall")
            .with_source("Hendrix, SRV")
            .with_notes("Protein + Kilt stacked into a driven Marshall"),
    );

    preset
}

/// "Edge of Breakup" — Amp=Dumble+Two-Rock/Breakup.
pub fn edge_of_breakup(m: &GuitarModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Edge of Breakup",
        PresetCategory::Genre {
            base_tone: BaseTone::Crunch,
            genre: Genre::Blues,
        },
    );
    preset.description = Some("Touch-sensitive breakup from boutique amps — cleans up with guitar volume".into());

    preset.add_module_assignment(assign(&m.source, 0));
    preset.add_module_assignment(assign(&m.dynamics, 1));
    preset.add_module_assignment(assign_snap(&m.dumble_two_rock, 6, "Breakup"));
    preset.add_module_assignment(assign(&m.reverb, 9));
    preset.add_module_assignment(assign(&m.master, 11));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("John Mayer / Robben Ford")
            .with_source("Gravity, Talk to Your Daughter")
            .with_notes("Dumble + Two-Rock just at the edge of breakup"),
    );

    preset
}

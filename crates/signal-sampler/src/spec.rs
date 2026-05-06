//! Library specification types.
//!
//! Specs are `.styx` files (preferred) or `.toml` files.
//! Load with [`LibrarySpec::from_file`] — format is auto-detected by extension.
//!
//! Third-party libraries can be supported by writing a new spec file with no
//! code changes.

use std::collections::HashMap;
use std::path::Path;

use facet::Facet;

use crate::SamplerError;

// ── Top-level ─────────────────────────────────────────────────────────────────

/// Complete specification for one sample library.
///
/// Loaded from a `.styx` file (preferred) or `.toml` file.
#[derive(Debug, Clone, Facet)]
pub struct LibrarySpec {
    /// Display name, e.g. "Cinematic Strings".
    pub name: String,
    /// Library version string.
    #[facet(default)]
    pub version: String,
    /// Vendor / developer name.
    #[facet(default)]
    pub vendor: String,

    /// Instrument sections (e.g. 1v / 2v / Va / Ce / Ba for strings).
    #[facet(default)]
    pub sections: Vec<SectionSpec>,

    /// Microphone positions.
    #[facet(default)]
    pub mics: Vec<MicSpec>,

    /// Dynamic control model (CC1, velocity, etc.).
    #[facet(default)]
    pub dynamics: DynamicsSpec,

    /// All articulations in this library.
    #[facet(default)]
    pub articulations: Vec<ArticulationSpec>,

    /// Legato engine configuration (absent for piano/drums).
    pub legato_engine: Option<LegatoEngineSpec>,

    /// Short-note pre-delay compensation.
    pub short_note_timing: Option<ShortNoteTimingSpec>,

    /// Keyswitch and CC58 articulation switching.
    pub keyswitch: Option<KeyswitchSpec>,

    /// Explicit zone map — sample-per-(key range × velocity range × RR slot).
    ///
    /// When non-empty, the engine plays in **zone mode**: every note-on looks
    /// up matching zones by `key_min..=key_max` and `vel_min..=vel_max`,
    /// RR-cycles within the matching set, and uses each zone's `root_key`,
    /// `gain_db`, and `tune_cents` for playback. This bypasses the
    /// section/articulation/dynamic filename-convention path entirely.
    ///
    /// Used by Spectrasonics-style libraries (Omnisphere, Trilian) where the
    /// keymap is encoded in patch metadata rather than filenames.
    #[facet(default)]
    pub zones: Vec<ZoneSpec>,

    /// Wavetables exposed by this library, for the synth side of Signal.
    ///
    /// Sampler engine ignores these; future synth/oscillator engine consumes
    /// them. Stored alongside zones in the same `LibrarySpec` so a single
    /// `.styx` can describe a hybrid library (e.g. Omnisphere-style sampled
    /// soundsources + wavetable bank).
    #[facet(default)]
    pub wavetables: Vec<WavetableSpec>,
}

impl LibrarySpec {
    /// Load a spec from a `.styx` or `.toml` file (format detected by extension).
    pub fn from_file(path: &Path) -> Result<Self, SamplerError> {
        let text = std::fs::read_to_string(path).map_err(SamplerError::Io)?;
        match path.extension().and_then(|e| e.to_str()) {
            Some("toml") => Self::from_toml(&text),
            _ => Self::from_styx(&text),
        }
    }

    /// Parse from styx format.
    pub fn from_styx(s: &str) -> Result<Self, SamplerError> {
        facet_styx::from_str(s).map_err(|e| SamplerError::SpecParse(e.to_string()))
    }

    /// Parse from TOML format.
    pub fn from_toml(s: &str) -> Result<Self, SamplerError> {
        facet_toml::from_str(s).map_err(|e| SamplerError::SpecParse(e.to_string()))
    }

    /// Look up an articulation by its `id` field.
    pub fn articulation(&self, id: &str) -> Option<&ArticulationSpec> {
        self.articulations.iter().find(|a| a.id == id)
    }

    /// Look up a section by its `id` field.
    pub fn section(&self, id: &str) -> Option<&SectionSpec> {
        self.sections.iter().find(|s| s.id == id)
    }

    /// Look up a mic by its `id` field.
    pub fn mic(&self, id: &str) -> Option<&MicSpec> {
        self.mics.iter().find(|m| m.id == id)
    }
}

// ── Section ───────────────────────────────────────────────────────────────────

/// One instrument section (violin, viola, cello, etc.).
#[derive(Debug, Clone, Facet)]
pub struct SectionSpec {
    /// Short identifier used in filenames: `"1v"`, `"Va"`, `"Ce"`, `"Ba"`.
    pub id: String,
    /// Human-readable label.
    pub label: String,

    /// Pitch classes that were sampled (e.g. `["G","A","B","C#","D#","F"]`).
    /// Every 2 semitones; sampler pitch-shifts to fill the gaps.
    #[facet(default)]
    pub note_grid: Vec<String>,

    /// Lowest sampled MIDI note as a name ("G2").
    pub lowest_note: String,
    /// Highest sampled MIDI note as a name ("C#6").
    pub highest_note: String,
}

// ── Mic ───────────────────────────────────────────────────────────────────────

/// One microphone / output bus position.
#[derive(Debug, Clone, Facet)]
pub struct MicSpec {
    /// Short identifier: `"Mix"`, `"Main"`, `"Room"`, `"Spot1"`, `"Spot2"`.
    pub id: String,
    /// Human-readable label.
    pub label: String,
    /// `"blended"` (pre-mixed stereo bus) or `"separate"` (individual channel).
    #[facet(default)]
    pub kind: String,
}

// ── Dynamics ─────────────────────────────────────────────────────────────────

/// Dynamic control model for the library.
#[derive(Debug, Clone, Default, Facet)]
pub struct DynamicsSpec {
    /// Controller for long-note dynamics. `"CC1"` for most libraries.
    pub sustain_controller: Option<String>,
    /// Controller for vibrato crossfade. `"CC2"` for strings.
    pub vibrato_controller: Option<String>,
    /// `"crossfade"` or `"on_off"` (solo strings: vibrato is binary).
    pub vibrato_mode: Option<String>,
    /// Controller for short-note dynamics. `"velocity"` for most libraries.
    pub short_note_controller: Option<String>,

    /// CC1 ranges that select short-note type.
    #[facet(default)]
    pub short_note_cc1_map: HashMap<String, String>,
    /// CC1 ranges for pizzicato sub-types.
    #[facet(default)]
    pub pizzicato_cc1_map: HashMap<String, String>,
    /// Velocity ranges for sustain attack character (winds: normal / accented).
    #[facet(default)]
    pub sustain_attack_velocity: HashMap<String, String>,

    /// Two-layer CC1 crossfade zones.
    #[facet(default)]
    pub cc1_layers_2: Vec<Cc1Layer>,
    /// Three-layer CC1 crossfade zones.
    #[facet(default)]
    pub cc1_layers_3: Vec<Cc1Layer>,
    /// Four-layer CC1 crossfade zones.
    #[facet(default)]
    pub cc1_layers_4: Vec<Cc1Layer>,
    /// Five-layer CC1 crossfade zones.
    #[facet(default)]
    pub cc1_layers_5: Vec<Cc1Layer>,
    /// Six-layer CC1 crossfade zones (piano has 6 dynamics).
    #[facet(default)]
    pub cc1_layers_6: Vec<Cc1Layer>,
}

/// One CC1 dynamic layer with its crossfade range.
#[derive(Debug, Clone, Facet)]
pub struct Cc1Layer {
    /// Dynamic label: `"p"`, `"mf"`, `"ff"`, etc.
    pub label: String,
    /// `[lo, hi]` inclusive CC1 range for this layer (with crossfade on both edges).
    pub cc_range: [u8; 2],
}

// ── Articulation ─────────────────────────────────────────────────────────────

/// One playing technique in the library.
#[derive(Debug, Clone, Facet)]
pub struct ArticulationSpec {
    /// Token used in WAV filenames: `"Vibsus"`, `"Leg"`, `"Staccato"`, etc.
    pub id: String,
    /// Human-readable name.
    pub label: String,

    /// Playback category.
    pub kind: ArticulationKind,

    /// Sampled dynamic layers (soft → loud): `["p", "mf", "ff"]`.
    #[facet(default)]
    pub dynamics: Vec<String>,
    /// Round-robin count per note per dynamic layer.
    #[facet(default)]
    pub rr: usize,
    /// How dynamics are controlled: `"cc1"`, `"velocity"`, or `"fixed"`.
    #[facet(default)]
    pub dyn_ctrl: String,

    /// ID of the release articulation to trigger on note-off, if any.
    pub release_artic: Option<String>,
    /// Whether separate up/down transition samples exist (legato only).
    pub directional: Option<bool>,
    /// `"full"` = full section range; `"short"` = reduced range.
    pub notes: Option<String>,
    /// If set, this articulation only exists for these section ids.
    #[facet(default)]
    pub instrument_filter: Vec<String>,

    /// Alternative filename tokens to try if the primary `id` is not found
    /// in the sample map for a given section.
    #[facet(default)]
    pub aliases: Vec<String>,
}

/// High-level category for an articulation's playback behaviour.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub enum ArticulationKind {
    /// Held note with CC1-driven dynamics (sustain, tremolo, harmonics).
    Sustain,
    /// Short one-shot note with velocity-driven dynamics.
    Short,
    /// Legato transition sample (played when a second note is held).
    Legato,
    /// Triggered on note-off after a sustain.
    Release,
    /// Half-tone or whole-tone trill (two simultaneous notes).
    Trill,
    /// Library-specific special use (FX, col legno looped, etc.).
    Special,
    /// One-shot playback — no note-off.
    OneShot,
    /// Looped sample with CC1-driven x-fade.
    Looped,
}

// ── Legato engine ─────────────────────────────────────────────────────────────

/// Full legato engine specification.
#[derive(Debug, Clone, Facet)]
pub struct LegatoEngineSpec {
    /// Flat zones for libraries with a single legato mode (e.g. brass).
    /// When populated, `expressive` and `low_latency` are typically absent.
    #[facet(default)]
    pub zones: Vec<LegatoZoneSpec>,
    /// Expressive mode: 3 velocity zones with longer pre-delays.
    pub expressive: Option<LegatoModeSpec>,
    /// Low-latency mode: 2 velocity zones with shorter pre-delays.
    pub low_latency: Option<LegatoModeSpec>,
    /// Portamento slide configuration.
    pub portamento: Option<PortamentoSpec>,
    /// Same-note re-trigger (Legzero) configuration.
    pub retrigger: Option<RetriggerSpec>,
}

impl LegatoEngineSpec {
    /// Get the flat mode (for single-mode libraries like brass), or fall back
    /// to the expressive mode if flat zones are absent.
    pub fn primary_mode(&self) -> Option<LegatoModeSpec> {
        if !self.zones.is_empty() {
            Some(LegatoModeSpec {
                enabled_cc58_range: None,
                zones: self.zones.clone(),
            })
        } else {
            self.expressive.clone()
        }
    }
}

/// One legato mode (expressive or low-latency) with its velocity zones.
#[derive(Debug, Clone, Facet)]
pub struct LegatoModeSpec {
    /// CC58 range that enables this mode (e.g. `"0-5"` or `"6-10"`).
    pub enabled_cc58_range: Option<String>,
    /// Velocity → pre-delay mapping.
    #[facet(default)]
    pub zones: Vec<LegatoZoneSpec>,
}

/// One velocity zone within a legato mode.
#[derive(Debug, Clone, Facet)]
pub struct LegatoZoneSpec {
    /// `[lo, hi]` inclusive velocity range.
    pub vel_range: [u8; 2],
    /// Human label: `"slow"`, `"medium"`, `"fast"`.
    pub label: String,
    /// Pre-delay in milliseconds before the transition sample plays.
    pub delay_ms: u32,
}

impl LegatoModeSpec {
    /// Look up the pre-delay for a given MIDI velocity.
    pub fn delay_for_velocity(&self, vel: u8) -> Option<u32> {
        self.zones
            .iter()
            .find(|z| vel >= z.vel_range[0] && vel <= z.vel_range[1])
            .map(|z| z.delay_ms)
    }
}

/// Portamento slide configuration.
#[derive(Debug, Clone, Facet)]
pub struct PortamentoSpec {
    /// Maximum velocity at which portamento triggers (default 20).
    pub trigger_vel_max: u8,
    /// CC controller for portamento volume (default "CC5").
    pub volume_controller: String,
}

/// Same-note re-trigger (re-bowing / re-tonguing) configuration.
#[derive(Debug, Clone, Facet)]
pub struct RetriggerSpec {
    /// How re-trigger is activated. `"sustain_pedal_held"` = CC64 must be on.
    pub trigger: String,
    /// Number of round robins for re-trigger samples.
    pub rr: usize,
}

// ── Short note timing ─────────────────────────────────────────────────────────

/// Pre-delay compensation for short note samples.
#[derive(Debug, Clone, Facet)]
pub struct ShortNoteTimingSpec {
    /// All short-note samples start this many ms before their "rhythmic peak."
    /// Apply a negative track delay of this amount when sequencing short notes.
    pub pre_delay_ms: u32,
}

// ── Keyswitch ─────────────────────────────────────────────────────────────────

/// Keyswitch and CC58 articulation switching configuration.
#[derive(Debug, Clone, Facet)]
pub struct KeyswitchSpec {
    /// Whether keyswitches are velocity-sensitive.
    #[facet(default)]
    pub velocity_sensitive: bool,
    /// Whether keyswitch assignments are user-configurable in the GUI.
    #[facet(default)]
    pub user_configurable: bool,

    /// CC58 value range → articulation/function label.
    /// Keys are range strings like `"0-5"`, `"6-10"`, etc.
    #[facet(default)]
    pub cc58_map: HashMap<String, String>,
}

impl KeyswitchSpec {
    /// Look up the function name for a given CC58 value.
    pub fn cc58_function(&self, value: u8) -> Option<&str> {
        for (range_str, function) in &self.cc58_map {
            if let Some((lo, hi)) = parse_range(range_str) {
                if value >= lo && value <= hi {
                    return Some(function);
                }
            }
        }
        None
    }
}

// ── Zones ────────────────────────────────────────────────────────────────────

/// One sample placed at a specific (key range × velocity range × RR slot).
///
/// Multiple zones may match the same `(note, velocity)`; the engine treats
/// them as a round-robin group and cycles by `rr_index`.
#[derive(Debug, Clone, Facet)]
pub struct ZoneSpec {
    /// Sample file path, relative to the library's `samples_root`.
    pub file: String,
    /// Lowest MIDI note in the zone (inclusive).
    pub key_min: u8,
    /// Highest MIDI note in the zone (inclusive).
    pub key_max: u8,
    /// Root MIDI note: pitch at which the sample plays back unchanged.
    pub root_key: u8,
    /// Lowest MIDI velocity in the zone (inclusive). Default 0.
    #[facet(default)]
    pub vel_min: u8,
    /// Highest MIDI velocity in the zone (inclusive). Default 127.
    #[facet(default)]
    pub vel_max: u8,
    /// Round-robin slot index (0-based). Zones with the same key/vel range
    /// but different `rr_index` form one round-robin group.
    #[facet(default)]
    pub rr_index: u32,
    /// Per-zone gain in dB. Default 0.
    #[facet(default)]
    pub gain_db: f32,
    /// Pitch fine-tune in cents. Default 0.
    #[facet(default)]
    pub tune_cents: f32,
    /// Microphone / output-bus identifier — references a `MicSpec.id` in the
    /// containing `LibrarySpec.mics`. Empty string means the zone is
    /// mic-agnostic (single-mic libraries / synth zones).
    ///
    /// Multi-mic libraries (drum kits, multi-position orchestral) declare
    /// each mic in `LibrarySpec.mics` and tag each zone with the matching
    /// `mic`. The engine fires the matching zone for **every active mic**
    /// at note-on; each mic is routed to its own output bus.
    ///
    /// Many zones will share the same `(key_min, key_max, vel_min, vel_max,
    /// rr_index)` and differ only by `mic` — these form a "multi-mic group".
    #[facet(default)]
    pub mic: String,
    /// Articulation identifier for percussion / multi-articulation libraries
    /// (e.g. drum kit "Hit" / "Sidestick" / "Flam"). Empty = no articulation
    /// distinction. Articulation switching is a Layer-level concern; this
    /// field just tags the source so the importer / UI can group zones.
    #[facet(default)]
    pub articulation: String,
}

// ── Wavetables ───────────────────────────────────────────────────────────────

/// One wavetable file in the library — a single-cycle waveform morphing
/// across `frame_count` frames of `cycle_length` samples each.
///
/// Sample data lives in the file referenced by `file`, in raw little-endian
/// IEEE 754 32-bit float (mono) — the same format Spectrasonics' `.stmwf`
/// files and standard wavetable WAVs (Serum / Vital `clm `-tagged) use after
/// stripping their RIFF header.
#[derive(Debug, Clone, Facet)]
pub struct WavetableSpec {
    /// Wavetable file path, relative to the library's `samples_root`.
    /// Either a raw `.stmwf` (no header) or a 32-bit float WAV with a
    /// Serum-style `clm ` chunk declaring the cycle length.
    pub file: String,
    /// Number of frames (single-cycle waveforms) in the bank.
    pub frame_count: u32,
    /// Samples per frame. Almost always 2048 (Spectrasonics, Serum, Vital).
    pub cycle_length: u32,
    /// Optional human label (e.g. `"Waldorf R30 00"`).
    #[facet(default)]
    pub label: String,
    /// Optional category (e.g. `"Classic Waveforms"`, `"Analog Timbres"`).
    #[facet(default)]
    pub category: String,
    /// Per-wavetable gain in dB. Default 0.
    #[facet(default)]
    pub gain_db: f32,
}

impl ZoneSpec {
    /// Whether this zone contains the given `(note, velocity)`.
    pub fn contains(&self, note: u8, velocity: u8) -> bool {
        note >= self.key_min
            && note <= self.key_max
            && velocity >= self.vel_min
            && velocity <= self.vel_max
    }
}

/// Parse a range string like `"0-5"` into `(lo, hi)`.
pub fn parse_range(s: &str) -> Option<(u8, u8)> {
    let (a, b) = s.split_once('-')?;
    Some((a.trim().parse().ok()?, b.trim().parse().ok()?))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn specs_dir() -> std::path::PathBuf {
        // Prefer the sample-collector repo next to signal, fall back to a local specs/ dir.
        let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let signal_root = std::path::Path::new(&manifest)
            .parent()
            .unwrap()
            .parent()
            .unwrap();
        let sc_specs = signal_root.parent().unwrap().join("sample-collector/specs");
        if sc_specs.exists() {
            sc_specs
        } else {
            signal_root.join("specs")
        }
    }

    #[test]
    fn test_parse_range() {
        assert_eq!(parse_range("0-5"), Some((0, 5)));
        assert_eq!(parse_range("76-80"), Some((76, 80)));
        assert_eq!(parse_range("bad"), None);
    }

    #[test]
    fn load_css_spec_styx() {
        let path = specs_dir().join("cinematic-strings.styx");
        if !path.exists() {
            return;
        }
        let spec = LibrarySpec::from_file(&path).expect("parse CSS styx spec");
        assert_eq!(spec.sections.len(), 5);
        assert!(spec.articulations.len() > 10);
        let le = spec.legato_engine.as_ref().unwrap();
        assert_eq!(
            le.expressive.as_ref().unwrap().delay_for_velocity(30),
            Some(333)
        );
        assert_eq!(
            le.expressive.as_ref().unwrap().delay_for_velocity(80),
            Some(250)
        );
        assert_eq!(
            le.expressive.as_ref().unwrap().delay_for_velocity(110),
            Some(100)
        );
        let ks = spec.keyswitch.as_ref().unwrap();
        assert_eq!(ks.cc58_function(0), Some("Sustain: Low Latency Legato"));
        assert_eq!(ks.cc58_function(88), Some("Con Sordino On"));
    }
}

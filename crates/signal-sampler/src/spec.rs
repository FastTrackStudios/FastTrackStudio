//! Library specification types.
//!
//! Specs are `.styx` files (preferred) or `.toml` files (legacy migration path).
//! Load with [`LibrarySpec::from_file`] — format is auto-detected by extension.
//!
//! Third-party libraries can be supported by writing a new spec file with no
//! code changes.

use std::collections::HashMap;
use std::path::Path;

use facet::Facet;
use serde::Deserialize;

use crate::SamplerError;

// ── Top-level ─────────────────────────────────────────────────────────────────

/// Complete specification for one sample library.
///
/// Loaded from a `.styx` file (preferred) or `.toml` file (legacy).
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct LibrarySpec {
    /// Display name, e.g. "Cinematic Studio Strings".
    pub name: String,
    /// Library version string.
    #[serde(default)]
    pub version: String,
    /// Vendor / developer name.
    #[serde(default)]
    pub vendor: String,

    /// Instrument sections (e.g. 1v / 2v / Va / Ce / Ba for CSS).
    #[serde(default, rename = "section")]
    pub sections: Vec<SectionSpec>,

    /// Microphone positions.
    #[serde(default, rename = "mic")]
    pub mics: Vec<MicSpec>,

    /// Dynamic control model (CC1, velocity, etc.).
    #[serde(default)]
    pub dynamics: DynamicsSpec,

    /// All articulations in this library.
    #[serde(default, rename = "articulation")]
    pub articulations: Vec<ArticulationSpec>,

    /// Legato engine configuration (absent for CSP).
    pub legato_engine: Option<LegatoEngineSpec>,

    /// Short-note pre-delay compensation.
    pub short_note_timing: Option<ShortNoteTimingSpec>,

    /// Keyswitch and CC58 articulation switching.
    pub keyswitch: Option<KeyswitchSpec>,
}

impl LibrarySpec {
    /// Load a spec from a `.styx` or `.toml` file (format detected by extension).
    pub fn from_file(path: &Path) -> Result<Self, SamplerError> {
        let text = std::fs::read_to_string(path).map_err(SamplerError::Io)?;
        match path.extension().and_then(|e| e.to_str()) {
            Some("styx") => Self::from_styx(&text),
            _ => Self::from_toml(&text),
        }
    }

    /// Parse from styx format using facet-styx.
    pub fn from_styx(s: &str) -> Result<Self, SamplerError> {
        facet_styx::from_str(s)
            .map_err(|e| SamplerError::SpecParse(e.to_string()))
    }

    /// Parse from TOML format (legacy / migration path).
    pub fn from_toml(s: &str) -> Result<Self, SamplerError> {
        toml::from_str(s).map_err(|e| SamplerError::SpecParse(e.to_string()))
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
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct SectionSpec {
    /// Short identifier used in filenames: `"1v"`, `"Va"`, `"Ce"`, `"Ba"`.
    pub id: String,
    /// Human-readable label.
    pub label: String,

    /// Pitch classes that were sampled (e.g. `["G","A","B","C#","D#","F"]`).
    /// Every 2 semitones; sampler pitch-shifts to fill the gaps.
    #[serde(default)]
    pub note_grid: Vec<String>,

    /// Lowest sampled MIDI note as a name ("G2").
    pub lowest_note: String,
    /// Highest sampled MIDI note as a name ("C#6").
    pub highest_note: String,
}

// ── Mic ───────────────────────────────────────────────────────────────────────

/// One microphone / output bus position.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct MicSpec {
    /// Short identifier: `"Mix"`, `"Main"`, `"Room"`, `"Spot1"`, `"Spot2"`.
    pub id: String,
    /// Human-readable label.
    pub label: String,
    /// `"blended"` (pre-mixed stereo bus) or `"separate"` (individual channel).
    #[serde(default, rename = "type")]
    pub kind: String,
}

// ── Dynamics ─────────────────────────────────────────────────────────────────

/// Dynamic control model for the library.
#[derive(Debug, Clone, Deserialize, Default, Facet)]
pub struct DynamicsSpec {
    /// Controller for long-note dynamics. `"CC1"` for most CS libraries.
    #[serde(default)]
    pub sustain_controller: Option<String>,
    /// Controller for vibrato crossfade. `"CC2"` for CSS strings.
    #[serde(default)]
    pub vibrato_controller: Option<String>,
    /// `"crossfade"` or `"on_off"` (CSSS: vibrato is binary, not continuous).
    #[serde(default)]
    pub vibrato_mode: Option<String>,
    /// Controller for short-note dynamics. `"velocity"` for all CS libraries.
    #[serde(default)]
    pub short_note_controller: Option<String>,

    /// CC1 ranges that select short-note type (CSS-style).
    #[serde(default)]
    pub short_note_cc1_map: HashMap<String, String>,
    /// CC1 ranges for pizzicato sub-types.
    #[serde(default)]
    pub pizzicato_cc1_map: HashMap<String, String>,
    /// Velocity ranges for sustain attack character (CSW: normal / accented).
    #[serde(default)]
    pub sustain_attack_velocity: HashMap<String, String>,

    /// Two-layer CC1 crossfade zones.
    #[serde(default)]
    pub cc1_layers_2: Vec<Cc1Layer>,
    /// Three-layer CC1 crossfade zones (label → CC range).
    #[serde(default)]
    pub cc1_layers_3: Vec<Cc1Layer>,
    /// Four-layer CC1 crossfade zones.
    #[serde(default)]
    pub cc1_layers_4: Vec<Cc1Layer>,
    /// Five-layer CC1 crossfade zones (CSS Clegno has 5 dynamics).
    #[serde(default)]
    pub cc1_layers_5: Vec<Cc1Layer>,
    /// Six-layer CC1 crossfade zones (CSP has 6 dynamics).
    #[serde(default)]
    pub cc1_layers_6: Vec<Cc1Layer>,
}

/// One CC1 dynamic layer with its crossfade range.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct Cc1Layer {
    /// Dynamic label: `"p"`, `"mf"`, `"ff"`, etc.
    pub label: String,
    /// `[lo, hi]` inclusive CC1 range for this layer (with crossfade on both edges).
    pub cc_range: [u8; 2],
}

// ── Articulation ─────────────────────────────────────────────────────────────

/// One playing technique in the library.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct ArticulationSpec {
    /// Token used in WAV filenames: `"Vibsus"`, `"Leg"`, `"Staccato"`, etc.
    pub id: String,
    /// Human-readable name.
    pub label: String,

    /// Playback category.
    #[serde(rename = "type")]
    pub kind: ArticulationKind,

    /// Sampled dynamic layers (soft → loud): `["p", "mf", "ff"]`.
    #[serde(default)]
    pub dynamics: Vec<String>,
    /// Round-robin count per note per dynamic layer.
    #[serde(default = "default_rr")]
    pub rr: usize,
    /// How dynamics are controlled: `"cc1"`, `"velocity"`, or `"fixed"`.
    #[serde(default = "default_dyn_ctrl")]
    pub dyn_ctrl: String,

    /// ID of the release articulation to trigger on note-off, if any.
    #[serde(default)]
    pub release_artic: Option<String>,
    /// Whether separate up/down transition samples exist (legato only).
    #[serde(default)]
    pub directional: Option<bool>,
    /// `"full"` = full section range; `"short"` = reduced range.
    #[serde(default)]
    pub notes: Option<String>,
    /// If set, this articulation only exists for these section ids.
    #[serde(default)]
    pub instrument_filter: Vec<String>,

    /// Alternative file-name tokens to try if the primary `id` is not found
    /// in the sample map for a given section. Used when the library uses
    /// different tokens across sections (e.g. `"Pizzicato"` for 1v but
    /// `"Pizz"` for 2v/Va/Ce/Ba).
    #[serde(default)]
    pub aliases: Vec<String>,
}

fn default_rr() -> usize { 1 }
fn default_dyn_ctrl() -> String { "cc1".to_string() }

/// High-level category for an articulation's playback behaviour.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Facet)]
#[serde(rename_all = "snake_case")]
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
    /// One-shot playback — no note-off (CSP FX, pedal noise, etc.).
    OneShot,
    /// Looped sample with CC1-driven x-fade (CSP FX looped beds).
    Looped,
}

// ── Legato engine ─────────────────────────────────────────────────────────────

/// Full legato engine specification.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct LegatoEngineSpec {
    /// Flat zones for libraries with a single legato mode (e.g. CSB).
    /// When this is populated, `expressive` and `low_latency` are typically absent.
    #[serde(default)]
    pub zones: Vec<LegatoZoneSpec>,
    /// Expressive mode: 3 velocity zones with longer pre-delays (CSS, CSSS).
    pub expressive: Option<LegatoModeSpec>,
    /// Low-latency mode: 2 velocity zones with shorter pre-delays (CSS).
    pub low_latency: Option<LegatoModeSpec>,
    /// Portamento slide configuration.
    pub portamento: Option<PortamentoSpec>,
    /// Same-note re-trigger (Legzero) configuration.
    pub retrigger: Option<RetriggerSpec>,
}

impl LegatoEngineSpec {
    /// Get the flat mode (for single-mode libraries like CSB), or fall back
    /// to the expressive mode if flat zones are absent.
    pub fn primary_mode(&self) -> Option<LegatoModeSpec> {
        if !self.zones.is_empty() {
            Some(LegatoModeSpec { enabled_cc58_range: None, zones: self.zones.clone() })
        } else {
            self.expressive.clone()
        }
    }
}

/// One legato mode (expressive or low-latency) with its velocity zones.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct LegatoModeSpec {
    /// CC58 range that enables this mode (e.g. `"0-5"` or `"6-10"`).
    #[serde(default)]
    pub enabled_cc58_range: Option<String>,
    /// Velocity → pre-delay mapping.
    #[serde(default)]
    pub zones: Vec<LegatoZoneSpec>,
}

/// One velocity zone within a legato mode.
#[derive(Debug, Clone, Deserialize, Facet)]
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
        self.zones.iter()
            .find(|z| vel >= z.vel_range[0] && vel <= z.vel_range[1])
            .map(|z| z.delay_ms)
    }
}

/// Portamento slide configuration.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct PortamentoSpec {
    /// Maximum velocity at which portamento triggers (default 20).
    #[serde(default = "default_portamento_vel")]
    pub trigger_vel_max: u8,
    /// CC controller for portamento volume (default "CC5").
    #[serde(default = "default_portamento_cc")]
    pub volume_controller: String,
}

fn default_portamento_vel() -> u8 { 20 }
fn default_portamento_cc() -> String { "CC5".to_string() }

/// Same-note re-trigger (re-bowing / re-tonguing) configuration.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct RetriggerSpec {
    /// How re-trigger is activated. `"sustain_pedal_held"` = CC64 must be on.
    pub trigger: String,
    /// Number of round robins for re-trigger samples.
    pub rr: usize,
}

// ── Short note timing ─────────────────────────────────────────────────────────

/// Pre-delay compensation for short note samples.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct ShortNoteTimingSpec {
    /// All short-note samples start this many ms before their "rhythmic peak."
    /// Apply a negative track delay of this amount when sequencing short notes.
    pub pre_delay_ms: u32,
}

// ── Keyswitch ─────────────────────────────────────────────────────────────────

/// Keyswitch and CC58 articulation switching configuration.
#[derive(Debug, Clone, Deserialize, Facet)]
pub struct KeyswitchSpec {
    /// Whether keyswitches are velocity-sensitive (most CS libraries: true).
    #[serde(default)]
    pub velocity_sensitive: bool,
    /// Whether keyswitch assignments are user-configurable in the GUI.
    #[serde(default)]
    pub user_configurable: bool,

    /// CC58 value range → articulation/function label.
    /// Keys are range strings like `"0-5"`, `"6-10"`, etc.
    #[serde(default)]
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

/// Parse a range string like `"0-5"` into `(lo, hi)`.
pub fn parse_range(s: &str) -> Option<(u8, u8)> {
    let (a, b) = s.split_once('-')?;
    Some((a.trim().parse().ok()?, b.trim().parse().ok()?))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn specs_dir() -> std::path::PathBuf {
        let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        std::path::Path::new(&manifest).parent().unwrap().parent().unwrap().join("specs")
    }

    #[test]
    fn test_parse_range() {
        assert_eq!(parse_range("0-5"), Some((0, 5)));
        assert_eq!(parse_range("76-80"), Some((76, 80)));
        assert_eq!(parse_range("bad"), None);
    }

    #[test]
    fn load_css_spec() {
        let path = specs_dir().join("cinematic-studio-strings.toml");
        if !path.exists() { return; }
        let spec = LibrarySpec::from_file(&path).expect("parse CSS spec");
        assert_eq!(spec.sections.len(), 5);
        assert!(spec.articulations.len() > 10);
        let le = spec.legato_engine.as_ref().unwrap();
        assert_eq!(le.expressive.as_ref().unwrap().delay_for_velocity(30), Some(333));
        assert_eq!(le.expressive.as_ref().unwrap().delay_for_velocity(80), Some(250));
        assert_eq!(le.expressive.as_ref().unwrap().delay_for_velocity(110), Some(100));
        let ks = spec.keyswitch.unwrap();
        assert_eq!(ks.cc58_function(0), Some("Sustain: Low Latency Legato"));
        assert_eq!(ks.cc58_function(77), Some("Legato On"));
    }

    #[test]
    fn load_csb_spec() {
        let path = specs_dir().join("cinematic-studio-brass.toml");
        if !path.exists() { return; }
        let spec = LibrarySpec::from_file(&path).expect("parse CSB spec");
        assert!(spec.sections.len() >= 8);
        assert!(spec.articulations.len() > 5);
        // CSB has a single mode with 2 zones (no slow)
        let le = spec.legato_engine.as_ref().unwrap();
        let mode = le.primary_mode().unwrap();
        assert_eq!(mode.zones.len(), 2);
    }

    #[test]
    fn load_csw_spec() {
        let path = specs_dir().join("cinematic-studio-woodwinds.toml");
        if !path.exists() { return; }
        let spec = LibrarySpec::from_file(&path).expect("parse CSW spec");
        assert!(spec.sections.len() >= 10);
        assert!(spec.articulations.len() > 5);
    }

    #[test]
    fn load_csss_spec() {
        let path = specs_dir().join("cinematic-studio-solo-strings.toml");
        if !path.exists() { return; }
        let spec = LibrarySpec::from_file(&path).expect("parse CSSS spec");
        assert_eq!(spec.sections.len(), 4);
        // CSSS vibrato is on/off, not crossfade
        assert_eq!(spec.dynamics.vibrato_mode.as_deref(), Some("on_off"));
    }

    #[test]
    fn load_csp_spec() {
        let path = specs_dir().join("cinematic-studio-piano.toml");
        if !path.exists() { return; }
        let spec = LibrarySpec::from_file(&path).expect("parse CSP spec");
        // CSP has no sections (whole keyboard) or one section
        assert!(spec.articulations.len() >= 2); // Sus + Suspedal at minimum
        // No legato engine in CSP
        assert!(spec.legato_engine.is_none());
    }
}

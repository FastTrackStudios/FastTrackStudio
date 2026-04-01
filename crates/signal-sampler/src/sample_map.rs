//! Sample file discovery and lookup.
//!
//! The sample map builds an index over all WAV files in a library root
//! directory, then provides O(1) lookup by (section, mic, articulation,
//! dynamic, note, rr_index, direction).
//!
//! # File naming convention
//!
//! CS-family libraries extracted from NKX archives use this flat naming pattern:
//!
//! ```text
//! {Section}_{Artic}_{Mic}_{Dyn}_{NoteOct}[_{Dir}][_{RR}].wav
//! ```
//!
//! Examples:
//! - `1v_Vibsus_Mix_ppp_G2.wav`            (single RR, no direction)
//! - `1v_Leg_Mix_p_G2_up_RR1.wav`          (directional legato, RR 1)
//! - `1v_Staccato_Mix_pp_G2_RR3.wav`       (short note, RR 3)
//! - `Ce_NVLeg_Main_mf_C2_down_RR07.wav`   (padded RR index)
//!
//! The scanner is tolerant of minor formatting variations (padded/unpadded RR
//! numbers, `RR` vs `rr` prefix, missing RR suffix for single-RR articulations).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::{
    SamplerError,
    midi::{note_name_to_midi, nearest_grid_note},
    spec::LibrarySpec,
};

// ── Sample key ────────────────────────────────────────────────────────────────

/// Unique lookup key for one WAV file.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SampleKey {
    /// Section id: `"1v"`, `"Ce"`, `"2tpt"`, etc.
    pub section: String,
    /// Articulation id: `"Vibsus"`, `"Leg"`, `"Staccato"`, etc.
    pub articulation: String,
    /// Mic id: `"Mix"`, `"Main"`, `"Room"`, etc.
    pub mic: String,
    /// Dynamic label: `"ppp"`, `"p"`, `"mf"`, `"ff"`, `"fff"`, etc.
    pub dynamic: String,
    /// MIDI note number (the sampled root note).
    pub note: u8,
    /// Direction for legato transitions: `"up"`, `"down"`, or `""` (none).
    pub direction: String,
    /// Round-robin index (0-based).
    pub rr: usize,
}

// ── Sample map ────────────────────────────────────────────────────────────────

/// In-memory index: `SampleKey → absolute WAV path`.
pub struct SampleMap {
    /// Primary index.
    map: HashMap<SampleKey, PathBuf>,
    /// Total files indexed.
    total: usize,
}

impl SampleMap {
    /// Build an empty map (no WAV files). Useful in tests.
    pub fn empty() -> Self {
        Self { map: HashMap::new(), total: 0 }
    }

    /// Scan `root_dir` and build a sample map.
    ///
    /// Expects WAV files either directly in `root_dir` (flat layout) or
    /// nested in subdirectories (organised layout). All `.wav` files found
    /// are parsed using [`parse_wav_stem`].
    pub fn scan(root_dir: &Path) -> Result<Self, SamplerError> {
        let mut map = HashMap::new();
        scan_dir(root_dir, &mut map)?;
        let total = map.len();
        Ok(Self { map, total })
    }

    /// Total number of WAV files indexed.
    pub fn total(&self) -> usize { self.total }

    /// Look up the exact path for a sample key.
    pub fn get(&self, key: &SampleKey) -> Option<&PathBuf> {
        self.map.get(key)
    }

    /// Resolve a playback lookup to a WAV path, performing pitch-rounding
    /// to the nearest sampled note in the spec's `note_grid`.
    ///
    /// If `target_note` is not directly sampled, the nearest grid note is
    /// used and the engine is expected to transpose the sample at playback.
    pub fn resolve(
        &self,
        spec: &LibrarySpec,
        section_id: &str,
        articulation_id: &str,
        mic_id: &str,
        dynamic: &str,
        target_note: u8,
        direction: &str,
        rr: usize,
    ) -> Option<(PathBuf, u8 /* sampled_note */)> {
        // Find the section to get the note grid.
        let section = spec.section(section_id)?;
        let lowest = note_name_to_midi(&section.lowest_note).ok()?;
        let highest = note_name_to_midi(&section.highest_note).ok()?;

        let sampled = if section.note_grid.is_empty() {
            // No grid — try exact note first, then walk outward.
            target_note.clamp(lowest, highest)
        } else {
            nearest_grid_note(target_note, &section.note_grid, lowest, highest)
        };

        // Build the candidate token list: primary id + any aliases from the spec.
        let aliases = spec.articulation(articulation_id)
            .map(|a| a.aliases.as_slice())
            .unwrap_or(&[]);

        let mut key = SampleKey {
            section: section_id.to_string(),
            articulation: articulation_id.to_string(),
            mic: mic_id.to_string(),
            dynamic: dynamic.to_string(),
            note: sampled,
            direction: direction.to_string(),
            rr,
        };

        // Try primary token first, then each alias in order.
        if let Some(p) = self.map.get(&key) {
            return Some((p.clone(), sampled));
        }
        for alias in aliases {
            key.articulation = alias.clone();
            if let Some(p) = self.map.get(&key) {
                return Some((p.clone(), sampled));
            }
        }
        None
    }

    /// Iterate all indexed sample keys.
    pub fn iter(&self) -> impl Iterator<Item = (&SampleKey, &PathBuf)> {
        self.map.iter()
    }

    /// All (section_id, articulation_id) pairs present in the map.
    pub fn articulations_present(&self) -> Vec<(String, String)> {
        let mut pairs: Vec<(String, String)> = self.map.keys()
            .map(|k| (k.section.clone(), k.articulation.clone()))
            .collect();
        pairs.sort();
        pairs.dedup();
        pairs
    }
}

// ── Directory scanner ─────────────────────────────────────────────────────────

fn scan_dir(dir: &Path, map: &mut HashMap<SampleKey, PathBuf>) -> Result<(), SamplerError> {
    for entry in std::fs::read_dir(dir).map_err(SamplerError::Io)? {
        let entry = entry.map_err(SamplerError::Io)?;
        let path = entry.path();
        if path.is_dir() {
            scan_dir(&path, map)?;
        } else if let Some(ext) = path.extension() {
            if ext.eq_ignore_ascii_case("wav") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    if let Some(key) = parse_wav_stem(stem) {
                        map.insert(key, path);
                    }
                }
            }
        }
    }
    Ok(())
}

// ── Filename parser ───────────────────────────────────────────────────────────

/// Parse a WAV filename stem into a `SampleKey`.
///
/// Expected patterns (underscore-separated):
/// - Standard:    `{Section}_{Artic}_{Mic}_{Dyn}_{Note}[_{Dir}][_{RR}]`
/// - Dir-legato:  `{Section}_{Artic}_{Mic}_{Dyn}_{Dir}_{Note}_{RR}`
///   (Leg / NVLeg / Port have direction **before** the note in CSS filenames)
///
/// Returns `None` if the stem cannot be parsed (non-CS file, etc.).
pub fn parse_wav_stem(stem: &str) -> Option<SampleKey> {
    let parts: Vec<&str> = stem.split('_').collect();
    if parts.len() < 5 { return None; }

    let section = parts[0].to_string();
    let articulation = parts[1].to_string();
    let mic = parts[2].to_string();
    let dynamic = parts[3].to_string();

    // Two naming variants exist:
    //   Standard:   {sec}_{artic}_{mic}_{dyn}_{note}[_{dir}][_{rr}]
    //   Directional legato: {sec}_{artic}_{mic}_{dyn}_{dir}_{note}_{rr}
    //     (Leg, NVLeg, Port have direction BEFORE the note in the filename)
    //
    // Discriminate by checking whether parts[4] is a direction word or a note name.
    let mut direction = String::new();
    let mut rr: usize = 0;
    let note_str;
    let remaining_start;

    let p4_lower = parts[4].to_ascii_lowercase();
    if p4_lower == "up" || p4_lower == "down" {
        // Directional-legato layout: dir at [4], note at [5].
        if parts.len() < 6 { return None; }
        direction = p4_lower;
        note_str = parts[5];
        remaining_start = 6;
    } else {
        // Standard layout: note at [4].
        note_str = parts[4];
        remaining_start = 5;
    }

    // Parse MIDI note (e.g. "G2", "C#4", "A#1").
    let note = note_name_to_midi(note_str).ok()?;

    // Remaining tokens: optional direction (standard layout) + optional RR.
    for tok in &parts[remaining_start..] {
        let lower = tok.to_ascii_lowercase();
        if lower == "up" || lower == "down" {
            direction = lower;
        } else if let Some(rr_str) = lower.strip_prefix("rr") {
            rr = rr_str.parse::<usize>().unwrap_or(1).saturating_sub(1); // 1-based → 0-based
        } else if let Ok(n) = lower.parse::<usize>() {
            // Bare number treated as 1-based RR index.
            rr = n.saturating_sub(1);
        }
    }

    Some(SampleKey { section, articulation, mic, dynamic, note, direction, rr })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic() {
        let key = parse_wav_stem("1v_Vibsus_Mix_ppp_G2").unwrap();
        assert_eq!(key.section, "1v");
        assert_eq!(key.articulation, "Vibsus");
        assert_eq!(key.mic, "Mix");
        assert_eq!(key.dynamic, "ppp");
        assert_eq!(key.note, 43); // G2
        assert_eq!(key.direction, "");
        assert_eq!(key.rr, 0);
    }

    #[test]
    fn parse_legato_with_direction_and_rr() {
        let key = parse_wav_stem("1v_Leg_Mix_p_G2_up_RR1").unwrap();
        assert_eq!(key.articulation, "Leg");
        assert_eq!(key.direction, "up");
        assert_eq!(key.rr, 0); // RR1 → index 0
    }

    #[test]
    fn parse_rr3() {
        let key = parse_wav_stem("Ce_Staccato_Main_pp_C2_RR3").unwrap();
        assert_eq!(key.rr, 2); // RR3 → index 2
        assert_eq!(key.note, 36); // C2
    }

    #[test]
    fn parse_directional_legato() {
        // CSS legato: direction comes BEFORE note  →  {sec}_{artic}_{mic}_{dyn}_{dir}_{note}_{rr}
        let key = parse_wav_stem("1v_Leg_Mix_ff_up_A3_12").unwrap();
        assert_eq!(key.articulation, "Leg");
        assert_eq!(key.direction, "up");
        assert_eq!(key.note, 57); // A3
        assert_eq!(key.rr, 11);   // 12 → index 11

        let key = parse_wav_stem("1v_Leg_Mix_mf_down_B5_3").unwrap();
        assert_eq!(key.direction, "down");
        assert_eq!(key.note, 83); // B5
        assert_eq!(key.rr, 2);

        // NVLeg same layout
        let key = parse_wav_stem("1v_NVLeg_Mix_mf_up_A2_1").unwrap();
        assert_eq!(key.articulation, "NVLeg");
        assert_eq!(key.direction, "up");
        assert_eq!(key.note, 45); // A2
        assert_eq!(key.rr, 0);
    }

    #[test]
    fn parse_legzero_standard_layout() {
        // Legzero has no direction — stays on the standard layout
        let key = parse_wav_stem("1v_NVLegzero_Mix_ff_F3_3").unwrap();
        assert_eq!(key.articulation, "NVLegzero");
        assert_eq!(key.direction, "");
        assert_eq!(key.note, 53); // F3
        assert_eq!(key.rr, 2);   // 3 → index 2
    }

    #[test]
    fn parse_too_short_returns_none() {
        assert!(parse_wav_stem("random_file").is_none());
    }
}

//! Keyscape LA Custom C7 Grand sample-set model + filename parser.
//!
//! Observed filename shapes in the library:
//!   RR01_SL01LACPPUr09_57-46.flac      pedal-up   note 57 vel 46 layer SL01
//!   RR01_SL02LACPPDr09_102-115.flac    pedal-down note 102 vel 115 layer SL02
//!   RR01 LACP Rel r08_46-40.flac       release    note 46 vel 40
//!   RR01 grndpno pdl_0 r07.flac        pedal mechanism noise (no note/vel)

use std::path::{Path, PathBuf};

use regex::Regex;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Artic {
    /// Struck note, sustain pedal UP (dampers active) — the clean single-note voice.
    PedalUp,
    /// Struck note, sustain pedal DOWN — carries sympathetic resonance.
    PedalDown,
    /// Key-release sample — damper-on transient.
    Release,
    /// Pedal mechanism noise one-shot (no pitch).
    PedalNoise,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sample {
    pub path: PathBuf,
    pub artic: Artic,
    /// MIDI note (None for pedal noise).
    pub note: Option<u8>,
    /// MIDI velocity layer (None for pedal noise).
    pub vel: Option<u8>,
    /// Round-robin index (RRnn).
    pub rr: u8,
    /// Sample layer (SLnn) when present.
    pub layer: Option<u8>,
}

pub struct Parser {
    rr: Regex,
    layer: Regex,
    note_vel: Regex,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            rr: Regex::new(r"(?i)\bRR(\d+)").unwrap(),
            layer: Regex::new(r"(?i)SL(\d+)").unwrap(),
            // note-velocity suffix: `_57-46.flac`
            note_vel: Regex::new(r"_(\d+)-(\d+)\.flac$").unwrap(),
        }
    }

    pub fn parse(&self, path: &Path) -> Option<Sample> {
        let name = path.file_name()?.to_str()?;

        // Articulation by marker substring (order matters: Rel before PU/PD checks).
        let artic = if name.contains("grndpno") {
            Artic::PedalNoise
        } else if name.contains("Rel") {
            Artic::Release
        } else if name.contains("LACPPU") {
            Artic::PedalUp
        } else if name.contains("LACPPD") {
            Artic::PedalDown
        } else {
            return None; // unknown shape — surfaced by the caller's reject count
        };

        let rr = self
            .rr
            .captures(name)
            .and_then(|c| c[1].parse().ok())
            .unwrap_or(0);
        let layer = self.layer.captures(name).and_then(|c| c[1].parse().ok());

        let (note, vel) = match self.note_vel.captures(name) {
            Some(c) => (c[1].parse().ok(), c[2].parse().ok()),
            None => (None, None),
        };

        // Pitched articulations must carry a note/vel; otherwise it's malformed.
        if matches!(artic, Artic::PedalUp | Artic::PedalDown | Artic::Release)
            && (note.is_none() || vel.is_none())
        {
            return None;
        }

        Some(Sample {
            path: path.to_path_buf(),
            artic,
            note,
            vel,
            rr,
            layer,
        })
    }
}

/// Scan a library directory, returning parsed samples and the count of files
/// that matched no known shape (so silent misses are visible).
pub fn scan(dir: &Path) -> (Vec<Sample>, usize) {
    let parser = Parser::new();
    let mut samples = Vec::new();
    let mut rejected = 0;
    for entry in WalkDir::new(dir).into_iter().filter_map(Result::ok) {
        let p = entry.path();
        if p.extension().and_then(|e| e.to_str()) != Some("flac") {
            continue;
        }
        match parser.parse(p) {
            Some(s) => samples.push(s),
            None => rejected += 1,
        }
    }
    (samples, rejected)
}

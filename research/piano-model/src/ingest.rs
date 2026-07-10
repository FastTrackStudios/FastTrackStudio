//! Library-agnostic sample ingest — turns ANY folder of piano samples into
//! the (note, velocity, path) manifest the preset pipeline tunes against.
//!
//! Every vendor encodes differently (Keyscape: `..PU..._57-94.flac`; NI
//! Grandeur: `GI_PP_SD_C#1_075_2014...wav`; others: `vel127`, `-f-`, nothing
//! at all), so parsing is layered:
//!
//! 1. **note**: a note NAME (`C#1`, `Ab5`) or a plausible MIDI number in the
//!    filename; verified (or recovered) by f0 detection on the audio.
//! 2. **velocity**: an explicit number (`_94`, `v100`), else a dynamic tag
//!    (`pp`→28, `p`→44, `mp`→60, `mf`→76, `f`→92, `ff`→108), else assigned by
//!    **loudness ranking** within the note — the tuner only needs layers
//!    ordered soft→loud, not true MIDI numbers.
//! 3. release/pedal/noise samples are excluded by tag heuristics.

use std::path::{Path, PathBuf};

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::{analyze, audio};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entry {
    pub path: PathBuf,
    pub note: u8,
    pub vel: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub library: String,
    pub entries: Vec<Entry>,
}

impl Manifest {
    pub fn load(path: &Path) -> Result<Self> {
        Ok(serde_json::from_str(&std::fs::read_to_string(path)?)?)
    }
    pub fn save(&self, path: &Path) -> Result<()> {
        if let Some(p) = path.parent() {
            std::fs::create_dir_all(p)?;
        }
        std::fs::write(path, serde_json::to_string_pretty(self)?)?;
        Ok(())
    }
}

/// Note NAME → MIDI (C-1 = 0 convention; `C4` = 60). Accepts C#4, Db2, A0…
fn note_name_midi(s: &str) -> Option<u8> {
    let b = s.as_bytes();
    let base: i32 = match b.first()?.to_ascii_uppercase() {
        b'C' => 0,
        b'D' => 2,
        b'E' => 4,
        b'F' => 5,
        b'G' => 7,
        b'A' => 9,
        b'B' => 11,
        _ => return None,
    };
    let mut i = 1;
    let mut acc = 0i32;
    match b.get(i) {
        Some(b'#') => {
            acc = 1;
            i += 1;
        }
        Some(b'b') => {
            acc = -1;
            i += 1;
        }
        _ => {}
    }
    let oct: i32 = s.get(i..)?.parse().ok()?;
    let midi = (oct + 1) * 12 + base + acc;
    (0..=127).contains(&midi).then_some(midi as u8)
}

/// All plausible (note, source) candidates from a filename.
fn parse_note(stem: &str) -> Option<u8> {
    // note names first — least ambiguous (require an octave digit, and a
    // non-alnum or string boundary before the letter)
    let bytes = stem.as_bytes();
    for i in 0..bytes.len() {
        let ok_start = i == 0 || !bytes[i - 1].is_ascii_alphanumeric();
        if !ok_start {
            continue;
        }
        for len in (2..=4).rev() {
            if i + len > stem.len() {
                continue;
            }
            let cand = &stem[i..i + len];
            // must end at a boundary
            let end_ok = i + len == stem.len()
                || !bytes[i + len].is_ascii_alphanumeric()
                || bytes[i + len] == b'_';
            if end_ok {
                if let Some(m) = note_name_midi(cand) {
                    return Some(m);
                }
            }
        }
    }
    None
}

/// Explicit velocity number or dynamic tag.
fn parse_vel(stem: &str) -> Option<u8> {
    let lower = stem.to_lowercase();
    // vNN / velNN
    if let Some(pos) = lower.find("vel").or_else(|| lower.find('v').filter(|&p| {
        lower.as_bytes().get(p + 1).is_some_and(|c| c.is_ascii_digit())
    })) {
        let digits: String = lower[pos..]
            .chars()
            .skip_while(|c| !c.is_ascii_digit())
            .take_while(|c| c.is_ascii_digit())
            .collect();
        if let Ok(v) = digits.parse::<u16>() {
            if (1..=127).contains(&v) {
                return Some(v as u8);
            }
        }
    }
    // dynamic tags as separate tokens
    for (tag, v) in [("ppp", 16u8), ("pp", 28), ("mp", 60), ("mf", 76), ("ff", 108), ("fff", 120)] {
        if lower
            .split(|c: char| !c.is_ascii_alphanumeric())
            .any(|t| t == tag)
        {
            return Some(v);
        }
    }
    for (tag, v) in [("p", 44u8), ("f", 92)] {
        if lower
            .split(|c: char| !c.is_ascii_alphanumeric())
            .any(|t| t == tag)
        {
            return Some(v);
        }
    }
    None
}

/// Files that are not struck-note sustains.
fn is_excluded(stem_lower: &str) -> bool {
    ["release", "rel_", "_rel", "pedal", "noise", "resonan", "harmonic", "staccato", "_rl", "keyup", "thump"]
        .iter()
        .any(|t| stem_lower.contains(t))
}

/// Crude standalone f0 (Hz) via harmonic product spectrum on the first ~2 s.
pub fn detect_f0(samples: &[f32], sr: u32) -> Option<f32> {
    use rustfft::{num_complex::Complex, FftPlanner};
    const N: usize = 65536;
    let on = analyze::onset(samples).min(samples.len().saturating_sub(1));
    let x = &samples[on..];
    if x.len() < N / 4 {
        return None;
    }
    let mut buf = vec![Complex::new(0.0f32, 0.0); N];
    for i in 0..N.min(x.len()) {
        let w = 0.5 - 0.5 * (std::f32::consts::TAU * i as f32 / N as f32).cos();
        buf[i] = Complex::new(x[i] * w, 0.0);
    }
    FftPlanner::new().plan_fft_forward(N).process(&mut buf);
    let mag: Vec<f32> = buf[..N / 2].iter().map(|c| c.norm()).collect();
    let bin_hz = sr as f32 / N as f32;
    let lo = (20.0 / bin_hz) as usize;
    let hi = ((5000.0 / bin_hz) as usize).min(N / 2 - 1);
    // harmonic product spectrum, 4 harmonics
    let (mut best, mut best_v) = (0usize, 0.0f32);
    for b in lo..hi {
        let mut v = mag[b].ln().max(-30.0);
        for h in 2..=4 {
            if b * h < N / 2 {
                v += mag[b * h].ln().max(-30.0);
            }
        }
        if v > best_v || best == 0 {
            best_v = v;
            best = b;
        }
    }
    (best > 0).then(|| best as f32 * bin_hz)
}

/// Scan a library folder into a manifest.
///
/// `verify`: cross-check filename notes against detected f0 (and recover the
/// note where the filename has none) — slower but robust.
pub fn scan(dir: &Path, include: Option<&str>, verify: bool) -> Result<Manifest> {
    use rayon::prelude::*;

    let mut files: Vec<PathBuf> = walkdir::WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|e| e.into_path())
        .filter(|p| {
            matches!(
                p.extension().and_then(|e| e.to_str()).map(|e| e.to_lowercase()).as_deref(),
                Some("wav") | Some("flac") | Some("aif") | Some("aiff")
            )
        })
        .filter(|p| {
            include.is_none_or(|pat| p.to_string_lossy().to_lowercase().contains(&pat.to_lowercase()))
        })
        .collect();
    files.sort();

    #[derive(Clone)]
    struct Raw {
        path: PathBuf,
        fname_note: Option<u8>,
        det_note: Option<u8>,
        vel: Option<u8>,
        rms: f32,
    }

    let raws: Vec<Raw> = files
        .par_iter()
        .filter_map(|p| {
            let stem = p.file_stem()?.to_string_lossy().to_string();
            let lower = stem.to_lowercase();
            if is_excluded(&lower) {
                return None;
            }
            let fname_note = parse_note(&stem);
            let vel = parse_vel(&stem);
            let mut rms = 0.0f32;
            let mut det_note = None;
            if verify || fname_note.is_none() || vel.is_none() {
                let a = audio::load_any(p).ok()?;
                let n = (a.sr as usize * 2).min(a.samples.len());
                rms = (a.samples[..n].iter().map(|x| x * x).sum::<f32>() / n.max(1) as f32).sqrt();
                if let Some(f0) = detect_f0(&a.samples, a.sr) {
                    let d = (69.0 + 12.0 * (f0 / 440.0).log2()).round();
                    if (0.0..=127.0).contains(&d) {
                        det_note = Some(d as u8);
                    }
                }
            }
            Some(Raw { path: p.clone(), fname_note, det_note, vel, rms })
        })
        .collect();

    // OCTAVE-CONVENTION CALIBRATION: vendors disagree on which octave is
    // C4 (Yamaha C3=60 vs scientific C4=60), so filename notes can be off
    // by a whole octave library-wide. The median (detected − filename)
    // difference, snapped to the nearest octave, is the library's offset.
    let mut diffs: Vec<i32> = raws
        .iter()
        .filter_map(|r| Some(r.det_note? as i32 - r.fname_note? as i32))
        .collect();
    let offset = if diffs.len() >= 8 {
        diffs.sort_unstable();
        let med = diffs[diffs.len() / 2];
        ((med as f32 / 12.0).round() as i32) * 12
    } else {
        0
    };
    if offset != 0 {
        eprintln!("octave-convention offset detected: filename notes shifted {offset} semitones");
    }

    // resolve each file's note: calibrated filename first, detection as
    // fallback/override when it disagrees by more than a semitone (and isn't
    // just a detection octave error)
    #[derive(Clone)]
    struct Resolved {
        path: PathBuf,
        note: Option<u8>,
        vel: Option<u8>,
        rms: f32,
    }
    let raws: Vec<Resolved> = raws
        .into_iter()
        .map(|r| {
            let cal = r
                .fname_note
                .map(|f| (f as i32 + offset).clamp(0, 127) as u8);
            let note = match (cal, r.det_note) {
                (Some(c), Some(d)) => {
                    let diff = (c as i16 - d as i16).abs();
                    if diff <= 1 || diff % 12 == 0 {
                        Some(c)
                    } else {
                        Some(d)
                    }
                }
                (Some(c), None) => Some(c),
                (None, d) => d,
            };
            Resolved { path: r.path, note, vel: r.vel, rms: r.rms }
        })
        .collect();

    // velocity fallback: loudness-rank within each note → pseudo velocities
    let mut entries: Vec<Entry> = Vec::new();
    let mut notes: Vec<u8> = raws.iter().filter_map(|r| r.note).collect();
    notes.sort_unstable();
    notes.dedup();
    for note in notes {
        let mut group: Vec<&Resolved> = raws.iter().filter(|r| r.note == Some(note)).collect();
        let have_vels = group.iter().filter(|r| r.vel.is_some()).count();
        if have_vels == group.len() {
            for r in group {
                entries.push(Entry { path: r.path.clone(), note, vel: r.vel.unwrap() });
            }
        } else {
            // rank by loudness; spread 20..120
            group.sort_by(|a, b| a.rms.partial_cmp(&b.rms).unwrap());
            let n = group.len().max(1);
            for (i, r) in group.iter().enumerate() {
                let v = if n == 1 {
                    90
                } else {
                    20 + (100 * i / (n - 1)) as u8
                };
                entries.push(Entry { path: r.path.clone(), note, vel: r.vel.unwrap_or(v) });
            }
        }
    }
    // prune phantom notes (a lone mis-parsed file makes a sparse "note";
    // real notes carry a full velocity stack)
    let mut counts = std::collections::BTreeMap::new();
    for e in &entries {
        *counts.entry(e.note).or_insert(0usize) += 1;
    }
    let mut per_note: Vec<usize> = counts.values().copied().collect();
    per_note.sort_unstable();
    let median = per_note.get(per_note.len() / 2).copied().unwrap_or(1);
    let min_keep = (median / 3).max(1);
    entries.retain(|e| counts[&e.note] >= min_keep);

    entries.sort_by_key(|e| (e.note, e.vel));

    Ok(Manifest {
        library: dir.display().to_string(),
        entries,
    })
}

/// Manifest → the `sample::Sample` shape the tuning pipeline consumes.
pub fn to_samples(m: &Manifest) -> Vec<crate::sample::Sample> {
    m.entries
        .iter()
        .map(|e| crate::sample::Sample {
            path: e.path.clone(),
            artic: crate::sample::Artic::PedalUp,
            note: Some(e.note),
            vel: Some(e.vel),
            rr: 1,
            layer: None,
        })
        .collect()
}

//! A/B the sampled pack against the instrument it was sampled from.
//!
//! Plays one note script twice — once out to the hardware while recording its
//! audio, once through the pack offline — then lines the two up and reports
//! where they differ. Both renders are written to disk so they can be listened
//! to as well as measured.
//!
//! The point is to separate "the pack is wrong" from "playing it live feels
//! wrong". A dropped note that shows up in the *offline* render is a mapping or
//! cache bug; one that only shows up live is a realtime problem. Measuring both
//! against the same script is what tells them apart.

use std::path::{Path, PathBuf};
use std::time::Duration;

use eyre::{Result, WrapErr, bail, eyre};
use signal_sampler::block::SamplerBlock;

use crate::capture::{Capture, Take};
use crate::config::{AudioRoute, MidiRoute, Timing};
use crate::latency::{calibrate, measure_noise_floor};
use crate::midi::Instrument;

/// One note in a comparison script.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScriptNote {
    pub note: u8,
    pub velocity: u8,
    pub start_ms: u32,
    pub dur_ms: u32,
}

/// Parse `"60@0:2,64@2:1.5v80"` — `note[@start_s][:dur_s][vNN]`.
///
/// Same shape as `fts signal pack render-report --notes`, so a script can be
/// moved between the two without rewriting it.
pub fn parse_script(s: &str) -> Result<Vec<ScriptNote>> {
    let mut out = Vec::new();
    for token in s.split(',').map(str::trim).filter(|t| !t.is_empty()) {
        let (head, velocity) = match token.rsplit_once('v') {
            // Only treat a trailing `vNN` as velocity — `v` never appears in a
            // note number, but guard anyway so "60@0:2" can't misparse.
            Some((h, v)) if !v.is_empty() && v.chars().all(|c| c.is_ascii_digit()) => {
                (h, v.parse::<u8>()?)
            }
            _ => (token, 100),
        };
        let (head, dur) = match head.split_once(':') {
            Some((h, d)) => (h, d.parse::<f32>()?),
            None => (head, 1.0),
        };
        let (note, start) = match head.split_once('@') {
            Some((n, s)) => (n, s.parse::<f32>()?),
            None => (head, 0.0),
        };
        let note: u8 = note
            .trim()
            .parse()
            .wrap_err_with(|| format!("bad note in {token:?} — expected a MIDI number"))?;
        out.push(ScriptNote {
            note,
            velocity: velocity.min(127),
            start_ms: (start * 1000.0) as u32,
            dur_ms: (dur * 1000.0) as u32,
        });
    }
    if out.is_empty() {
        bail!("empty note script");
    }
    out.sort_by_key(|n| n.start_ms);
    Ok(out)
}

/// Total length of a script plus a tail, in milliseconds.
pub fn script_len_ms(script: &[ScriptNote], tail_ms: u32) -> u32 {
    script
        .iter()
        .map(|n| n.start_ms + n.dur_ms)
        .max()
        .unwrap_or(0)
        + tail_ms
}

/// Play `script` out to the instrument and record what comes back.
pub fn record_hardware(
    instrument: &mut Instrument,
    capture: &Capture,
    script: &[ScriptNote],
    tail_ms: u32,
) -> Result<Take> {
    // Build one timeline of (when, what) so note-ons and note-offs interleave
    // correctly — a naive "for each note: on, sleep, off" loop would serialise
    // overlapping notes and change the music.
    let mut events: Vec<(u32, u8, Option<u8>)> = Vec::new();
    for n in script {
        events.push((n.start_ms, n.note, Some(n.velocity)));
        events.push((n.start_ms + n.dur_ms, n.note, None));
    }
    events.sort_by_key(|(t, _, _)| *t);

    capture.arm();
    let started = std::time::Instant::now();
    for (at_ms, note, velocity) in events {
        // Sleep relative to the start, not the previous event, so per-event
        // scheduling error can't accumulate across the script.
        let target = Duration::from_millis(at_ms as u64);
        let elapsed = started.elapsed();
        if target > elapsed {
            std::thread::sleep(target - elapsed);
        }
        match velocity {
            Some(v) => instrument.note_on(note, v)?,
            None => instrument.note_off(note)?,
        }
    }
    std::thread::sleep(Duration::from_millis(tail_ms as u64));
    let take = capture.finish()?;
    instrument.silence()?;
    Ok(take)
}

/// Render `script` through `pack`, offline and deterministically.
pub fn render_pack(
    pack: &Path,
    script: &[ScriptNote],
    sample_rate: u32,
    total_ms: u32,
) -> Result<Take> {
    let mut block = SamplerBlock::from_pack(pack, sample_rate)
        .map_err(|e| eyre!("load pack {}: {e}", pack.display()))?;
    // Offline, but preload anyway: an uncached zone is dropped rather than
    // waited for, which would show up as a missing note and be blamed on the
    // mapping.
    block.preload_samples();

    const BLOCK: usize = 256;
    let total_frames = (sample_rate as u64 * total_ms as u64 / 1000) as usize;
    let frame_of = |ms: u32| (sample_rate as u64 * ms as u64 / 1000) as usize;

    let mut events: Vec<(usize, u8, Option<u8>)> = Vec::new();
    for n in script {
        events.push((frame_of(n.start_ms), n.note, Some(n.velocity)));
        events.push((frame_of(n.start_ms + n.dur_ms), n.note, None));
    }
    events.sort_by_key(|(f, _, _)| *f);

    let mut take = Take::default();
    let mut scratch = vec![0.0f32; BLOCK * 2];
    let mut next = 0usize;
    let mut pos = 0usize;

    while pos < total_frames {
        // Events are applied at block boundaries — the same granularity the
        // live path has, so the two renders are comparable.
        while next < events.len() && events[next].0 <= pos {
            let (_, note, velocity) = events[next];
            match velocity {
                Some(v) => block.note_on(note, v),
                None => block.note_off(note),
            }
            next += 1;
        }

        scratch.fill(0.0);
        block.render(&mut scratch);
        let take_frames = BLOCK.min(total_frames - pos);
        for f in 0..take_frames {
            take.left.push(scratch[f * 2]);
            take.right.push(scratch[f * 2 + 1]);
        }
        pos += BLOCK;
    }
    Ok(take)
}

/// When each note actually started, and how loud it was.
#[derive(Debug, Clone)]
pub struct Onset {
    pub note: ScriptNote,
    /// Detected start, in milliseconds from the render's origin. `None` when
    /// nothing rose above the threshold in that note's window — a dropped note.
    pub at_ms: Option<f64>,
    /// Peak level within the note's window.
    pub peak: f32,
}

/// Find where each scripted note begins in `take`.
///
/// Each note is searched for only inside its own window, starting slightly
/// before its scheduled time. Searching the whole take would just re-find the
/// first note every time; the window is what makes per-note timing measurable.
pub fn detect_onsets(take: &Take, script: &[ScriptNote], sample_rate: u32, threshold: f32) -> Vec<Onset> {
    let frame_of = |ms: f64| ((sample_rate as f64) * ms / 1000.0) as usize;
    // Notes are searched from 50 ms early, so an early arrival still registers
    // rather than being attributed to the previous note.
    const LOOKBEHIND_MS: f64 = 50.0;

    script
        .iter()
        .map(|n| {
            let from = frame_of((n.start_ms as f64 - LOOKBEHIND_MS).max(0.0));
            let to = frame_of((n.start_ms + n.dur_ms) as f64).min(take.frames());
            let mut at = None;
            let mut peak = 0.0f32;
            for i in from..to {
                let level = take.left[i].abs().max(take.right[i].abs());
                peak = peak.max(level);
                if at.is_none() && level >= threshold {
                    at = Some(i as f64 * 1000.0 / sample_rate as f64);
                }
            }
            Onset {
                note: *n,
                at_ms: at,
                peak,
            }
        })
        .collect()
}

/// Everything a comparison produced.
#[derive(Debug)]
pub struct CompareReport {
    pub hardware_path: PathBuf,
    pub sampled_path: PathBuf,
    pub hardware: Vec<Onset>,
    pub sampled: Vec<Onset>,
    pub sample_rate: u32,
    /// Round-trip latency removed from the hardware recording.
    pub latency_ms: f64,
}

impl CompareReport {
    /// Per-note timing difference, sampled minus hardware, in milliseconds.
    /// `None` when either side dropped the note.
    pub fn timing_deltas(&self) -> Vec<(ScriptNote, Option<f64>)> {
        self.hardware
            .iter()
            .zip(self.sampled.iter())
            .map(|(h, s)| {
                let delta = match (h.at_ms, s.at_ms) {
                    (Some(h), Some(s)) => Some(s - h),
                    _ => None,
                };
                (h.note, delta)
            })
            .collect()
    }

    /// Notes that never sounded on one side or the other.
    pub fn dropped(&self) -> Vec<(ScriptNote, &'static str)> {
        let mut out = Vec::new();
        for (h, s) in self.hardware.iter().zip(self.sampled.iter()) {
            match (h.at_ms, s.at_ms) {
                (None, Some(_)) => out.push((h.note, "hardware")),
                (Some(_), None) => out.push((h.note, "sampled")),
                (None, None) => out.push((h.note, "both")),
                _ => {}
            }
        }
        out
    }
}

/// Configuration for a comparison run.
pub struct CompareConfig {
    pub pack: PathBuf,
    pub script: Vec<ScriptNote>,
    pub tail_ms: u32,
    pub midi: MidiRoute,
    pub audio: AudioRoute,
    pub timing: Timing,
    pub out_dir: PathBuf,
}

/// Record the hardware, render the pack, and compare.
pub fn run(config: &CompareConfig) -> Result<CompareReport> {
    std::fs::create_dir_all(&config.out_dir)
        .wrap_err_with(|| format!("create {}", config.out_dir.display()))?;

    let capture = Capture::open(&config.audio)?;
    let mut instrument = Instrument::open(&config.midi.port, config.midi.channel)?;

    instrument.silence()?;
    std::thread::sleep(Duration::from_millis(config.timing.settle_ms as u64));
    let noise_floor = measure_noise_floor(&capture, 500)?;

    // The hardware recording starts when we send the first note, but the audio
    // does not arrive until a round trip later. Measuring and removing it is
    // what puts the two renders on the same clock — without it every note would
    // read as uniformly "late" by the interface's latency, which says nothing
    // about the pack.
    let cal_note = config.script[0].note;
    let latency = calibrate(
        &mut instrument,
        &capture,
        cal_note,
        127,
        &config.timing,
        noise_floor,
    )?;

    let total_ms = script_len_ms(&config.script, config.tail_ms);

    let mut hardware = record_hardware(&mut instrument, &capture, &config.script, config.tail_ms)?;
    hardware.trim_start(latency.frames);

    let sampled = render_pack(
        &config.pack,
        &config.script,
        capture.sample_rate,
        total_ms,
    )?;

    let hardware_path = config.out_dir.join("hardware.wav");
    let sampled_path = config.out_dir.join("sampled.wav");
    crate::wav::write(&hardware_path, &hardware, capture.sample_rate)?;
    crate::wav::write(&sampled_path, &sampled, capture.sample_rate)?;

    // Threshold each render against its own peak: the pack and the hardware
    // sit at different absolute levels, and a shared absolute threshold would
    // report the quieter one as full of dropped notes.
    let hw_threshold = (hardware.peak() * 0.02).max(latency.threshold);
    let sm_threshold = (sampled.peak() * 0.02).max(1.0e-4);

    Ok(CompareReport {
        hardware: detect_onsets(&hardware, &config.script, capture.sample_rate, hw_threshold),
        sampled: detect_onsets(&sampled, &config.script, capture.sample_rate, sm_threshold),
        hardware_path,
        sampled_path,
        sample_rate: capture.sample_rate,
        latency_ms: latency.millis(capture.sample_rate),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn script_parses_note_start_duration_and_velocity() {
        let s = parse_script("60@0:2,64@2:1.5v80").unwrap();
        assert_eq!(
            s[0],
            ScriptNote {
                note: 60,
                velocity: 100,
                start_ms: 0,
                dur_ms: 2000
            }
        );
        assert_eq!(
            s[1],
            ScriptNote {
                note: 64,
                velocity: 80,
                start_ms: 2000,
                dur_ms: 1500
            }
        );
    }

    #[test]
    fn bare_note_gets_sensible_defaults() {
        let s = parse_script("60").unwrap();
        assert_eq!(s[0].start_ms, 0);
        assert_eq!(s[0].dur_ms, 1000);
        assert_eq!(s[0].velocity, 100);
    }

    #[test]
    fn script_is_sorted_by_start_time() {
        let s = parse_script("72@4:1,60@0:1,64@2:1").unwrap();
        assert_eq!(
            s.iter().map(|n| n.note).collect::<Vec<_>>(),
            vec![60, 64, 72]
        );
    }

    #[test]
    fn empty_script_is_rejected() {
        assert!(parse_script("").is_err());
        assert!(parse_script("   ,  ").is_err());
    }

    #[test]
    fn script_length_covers_the_last_note_off_plus_tail() {
        let s = parse_script("60@0:1,64@5:2").unwrap();
        assert_eq!(script_len_ms(&s, 1000), 8000);
    }

    fn take_with_pulses(sample_rate: u32, pulses: &[(u32, u32)]) -> Take {
        // (start_ms, dur_ms) pulses at full level.
        let total = (sample_rate as usize) * 10;
        let mut take = Take {
            left: vec![0.0; total],
            right: vec![0.0; total],
        };
        for (start, dur) in pulses {
            let from = (sample_rate as usize) * (*start as usize) / 1000;
            let to = from + (sample_rate as usize) * (*dur as usize) / 1000;
            for i in from..to.min(total) {
                take.left[i] = 0.8;
                take.right[i] = 0.8;
            }
        }
        take
    }

    #[test]
    fn onsets_are_found_per_note_not_just_the_first() {
        let sr = 48_000;
        let script = parse_script("60@0:1,64@2:1,67@4:1").unwrap();
        let take = take_with_pulses(sr, &[(0, 500), (2000, 500), (4000, 500)]);
        let onsets = detect_onsets(&take, &script, sr, 0.1);
        assert_eq!(onsets.len(), 3);
        assert!((onsets[0].at_ms.unwrap() - 0.0).abs() < 5.0);
        assert!((onsets[1].at_ms.unwrap() - 2000.0).abs() < 5.0);
        assert!((onsets[2].at_ms.unwrap() - 4000.0).abs() < 5.0);
    }

    #[test]
    fn a_missing_note_reads_as_dropped() {
        let sr = 48_000;
        let script = parse_script("60@0:1,64@2:1").unwrap();
        // Second note never sounds.
        let take = take_with_pulses(sr, &[(0, 500)]);
        let onsets = detect_onsets(&take, &script, sr, 0.1);
        assert!(onsets[0].at_ms.is_some());
        assert!(onsets[1].at_ms.is_none(), "silent note must read as dropped");
    }

    #[test]
    fn a_late_note_reports_its_lateness() {
        let sr = 48_000;
        let script = parse_script("60@0:1").unwrap();
        // Sounds 120 ms after it was scheduled.
        let take = take_with_pulses(sr, &[(120, 500)]);
        let onsets = detect_onsets(&take, &script, sr, 0.1);
        assert!((onsets[0].at_ms.unwrap() - 120.0).abs() < 5.0);
    }
}

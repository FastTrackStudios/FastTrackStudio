//! Recompute the loop points of an already-sampled folder.
//!
//! Loop points are metadata, not audio. Getting them wrong costs nothing but a
//! rewrite of `library.styx` — there is no reason to re-record 180 notes to try
//! a different loop length. This turns a 30-minute experiment into a
//! sub-second one, which matters because loop quality is judged by ear and
//! therefore needs several attempts.
//!
//! The sustained region is recovered from the existing loop: the sampler placed
//! `loop_end` exactly `release_guard_ms` before note-off, so note-off is
//! `loop_end + release_guard`. That means a re-loop can re-derive the same
//! constraint the original run had, without needing the run's timing log.

use std::path::{Path, PathBuf};

use eyre::{Result, WrapErr, eyre};
use signal_sampler::spec::LibrarySpec;

use crate::grid::Cell;
use crate::loops::{LoopPoints, LoopPolicy, choose_for_note};
use crate::pack::Recorded;

/// What a re-loop changed.
#[derive(Debug)]
pub struct ReloopReport {
    pub styx_path: PathBuf,
    pub relooped: usize,
    pub unlooped: usize,
    /// Loop length actually used, in frames, for the first zone — a quick sanity
    /// figure for the caller to print.
    pub example_len: Option<u32>,
    /// Mean seam score across searched zones, when correlation search was used.
    /// 1.0 is a perfect join.
    pub mean_score: Option<f32>,
    /// The worst seam, and which file it belongs to.
    pub worst: Option<(f32, String)>,
    /// Zones left unlooped because no seam met `min_score`.
    pub rejected: usize,
}

/// Recompute every zone's loop and rewrite `library.styx`.
pub fn run(
    samples_dir: &Path,
    policy: &LoopPolicy,
    snap_to_pitch: bool,
    sample_rate: u32,
) -> Result<ReloopReport> {
    run_with_search(samples_dir, policy, snap_to_pitch, sample_rate, None)
}

/// How wide a correlation search to run.
#[derive(Debug, Clone, Copy)]
pub struct SearchRange {
    pub min_len_ms: u32,
    pub max_len_ms: u32,
    /// Comparison window at each seam, in milliseconds. Longer is more
    /// discriminating but slower, and too long starts penalising slow drift
    /// that is inaudible anyway.
    pub window_ms: u32,
    /// Leave a zone unlooped when the best seam it can manage scores below
    /// this.
    ///
    /// Not every sound loops. A decaying, inharmonic note — a piano, a plucked
    /// string — has no steady region that repeats, so the best available join
    /// is still audible. Playing its recorded decay and stopping is both more
    /// honest and better sounding than looping it badly. The adaptive tail
    /// already captured the whole decay, so an unlooped zone is complete.
    pub min_score: f32,
}

impl Default for SearchRange {
    fn default() -> Self {
        Self {
            min_len_ms: 400,
            max_len_ms: 2500,
            window_ms: 80,
            min_score: 0.0,
        }
    }
}

/// Recompute loops, optionally by searching the audio for the best seam.
pub fn run_with_search(
    samples_dir: &Path,
    policy: &LoopPolicy,
    snap_to_pitch: bool,
    sample_rate: u32,
    search: Option<SearchRange>,
) -> Result<ReloopReport> {
    let styx_path = samples_dir.join("library.styx");
    let spec = LibrarySpec::from_file(&styx_path)
        .map_err(|e| eyre!("parse {}: {e}", styx_path.display()))?;
    if spec.zones.is_empty() {
        return Err(eyre!("{} has no zones", styx_path.display()));
    }

    let guard = (sample_rate as u64 * policy.release_guard_ms as u64 / 1000) as usize;
    let mut recorded = Vec::with_capacity(spec.zones.len());
    let mut relooped = 0;
    let mut unlooped = 0;
    let mut example_len = None;
    let mut scores: Vec<f32> = Vec::new();
    let mut worst: Option<(f32, String)> = None;
    let mut rejected = 0usize;

    for z in &spec.zones {
        // Where note-off landed. `release_start` records it directly; older
        // specs only have it implicitly, via a loop that was placed relative to
        // it. A zone with neither cannot be reconsidered and stays unlooped.
        let sustain_end = if z.release_start > 0 {
            z.release_start as usize
        } else if z.loop_end > z.loop_start + 1 {
            z.loop_end as usize + guard
        } else {
            unlooped += 1;
            recorded.push(Recorded {
                cell: cell_of(z),
                file: z.file.clone(),
                loop_points: None,
                sustain_end: None,
            });
            continue;
        };

        // The recording is at least as long as note-off plus whatever tail was
        // kept; using sustain_end as the floor keeps `choose` honest without
        // decoding the audio to measure it.
        let total = (z.sample_end as usize).max(sustain_end);
        let mut points = choose_for_note(
            sustain_end,
            total,
            sample_rate,
            policy,
            snap_to_pitch.then_some(z.root_key),
        );

        // Correlation search: read the audio and find the length whose seam
        // actually matches, rather than assuming what repeats.
        if let (Some(range), Some(base)) = (search, points) {
            match search_zone(samples_dir, &z.file, base, range, sample_rate, policy) {
                Ok((refined, score)) => {
                    if score < range.min_score {
                        // Better unlooped than looped badly.
                        rejected += 1;
                        points = None;
                    } else {
                        scores.push(score);
                        if worst.as_ref().is_none_or(|(w, _)| score < *w) {
                            worst = Some((score, z.file.clone()));
                        }
                        points = Some(refined);
                    }
                }
                Err(e) => tracing::warn!(file = %z.file, "loop search failed: {e}"),
            }
        }
        if points.is_some() {
            relooped += 1;
            if example_len.is_none() {
                example_len = points.map(|p| p.end - p.start);
            }
        } else {
            unlooped += 1;
        }
        recorded.push(Recorded {
            cell: cell_of(z),
            file: z.file.clone(),
            loop_points: points,
            sustain_end: Some(sustain_end as u32),
        });
    }

    // Regenerate the styx from our own renderer rather than editing the old
    // text or re-serialising the parsed spec. Re-serialising through facet
    // emits defaulted `Option`s as variant tags the parser then rejects, which
    // loads as a silent pack — the documented failure mode.
    let styx = crate::pack::render_styx_parts(
        &spec.name,
        &spec.vendor,
        &recorded,
        spec.sections
            .first()
            .map(|s| s.lowest_note.as_str())
            .unwrap_or("A0"),
        spec.sections
            .first()
            .map(|s| s.highest_note.as_str())
            .unwrap_or("C8"),
        &velocity_ceilings(&recorded),
    );
    std::fs::write(&styx_path, styx)
        .wrap_err_with(|| format!("write {}", styx_path.display()))?;

    Ok(ReloopReport {
        styx_path,
        relooped,
        unlooped,
        example_len,
        mean_score: (!scores.is_empty())
            .then(|| scores.iter().sum::<f32>() / scores.len() as f32),
        worst,
        rejected,
    })
}

/// Read one sample and search it for the best loop ending at `base.end`.
fn search_zone(
    samples_dir: &Path,
    file: &str,
    base: LoopPoints,
    range: SearchRange,
    sample_rate: u32,
    policy: &LoopPolicy,
) -> Result<(LoopPoints, f32)> {
    let path = samples_dir.join(file);
    let (left, right) = crate::wav::read(&path)?;
    let mono = crate::loopfind::to_mono(&left, &right);

    let ms = |v: u32| (sample_rate as u64 * v as u64 / 1000) as usize;
    let end = base.end as usize;
    // Never let the search reach back into the attack.
    let ceiling = end.saturating_sub(ms(policy.attack_skip_ms));
    let max_len = ms(range.max_len_ms).min(ceiling);
    let min_len = ms(range.min_len_ms);

    let found = crate::loopfind::best_loop(&mono, end, min_len, max_len, ms(range.window_ms))
        .ok_or_else(|| eyre!("no candidate loop fits in {file}"))?;

    let start = end.saturating_sub(found.len) as u32;
    // The crossfade blends material from before `start`, so it can be no longer
    // than that, nor than half the loop.
    let xfade = ms(policy.xfade_ms)
        .min(found.len / 2)
        .min(start as usize) as u32;
    Ok((
        LoopPoints {
            start,
            end: base.end,
            xfade,
        },
        found.score,
    ))
}

/// The velocity ceilings present in the zones, low to high.
fn velocity_ceilings(recorded: &[Recorded]) -> Vec<u8> {
    let mut tops: Vec<u8> = recorded.iter().map(|r| r.cell.vel_max).collect();
    tops.sort_unstable();
    tops.dedup();
    tops
}

fn cell_of(z: &signal_sampler::spec::ZoneSpec) -> Cell {
    Cell {
        note: z.root_key,
        // The struck velocity is not stored in the zone; the band's top is the
        // closest recoverable equivalent and is what the styx uses anyway.
        velocity: z.vel_max,
        key_min: z.key_min,
        key_max: z.key_max,
        vel_min: z.vel_min,
        vel_max: z.vel_max,
    }
}

//! Emitting `library.styx` and building the `.signalpack`.
//!
//! The styx is written as **text**, deliberately. Round-tripping a `LibrarySpec`
//! through `facet_styx::to_string` emits defaulted `Option` fields as variant
//! tags the styx *parser* then rejects, so the pack loads silent — the known
//! failure mode documented in the signalpack notes. Text out, always.
//!
//! Every zone here is authoritative: the auto-sampler chose the note and the
//! velocity, so nothing is inferred from filenames. That sidesteps
//! convention-mode parsing entirely.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use eyre::{Result, WrapErr};
use signal_sampler::engine::cache::create_signal_pack;

use crate::config::AutoSampleConfig;
use crate::grid::{Cell, spec_note_name, velocity_bands};
use crate::loops::LoopPoints;

/// The articulation every zone is tagged with.
///
/// One `@Sustain` articulation, declared explicitly. Zone-mode resolves sample
/// paths without consulting this list, but the engine still picks a default
/// articulation at construction and can fire only zones that match it — so a
/// zone tagged with an undeclared articulation is loaded and silent. Declaring
/// exactly one, and marking it `@Sustain` (never `@Release`, which the default
/// picker skips), keeps that trap shut.
const ARTICULATION: &str = "main";

/// A sample that was successfully recorded.
#[derive(Debug, Clone)]
pub struct Recorded {
    /// What the cell covers.
    pub cell: Cell,
    /// Filename, relative to the samples directory.
    pub file: String,
    /// Sustain loop, if one fits in the recording's sustained portion.
    pub loop_points: Option<LoopPoints>,
    /// Frame where note-off took effect.
    ///
    /// Written to the spec as `release_start`. The engine ignores that field,
    /// but preserving it makes re-looping repeatable: without it the note-off
    /// frame can only be inferred from an existing loop, so any zone left
    /// unlooped loses the information needed to ever loop it again.
    pub sustain_end: Option<u32>,
}

/// Render the `library.styx` text for a finished run.
pub fn render_styx(config: &AutoSampleConfig, recorded: &[Recorded]) -> String {
    let ceilings: Vec<u8> = velocity_bands(&config.grid)
        .iter()
        .map(|(struck, _, _)| *struck)
        .collect();
    render_styx_parts(
        &config.name,
        &config.vendor,
        recorded,
        &spec_note_name(config.grid.low_note),
        &spec_note_name(config.grid.high_note),
        &ceilings,
    )
}

/// Render `library.styx` from explicit parts.
///
/// Split out from [`render_styx`] so a re-loop can regenerate the spec from a
/// parsed one without inventing an [`AutoSampleConfig`] — and, more importantly,
/// without re-serialising the parsed spec through facet, which emits defaulted
/// `Option`s the styx parser rejects and produces a silent pack.
pub fn render_styx_parts(
    name: &str,
    vendor: &str,
    recorded: &[Recorded],
    lowest_note: &str,
    highest_note: &str,
    velocity_ceilings: &[u8],
) -> String {
    let mut s = String::new();
    let name = escape(name);

    let _ = writeln!(s, "name    \"{name}\"");
    let _ = writeln!(s, "version \"1.0\"");
    let _ = writeln!(s, "vendor  \"{}\"", escape(vendor));
    let _ = writeln!(s);

    let _ = writeln!(s, "sections ({{");
    let _ = writeln!(s, "  id           main");
    let _ = writeln!(s, "  label        \"{name}\"");
    // Empty note_grid = "every semitone is playable"; the zones say which are
    // actually sampled and how far each stretches.
    let _ = writeln!(s, "  note_grid    ()");
    let _ = writeln!(s, "  lowest_note  \"{lowest_note}\"");
    let _ = writeln!(s, "  highest_note \"{highest_note}\"");
    let _ = writeln!(s, "}})");
    let _ = writeln!(s);

    // One mic. Zones reference it by id; a single-mic library could leave the
    // zone's `mic` empty, but naming it keeps the spec self-describing.
    let _ = writeln!(s, "mics ({{");
    let _ = writeln!(s, "  id    Main");
    let _ = writeln!(s, "  label Main");
    let _ = writeln!(s, "  kind  blended");
    let _ = writeln!(s, "}})");
    let _ = writeln!(s);

    let _ = writeln!(s, "dynamics {{");
    let _ = writeln!(s, "  short_note_controller velocity");
    let _ = writeln!(s, "}}");
    let _ = writeln!(s);

    // Velocity ceilings, matching the bands the run actually struck.
    let ceilings = velocity_ceilings
        .iter()
        .map(|v| format!("\"{v}\""))
        .collect::<Vec<_>>()
        .join(" ");
    let _ = writeln!(s, "articulations ({{");
    let _ = writeln!(s, "  id       {ARTICULATION}");
    let _ = writeln!(s, "  label    \"Main\"");
    let _ = writeln!(s, "  kind     @Sustain");
    let _ = writeln!(s, "  dynamics ({ceilings})");
    let _ = writeln!(s, "  rr       1");
    let _ = writeln!(s, "  dyn_ctrl velocity");
    let _ = writeln!(s, "}})");
    let _ = writeln!(s);

    let _ = writeln!(s, "zones (");
    for r in recorded {
        let c = &r.cell;
        let _ = writeln!(s, "  {{");
        let _ = writeln!(s, "    file         \"{}\"", escape(&r.file));
        let _ = writeln!(s, "    key_min      {}", c.key_min);
        let _ = writeln!(s, "    key_max      {}", c.key_max);
        let _ = writeln!(s, "    root_key     {}", c.note);
        let _ = writeln!(s, "    vel_min      {}", c.vel_min);
        let _ = writeln!(s, "    vel_max      {}", c.vel_max);
        let _ = writeln!(s, "    rr_index     0");
        if let Some(end) = r.sustain_end {
            // Metadata only — the engine does not read this. It records where
            // note-off landed so a re-loop can reconsider every zone, not just
            // the ones that happen to be looped right now.
            let _ = writeln!(s, "    release_start {end}");
        }
        if let Some(l) = &r.loop_points {
            // Without these a held key stops when the sample runs out — the
            // engine does not fall back to looping the whole file.
            let _ = writeln!(s, "    loop_start   {}", l.start);
            let _ = writeln!(s, "    loop_end     {}", l.end);
            let _ = writeln!(s, "    loop_xfade   {}", l.xfade);
        }
        let _ = writeln!(s, "    mic          Main");
        let _ = writeln!(s, "    articulation {ARTICULATION}");
        let _ = writeln!(s, "  }}");
    }
    let _ = writeln!(s, ")");
    s
}

/// Write `library.styx` into the samples directory.
pub fn write_styx(config: &AutoSampleConfig, recorded: &[Recorded]) -> Result<PathBuf> {
    let path = config.out_dir.join("library.styx");
    std::fs::write(&path, render_styx(config, recorded))
        .wrap_err_with(|| format!("write {}", path.display()))?;
    Ok(path)
}

/// Build the `.signalpack` from the recorded samples and the spec beside them.
pub fn build(config: &AutoSampleConfig, spec_path: &Path, recorded: &[Recorded]) -> Result<PathBuf> {
    let Some(pack_path) = config.pack_path.clone() else {
        return Err(eyre::eyre!("no pack path configured"));
    };
    if let Some(parent) = pack_path.parent() {
        std::fs::create_dir_all(parent)
            .wrap_err_with(|| format!("create {}", parent.display()))?;
    }

    let sample_paths: Vec<PathBuf> = recorded
        .iter()
        .map(|r| config.out_dir.join(&r.file))
        .collect();

    create_signal_pack(
        &pack_path,
        spec_path,
        &config.out_dir,
        sample_paths.iter().map(|p| p.as_path()),
    )
    .wrap_err_with(|| format!("build pack {}", pack_path.display()))?;

    Ok(pack_path)
}

/// Escape a string for a styx double-quoted literal.
fn escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{AudioRoute, Grid, MidiRoute, Timing};
    use crate::grid::cells;

    fn config() -> AutoSampleConfig {
        AutoSampleConfig {
            name: "Kronos Strings".to_string(),
            vendor: "Korg".to_string(),
            grid: Grid {
                low_note: 36,
                high_note: 48,
                note_interval: 4,
                low_velocity: 1,
                high_velocity: 127,
                velocity_layers: 2,
            },
            timing: Timing::default(),
            midi: MidiRoute::default(),
            audio: AudioRoute::default(),
            loops: true,
            resume_samples: true,
            loop_policy: crate::loops::LoopPolicy::default(),
            probe_search: None,
            out_dir: PathBuf::from("/tmp/x"),
            pack_path: None,
        }
    }

    fn recorded(config: &AutoSampleConfig) -> Vec<Recorded> {
        cells(&config.grid)
            .into_iter()
            .map(|cell| Recorded {
                file: format!("s_{}_{}.wav", cell.note, cell.velocity),
                cell,
                loop_points: Some(LoopPoints {
                    start: 24_000,
                    end: 72_000,
                    xfade: 7_200,
                }),
                sustain_end: Some(144_000),
            })
            .collect()
    }

    #[test]
    fn styx_declares_the_articulation_every_zone_references() {
        let c = config();
        let styx = render_styx(&c, &recorded(&c));
        assert!(styx.contains(&format!("id       {ARTICULATION}")));
        assert!(styx.contains(&format!("articulation {ARTICULATION}")));
        assert!(
            styx.contains("kind     @Sustain"),
            "must not be @Release — the default picker skips those"
        );
    }

    #[test]
    fn styx_has_one_zone_per_recorded_sample() {
        let c = config();
        let rec = recorded(&c);
        let styx = render_styx(&c, &rec);
        assert_eq!(styx.matches("root_key").count(), rec.len());
        assert!(!rec.is_empty());
    }

    #[test]
    fn range_notes_use_the_parser_spelling() {
        let mut c = config();
        c.grid.low_note = 37; // C#2
        let styx = render_styx(&c, &recorded(&c));
        assert!(styx.contains("lowest_note  \"C#2\""), "got: {styx}");
    }

    #[test]
    fn the_note_off_frame_survives_a_round_trip() {
        // Without this, a zone left unlooped can never be reconsidered: the
        // note-off frame would only be recoverable from a loop that no longer
        // exists.
        let c = config();
        let styx = render_styx(&c, &recorded(&c));
        assert!(styx.contains("release_start 144000"), "got: {styx}");
    }

    #[test]
    fn looped_zones_emit_loop_fields() {
        let c = config();
        let styx = render_styx(&c, &recorded(&c));
        assert!(styx.contains("loop_start   24000"), "got: {styx}");
        assert!(styx.contains("loop_end     72000"));
        assert!(styx.contains("loop_xfade   7200"));
    }

    #[test]
    fn unlooped_zones_omit_loop_fields_entirely() {
        // Emitting `loop_end 0` would be harmless but noise; more importantly
        // the absence must not be confused with a zero-length loop.
        let c = config();
        let mut rec = recorded(&c);
        for r in &mut rec {
            r.loop_points = None;
        }
        let styx = render_styx(&c, &rec);
        assert!(!styx.contains("loop_start"), "got: {styx}");
        assert!(!styx.contains("loop_xfade"));
    }

    #[test]
    fn quotes_in_a_name_are_escaped() {
        let mut c = config();
        c.name = "The \"Big\" Pad".to_string();
        let styx = render_styx(&c, &recorded(&c));
        assert!(styx.contains(r#"name    "The \"Big\" Pad""#), "got: {styx}");
    }

    #[test]
    fn zones_cover_every_velocity_and_key_in_range() {
        let c = config();
        let rec = recorded(&c);
        for note in c.grid.low_note..=c.grid.high_note {
            for vel in [1u8, 40, 64, 100, 127] {
                assert!(
                    rec.iter().any(|r| {
                        let z = &r.cell;
                        (z.key_min..=z.key_max).contains(&note)
                            && (z.vel_min..=z.vel_max).contains(&vel)
                    }),
                    "no zone covers note {note} at velocity {vel}"
                );
            }
        }
    }
}

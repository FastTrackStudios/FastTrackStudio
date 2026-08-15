//! `fts signal sample …` — the auto-sampler CLI.

use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};

use clap::{Parser, Subcommand};
use eyre::{Result, WrapErr, bail};

use crate::batch::{BatchConfig, Outcome, parse_list};
use crate::compare::{CompareConfig, parse_script};
use crate::config::{AudioRoute, AutoSampleConfig, Grid, MidiRoute, Timing};
use crate::grid::{cells, note_name, roots, velocity_bands};
use crate::loops::LoopPolicy;
use crate::play::PlayRoute;

#[derive(Parser)]
#[command(
    name = "sample",
    about = "Auto-sample an instrument into a .signalpack"
)]
struct SampleCli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// List MIDI output ports and audio input devices, so you can name them.
    Devices,
    /// Print the note/velocity grid a set of options would record, without
    /// touching any hardware.
    Plan(GridArgs),
    /// Record the grid and build the pack.
    Run(RunArgs),
    /// Play a pack live from a MIDI keyboard — the sound comes from the
    /// computer, the keyboard just sends notes.
    Play(PlayArgs),
    /// A/B the pack against the hardware: play one note script through both and
    /// report where they differ.
    Compare(CompareArgs),
    /// Sample many patches unattended, selecting each by set-list slot.
    Batch(BatchArgs),
    /// Export a sampled folder as a DecentSampler .dspreset, to audition the
    /// same WAVs through an independent sampler.
    ExportDecent(ExportDecentArgs),
    /// Recompute loop points on an already-sampled folder. Loop points are
    /// metadata, so this is seconds, not another sampling run.
    Reloop(ReloopArgs),
}

#[derive(clap::Args)]
struct ReloopArgs {
    /// The sampled folder (containing library.styx and the WAVs).
    samples_dir: PathBuf,
    /// Preferred loop length, in milliseconds.
    #[arg(long, default_value_t = 1000)]
    loop_len: u32,
    /// Crossfade at the loop seam, in milliseconds. 0 by default: with
    /// correlation-chosen loop points the raw join is already smoother than the
    /// waveform's own motion, and crossfading two near-identical copies combs
    /// rather than smooths.
    #[arg(long, default_value_t = 0)]
    loop_xfade: u32,
    /// Don't snap the loop to a whole number of cycles of the note's pitch.
    #[arg(long)]
    no_snap: bool,
    /// Search the audio for the most seamless loop by cross-correlation,
    /// instead of assuming a fixed length. Slower (reads every WAV) but finds
    /// the length where the sound actually repeats — including modulation the
    /// pitch alone cannot predict.
    #[arg(long)]
    search: bool,
    /// Shortest loop the search may pick, in milliseconds.
    #[arg(long, default_value_t = 400)]
    search_min: u32,
    /// Longest loop the search may pick, in milliseconds.
    #[arg(long, default_value_t = 2500)]
    search_max: u32,
    /// Seam comparison window, in milliseconds.
    #[arg(long, default_value_t = 80)]
    search_window: u32,
    /// Leave a zone unlooped when its best seam scores below this (0-1).
    /// Decaying, inharmonic sounds loop badly; playing their recorded decay and
    /// stopping sounds better than an audible loop.
    #[arg(long, default_value_t = 0.0)]
    min_seam: f32,
    /// Sample rate of the recordings.
    #[arg(long, default_value_t = 48_000)]
    sample_rate: u32,
    /// How much the seam score weights level continuity, 0-1. 0 scores
    /// waveform shape alone; 1 also penalises a level jump at the wrap, which
    /// is what catches a tremolo or LFO cut mid-cycle.
    #[arg(long, default_value_t = 0.0)]
    level_weight: f32,
    /// Also rebuild this pack from the updated spec.
    #[arg(long)]
    pack: Option<PathBuf>,
    /// Also refresh the DecentSampler .dspreset beside the samples.
    #[arg(long)]
    decent: bool,
    /// Velocity-to-volume tracking for the refreshed .dspreset, 0-1. See
    /// `export-decent --help`.
    #[arg(long, default_value_t = 1.0)]
    amp_vel_track: f32,
}

#[derive(clap::Args)]
struct ExportDecentArgs {
    /// The sampled folder (the one containing library.styx and the WAVs).
    samples_dir: PathBuf,
    /// Output .dspreset path. Defaults to `<samples_dir>/<name>.dspreset`.
    #[arg(long)]
    out: Option<PathBuf>,
    /// How much velocity should scale volume, 0-1.
    ///
    /// 1 (the default) gives smooth dynamics BETWEEN the sampled layers: each
    /// zone is pre-boosted so the tracking cancels at the velocity it was
    /// recorded at, then velocities either side ramp continuously. 0 plays each
    /// layer at exactly its recorded level, which is stepped — every velocity
    /// inside a band sounds identical.
    #[arg(long, default_value_t = 1.0)]
    amp_vel_track: f32,
}

#[derive(clap::Args)]
struct BatchArgs {
    /// List file: one `<slot> <name>` per line, `#` for comments.
    #[arg(long)]
    list: PathBuf,
    /// Directory receiving `<name>/` and `<name>.signalpack` per patch.
    #[arg(long)]
    out_root: PathBuf,
    /// Vendor recorded in every spec.
    #[arg(long, default_value = "Korg")]
    vendor: String,
    /// Milliseconds to wait after selecting a slot, before sampling. A slot
    /// change loads samples and rebuilds effects.
    #[arg(long, default_value_t = 3000)]
    patch_settle: u32,
    /// Print the plan without touching the instrument.
    #[arg(long)]
    dry_run: bool,
    /// Only these slots (comma separated). Everything else is left alone.
    #[arg(long)]
    only: Option<String>,
    /// Stop after this many patches are sampled in this run, so a long job can
    /// be taken in sittings. Rerun the same command to continue — patches with
    /// a pack already built are skipped.
    #[arg(long)]
    limit: Option<usize>,
    /// Re-sample patches that already have a pack, instead of skipping them.
    /// The old pack survives until the new one is built, so you never have to
    /// delete a good recording to try different settings.
    #[arg(long)]
    force: bool,
    /// Skip the per-patch loop search (faster, but loops are fixed-length and
    /// more likely to be audible).
    #[arg(long)]
    no_search: bool,
    /// Shortest loop the search may pick, in milliseconds.
    #[arg(long, default_value_t = 400)]
    search_min: u32,
    /// Longest loop the search may pick, in milliseconds.
    #[arg(long, default_value_t = 2500)]
    search_max: u32,
    /// Leave a zone unlooped when its best seam scores below this (0-1).
    #[arg(long, default_value_t = 0.0)]
    min_seam: f32,
    /// Don't write a DecentSampler .dspreset beside each patch.
    #[arg(long)]
    no_decent: bool,
    /// Velocity-to-volume tracking for those presets, 0-1.
    #[arg(long, default_value_t = 1.0)]
    amp_vel_track: f32,

    #[command(flatten)]
    grid: GridArgs,

    /// MIDI output port driving the instrument.
    #[arg(long, default_value = "")]
    midi_port: String,
    /// MIDI channel, 1-16.
    #[arg(long, default_value_t = 1)]
    midi_channel: u8,
    /// Audio input device recording the instrument.
    #[arg(long, default_value = "")]
    input_device: String,
    /// 1-based input carrying the instrument's left output.
    #[arg(long, default_value_t = 1)]
    left_input: u16,
    /// 1-based input carrying the right output.
    #[arg(long, default_value_t = 2)]
    right_input: u16,
    /// Capture sample rate.
    #[arg(long, default_value_t = 48_000)]
    sample_rate: u32,

    /// Longest to hold each note, in milliseconds.
    #[arg(long, default_value_t = 3000)]
    note_length: u32,
    /// Longest to keep recording after note-off.
    #[arg(long, default_value_t = 8000)]
    max_tail: u32,
    /// Silence between notes, in milliseconds.
    #[arg(long, default_value_t = 250)]
    settle: u32,
    /// Measure each patch's hold from the instrument rather than using
    /// --note-length for all of them. A piano settles on a short hold; a
    /// slowly evolving pad asks for several seconds. Costs ~30 s per patch.
    #[arg(long)]
    probe: bool,
    /// Longest hold the probe may settle on, in milliseconds.
    #[arg(long, default_value_t = 12_000)]
    probe_max: u32,
    /// Don't give zones a sustain loop.
    #[arg(long)]
    no_loop: bool,
    /// Preferred loop length, in milliseconds.
    #[arg(long, default_value_t = 1000)]
    loop_len: u32,
    /// Crossfade at the loop seam, in milliseconds. 0 by default: with
    /// correlation-chosen loop points the raw join is already smoother than the
    /// waveform's own motion, and crossfading two near-identical copies combs
    /// rather than smooths.
    #[arg(long, default_value_t = 0)]
    loop_xfade: u32,
}

#[derive(clap::Args)]
struct CompareArgs {
    /// Pack to compare against the instrument.
    pack: PathBuf,
    /// Note script: `note[@start_s][:dur_s][vNN]`, comma separated.
    #[arg(long, default_value = "60@0:2,64@2:2,67@4:2,72@6:2")]
    notes: String,
    /// Extra seconds recorded after the last note-off.
    #[arg(long, default_value_t = 2000)]
    tail: u32,
    /// Directory for hardware.wav / sampled.wav.
    #[arg(long)]
    out: PathBuf,

    /// MIDI output port driving the instrument.
    #[arg(long, default_value = "")]
    midi_port: String,
    /// MIDI channel, 1-16.
    #[arg(long, default_value_t = 1)]
    midi_channel: u8,
    /// Audio input device recording the instrument.
    #[arg(long, default_value = "")]
    input_device: String,
    /// 1-based input carrying the instrument's left output.
    #[arg(long, default_value_t = 1)]
    left_input: u16,
    /// 1-based input carrying the right output.
    #[arg(long, default_value_t = 2)]
    right_input: u16,
    /// Capture sample rate.
    #[arg(long, default_value_t = 48_000)]
    sample_rate: u32,
}

#[derive(clap::Args)]
struct PlayArgs {
    /// Pack to play.
    pack: PathBuf,
    /// MIDI input port to listen on (substring, case-insensitive).
    #[arg(long, default_value = "")]
    midi_port: String,
    /// Only respond to this MIDI channel (1-16). Omit to listen on all.
    #[arg(long)]
    midi_channel: Option<u8>,
    /// Audio output device (substring, case-insensitive).
    #[arg(long, default_value = "")]
    output_device: String,
    /// 1-based output channel for the left signal.
    #[arg(long, default_value_t = 1)]
    left_output: u16,
    /// 1-based output channel for the right signal.
    #[arg(long, default_value_t = 2)]
    right_output: u16,
    /// Output sample rate.
    #[arg(long, default_value_t = 48_000)]
    sample_rate: u32,
    /// Buffer size in frames — smaller is lower latency, at more CPU risk.
    #[arg(long, default_value_t = 256)]
    buffer_size: u32,
}

#[derive(clap::Args, Clone)]
struct GridArgs {
    /// Lowest note to sample (MIDI number).
    #[arg(long, default_value_t = 21)]
    low_note: u8,
    /// Highest note to sample (MIDI number).
    #[arg(long, default_value_t = 108)]
    high_note: u8,
    /// Semitones between sampled notes. 1 = chromatic.
    #[arg(long, default_value_t = 3)]
    note_interval: u8,
    /// Lowest velocity to strike.
    #[arg(long, default_value_t = 1)]
    low_velocity: u8,
    /// Highest velocity to strike.
    #[arg(long, default_value_t = 127)]
    high_velocity: u8,
    /// Number of velocity layers per note.
    #[arg(long, default_value_t = 3)]
    velocity_layers: u8,
}

impl GridArgs {
    fn grid(&self) -> Result<Grid> {
        if self.low_note > self.high_note {
            bail!(
                "--low-note {} is above --high-note {}",
                self.low_note,
                self.high_note
            );
        }
        if self.low_velocity > self.high_velocity {
            bail!(
                "--low-velocity {} is above --high-velocity {}",
                self.low_velocity,
                self.high_velocity
            );
        }
        if self.velocity_layers == 0 {
            bail!("--velocity-layers must be at least 1");
        }
        if self.note_interval == 0 {
            bail!("--note-interval must be at least 1");
        }
        Ok(Grid {
            low_note: self.low_note,
            high_note: self.high_note,
            note_interval: self.note_interval,
            low_velocity: self.low_velocity,
            high_velocity: self.high_velocity,
            velocity_layers: self.velocity_layers,
        })
    }
}

#[derive(clap::Args)]
struct RunArgs {
    /// Instrument name — used for the pack name and sample filenames.
    #[arg(long)]
    name: String,
    /// Vendor recorded in the spec.
    #[arg(long, default_value = "Unknown")]
    vendor: String,

    #[command(flatten)]
    grid: GridArgs,

    /// MIDI output port to send notes to (substring, case-insensitive).
    #[arg(long, default_value = "")]
    midi_port: String,
    /// MIDI channel, 1-16.
    #[arg(long, default_value_t = 1)]
    midi_channel: u8,

    /// Audio input device to record from (substring, case-insensitive).
    #[arg(long, default_value = "")]
    input_device: String,
    /// Interface input carrying the instrument's LEFT output (1-based, as
    /// labelled on the interface).
    #[arg(long, default_value_t = 1)]
    left_input: u16,
    /// Interface input carrying the RIGHT output (1-based).
    #[arg(long, default_value_t = 2)]
    right_input: u16,
    /// Capture sample rate.
    #[arg(long, default_value_t = 48_000)]
    sample_rate: u32,

    /// Longest to hold each note, in milliseconds. A patch that decays to
    /// silence while held is released early, so this is a cap, not a wait.
    #[arg(long, default_value_t = 3000)]
    note_length: u32,
    /// Longest to keep recording after note-off, in milliseconds. Recording
    /// stops as soon as the release has decayed.
    #[arg(long, default_value_t = 8000)]
    max_tail: u32,
    /// How long the level must stay quiet before a note counts as finished.
    #[arg(long, default_value_t = 150)]
    silence_hold: u32,
    /// Silence threshold, in dB below the note's own peak.
    #[arg(long, default_value_t = -60.0, allow_negative_numbers = true)]
    silence_db: f32,
    /// Silence between notes, in milliseconds.
    #[arg(long, default_value_t = 250)]
    settle: u32,

    /// Measure the hold from the instrument instead of using --note-length.
    /// A few notes are held for --probe-max and analysed for the shortest hold
    /// that still yields a loop above --min-seam.
    #[arg(long)]
    probe: bool,
    /// Longest hold the probe may settle on, in milliseconds.
    #[arg(long, default_value_t = 12_000)]
    probe_max: u32,

    /// Directory to write WAVs and library.styx into.
    #[arg(long)]
    out: PathBuf,
    /// Pack to build. Defaults to `<out>/../<name>.signalpack`; `--no-pack`
    /// leaves the folder unpacked.
    #[arg(long)]
    pack: Option<PathBuf>,
    /// Write samples and spec but don't build a pack.
    #[arg(long)]
    no_pack: bool,

    /// Don't give zones a sustain loop. Held notes then stop when the sample
    /// runs out, which is usually only what you want for percussive patches.
    #[arg(long)]
    no_loop: bool,
    /// Preferred loop length, in milliseconds.
    #[arg(long, default_value_t = 1000)]
    loop_len: u32,
    /// Crossfade at the loop seam, in milliseconds. 0 by default: with
    /// correlation-chosen loop points the raw join is already smoother than the
    /// waveform's own motion, and crossfading two near-identical copies combs
    /// rather than smooths.
    #[arg(long, default_value_t = 0)]
    loop_xfade: u32,
}

/// Entry point for fts-cli mounting (and tests). `argv` should NOT include the
/// program name; pass the args after `fts signal sample`.
pub fn cli_main(argv: impl IntoIterator<Item = OsString>) -> Result<()> {
    let argv = std::iter::once(OsString::from("sample")).chain(argv);
    match SampleCli::parse_from(argv).command {
        Cmd::Devices => devices(),
        Cmd::Plan(args) => plan(&args),
        Cmd::Run(args) => run(args),
        Cmd::Play(args) => play(args),
        Cmd::Compare(args) => compare(args),
        Cmd::Batch(args) => batch(args),
        Cmd::ExportDecent(args) => export_decent(args),
        Cmd::Reloop(args) => reloop(args),
    }
}

/// `fts signal sample reloop` — new loop points, no re-recording.
fn reloop(args: ReloopArgs) -> Result<()> {
    let policy = LoopPolicy {
        target_len_ms: args.loop_len,
        xfade_ms: args.loop_xfade,
        ..Default::default()
    };
    let search = args.search.then(|| crate::reloop::SearchRange {
        min_len_ms: args.search_min,
        max_len_ms: args.search_max,
        window_ms: args.search_window,
        min_score: args.min_seam,
        level_weight: args.level_weight,
    });
    let report = crate::reloop::run_with_search(
        &args.samples_dir,
        &policy,
        !args.no_snap,
        args.sample_rate,
        search,
    )?;
    println!(
        "relooped {} zone(s), {} left unlooped",
        report.relooped, report.unlooped
    );
    if let Some(len) = report.example_len {
        println!(
            "loop length {} frames ({:.1} ms){}",
            len,
            len as f64 * 1000.0 / args.sample_rate as f64,
            if args.no_snap {
                ""
            } else {
                ", snapped to whole cycles of each note"
            }
        );
    }
    if report.rejected > 0 {
        println!(
            "{} zone(s) left unlooped — no seam reached the {:.2} threshold",
            report.rejected, args.min_seam
        );
    }
    if let Some(mean) = report.mean_score {
        println!("seam match: mean {mean:.4} (1.0 = perfect join)");
        if let Some((score, file)) = &report.worst {
            println!("worst seam: {score:.4}  {file}");
        }
    }
    println!("spec  {}", report.styx_path.display());

    if let Some(pack) = &args.pack {
        // Rebuilding needs the sample list the spec references.
        let spec = signal_sampler::spec::LibrarySpec::from_file(&report.styx_path)
            .map_err(|e| eyre::eyre!("re-read spec: {e}"))?;
        let paths: Vec<PathBuf> = spec
            .zones
            .iter()
            .map(|z| args.samples_dir.join(&z.file))
            .collect();
        signal_sampler::engine::cache::create_signal_pack(
            pack,
            &report.styx_path,
            &args.samples_dir,
            paths.iter().map(|p| p.as_path()),
        )
        .map_err(|e| eyre::eyre!("rebuild pack: {e}"))?;
        println!("pack  {}", pack.display());
    }
    if args.decent {
        let p = crate::decent::export(&args.samples_dir, None, args.amp_vel_track)?;
        println!("dspreset  {}", p.display());
    }
    Ok(())
}

/// `fts signal sample export-decent` — same WAVs, independent sampler.
fn export_decent(args: ExportDecentArgs) -> Result<()> {
    let path = crate::decent::export(&args.samples_dir, args.out, args.amp_vel_track)?;
    println!("wrote {}", path.display());
    println!(
        "\nLoad it in DecentSampler: File > Load, then pick that .dspreset.\n\
         It references the WAVs beside it, so nothing was copied."
    );
    Ok(())
}

/// `fts signal sample batch` — many patches, unattended.
fn batch(args: BatchArgs) -> Result<()> {
    let grid = args.grid.grid()?;
    let text = std::fs::read_to_string(&args.list)
        .wrap_err_with(|| format!("read {}", args.list.display()))?;
    let mut entries = parse_list(&text)?;

    if let Some(only) = &args.only {
        let wanted: Vec<u8> = only
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(|s| s.parse::<u8>())
            .collect::<Result<_, _>>()
            .wrap_err("--only expects comma-separated slot numbers")?;
        entries.retain(|e| wanted.contains(&e.slot));
        if entries.is_empty() {
            bail!("--only matched no entries in {}", args.list.display());
        }
    }

    let template = AutoSampleConfig {
        name: String::new(),
        vendor: args.vendor,
        grid,
        timing: Timing {
            note_length_ms: args.note_length,
            max_tail_ms: args.max_tail,
            silence_hold_ms: 150,
            silence_db: -60.0,
            settle_ms: args.settle,
            probe_note_length: args.probe,
            probe_max_ms: args.probe_max,
            ..Default::default()
        },
        midi: MidiRoute {
            port: args.midi_port,
            channel: args.midi_channel,
        },
        audio: AudioRoute {
            device: args.input_device,
            sample_rate: args.sample_rate,
            left_input: args.left_input,
            right_input: args.right_input,
        },
        loops: !args.no_loop,
        loop_policy: LoopPolicy {
            target_len_ms: args.loop_len,
            xfade_ms: args.loop_xfade,
            ..Default::default()
        },
        // The probe must judge against the same seam bar the loop search will
        // later apply, or it would settle on a hold the search then rejects.
        probe_search: Some(crate::reloop::SearchRange {
            min_len_ms: args.search_min,
            max_len_ms: args.search_max,
            min_score: args.min_seam,
            ..Default::default()
        }),
        out_dir: PathBuf::new(),
        pack_path: None,
    };

    let per_patch = cells(&template.grid).len();
    let worst_cell = args.note_length + args.max_tail + args.settle;
    let remaining = if args.force {
        entries.len()
    } else {
        entries
            .iter()
            .filter(|e| !crate::batch::paths_for(&args.out_root, &e.name).1.exists())
            .count()
    };
    let planned = args.limit.map_or(remaining, |l| l.min(remaining));
    println!(
        "{} patch(es), {} already done, {remaining} remaining{}",
        entries.len(),
        entries.len() - remaining,
        args.limit
            .map(|l| format!(" — sampling up to {l} this run"))
            .unwrap_or_default()
    );
    println!(
        "{planned} × {per_patch} samples — up to {:.1} h",
        (planned * per_patch) as f64 * worst_cell as f64 / 3_600_000.0
    );
    if !args.dry_run {
        println!("resuming is automatic: a patch whose pack already exists is skipped\n");
    }

    let config = BatchConfig {
        entries,
        out_root: args.out_root,
        patch_settle_ms: args.patch_settle,
        template,
        dry_run: args.dry_run,
        search: (!args.no_search).then(|| crate::reloop::SearchRange {
            min_len_ms: args.search_min,
            max_len_ms: args.search_max,
            min_score: args.min_seam,
            ..Default::default()
        }),
        decent: !args.no_decent,
        amp_vel_track: args.amp_vel_track,
        limit: args.limit,
        force: args.force,
    };
    let results = crate::batch::run(&config)?;

    if config.dry_run {
        return Ok(());
    }

    let mut sampled = 0;
    let mut failed = Vec::new();
    let mut skipped = 0;
    for r in &results {
        match &r.outcome {
            Outcome::Sampled { .. } => sampled += 1,
            Outcome::Skipped => skipped += 1,
            Outcome::Failed(why) => failed.push((r.entry.clone(), why.clone())),
        }
    }
    println!("\n{sampled} sampled, {skipped} already done, {} failed", failed.len());
    for (entry, why) in &failed {
        println!("  slot {} {}: {why}", entry.slot, entry.name);
    }
    Ok(())
}

/// `fts signal sample compare` — hardware vs pack, same script.
fn compare(args: CompareArgs) -> Result<()> {
    if !args.pack.exists() {
        bail!("no such pack: {}", args.pack.display());
    }
    let script = parse_script(&args.notes)?;

    let config = CompareConfig {
        pack: args.pack,
        script,
        tail_ms: args.tail,
        midi: MidiRoute {
            port: args.midi_port,
            channel: args.midi_channel,
        },
        audio: AudioRoute {
            device: args.input_device,
            sample_rate: args.sample_rate,
            left_input: args.left_input,
            right_input: args.right_input,
        },
        timing: Timing::default(),
        out_dir: args.out,
    };

    let report = crate::compare::run(&config)?;

    println!(
        "\nround-trip latency {:.1} ms (removed from the hardware recording)\n",
        report.latency_ms
    );
    println!(
        "{:<6} {:>5}  {:>12}  {:>12}  {:>9}  {:>7}  {:>7}",
        "note", "vel", "hardware", "sampled", "delta", "hw peak", "pk peak"
    );

    let mut worst: f64 = 0.0;
    for ((h, s), (note, delta)) in report
        .hardware
        .iter()
        .zip(report.sampled.iter())
        .zip(report.timing_deltas())
    {
        let fmt = |v: Option<f64>| match v {
            Some(ms) => format!("{ms:9.1} ms"),
            None => "   DROPPED".to_string(),
        };
        let delta_s = match delta {
            Some(d) => {
                worst = worst.max(d.abs());
                format!("{d:+7.1}ms")
            }
            None => "      —".to_string(),
        };
        println!(
            "{:<6} {:>5}  {:>12}  {:>12}  {:>9}  {:>7.3}  {:>7.3}",
            note_name(note.note),
            note.velocity,
            fmt(h.at_ms),
            fmt(s.at_ms),
            delta_s,
            h.peak,
            s.peak,
        );
    }

    let dropped = report.dropped();
    if dropped.is_empty() {
        println!("\nevery note sounded on both sides");
    } else {
        println!("\n{} note(s) missing:", dropped.len());
        for (note, side) in &dropped {
            println!("  {} vel {} — no sound from {side}", note_name(note.note), note.velocity);
        }
    }
    println!("worst timing difference {worst:.1} ms");
    println!("\nhardware  {}", report.hardware_path.display());
    println!("sampled   {}", report.sampled_path.display());
    Ok(())
}

/// `fts signal sample devices` — what can we send to, and record from?
fn devices() -> Result<()> {
    println!("MIDI output ports (sampling: notes go OUT to the instrument):");
    let ports = midicore_midir::output_ports();
    if ports.is_empty() {
        println!("  (none)");
    }
    for p in ports {
        println!("  {p}");
    }

    println!("\nMIDI input ports (play: notes come IN from the keyboard):");
    let ports = midicore_midir::input_ports();
    if ports.is_empty() {
        println!("  (none)");
    }
    for p in ports {
        println!("  {p}");
    }

    println!("\nAudio input devices:");
    let host = daw_audio_io::audio_host();
    let inputs = daw_audio_io::input_devices(&host);
    if inputs.is_empty() {
        println!("  (none)");
    }
    for d in inputs {
        println!(
            "  {:<40} {:>3} ch  {} Hz",
            d.name, d.channels, d.default_sample_rate
        );
    }

    println!("\nAudio output devices:");
    let outputs = daw_audio_io::output_devices(&host);
    if outputs.is_empty() {
        println!("  (none)");
    }
    for d in outputs {
        println!(
            "  {:<40} {:>3} ch  {} Hz",
            d.name, d.channels, d.default_sample_rate
        );
    }
    Ok(())
}

/// `fts signal sample play` — play a pack from the keyboard.
fn play(args: PlayArgs) -> Result<()> {
    if let Some(ch) = args.midi_channel
        && !(1..=16).contains(&ch)
    {
        bail!("--midi-channel must be 1-16, got {ch}");
    }
    if !args.pack.exists() {
        bail!("no such pack: {}", args.pack.display());
    }

    let route = PlayRoute {
        midi_port: args.midi_port,
        midi_channel: args.midi_channel,
        output_device: args.output_device,
        left_output: args.left_output,
        right_output: args.right_output,
        sample_rate: args.sample_rate,
        buffer_size: args.buffer_size,
    };

    install_sigint_handler();
    println!("playing {} — press Ctrl-C to stop", args.pack.display());
    crate::play::run(&args.pack, &route, || STOP.load(Ordering::Acquire))?;
    println!("stopped");
    Ok(())
}

/// Set by the SIGINT handler; polled by the play loop.
static STOP: AtomicBool = AtomicBool::new(false);

/// Install a Ctrl-C handler without pulling in a signal-handling crate.
///
/// The handler does nothing but store to an atomic — the only thing that is
/// actually safe in a signal handler. Anything that allocates or takes a lock
/// (including a boxed closure behind a `Mutex`) risks deadlocking against
/// whatever the interrupted thread was holding.
fn install_sigint_handler() {
    extern "C" fn handler(_: i32) {
        STOP.store(true, Ordering::Release);
    }
    // SAFETY: installing a handler that only performs an atomic store.
    unsafe {
        libc_signal(SIGINT, handler as usize);
    }
}

const SIGINT: i32 = 2;

unsafe extern "C" {
    #[link_name = "signal"]
    fn libc_signal(sig: i32, handler: usize) -> usize;
}

/// `fts signal sample plan` — the grid, without touching hardware.
fn plan(args: &GridArgs) -> Result<()> {
    let grid = args.grid()?;
    let roots = roots(&grid);
    let bands = velocity_bands(&grid);
    let cells = cells(&grid);

    println!(
        "{} notes × {} velocity layers = {} samples",
        roots.len(),
        bands.len(),
        cells.len()
    );
    println!(
        "\nnotes: {}",
        roots
            .iter()
            .map(|&n| note_name(n))
            .collect::<Vec<_>>()
            .join(" ")
    );
    println!("\nvelocity layers:");
    for (struck, lo, hi) in &bands {
        println!("  strike {struck:>3}  covers {lo:>3}..={hi:<3}");
    }
    Ok(())
}

/// `fts signal sample run` — the real thing.
fn run(args: RunArgs) -> Result<()> {
    let grid = args.grid.grid()?;

    if args.midi_channel < 1 || args.midi_channel > 16 {
        bail!("--midi-channel must be 1-16, got {}", args.midi_channel);
    }
    if args.left_input == 0 || args.right_input == 0 {
        bail!("inputs are 1-based — input 0 does not exist");
    }

    let pack_path = if args.no_pack {
        None
    } else {
        Some(args.pack.clone().unwrap_or_else(|| {
            args.out
                .parent()
                .unwrap_or(&args.out)
                .join(format!("{}.signalpack", args.name))
        }))
    };

    let config = AutoSampleConfig {
        name: args.name,
        vendor: args.vendor,
        grid,
        timing: Timing {
            note_length_ms: args.note_length,
            max_tail_ms: args.max_tail,
            silence_hold_ms: args.silence_hold,
            silence_db: args.silence_db,
            settle_ms: args.settle,
            probe_note_length: args.probe,
            probe_max_ms: args.probe_max,
            ..Default::default()
        },
        midi: MidiRoute {
            port: args.midi_port,
            channel: args.midi_channel,
        },
        audio: AudioRoute {
            device: args.input_device,
            sample_rate: args.sample_rate,
            left_input: args.left_input,
            right_input: args.right_input,
        },
        loops: !args.no_loop,
        loop_policy: LoopPolicy {
            target_len_ms: args.loop_len,
            xfade_ms: args.loop_xfade,
            ..Default::default()
        },
        probe_search: None,
        out_dir: args.out,
        pack_path,
    };

    let total = cells(&config.grid).len();
    // Worst case: every note holds and decays for its full allowance. Real runs
    // finish sooner, since each note stops as soon as it goes quiet.
    let worst_cell =
        config.timing.note_length_ms + config.timing.max_tail_ms + config.timing.settle_ms;
    println!(
        "sampling {total} cells (up to {:.1} min; stops early as each note decays)",
        (total as f64 * worst_cell as f64) / 60_000.0
    );

    let report = crate::session::run(&config)?;

    println!("\nrecorded {} sample(s)", report.recorded.len());
    println!(
        "round-trip latency {:.1} ms (trimmed from every sample)",
        report.latency.millis(report.sample_rate)
    );
    println!("peak level {:.3}", report.peak);
    if report.clipped() {
        println!("  WARNING: peak reached full scale — the instrument is clipping the interface");
    }
    if !report.skipped.is_empty() {
        println!("\nskipped {} cell(s):", report.skipped.len());
        for (cell, why) in &report.skipped {
            println!("  {} vel {}: {why}", note_name(cell.note), cell.velocity);
        }
    }
    println!("\nspec  {}", report.styx_path.display());
    if let Some(p) = &report.pack_path {
        println!("pack  {}", p.display());
        println!("\nverify with:\n  fts signal pack check \"{}\"", p.display());
    }
    Ok(())
}

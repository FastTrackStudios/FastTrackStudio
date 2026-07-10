//! piano-model research CLI (`pm`).
//!
//! Personal, non-shipping tooling to turn the owned Keyscape LA Custom C7
//! Grand sample set into physical-model parameters, then (later) train a
//! neural residual against it.

mod analyze;
mod audio;
mod body;
mod ddsp;
mod model;
mod realtime;
mod sample;
mod soundboard;
mod synth;
mod table;
mod waveguide;

use model::ModelConfig;

use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};
use rayon::prelude::*;
use serde::Serialize;

use analyze::Partial;
use sample::{Artic, Sample};

const DEFAULT_LIB: &str =
    "/run/media/AudioHaven/Sampled/Keys/Keyscape/LA Custom C7 Grand";

#[derive(Parser)]
#[command(name = "pm", about = "City Grand physical-model research pipeline")]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Scan the library and print coverage; optionally write a JSON manifest.
    Index {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        out: Option<PathBuf>,
    },
    /// Spectrally analyze one note/velocity (pedal-up) and print parameters.
    Analyze {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        #[arg(long, default_value_t = 48)]
        partials: usize,
    },
    /// Resynthesize one note/velocity from its modal params and write both the
    /// model output and the real sample as WAV for A/B listening.
    Synth {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value_t = 6.0)]
        dur: f32,
        /// Stochastic-residual gain (SMS noise part). 0 disables; ~0.5 balances
        /// broadband body across the keyboard (1 = raw measured level).
        #[arg(long, default_value_t = 0.5)]
        residual: f32,
        /// Voicing config JSON (default: built-in city_grand). Disable a block by
        /// removing its object; every physical component is toggleable here.
        #[arg(long)]
        config: Option<PathBuf>,
        #[arg(long, default_value = "out")]
        outdir: PathBuf,
    },
    /// Write a voicing config (default: city_grand) to JSON for editing.
    Config {
        #[arg(long, default_value = "out/city_grand.json")]
        out: PathBuf,
    },
    /// Play the model live from a MIDI keyboard (cpal audio out + midir in).
    Play {
        /// Swept parameter table (run `pm sweep` first).
        #[arg(long, default_value = "out/city_grand_table.json")]
        table: PathBuf,
        /// Voicing config JSON (default: built-in city_grand).
        #[arg(long)]
        config: Option<PathBuf>,
        /// MIDI input port index (default: first available). Use --list to see them.
        #[arg(long)]
        midi_port: Option<usize>,
        /// List audio + MIDI devices and exit.
        #[arg(long)]
        list: bool,
        /// Render a test chord through the realtime engine to a WAV and exit
        /// (no hardware needed — verifies the engine produces sound).
        #[arg(long)]
        selftest: bool,
        #[arg(long, default_value_t = 32)]
        polyphony: usize,
    },
    /// Attack component: measure the hammer-noise model from the isolated attack
    /// window, combine with the harmonic tone, and score (LSD tone-only vs
    /// tone+attack) — proving the component transfers.
    Attack {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value_t = 50.0)]
        attack_ms: f32,
        #[arg(long, default_value_t = 1.5)]
        dur: f32,
        #[arg(long, default_value = "out")]
        outdir: PathBuf,
    },
    /// Decompose a note into its physical components (attack / tone / release /
    /// sympathetic / pedal noise) — isolated from the PU/PD/Rel/pedal samples —
    /// and write each as WAV. These are the per-component training targets.
    Decompose {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        /// Attack window (ms) — the strike transient split point.
        #[arg(long, default_value_t = 40.0)]
        attack_ms: f32,
        #[arg(long, default_value = "out")]
        outdir: PathBuf,
    },
    /// Render a note through the stiff-string WAVEGUIDE (the coupled physical
    /// voice) and write a WAV — probe it to check pitch, inharmonicity, decay,
    /// and the velocity→brightness response vs Pianoteq/Keyscape.
    Wg {
        #[arg(long)]
        note: u8,
        #[arg(long, default_value_t = 90)]
        vel: u8,
        #[arg(long, default_value_t = 4.0)]
        dur: f32,
        #[arg(long, default_value_t = 12.0)]
        t60: f32,
        #[arg(long, default_value_t = 0.5)]
        brightness: f32,
        #[arg(long, default_value_t = 3.0e-4)]
        inharm: f32,
        #[arg(long, default_value_t = 8)]
        n_disp: usize,
        #[arg(long, default_value_t = 0.13)]
        strike: f32,
        /// Unison strings per note (2–3 → two-stage decay + beating emerge).
        #[arg(long, default_value_t = 3)]
        strings: usize,
        #[arg(long, default_value_t = 0.6)]
        detune: f32,
        /// Bridge/string impedance ratio (bridge junction). Large = rigid =
        /// long prompt decay; prompt T60 ≈ 6.908/(f0·−ln((zb−N)/(zb+N))).
        #[arg(long, default_value_t = 400.0)]
        zb: f32,
        #[arg(long, default_value = "out/wg.wav")]
        out: PathBuf,
        /// Reference recording: design a soundboard/body FIR from the smoothed
        /// spectral-envelope ratio (ref ÷ raw bridge) and apply it post-bridge.
        #[arg(long)]
        body_ref: Option<PathBuf>,
    },
    /// THE accuracy metric between any two audio files (multi-res log-spectral
    /// distance, dB; 0 = perfect null, single digits = perceptually close).
    Lsd {
        /// Model / candidate render.
        #[arg(long)]
        a: PathBuf,
        /// Reference (sample / Pianoteq render).
        #[arg(long)]
        b: PathBuf,
    },
    /// Probe an arbitrary WAV (e.g. a Pianoteq render): measured f0, inharmonicity,
    /// two-stage decay, high/low partial balance (brightness), and broadband body.
    Probe {
        #[arg(long)]
        path: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long, default_value_t = 32)]
        partials: usize,
    },
    /// Batch DDSP train: fit every (note, velocity) cell for a set of notes,
    /// bank the fitted params to a table JSON, and report LSD before/after.
    TrainSet {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        /// Comma-separated MIDI notes (default C4 D4 E4 F4 G4).
        #[arg(long, default_value = "60,62,64,65,67")]
        notes: String,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value_t = 1.5)]
        dur: f32,
        #[arg(long, default_value_t = 250)]
        steps: usize,
        #[arg(long, default_value = "out/trained_set.json")]
        out: PathBuf,
    },
    /// DDSP train: fit one note's harmonic params to the real sample by gradient
    /// descent (candle) against a multi-resolution STFT loss. Reports LSD
    /// before/after and writes dry / trained / real WAVs.
    Train {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value_t = 2.0)]
        dur: f32,
        #[arg(long, default_value_t = 300)]
        steps: usize,
        #[arg(long, default_value = "out")]
        outdir: PathBuf,
    },
    /// Metric-driven fit: optimize one note's partial amplitudes to MINIMIZE the
    /// log-spectral distance to the real sample (coordinate descent, keep-if-
    /// better). Reports LSD before/after and writes the fitted WAV.
    Fit {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        #[arg(long)]
        note: u8,
        #[arg(long)]
        vel: u8,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value_t = 3.0)]
        dur: f32,
        #[arg(long, default_value = "out")]
        outdir: PathBuf,
    },
    /// Sweep the keyboard: analyze every (note, velocity) pedal-up sample and
    /// write the full modal parameter table as JSON.
    Sweep {
        #[arg(long, default_value = DEFAULT_LIB)]
        lib: PathBuf,
        /// Comma-separated velocity layers to include (default: a 4-layer subset).
        #[arg(long, default_value = "40,70,100,127")]
        vels: String,
        #[arg(long, default_value_t = 48)]
        partials: usize,
        #[arg(long, default_value = "out/c7_table.json")]
        out: PathBuf,
    },
}

#[derive(Serialize)]
struct NoteRecord {
    note: u8,
    vel: u8,
    f0: f32,
    inharmonicity_b: f32,
    decay_t60: f32,
    peak_rms: f32,
    modal: Vec<Partial>,
    /// Self-calibrated stochastic residual (SMS body), ready for realtime.
    residual: analyze::Residual,
}

fn main() -> Result<()> {
    match Cli::parse().cmd {
        Cmd::Index { lib, out } => cmd_index(lib, out),
        Cmd::Analyze {
            lib,
            note,
            vel,
            partials,
        } => cmd_analyze(lib, note, vel, partials),
        Cmd::Synth {
            lib,
            note,
            vel,
            partials,
            dur,
            residual,
            config,
            outdir,
        } => cmd_synth(lib, note, vel, partials, dur, residual, config, outdir),
        Cmd::Config { out } => {
            if let Some(p) = out.parent() {
                std::fs::create_dir_all(p)?;
            }
            std::fs::write(&out, serde_json::to_vec_pretty(&ModelConfig::city_grand())?)?;
            println!("wrote default voicing → {}", out.display());
            Ok(())
        }
        Cmd::Attack {
            lib,
            note,
            vel,
            partials,
            attack_ms,
            dur,
            outdir,
        } => cmd_attack(lib, note, vel, partials, attack_ms, dur, outdir),
        Cmd::Decompose {
            lib,
            note,
            vel,
            attack_ms,
            outdir,
        } => cmd_decompose(lib, note, vel, attack_ms, outdir),
        Cmd::Lsd { a, b } => {
            let aa = audio::load_any(&a)?;
            let bb = audio::load_any(&b)?;
            anyhow::ensure!(aa.sr == bb.sr, "sample rates differ ({} vs {})", aa.sr, bb.sr);
            let lsd = analyze::accuracy_lsd(&aa.samples, &bb.samples, aa.sr);
            println!("ACCURACY LSD {lsd:.2} dB  ({} vs {})", a.display(), b.display());
            Ok(())
        }
        Cmd::Wg { note, vel, dur, t60, brightness, inharm, n_disp, strike, strings, detune, zb, out, body_ref } => {
            cmd_wg(note, vel, dur, t60, brightness, inharm, n_disp, strike, strings, detune, zb, out, body_ref)
        }
        Cmd::Probe { path, note, partials } => cmd_probe(path, note, partials),
        Cmd::TrainSet {
            lib,
            notes,
            partials,
            dur,
            steps,
            out,
        } => cmd_train_set(lib, notes, partials, dur, steps, out),
        Cmd::Train {
            lib,
            note,
            vel,
            partials,
            dur,
            steps,
            outdir,
        } => cmd_train(lib, note, vel, partials, dur, steps, outdir),
        Cmd::Fit {
            lib,
            note,
            vel,
            partials,
            dur,
            outdir,
        } => cmd_fit(lib, note, vel, partials, dur, outdir),
        Cmd::Sweep {
            lib,
            vels,
            partials,
            out,
        } => cmd_sweep(lib, vels, partials, out),
        Cmd::Play {
            table,
            config,
            midi_port,
            list,
            selftest,
            polyphony,
        } => cmd_play(table, config, midi_port, list, selftest, polyphony),
    }
}

fn cmd_index(lib: PathBuf, out: Option<PathBuf>) -> Result<()> {
    let (samples, rejected) = sample::scan(&lib);
    println!("scanned {}", lib.display());
    println!("  parsed:   {}", samples.len());
    println!("  rejected: {rejected}");

    let mut by_artic: BTreeMap<&str, usize> = BTreeMap::new();
    let mut notes = std::collections::BTreeSet::new();
    let mut vels = std::collections::BTreeSet::new();
    for s in &samples {
        let key = match s.artic {
            Artic::PedalUp => "pedal_up",
            Artic::PedalDown => "pedal_down",
            Artic::Release => "release",
            Artic::PedalNoise => "pedal_noise",
        };
        *by_artic.entry(key).or_default() += 1;
        if let Some(n) = s.note {
            notes.insert(n);
        }
        if let Some(v) = s.vel {
            vels.insert(v);
        }
    }
    println!("  by articulation:");
    for (k, v) in &by_artic {
        println!("    {k:12} {v}");
    }
    println!(
        "  notes: {} (MIDI {}..={})",
        notes.len(),
        notes.iter().next().copied().unwrap_or(0),
        notes.iter().next_back().copied().unwrap_or(0),
    );
    println!("  velocity layers: {} {:?}", vels.len(), vels);

    if let Some(out) = out {
        std::fs::write(&out, serde_json::to_vec_pretty(&samples)?)?;
        println!("wrote manifest → {}", out.display());
    }
    Ok(())
}

fn find<'a>(
    samples: &'a [Sample],
    artic: Artic,
    note: u8,
    vel: u8,
) -> Option<&'a Sample> {
    samples
        .iter()
        .find(|s| s.artic == artic && s.note == Some(note) && s.vel == Some(vel))
}

fn cmd_analyze(lib: PathBuf, note: u8, vel: u8, partials: usize) -> Result<()> {
    let (samples, _) = sample::scan(&lib);
    let s = find(&samples, Artic::PedalUp, note, vel).ok_or_else(|| {
        anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}")
    })?;
    let a = audio::load_mono(&s.path)?;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);

    let dur = a.samples.len() as f32 / a.sr as f32;
    println!("note {note} vel {vel}  ({})", s.path.file_name().unwrap().to_string_lossy());
    println!("  sr {} Hz, {:.2} s", r.sr, dur);
    println!("  f0 measured {:.3} Hz (ET {:.3} Hz, {:+.1} cents)",
        r.f0, expected, 1200.0 * (r.f0 / expected).log2());
    println!("  inharmonicity B = {:.3e}", r.inharmonicity_b);
    println!("  decay T60 ≈ {:.2} s", r.decay_t60);
    println!("  peak RMS = {:.4}", r.peak_rms);
    println!("  partials (Hz): measured / ideal-ET / cents");
    for (i, &fk) in r.partials.iter().enumerate().take(16) {
        let k = (i + 1) as f32;
        let ideal = expected * k;
        println!("    k{:<2} {:>10.2}  {:>10.2}  {:+7.1}", i + 1, fk, ideal,
            1200.0 * (fk / ideal).log2());
    }
    Ok(())
}

fn cmd_synth(
    lib: PathBuf,
    note: u8,
    vel: u8,
    partials: usize,
    dur: f32,
    residual: f32,
    config: Option<PathBuf>,
    outdir: PathBuf,
) -> Result<()> {
    let cfg = match &config {
        Some(p) => ModelConfig::load(p)?,
        None => ModelConfig::city_grand(),
    };
    let (samples, _) = sample::scan(&lib);
    let s = find(&samples, Artic::PedalUp, note, vel)
        .ok_or_else(|| anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}"))?;
    let a = audio::load_mono(&s.path)?;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);

    std::fs::create_dir_all(&outdir)?;

    let vel01 = (vel as f32 / 127.0).clamp(0.0, 1.0);
    let seed = ((note as u32) << 8) | vel as u32;
    let mut model = synth::render(&r.modal, a.sr, dur, vel01, seed, note, &cfg);
    if residual > 0.0 {
        let mut res = r.residual.clone();
        let freqs: Vec<f32> = r.modal.iter().map(|p| p.freq).collect();
        calibrate_residual(&mut res, &a.samples, &model, a.sr, &freqs, dur.min(3.0));
        synth::add_residual(&mut model, a.sr, &res, residual, seed ^ 0x00abcdef);
    }
    // Match the model's loudness to the real sample's peak for fair A/B.
    let real_peak = a.samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
    synth::normalize(&mut model, real_peak.max(1e-4));

    let model_path = outdir.join(format!("note{note}_v{vel}_model.wav"));
    let real_path = outdir.join(format!("note{note}_v{vel}_real.wav"));
    synth::write_wav(&model_path, &model, a.sr)?;
    // Real sample trimmed to the same duration for side-by-side.
    let real_len = ((dur * a.sr as f32) as usize).min(a.samples.len());
    synth::write_wav(&real_path, &a.samples[..real_len], a.sr)?;

    let n_strings = cfg.unison.as_ref().map_or(1, |u| u.strings_for(note));
    println!("model \"{}\" — blocks: {}", cfg.name, enabled_blocks(&cfg));
    println!("note {note} vel {vel}: {} modal partials, {} unison strings",
        r.modal.len(), n_strings);
    println!("  f0 {:.2} Hz  B {:.3e}  T60 {:.2}s", r.f0, r.inharmonicity_b, r.decay_t60);
    println!("  modal (k: freq | prompt T60 / after T60 | fast-frac):");
    for p in r.modal.iter().take(12) {
        let tf = if p.decay_fast > 0.0 { 6.908 / p.decay_fast } else { f32::INFINITY };
        let ts = if p.decay_slow > 0.0 { 6.908 / p.decay_slow } else { f32::INFINITY };
        println!("    k{:<2} {:>9.2}Hz  a={:.4}  prompt={:.1}s after={:.1}s  mix={:.2}",
            p.k, p.freq, p.amp, tf, ts, p.mix);
    }
    // Objective A/B: spectral + envelope similarity of model vs real.
    let model_spec = analyze::logmag(&model, a.sr, dur.min(3.0));
    let real_spec = analyze::logmag(&a.samples[..real_len], a.sr, dur.min(3.0));
    let spec_sim = analyze::cosine(&model_spec, &real_spec);
    let model_env = analyze::envelope(&model, a.sr);
    let real_env = analyze::envelope(&a.samples[..real_len], a.sr);
    let env_sim = analyze::cosine(&model_env, &real_env);
    let freqs: Vec<f32> = r.modal.iter().map(|p| p.freq).collect();
    let (harm_cos, bb_ratio) =
        analyze::spectral_diag(&model, &a.samples[..real_len], a.sr, &freqs, dur.min(3.0));
    println!("  A/B  spectral cos {:.3}   envelope cos {:.3}", spec_sim, env_sim);
    println!("  diag harmonic cos {:.3}   broadband real/model {:.1}× (>1 = model too thin)",
        harm_cos, bb_ratio);
    let lsd = analyze::accuracy_lsd(&model, &a.samples[..real_len], a.sr);
    println!("  ACCURACY  log-spectral distance {:.2} dB  (0 = perfect null; lower better)", lsd);
    println!("wrote:\n  model → {}\n  real  → {}", model_path.display(), real_path.display());
    Ok(())
}

/// Self-calibrate a note's residual level so the FULL model's broadband energy
/// (dry model + residual) matches the real recording's. Targets only the
/// deficit the dry model doesn't already cover (its partial skirts + attack
/// noise), so the treble — where the model is already broadband-rich — doesn't
/// overshoot. Makes gain=1.0 correct across the keyboard.
fn calibrate_residual(
    res: &mut analyze::Residual,
    real: &[f32],
    dry_model: &[f32],
    sr: u32,
    freqs: &[f32],
    secs: f32,
) {
    if res.level <= 0.0 {
        return;
    }
    let real_bb = analyze::broadband_energy(real, sr, freqs, secs);
    let dry_bb = analyze::broadband_energy(dry_model, sr, freqs, secs);
    let target = (real_bb - dry_bb).max(0.0);
    let n = (secs * sr as f32) as usize;
    let mut probe = vec![0.0f32; n];
    synth::add_residual(&mut probe, sr, res, 1.0, 0x1234_5678);
    let syn_bb = analyze::broadband_energy(&probe, sr, freqs, secs);
    if syn_bb > 1e-12 {
        let corr = (target / syn_bb).sqrt() as f32;
        res.level *= corr.clamp(0.0, 20.0);
    }
}

#[derive(Clone, Copy)]
enum MidiEvent {
    NoteOn(u8, u8),
    NoteOff(u8),
}

fn cmd_play(
    table_path: PathBuf,
    config: Option<PathBuf>,
    midi_port: Option<usize>,
    list: bool,
    selftest: bool,
    polyphony: usize,
) -> Result<()> {
    use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

    if selftest {
        let cfg = match &config {
            Some(p) => ModelConfig::load(p)?,
            None => ModelConfig::city_grand(),
        };
        let tbl = table::Table::load(&table_path)?;
        let sr = 44100u32;
        let mut engine = realtime::Engine::new(tbl, cfg, sr as f32, polyphony);
        // C major chord: C4 E4 G4 (MIDI 60 64 67) at velocity 90.
        for n in [60u8, 64, 67] {
            engine.note_on(n, 90);
        }
        let total = (sr as f32 * 3.0) as usize;
        let mut buf = vec![0.0f32; total];
        let block = 512;
        let mut pos = 0;
        while pos < total {
            let end = (pos + block).min(total);
            // release the chord at 1.5s to exercise note_off
            if pos <= (sr as usize * 3 / 2) && (sr as usize * 3 / 2) < end {
                for n in [60u8, 64, 67] {
                    engine.note_off(n);
                }
            }
            engine.process(&mut buf[pos..end]);
            pos = end;
        }
        let rms = (buf.iter().map(|x| x * x).sum::<f32>() / total as f32).sqrt();
        let peak = buf.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
        std::fs::create_dir_all("out")?;
        synth::write_wav(std::path::Path::new("out/play_selftest.wav"), &buf, sr)?;
        println!("realtime engine selftest: C major chord, 3s");
        println!("  RMS {rms:.4}  peak {peak:.4}  → out/play_selftest.wav");
        anyhow::ensure!(rms > 1e-3, "engine produced (near) silence — bug");
        return Ok(());
    }

    let host = cpal::default_host();
    let device = host
        .default_output_device()
        .ok_or_else(|| anyhow::anyhow!("no default output device"))?;
    let midi_in = midir::MidiInput::new("city-grand")?;
    let ports = midi_in.ports();

    if list {
        println!("Audio output: {}", device.name().unwrap_or_default());
        println!("MIDI inputs:");
        for (i, p) in ports.iter().enumerate() {
            println!("  [{i}] {}", midi_in.port_name(p).unwrap_or_default());
        }
        return Ok(());
    }

    let cfg = match &config {
        Some(p) => ModelConfig::load(p)?,
        None => ModelConfig::city_grand(),
    };
    let tbl = table::Table::load(&table_path)?;
    let (lo, hi) = tbl.note_range();

    let out_cfg = device.default_output_config()?;
    let sr = out_cfg.sample_rate().0 as f32;
    let channels = out_cfg.channels() as usize;

    let mut engine = realtime::Engine::new(tbl, cfg.clone(), sr, polyphony);
    let (tx, rx) = crossbeam_channel::unbounded::<MidiEvent>();

    // MIDI input → event channel.
    let port = ports
        .get(midi_port.unwrap_or(0))
        .ok_or_else(|| anyhow::anyhow!("no MIDI input port (use --list)"))?;
    let port_name = midi_in.port_name(port).unwrap_or_default();
    let _conn = midi_in
        .connect(
            port,
            "city-grand-in",
            move |_t, msg, _| {
                if msg.len() >= 3 {
                    match msg[0] & 0xF0 {
                        0x90 if msg[2] > 0 => {
                            let _ = tx.send(MidiEvent::NoteOn(msg[1], msg[2]));
                        }
                        0x80 | 0x90 => {
                            let _ = tx.send(MidiEvent::NoteOff(msg[1]));
                        }
                        _ => {}
                    }
                }
            },
            (),
        )
        .map_err(|e| anyhow::anyhow!("MIDI connect failed: {e}"))?;

    // Audio output callback: drain MIDI events, render.
    let mut scratch = vec![0.0f32; 4096];
    let err_fn = |e| eprintln!("audio stream error: {e}");
    let stream = device.build_output_stream(
        &out_cfg.config(),
        move |data: &mut [f32], _| {
            while let Ok(ev) = rx.try_recv() {
                match ev {
                    MidiEvent::NoteOn(n, v) => engine.note_on(n, v),
                    MidiEvent::NoteOff(n) => engine.note_off(n),
                }
            }
            let frames = data.len() / channels;
            if scratch.len() < frames {
                scratch.resize(frames, 0.0);
            }
            engine.process(&mut scratch[..frames]);
            for (frame, &m) in data.chunks_mut(channels).zip(scratch.iter()) {
                for s in frame.iter_mut() {
                    *s = m;
                }
            }
        },
        err_fn,
        None,
    )?;
    stream.play()?;

    println!("City Grand — playing \"{}\"", cfg.name);
    println!("  audio: {} @ {} Hz, {} ch", device.name().unwrap_or_default(), sr as u32, channels);
    println!("  MIDI:  {port_name}");
    println!("  notes: MIDI {lo}..={hi}, polyphony {polyphony}");
    println!("  blocks: {}", enabled_blocks(&cfg));
    println!("\nPlay your keyboard. Press Enter to quit.");
    let mut _s = String::new();
    std::io::stdin().read_line(&mut _s).ok();
    Ok(())
}

/// One-line summary of which physical blocks a config turns on.
fn enabled_blocks(cfg: &ModelConfig) -> String {
    let mut v = Vec::new();
    if cfg.two_stage_decay {
        v.push("two-stage-decay");
    }
    if cfg.unison.is_some() {
        v.push("unison");
    }
    if cfg.jitter.is_some() {
        v.push("jitter");
    }
    if cfg.attack.is_some() {
        v.push("attack");
    }
    if cfg.soundboard.is_some() {
        v.push("soundboard");
    }
    if v.is_empty() {
        "none (bare modal)".into()
    } else {
        v.join(", ")
    }
}

#[allow(clippy::too_many_arguments)]
fn cmd_wg(
    note: u8,
    vel: u8,
    dur: f32,
    t60: f32,
    brightness: f32,
    inharm: f32,
    n_disp: usize,
    strike: f32,
    strings: usize,
    detune: f32,
    zb: f32,
    out: PathBuf,
    body_ref: Option<PathBuf>,
) -> Result<()> {
    let sr = 44100u32;
    let f0 = analyze::midi_hz(note);
    let p = waveguide::StringParams { f0, t60, brightness, inharmonicity: inharm, n_disp };
    let vel01 = (vel as f32 / 127.0).clamp(0.0, 1.0);
    let n = (dur * sr as f32) as usize;
    let mut buf = vec![0.0f32; n];
    let mut cs = waveguide::CoupledStrings::new(&p, sr, strings, detune, zb);
    cs.strike(vel01, strike);
    for x in buf.iter_mut() {
        *x = cs.process();
    }
    if let Some(rp) = &body_ref {
        let real = audio::load_any(rp)?;
        anyhow::ensure!(real.sr == sr, "body ref sample rate {} != {sr}", real.sr);
        let fir = body::design_fir(&buf, &real.samples, sr, 512);
        buf = body::apply_fir(&buf, &fir);
        println!("body FIR designed from {} (512 taps)", rp.display());
    }
    synth::normalize(&mut buf, 0.9);
    if let Some(par) = out.parent() {
        std::fs::create_dir_all(par)?;
    }
    synth::write_wav(&out, &buf, sr)?;
    println!("waveguide note {note} vel {vel} (f0 {f0:.2} Hz) → {}", out.display());
    Ok(())
}

fn cmd_probe(path: PathBuf, note: u8, partials: usize) -> Result<()> {
    let a = audio::load_any(&path)?;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);
    let freqs: Vec<f32> = r.modal.iter().map(|p| p.freq).collect();
    let bb = analyze::broadband_energy(&a.samples, a.sr, &freqs, 3.0);
    // harmonic energy at the partials
    let mag = analyze::avg_mag(&a.samples, a.sr, 3.0);
    let bin_hz = a.sr as f32 / (mag.len() as f32 * 2.0);
    let harm: f64 = freqs.iter().filter_map(|&f| {
        let b = (f / bin_hz).round() as usize;
        mag.get(b).map(|&m| (m as f64).powi(2))
    }).sum();
    // brightness: energy in partials >5 vs first 5
    let (mut lo, mut hi) = (0.0f64, 0.0f64);
    for (i, p) in r.modal.iter().enumerate() {
        let b = (p.freq / bin_hz).round() as usize;
        let e = mag.get(b).map(|&m| (m as f64).powi(2)).unwrap_or(0.0);
        if i < 5 { lo += e } else { hi += e }
    }
    println!("{}", path.display());
    println!("  f0 {:.2} Hz ({:+.1} cents)  B {:.3e}  peakRMS {:.4}",
        r.f0, 1200.0 * (r.f0 / expected).log2(), r.inharmonicity_b, r.peak_rms);
    if let Some(p) = r.modal.first() {
        let t60f = if p.decay_fast > 0.0 { 6.908 / p.decay_fast } else { 0.0 };
        let t60s = if p.decay_slow > 0.0 { 6.908 / p.decay_slow } else { 0.0 };
        println!("  k1 two-stage decay: prompt {:.1}s / after {:.1}s  (mix {:.2})", t60f, t60s, p.mix);
    }
    println!("  brightness (partials>5 / first5): {:.3}", hi / lo.max(1e-12));
    println!("  broadband/harmonic energy ratio: {:.4}", bb / harm.max(1e-12));
    Ok(())
}

#[derive(Serialize)]
struct TrainedCell {
    note: u8,
    vel: u8,
    f0: f32,
    lsd_base: f32,
    lsd_trained: f32,
    modal: Vec<Partial>,
    residual: analyze::Residual,
}

fn cmd_train_set(
    lib: PathBuf,
    notes: String,
    partials: usize,
    dur: f32,
    steps: usize,
    out: PathBuf,
) -> Result<()> {
    let want: Vec<u8> = notes.split(',').filter_map(|s| s.trim().parse().ok()).collect();
    let (samples, _) = sample::scan(&lib);
    // every (note, vel) PU cell for the requested notes
    let cells: Vec<(u8, u8)> = samples
        .iter()
        .filter(|s| s.artic == Artic::PedalUp)
        .filter_map(|s| Some((s.note?, s.vel?)))
        .filter(|(n, _)| want.contains(n))
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    let total = cells.len();
    println!("training {total} cells: notes {want:?}, {steps} steps each");

    let done = std::sync::atomic::AtomicUsize::new(0);
    let mut records: Vec<TrainedCell> = cells
        .par_iter()
        .filter_map(|&(note, vel)| {
            let s = find(&samples, Artic::PedalUp, note, vel)?;
            let a = audio::load_mono(&s.path).ok()?;
            let sr = a.sr;
            let r = analyze::analyze_note(&a.samples, sr, analyze::midi_hz(note), partials);
            // onset-align target
            let peak = a.samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
            let onset = a.samples.iter().position(|x| x.abs() > 0.01 * peak).unwrap_or(0);
            let t_len = ((dur * sr as f32) as usize).min(a.samples.len() - onset);
            let real = &a.samples[onset..onset + t_len];
            let mut target = real.to_vec();
            let tp = target.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-6);
            for x in &mut target {
                *x /= tp;
            }
            // DDSP fit (harmonic + noise jointly; bank the harmonic, it transfers)
            let (fitted, _res, _l0, _l1) =
                ddsp::fit_note(&r.modal, &r.residual, &target, real, sr, steps, false).ok()?;
            // measured broadband strike (small, tonal-safe)
            let freqs: Vec<f32> = fitted.iter().map(|p| p.freq).collect();
            let aw = (0.05 * sr as f32) as usize;
            let attack = analyze::attack_noise_model(&real[..aw.min(real.len())], sr, &freqs);
            // LSD baseline (analysis) vs trained (fitted + attack)
            let base = ddsp::render_additive(&r.modal, sr, t_len);
            let mut trained = ddsp::render_additive(&fitted, sr, t_len);
            synth::add_residual(&mut trained, sr, &attack, 1.0, 0xA77AC);
            let lsd_base = analyze::accuracy_lsd(&base, real, sr);
            let lsd_trained = analyze::accuracy_lsd(&trained, real, sr);

            let n = done.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
            eprintln!("  [{n}/{total}] note {note} vel {vel}: {lsd_base:.1} → {lsd_trained:.1} dB");
            Some(TrainedCell {
                note,
                vel,
                f0: r.f0,
                lsd_base,
                lsd_trained,
                modal: fitted,
                residual: attack,
            })
        })
        .collect();
    records.sort_by_key(|c| (c.note, c.vel));

    if let Some(p) = out.parent() {
        std::fs::create_dir_all(p)?;
    }
    std::fs::write(&out, serde_json::to_vec_pretty(&records)?)?;

    // summary
    let mean = |f: &dyn Fn(&TrainedCell) -> f32| {
        records.iter().map(|c| f(c)).sum::<f32>() / records.len().max(1) as f32
    };
    let mb = mean(&|c| c.lsd_base);
    let mt = mean(&|c| c.lsd_trained);
    println!("\n=== {} cells trained ===", records.len());
    for &nn in &want {
        let cells: Vec<&TrainedCell> = records.iter().filter(|c| c.note == nn).collect();
        if cells.is_empty() {
            continue;
        }
        let b = cells.iter().map(|c| c.lsd_base).sum::<f32>() / cells.len() as f32;
        let t = cells.iter().map(|c| c.lsd_trained).sum::<f32>() / cells.len() as f32;
        println!("  note {nn:3} ({} vels): {b:.2} → {t:.2} dB ({:+.2})", cells.len(), t - b);
    }
    println!("  OVERALL: {mb:.2} → {mt:.2} dB  ({:+.2} dB mean)", mt - mb);
    println!("  wrote {} → {}", records.len(), out.display());
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn cmd_attack(
    lib: PathBuf,
    note: u8,
    vel: u8,
    partials: usize,
    attack_ms: f32,
    dur: f32,
    outdir: PathBuf,
) -> Result<()> {
    let (samples, _) = sample::scan(&lib);
    let pu = find(&samples, Artic::PedalUp, note, vel)
        .ok_or_else(|| anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}"))?;
    let a = audio::load_mono(&pu.path)?;
    let sr = a.sr;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, sr, expected, partials);

    // onset-align real
    let peak = a.samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
    let onset = a.samples.iter().position(|x| x.abs() > 0.01 * peak).unwrap_or(0);
    let t_len = ((dur * sr as f32) as usize).min(a.samples.len() - onset);
    let real = &a.samples[onset..onset + t_len];

    // measure the attack-noise model from the isolated strike window
    let aw = (attack_ms * 0.001 * sr as f32) as usize;
    let window = &real[..aw.min(real.len())];
    let freqs: Vec<f32> = r.modal.iter().map(|p| p.freq).collect();
    let attack = analyze::attack_noise_model(window, sr, &freqs);

    // harmonic tone (analysis partials → pure additive, real units)
    let tone = ddsp::render_additive(&r.modal, sr, t_len);
    // tone + measured attack
    let mut tone_attack = tone.clone();
    synth::add_residual(&mut tone_attack, sr, &attack, 1.0, 0xA77AC);

    let lsd_tone = analyze::accuracy_lsd(&tone, real, sr);
    let lsd_ta = analyze::accuracy_lsd(&tone_attack, real, sr);
    println!("note {note} vel {vel}: attack {:.0}ms, T60 {:.1}ms, level {:.4}",
        attack_ms, 6908.0 / attack.decay, attack.level);
    println!("  ACCURACY LSD  tone {lsd_tone:.2} → tone+attack {lsd_ta:.2} dB  ({:+.2} dB)",
        lsd_ta - lsd_tone);

    std::fs::create_dir_all(&outdir)?;
    let mut tw = tone;
    let mut taw = tone_attack;
    let mut realw = real.to_vec();
    for b in [&mut tw, &mut taw, &mut realw] {
        synth::normalize(b, 0.9);
    }
    synth::write_wav(&outdir.join(format!("atk{note}_tone.wav")), &tw, sr)?;
    synth::write_wav(&outdir.join(format!("atk{note}_tone_attack.wav")), &taw, sr)?;
    synth::write_wav(&outdir.join(format!("atk{note}_real.wav")), &realw, sr)?;
    println!("  wrote atk{note}_tone / _tone_attack / _real .wav → {}", outdir.display());
    Ok(())
}

fn cmd_decompose(
    lib: PathBuf,
    note: u8,
    vel: u8,
    attack_ms: f32,
    outdir: PathBuf,
) -> Result<()> {
    let (samples, _) = sample::scan(&lib);
    let pu = find(&samples, Artic::PedalUp, note, vel)
        .ok_or_else(|| anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}"))?;
    let a = audio::load_mono(&pu.path)?;
    let sr = a.sr;
    std::fs::create_dir_all(&outdir)?;

    // onset
    let peak = a.samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs()));
    let onset = a.samples.iter().position(|x| x.abs() > 0.01 * peak).unwrap_or(0);
    let sig = &a.samples[onset..];
    let aw = (attack_ms * 0.001 * sr as f32) as usize;
    let fade = (aw / 4).max(1);

    // ATTACK = strike transient: first `attack_ms`, faded out over the last 25%.
    let mut attack = vec![0.0f32; sig.len().min(aw)];
    for i in 0..attack.len() {
        let w = if i >= attack.len() - fade {
            let x = (attack.len() - i) as f32 / fade as f32;
            0.5 - 0.5 * (std::f32::consts::PI * x).cos()
        } else {
            1.0
        };
        attack[i] = sig[i] * w;
    }

    // TONE = the note with the strike faded IN (its complement): the sustained
    // string sound without the hammer transient.
    let mut tone = sig.to_vec();
    for i in 0..aw.min(tone.len()) {
        let x = i as f32 / aw as f32;
        tone[i] *= 0.5 - 0.5 * (std::f32::consts::PI * x).cos();
    }

    synth::write_wav(&outdir.join(format!("dec{note}_attack.wav")), &attack, sr)?;
    synth::write_wav(&outdir.join(format!("dec{note}_tone.wav")), &tone, sr)?;
    synth::write_wav(&outdir.join(format!("dec{note}_pu.wav")), sig, sr)?;
    println!("note {note} vel {vel}  (sr {sr})");
    println!("  ATTACK  {:.0} ms strike transient → dec{note}_attack.wav", attack_ms);
    println!("  TONE    sustained strings (strike removed) → dec{note}_tone.wav");

    // RELEASE = the damper/release sample.
    if let Some(rel) = find(&samples, Artic::Release, note, vel) {
        let ra = audio::load_mono(&rel.path)?;
        synth::write_wav(&outdir.join(format!("dec{note}_release.wav")), &ra.samples, sr)?;
        println!("  RELEASE damper sample → dec{note}_release.wav");
    } else {
        println!("  RELEASE (no Rel sample at this vel)");
    }

    // SYMPATHETIC = what pedal-down adds over pedal-up (broadband energy diff).
    if let Some(pd) = find(&samples, Artic::PedalDown, note, vel) {
        let pda = audio::load_mono(&pd.path)?;
        let n = pda.samples.len().min(a.samples.len());
        let freqs: Vec<f32> = analyze::analyze_note(&a.samples, sr, analyze::midi_hz(note), 24)
            .modal
            .iter()
            .map(|p| p.freq)
            .collect();
        let pu_bb = analyze::broadband_energy(&a.samples[..n], sr, &freqs, 3.0);
        let pd_bb = analyze::broadband_energy(&pda.samples[..n], sr, &freqs, 3.0);
        synth::write_wav(&outdir.join(format!("dec{note}_pd.wav")), &pda.samples, sr)?;
        println!("  SYMPATHETIC pedal-down inter-partial energy {:.1}× pedal-up → target = PD−PU (dec{note}_pd.wav vs _pu.wav)",
            pd_bb / pu_bb.max(1e-12));
    } else {
        println!("  SYMPATHETIC (no PD sample at this vel)");
    }

    // PEDAL NOISE = a mechanism one-shot.
    if let Some(pn) = samples.iter().find(|s| s.artic == Artic::PedalNoise) {
        let pna = audio::load_mono(&pn.path)?;
        synth::write_wav(&outdir.join("dec_pedal_noise.wav"), &pna.samples, sr)?;
        println!("  PEDAL NOISE mechanism one-shot → dec_pedal_noise.wav");
    }
    println!("wrote components → {}", outdir.display());
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn cmd_train(
    lib: PathBuf,
    note: u8,
    vel: u8,
    partials: usize,
    dur: f32,
    steps: usize,
    outdir: PathBuf,
) -> Result<()> {
    let (samples, _) = sample::scan(&lib);
    let s = find(&samples, Artic::PedalUp, note, vel)
        .ok_or_else(|| anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}"))?;
    let a = audio::load_mono(&s.path)?;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);

    // Build the training target: onset-align the real note (the model renders
    // from the strike at t=0), peak-normalize, take `dur` seconds.
    let onset = a
        .samples
        .iter()
        .position(|x| x.abs() > 0.01 * a.samples.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())))
        .unwrap_or(0);
    let t_len = ((dur * a.sr as f32) as usize).min(a.samples.len() - onset);
    let real = &a.samples[onset..onset + t_len];
    let mut target: Vec<f32> = real.to_vec();
    let peak = target.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-6);
    for x in &mut target {
        *x /= peak;
    }

    println!("note {note} vel {vel}: DDSP fit (early-stopped on LSD), {} partials, {steps} steps",
        r.modal.len());
    let (fitted, _fitted_res, lsd0, lsd_best) =
        ddsp::fit_note(&r.modal, &r.residual, &target, real, a.sr, steps, true)?;
    println!("  harmonic LSD  {lsd0:.2} → {lsd_best:.2} dB (best-kept)");

    // A/B: the trained (best) harmonic + measured attack, vs the dry init.
    let init_render = ddsp::render_additive(&r.modal, a.sr, t_len);
    let mut fit_render = ddsp::render_additive(&fitted, a.sr, t_len);
    let freqs: Vec<f32> = fitted.iter().map(|p| p.freq).collect();
    let aw = (0.05 * a.sr as f32) as usize;
    let attack = analyze::attack_noise_model(&real[..aw.min(real.len())], a.sr, &freqs);
    synth::add_residual(&mut fit_render, a.sr, &attack, 1.0, 0xA77AC);
    let lsd_init = analyze::accuracy_lsd(&init_render, real, a.sr);
    let lsd_fit = analyze::accuracy_lsd(&fit_render, real, a.sr);
    println!("  ACCURACY LSD  {lsd_init:.2} → {lsd_fit:.2} dB  ({:+.2} dB)", lsd_fit - lsd_init);

    std::fs::create_dir_all(&outdir)?;
    let mut dry = init_render;
    let mut trained = fit_render;
    let mut realw = real.to_vec();
    for buf in [&mut dry, &mut trained, &mut realw] {
        synth::normalize(buf, 0.9);
    }
    synth::write_wav(&outdir.join(format!("train{note}_dry.wav")), &dry, a.sr)?;
    synth::write_wav(&outdir.join(format!("train{note}_trained.wav")), &trained, a.sr)?;
    synth::write_wav(&outdir.join(format!("train{note}_real.wav")), &realw, a.sr)?;
    println!("  wrote train{note}_dry / _trained / _real .wav → {}", outdir.display());
    Ok(())
}

fn cmd_fit(
    lib: PathBuf,
    note: u8,
    vel: u8,
    partials: usize,
    dur: f32,
    outdir: PathBuf,
) -> Result<()> {
    let cfg = ModelConfig::city_grand();
    let (samples, _) = sample::scan(&lib);
    let s = find(&samples, Artic::PedalUp, note, vel)
        .ok_or_else(|| anyhow::anyhow!("no pedal-up sample for note {note} vel {vel}"))?;
    let a = audio::load_mono(&s.path)?;
    let expected = analyze::midi_hz(note);
    let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);

    let vel01 = (vel as f32 / 127.0).clamp(0.0, 1.0);
    let seed = ((note as u32) << 8) | vel as u32;
    let real_len = ((dur * a.sr as f32) as usize).min(a.samples.len());
    let real = &a.samples[..real_len];

    // Objective: LSD of the model (partial amps scaled by `amps`) vs the real
    // note. Deterministic — jitter/attack seed is fixed by (note, vel).
    let base = r.modal.clone();
    let eval = |amps: &[f32]| -> f32 {
        let mut m = base.clone();
        for (p, s) in m.iter_mut().zip(amps) {
            p.amp *= *s;
        }
        let model = synth::render(&m, a.sr, dur, vel01, seed, note, &cfg);
        analyze::accuracy_lsd(&model, real, a.sr)
    };

    let n = base.len();
    let mut amps = vec![1.0f32; n];
    let mut best = eval(&amps);
    let lsd0 = best;
    println!("note {note} vel {vel}: fitting {n} partial amplitudes");
    println!("  start LSD {lsd0:.2} dB");

    // Coordinate descent with shrinking multiplicative steps: for each partial,
    // try scaling its amplitude up/down; keep the change if LSD drops.
    let mut evals = 1u32;
    for &step in &[0.6f32, 0.35, 0.18, 0.09, 0.04] {
        let mut improved = true;
        let mut pass = 0;
        while improved && pass < 3 {
            improved = false;
            pass += 1;
            for i in 0..n {
                for &factor in &[1.0 + step, 1.0 - step] {
                    let mut trial = amps.clone();
                    trial[i] = (trial[i] * factor).clamp(0.0, 20.0);
                    let l = eval(&trial);
                    evals += 1;
                    if l < best - 1e-4 {
                        best = l;
                        amps = trial;
                        improved = true;
                    }
                }
            }
        }
        eprintln!("  step {step:.2}: LSD {best:.2} dB ({evals} evals)");
    }

    // Render the fitted note + the dry (unfitted) one + real, for A/B.
    let mut fitted_modal = base.clone();
    for (p, sc) in fitted_modal.iter_mut().zip(&amps) {
        p.amp *= *sc;
    }
    std::fs::create_dir_all(&outdir)?;
    let real_peak = real.iter().cloned().fold(0.0f32, |m, x| m.max(x.abs())).max(1e-4);
    let mut fitted = synth::render(&fitted_modal, a.sr, dur, vel01, seed, note, &cfg);
    let mut dry = synth::render(&base, a.sr, dur, vel01, seed, note, &cfg);
    synth::normalize(&mut fitted, real_peak);
    synth::normalize(&mut dry, real_peak);
    synth::write_wav(&outdir.join(format!("fit{note}_fitted.wav")), &fitted, a.sr)?;
    synth::write_wav(&outdir.join(format!("fit{note}_dry.wav")), &dry, a.sr)?;
    synth::write_wav(&outdir.join(format!("fit{note}_real.wav")), real, a.sr)?;

    println!(
        "  DONE  {lsd0:.2} → {best:.2} dB  ({:+.2} dB, {evals} evals)",
        best - lsd0
    );
    println!("  wrote fit{note}_dry.wav / fit{note}_fitted.wav / fit{note}_real.wav → {}",
        outdir.display());
    Ok(())
}

fn cmd_sweep(lib: PathBuf, vels: String, partials: usize, out: PathBuf) -> Result<()> {
    let want_vels: Vec<u8> = vels
        .split(',')
        .filter_map(|s| s.trim().parse().ok())
        .collect();
    let (samples, _) = sample::scan(&lib);
    let targets: Vec<&Sample> = samples
        .iter()
        .filter(|s| {
            s.artic == Artic::PedalUp && s.vel.map_or(false, |v| want_vels.contains(&v))
        })
        .collect();
    println!("sweeping {} pedal-up samples (vels {:?})...", targets.len(), want_vels);

    let cfg = ModelConfig::city_grand();
    let done = std::sync::atomic::AtomicUsize::new(0);
    let total = targets.len();
    let mut records: Vec<NoteRecord> = targets
        .par_iter()
        .filter_map(|s| {
            let note = s.note?;
            let vel = s.vel?;
            let a = audio::load_mono(&s.path).ok()?;
            let expected = analyze::midi_hz(note);
            let r = analyze::analyze_note(&a.samples, a.sr, expected, partials);

            // Self-calibrate the residual against the dry model + real sample so
            // the stored level is correct for realtime playback (gain = 1).
            let vel01 = (vel as f32 / 127.0).clamp(0.0, 1.0);
            let seed = ((note as u32) << 8) | vel as u32;
            let dry = synth::render(&r.modal, a.sr, 3.0, vel01, seed, note, &cfg);
            let freqs: Vec<f32> = r.modal.iter().map(|p| p.freq).collect();
            let mut residual = r.residual.clone();
            calibrate_residual(&mut residual, &a.samples, &dry, a.sr, &freqs, 3.0);

            let n = done.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
            if n % 25 == 0 || n == total {
                eprintln!("  {n}/{total}");
            }
            Some(NoteRecord {
                note,
                vel,
                f0: r.f0,
                inharmonicity_b: r.inharmonicity_b,
                decay_t60: r.decay_t60,
                peak_rms: r.peak_rms,
                modal: r.modal,
                residual,
            })
        })
        .collect();
    records.sort_by_key(|r| (r.note, r.vel));

    if let Some(parent) = out.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&out, serde_json::to_vec_pretty(&records)?)?;
    println!("wrote {} records → {}", records.len(), out.display());
    Ok(())
}

//! `sampler` subcommand handlers — split from cli/lib.rs.

use super::*;

pub(crate) async fn run_sampler(cmd: &SamplerCommand) -> Result<()> {
    match cmd {
        SamplerCommand::Midi => {
            let ports = midi_port_names();
            if ports.is_empty() {
                println!("No MIDI input ports detected.");
            } else {
                println!("MIDI input ports:");
                for (idx, name) in ports.iter().enumerate() {
                    println!("  {}. {}", idx + 1, name);
                }
            }
            Ok(())
        }
        SamplerCommand::Prepare {
            spec,
            samples_root,
            cache_dir,
        } => {
            let start = std::time::Instant::now();
            let patch = PlayerPatch::load(spec, samples_root).map_err(|e| {
                eyre::eyre!(
                    "failed to scan library {} with samples root {}: {e}",
                    spec.display(),
                    samples_root.display()
                )
            })?;
            let cache_dir = cache_dir
                .clone()
                .unwrap_or_else(|| default_prepared_cache_dir(samples_root));

            println!(
                "Preparing {} samples into {}...",
                patch.total_samples(),
                cache_dir.display()
            );
            let stats =
                prepare_sample_cache(&cache_dir, patch.sample_paths().map(|p| p.as_path()))?;
            println!(
                "Prepared cache: {} samples, {:.1} MiB decoded PCM, {} failures, {:.2}s.",
                stats.prepared,
                stats.bytes as f64 / 1024.0 / 1024.0,
                stats.failed,
                start.elapsed().as_secs_f64()
            );
            Ok(())
        }
        SamplerCommand::Pack {
            spec,
            samples_root,
            output,
        } => {
            let start = std::time::Instant::now();
            let patch = PlayerPatch::load(spec, samples_root).map_err(|e| {
                eyre::eyre!(
                    "failed to scan library {} with samples root {}: {e}",
                    spec.display(),
                    samples_root.display()
                )
            })?;
            let output = output
                .clone()
                .unwrap_or_else(|| samples_root.join("library.signalpack"));

            println!(
                "Packing {} decoded PCM samples into {}...",
                patch.total_samples(),
                output.display()
            );
            let stats = create_signal_pack(
                &output,
                spec,
                samples_root,
                patch.sample_paths().map(|p| p.as_path()),
            )?;
            println!(
                "Signal pack complete: {} files, {:.1} MiB decoded PCM, {} failures, {:.2}s.",
                stats.prepared,
                stats.bytes as f64 / 1024.0 / 1024.0,
                stats.failed,
                start.elapsed().as_secs_f64()
            );
            Ok(())
        }
        SamplerCommand::Retag {
            root,
            skip,
            dry_run,
        } => {
            use signal_sampler::retag::{derive, discover_packs, retag_tree};
            if !root.is_dir() {
                eyre::bail!("not a directory: {}", root.display());
            }
            if *dry_run {
                let packs = discover_packs(root, skip);
                println!("found {} packs under {}", packs.len(), root.display());
                for p in packs.iter().take(20) {
                    let d = derive(p, root);
                    println!("\n{}", p.display());
                    println!("  instrument: {:?}", d.instrument);
                    println!("  category:   {:?}", d.category);
                    println!("  style:      {:?}", d.style);
                    println!("  tags:       {} entries", d.tags.len());
                }
                return Ok(());
            }
            let summary = retag_tree(root, skip, |n, total| {
                if n % 250 == 0 || n == total {
                    println!("  retagged {n}/{total}");
                }
            })?;
            println!(
                "Retag complete: {} ok, {} failed, {:.2}s.",
                summary.ok, summary.failed, summary.elapsed_secs,
            );
            Ok(())
        }
        SamplerCommand::Export { pack, output_dir } => {
            let start = std::time::Instant::now();
            println!(
                "Exporting {} into {}...",
                pack.display(),
                output_dir.display()
            );
            let stats = extract_signal_pack(pack, output_dir)?;
            println!(
                "Signal pack exported: {} WAV files, {:.1} MiB PCM, {:.2}s.",
                stats.prepared,
                stats.bytes as f64 / 1024.0 / 1024.0,
                start.elapsed().as_secs_f64()
            );
            Ok(())
        }
        SamplerCommand::Inspect { files } => {
            for file in files {
                let data = load_sample(file)
                    .map_err(|e| eyre::eyre!("failed to load sample {}: {e}", file.display()))?;
                let samples = data.frames.iter().copied();
                let mut peak = 0.0f32;
                let mut sum_sq = 0.0f64;
                let mut early_sum_sq = 0.0f64;
                let early_samples = (data.sample_rate as usize / 10 * data.channels as usize)
                    .min(data.frames.len());
                for (idx, sample) in samples.enumerate() {
                    peak = peak.max(sample.abs());
                    sum_sq += (sample as f64) * (sample as f64);
                    if idx < early_samples {
                        early_sum_sq += (sample as f64) * (sample as f64);
                    }
                }
                let rms = (sum_sq / data.frames.len().max(1) as f64).sqrt();
                let early_rms = (early_sum_sq / early_samples.max(1) as f64).sqrt();
                let seconds = data.num_frames as f64 / data.sample_rate as f64;
                println!(
                    "{}\n  channels={} sample_rate={} frames={} seconds={:.3} peak={:.4} rms={:.5} first_100ms_rms={:.5}",
                    file.display(),
                    data.channels,
                    data.sample_rate,
                    data.num_frames,
                    seconds,
                    peak,
                    rms,
                    early_rms
                );
            }
            Ok(())
        }
        SamplerCommand::Play {
            spec,
            samples_root,
            section,
            mic,
            instrument,
            channel,
            device,
            sample_rate,
            buffer_size,
            preload,
            cache_budget_mib,
            enforce_cache_budget,
            preload_profile,
            log_midi,
        } => {
            let spec_data = LibrarySpec::from_file(spec)
                .map_err(|e| eyre::eyre!("failed to read library spec {}: {e}", spec.display()))?;

            let section = section
                .clone()
                .or_else(|| spec_data.sections.first().map(|s| s.id.clone()))
                .ok_or_else(|| eyre::eyre!("library spec has no sections; pass --section"))?;
            let mic = mic
                .clone()
                .or_else(|| spec_data.mics.first().map(|m| m.id.clone()))
                .ok_or_else(|| eyre::eyre!("library spec has no mics; pass --mic"))?;

            println!(
                "Loading {} ({}) section={} mic={}",
                spec_data.name,
                spec.display(),
                section,
                mic
            );

            let buffer_size = if *buffer_size == 0 {
                None
            } else {
                Some(*buffer_size)
            };
            let cache_budget_bytes = cache_budget_mib.map(|mib| mib.saturating_mul(1024 * 1024));
            let player = SamplerRig::with_device_config_and_cache_budget(
                device.as_deref(),
                Some(*sample_rate),
                buffer_size,
                cache_budget_bytes,
            )?;
            let preload_profile = PreloadProfile::from_name(preload_profile)
                .ok_or_else(|| eyre::eyre!("unknown --preload-profile: {preload_profile}"))?;
            player.set_preload_profile(preload_profile);
            player.load_instrument(
                instrument.clone(),
                spec,
                samples_root.as_deref(),
                section,
                mic,
            )?;

            if *preload {
                println!("Preloading samples for {instrument}...");
                let start = std::time::Instant::now();
                let stats = player.preload_instrument(instrument)?;
                println!(
                    "Preload complete: {} samples, {:.1} MiB decoded PCM, {} failures, {:.2}s.",
                    stats.loaded,
                    stats.bytes as f64 / 1024.0 / 1024.0,
                    stats.failed,
                    start.elapsed().as_secs_f64()
                );
                if *enforce_cache_budget {
                    let evicted = player.evict_cache_over_budget();
                    if evicted.evicted > 0 {
                        println!(
                            "Cache eviction: {} samples, {:.1} MiB freed ({:.1} -> {:.1} MiB).",
                            evicted.evicted,
                            evicted.bytes_freed as f64 / 1024.0 / 1024.0,
                            evicted.bytes_before as f64 / 1024.0 / 1024.0,
                            evicted.bytes_after as f64 / 1024.0 / 1024.0
                        );
                    }
                }
            }

            if let Some(channel) = channel {
                if !(1..=16).contains(channel) {
                    eyre::bail!("--channel must be between 1 and 16");
                }
                player.set_midi_channel(instrument.clone(), *channel);
                println!("Routing MIDI channel {channel} -> {instrument}");
            } else {
                for ch in 1..=16 {
                    player.set_midi_channel(instrument.clone(), ch);
                }
                println!("Routing all MIDI channels -> {instrument}");
            }

            let ports = midi_port_names();
            if ports.is_empty() {
                println!(
                    "No MIDI input ports detected. Audio stream is open; press Ctrl-C to quit."
                );
            } else {
                println!("Listening to MIDI input ports:");
                for name in &ports {
                    println!("  - {name}");
                }
                println!("Press Ctrl-C to quit.");
            }

            let midi = CliMidiInput::open_all();
            let mut last_audio_stats = player.audio_stats();
            let mut last_audio_stats_log = std::time::Instant::now();
            loop {
                tokio::select! {
                    _ = tokio::signal::ctrl_c() => {
                        println!("Stopping sampler.");
                        break;
                    }
                    _ = tokio::time::sleep(std::time::Duration::from_millis(5)) => {
                        if let Some(ref midi) = midi {
                            for ev in midi.drain() {
                                let channel = (ev.status & 0x0F) + 1;
                                player.midi_message(channel, ev.status, ev.note, ev.velocity);
                                if *log_midi {
                                    if ev.is_note_on() {
                                        println!("note on  ch={channel:02} note={} vel={}", ev.note, ev.velocity);
                                    } else if ev.is_note_off() {
                                        println!("note off ch={channel:02} note={} vel={}", ev.note, ev.velocity);
                                    } else if ev.is_cc64() {
                                        println!("cc64     ch={channel:02} value={}", ev.velocity);
                                    }
                                }
                            }
                        }

                        if last_audio_stats_log.elapsed() >= std::time::Duration::from_secs(1) {
                            let stats = player.audio_stats();
                            if *enforce_cache_budget && stats.cache_over_budget_bytes > 0 {
                                let evicted = player.evict_cache_over_budget();
                                if evicted.evicted > 0 {
                                    eprintln!(
                                        "cache eviction: evicted={} freed_mib={:.1} cache_mib={:.1}->{:.1}",
                                        evicted.evicted,
                                        evicted.bytes_freed as f64 / 1024.0 / 1024.0,
                                        evicted.bytes_before as f64 / 1024.0 / 1024.0,
                                        evicted.bytes_after as f64 / 1024.0 / 1024.0
                                    );
                                }
                            }
                            if stats.stream_errors != last_audio_stats.stream_errors
                                || stats.callback_overruns != last_audio_stats.callback_overruns
                                || stats.lock_misses != last_audio_stats.lock_misses
                                || stats.midi_messages != last_audio_stats.midi_messages
                                || stats.dropped_events != last_audio_stats.dropped_events
                                || stats.cache_misses != last_audio_stats.cache_misses
                                || stats.sample_misses != last_audio_stats.sample_misses
                                || stats.resize_events != last_audio_stats.resize_events
                                || stats.cache_over_budget_bytes
                                    != last_audio_stats.cache_over_budget_bytes
                                || stats.recent_cache_misses
                                    != last_audio_stats.recent_cache_misses
                                || stats.recent_sample_misses
                                    != last_audio_stats.recent_sample_misses
                            {
                                eprintln!(
                                    "audio diag: stream_errors={} callback_overruns={} lock_misses={} callbacks={} midi_messages={} dropped_events={} pending_events={} stolen_voices={} cache_misses={} sample_misses={} resize_events={} recent_cache_misses={} recent_sample_misses={} cache_mib={:.1} cache_budget_mib={} cache_over_mib={:.1} last_render_us={} max_render_us={} buffer_budget_us={} last_callback_interval_us={} max_callback_interval_us={} last_midi_to_callback_us={} max_midi_to_callback_us={}",
                                    stats.stream_errors,
                                    stats.callback_overruns,
                                    stats.lock_misses,
                                    stats.callbacks,
                                    stats.midi_messages,
                                    stats.dropped_events,
                                    stats.pending_events,
                                    stats.stolen_voices,
                                    stats.cache_misses,
                                    stats.sample_misses,
                                    stats.resize_events,
                                    if stats.recent_cache_misses.is_empty() {
                                        "none".to_string()
                                    } else {
                                        stats.recent_cache_misses.join(" | ")
                                    },
                                    if stats.recent_sample_misses.is_empty() {
                                        "none".to_string()
                                    } else {
                                        stats.recent_sample_misses.join(" | ")
                                    },
                                    stats.loaded_sample_bytes as f64 / 1024.0 / 1024.0,
                                    stats
                                        .cache_budget_bytes
                                        .map(|bytes| format!("{:.1}", bytes as f64 / 1024.0 / 1024.0))
                                        .unwrap_or_else(|| "none".to_string()),
                                    stats.cache_over_budget_bytes as f64 / 1024.0 / 1024.0,
                                    stats.last_render_us,
                                    stats.max_render_us,
                                    stats.buffer_budget_us,
                                    stats.last_callback_interval_us,
                                    stats.max_callback_interval_us,
                                    stats.last_midi_to_callback_us,
                                    stats.max_midi_to_callback_us,
                                );
                                last_audio_stats = stats;
                            }
                            last_audio_stats_log = std::time::Instant::now();
                        }
                    }
                }
            }

            Ok(())
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CliMidiEvent {
    status: u8,
    note: u8,
    velocity: u8,
}

impl CliMidiEvent {
    fn is_note_on(self) -> bool {
        (self.status & 0xF0) == 0x90 && self.velocity > 0
    }

    fn is_note_off(self) -> bool {
        (self.status & 0xF0) == 0x80 || ((self.status & 0xF0) == 0x90 && self.velocity == 0)
    }

    fn is_cc64(self) -> bool {
        (self.status & 0xF0) == 0xB0 && self.note == 64
    }
}

struct CliMidiInput {
    rx: Receiver<CliMidiEvent>,
    _connections: Vec<MidiInputConnection<()>>,
}

impl CliMidiInput {
    fn open_all() -> Option<Self> {
        let midi_in = MidirInput::new("signal-cli").ok()?;
        let ports = midi_in.ports();
        if ports.is_empty() {
            return None;
        }

        let (tx, rx) = bounded::<CliMidiEvent>(256);
        let mut connections = Vec::new();

        for port in &ports {
            let port_name = midi_in.port_name(port).unwrap_or_else(|_| "unknown".into());
            let Ok(input) = MidirInput::new("signal-cli") else {
                continue;
            };
            let tx = tx.clone();

            match input.connect(
                port,
                "signal-sampler-rx",
                move |_timestamp, message, _| {
                    if message.len() >= 3 {
                        let _ = tx.try_send(CliMidiEvent {
                            status: message[0],
                            note: message[1],
                            velocity: message[2],
                        });
                    }
                },
                (),
            ) {
                Ok(connection) => connections.push(connection),
                Err(e) => tracing::warn!("MIDI: failed to open \"{}\": {}", port_name, e),
            }
        }

        if connections.is_empty() {
            None
        } else {
            Some(Self {
                rx,
                _connections: connections,
            })
        }
    }

    fn drain(&self) -> impl Iterator<Item = CliMidiEvent> + '_ {
        self.rx.try_iter()
    }
}

pub(crate) fn midi_port_names() -> Vec<String> {
    let Ok(midi_in) = MidirInput::new("signal-cli") else {
        return Vec::new();
    };

    midi_in
        .ports()
        .iter()
        .map(|port| midi_in.port_name(port).unwrap_or_else(|_| "unknown".into()))
        .collect()
}

// ============================================================================
// Block Type Resolution
// ============================================================================

pub(crate) fn parse_block_type(s: &str) -> Result<signal::BlockType> {
    signal::BlockType::from_str(s)
        .ok_or_else(|| eyre::eyre!("Unknown block type: \"{s}\". Valid types: amp, drive, eq, reverb, delay, compressor, gate, chorus, flanger, phaser, tremolo, cabinet, etc."))
}

pub(crate) fn parse_module_type(s: &str) -> Result<signal::ModuleType> {
    signal::ModuleType::from_str(s)
        .ok_or_else(|| eyre::eyre!("Unknown module type: \"{s}\". Valid types: amp, drive, eq, time, dynamics, modulation, special, source, volume, master, etc."))
}

// ============================================================================
// Command Implementations — Presets
// ============================================================================


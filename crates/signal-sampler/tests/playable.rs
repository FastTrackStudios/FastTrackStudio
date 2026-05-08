//! Verify that real packs load and produce non-zero audio when triggered.
//!
//! Skips when AudioHaven isn't mounted. Walks one representative pack from
//! each library family, calls `SamplerPlayer::load_pack`, fires note_on, and
//! asserts the rendered audio buffer has non-zero RMS.

use signal_sampler::{PlayerPatch, SampleEngine, SamplerBank, SamplerPlayer, read_pack_header};
use std::path::Path;

const SAMPLES: &[(&str, &str, u8)] = &[
    (
        "Keyscape Wurlitzer",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
        60,
    ),
    (
        "Stylus Default",
        "/run/media/AudioHaven/Signal/Libraries/Drum Kits/Stylus RMX/Packs/Stylus RMX/Core Library/Default/library.signalpack",
        36,
    ),
    (
        "Trilian Big Acid Bass",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Trilian/Packs/Synth Bass/Synth 01/Big Acid Bass/library.signalpack",
        48,
    ),
];

#[test]
fn header_parses_for_each_family() {
    for (label, path, _note) in SAMPLES {
        let p = Path::new(path);
        if !p.exists() {
            eprintln!("skip {label} (missing)");
            continue;
        }
        let h = match read_pack_header(p) {
            Ok(h) => h,
            Err(e) => panic!("{label}: header parse failed: {e}"),
        };
        eprintln!(
            "== {label} == name={:?} zones={} grooves={} samples={}",
            h.spec.name,
            h.spec.zones.len(),
            h.spec.grooves.len(),
            h.sample_count,
        );
    }
}

#[test]
fn keyscape_resolves_a_sample() {
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let patch = PlayerPatch::from_pack(path).expect("load_pack header");
    eprintln!(
        "patch zoned={} convention map size={}",
        patch.is_zoned(),
        patch.map.total()
    );
    eprintln!(
        "first articulation: {:?}",
        patch.spec.articulations.first().map(|a| (&a.id, &a.kind))
    );
    eprintln!(
        "first section: {:?}",
        patch.spec.sections.first().map(|s| &s.id)
    );
    eprintln!("first mic: {:?}", patch.spec.mics.first().map(|m| &m.id));

    // Try resolving C4 (60) at a few articulation/dynamic combos.
    let section = patch
        .spec
        .sections
        .first()
        .map(|s| s.id.clone())
        .unwrap_or_default();
    let mic = patch
        .spec
        .mics
        .first()
        .map(|m| m.id.clone())
        .unwrap_or_default();
    let artic = patch
        .spec
        .articulations
        .first()
        .map(|a| a.id.clone())
        .unwrap_or_default();
    // Mirror the engine's velocity → dynamic mapping.
    let artic_spec = patch.spec.articulation(&artic).unwrap();
    let dynamics = &artic_spec.dynamics;
    eprintln!("dynamics list: {:?}", dynamics);
    // Mirror the engine's velocity → dynamic mapping.
    let artic_spec = patch.spec.articulation(&artic).unwrap();
    let dynamics = &artic_spec.dynamics;
    eprintln!("dynamics list: {:?}", dynamics);
    for note in [48u8, 60u8, 72u8] {
        for vel in [40u8, 80u8, 100u8, 120u8] {
            let n = dynamics.len();
            let idx = (vel as usize * n / 128).min(n - 1);
            let dyn_label = &dynamics[idx];
            let r = patch.resolve(&section, &artic, &mic, dyn_label, note, "", 0);
            eprintln!(
                "  note={note} vel={vel} dyn={dyn_label:?} → {}",
                if r.is_some() { "FOUND" } else { "miss" }
            );
        }
    }
}

#[test]
fn keyscape_engine_renders_audio() {
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let patch = PlayerPatch::from_pack(path).expect("load_pack");
    let section = patch
        .spec
        .sections
        .first()
        .map(|s| s.id.clone())
        .unwrap_or_default();
    let mic = patch
        .spec
        .mics
        .first()
        .map(|m| m.id.clone())
        .unwrap_or_default();
    let mut engine = SampleEngine::new(patch, 48_000, &section, &mic);
    // Audio thread uses get_loaded (lock-free try_read) and skips voices
    // whose samples aren't decoded yet. Preload synchronously here so the
    // single render call has data to play.
    engine.preload_samples();
    engine.note_on(60, 100);
    let mut buf = vec![0.0f32; 4096 * 2]; // stereo block
    engine.render(&mut buf);
    let rms: f32 = (buf.iter().map(|s| s * s).sum::<f32>() / buf.len() as f32).sqrt();
    eprintln!("rms after note_on(60, 100): {rms}");
    assert!(rms > 1e-6, "Keyscape engine produced silent buffer");
}

#[test]
fn many_libraries_render_audio() {
    let candidates: &[&str] = &[
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Rhodes - Classic.signalpack",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/MKS-20 E Piano 2.signalpack",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Clavichord.signalpack",
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Glock Toy Piano.signalpack",
    ];
    let mut tested = 0;
    let mut silent: Vec<String> = Vec::new();
    for path in candidates {
        let p = Path::new(path);
        if !p.exists() {
            continue;
        }
        tested += 1;
        let mut bank = SamplerBank::new(48_000);
        if let Err(e) = bank.load_pack("tui", p) {
            eprintln!("load fail {}: {e}", p.display());
            silent.push(p.display().to_string());
            continue;
        }
        // Bank.load_pack now spawns a background preloader; force a
        // synchronous preload here so this offline-render test sees a fully
        // populated cache before we fire notes.
        bank.preload_instrument("tui").expect("preload");
        let mut max_rms: f32 = 0.0;
        for note in [36u8, 48, 60, 72, 84] {
            bank.note_on("tui", note, 100);
            let mut buf = vec![0.0f32; 4096 * 2];
            bank.render(&mut buf);
            let rms: f32 = (buf.iter().map(|s| s * s).sum::<f32>() / buf.len() as f32).sqrt();
            if rms > max_rms {
                max_rms = rms;
            }
            bank.note_off("tui", note);
        }
        eprintln!(
            "  {:55} max_rms={max_rms}",
            p.file_stem().unwrap().to_string_lossy()
        );
        if max_rms < 1e-6 {
            silent.push(p.display().to_string());
        }
    }
    eprintln!("tested {tested}, silent {}", silent.len());
    for s in &silent {
        eprintln!("  SILENT: {s}");
    }
    assert!(
        silent.is_empty(),
        "{} of {} packs silent",
        silent.len(),
        tested
    );
}

/// Full TUI-style integration: open the actual cpal output, load Keyscape,
/// fire a few notes, sleep so the audio callback drains them, query stats.
/// If voice activity is non-zero, the audio chain is end-to-end alive.
#[test]
fn keyscape_via_player_full_chain() {
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let player = match SamplerPlayer::new() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("skip — cpal output unavailable: {e}");
            return;
        }
    };
    player.load_pack("tui", path).expect("load_pack via player");
    // Settle: skip the lock-contention fallout from the synchronous preload
    // (callback was running but couldn't grab the bank mutex while decode
    // held it). Reset stats and measure clean post-preload behaviour.
    std::thread::sleep(std::time::Duration::from_millis(200));
    player.reset_audio_stats();

    for note in [60u8, 64, 67, 72] {
        player.note_on("tui", note, 100);
    }
    std::thread::sleep(std::time::Duration::from_millis(500));
    let stats = player.audio_stats();
    eprintln!(
        "post-preload stats: callbacks={} overruns={} stream_errors={} max_render_us={} lock_misses={}",
        stats.callbacks,
        stats.callback_overruns,
        stats.stream_errors,
        stats.max_render_us,
        stats.lock_misses,
    );
    for note in [60u8, 64, 67, 72] {
        player.note_off("tui", note);
    }
    assert!(stats.callbacks > 50, "cpal callbacks not flowing");
    assert!(
        stats.max_render_us < 5_000,
        "render time {}us exceeds 256-frame budget at 48k",
        stats.max_render_us,
    );
    assert_eq!(stats.stream_errors, 0, "stream errors after preload");
    assert_eq!(stats.callback_overruns, 0, "overruns after preload");
}

/// Verify the Kontakt-style background preloader: load_pack returns
/// quickly while a worker thread fills the cache, and notes become audible
/// shortly afterwards once enough samples are decoded.
#[test]
fn keyscape_background_preload_streams_in() {
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let player = match SamplerPlayer::new() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("skip — cpal output unavailable: {e}");
            return;
        }
    };
    let load_start = std::time::Instant::now();
    player.load_pack("tui", path).expect("load_pack");
    let load_elapsed = load_start.elapsed();
    eprintln!("load_pack returned in {} ms", load_elapsed.as_millis());
    for ms in [100, 500, 1000, 2000, 3000] {
        std::thread::sleep(std::time::Duration::from_millis(
            ms - if ms == 100 { 0 } else { ms / 2 },
        ));
        let (l, t) = player.preload_progress("tui");
        eprintln!("  +{ms} ms → {l}/{t}");
    }
    assert!(
        load_elapsed.as_millis() < 250,
        "load_pack blocked for {} ms — preload should be backgrounded",
        load_elapsed.as_millis()
    );

    // Wait for the preloader to make meaningful progress, then fire notes.
    std::thread::sleep(std::time::Duration::from_secs(3));
    let (loaded, total) = player.preload_progress("tui");
    eprintln!("after 3s wait: preload {loaded}/{total}");
    assert!(
        loaded > total / 2,
        "preloader should have decoded majority of samples"
    );

    player.reset_audio_stats();
    for note in [60u8, 64, 67, 72] {
        player.note_on("tui", note, 100);
    }
    std::thread::sleep(std::time::Duration::from_millis(500));
    let stats = player.audio_stats();
    eprintln!(
        "post-stream stats: callbacks={} max_render_us={} stream_errors={}",
        stats.callbacks, stats.max_render_us, stats.stream_errors,
    );
    assert!(stats.callbacks > 50, "no audio callbacks fired");
    assert!(
        stats.max_render_us < 5_000,
        "render time {}us exceeds budget",
        stats.max_render_us,
    );
    assert_eq!(stats.callback_overruns, 0);
}

/// Verify middle-out priority preloading: shortly after `load_pack`, the
/// central note range should be cached while the extremes are not.
#[test]
fn keyscape_middle_out_priority() {
    use signal_sampler::PlayerPatch;
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }

    let player = match SamplerPlayer::new() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("skip — cpal output unavailable: {e}");
            return;
        }
    };
    player.load_pack("tui", path).expect("load_pack");
    // Give the preloader long enough to do ~10–20% of the work.
    std::thread::sleep(std::time::Duration::from_millis(800));
    let (l, t) = player.preload_progress("tui");
    eprintln!("preloaded {l}/{t} after 800ms");
    assert!(l > 0 && l < t, "expected partial preload");

    // Inspect the patch's expected ordering: paths near middle C should
    // come BEFORE paths far from middle C.
    let patch = PlayerPatch::from_pack(path).expect("read pack");
    let ordered = patch.sample_paths_centered(60);
    let half = ordered.len() / 4;
    let mut near = 0;
    let mut far = 0;
    for p in ordered.iter().take(half) {
        let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        // Keyscape stems contain the note number as the second whitespace-
        // separated field (e.g. `NMWurl 60 a 80-89-o`).
        if let Some(note_str) = stem.split_whitespace().nth(1) {
            if let Ok(n) = note_str.parse::<i32>() {
                if (n - 60).abs() <= 24 {
                    near += 1
                } else {
                    far += 1
                }
            }
        }
    }
    eprintln!("first quarter of priority list: {near} within ±24 of C4, {far} beyond",);
    assert!(
        near > far * 4,
        "priority list isn't middle-out: near={near} far={far}",
    );
}

/// Verify a `.signalblock` file parses, resolves its referenced
/// `.signalpack`, and produces audio when triggered.
#[test]
fn signalblock_loads_and_plays() {
    use signal_sampler::BlockSpec;
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Drum Kits/GGD Modern and Massive 2/Blocks/Tama Bubinga Kick.signalblock",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let spec = BlockSpec::from_file(path).expect("parse .signalblock");
    eprintln!(
        "block: name={:?} type={:?} pack={:?} gain_db={}",
        spec.name, spec.block_type, spec.pack, spec.params.gain_db,
    );
    assert_eq!(spec.block_type, "sampler");
    assert!(spec.pack.contains("Kick"));

    let mut bank = SamplerBank::new(48_000);
    bank.load_block("kick", path).expect("load_block via bank");
    bank.preload_instrument("kick").expect("preload");
    bank.note_on("kick", 36, 100);
    let mut buf = vec![0.0f32; 4096 * 2];
    bank.render(&mut buf);
    let rms: f32 = (buf.iter().map(|s| s * s).sum::<f32>() / buf.len() as f32).sqrt();
    eprintln!("rms after note 36: {rms}");
    assert!(rms > 1e-6, "block produced silent buffer");
}

/// Verify the MM2 Standard Kit engine preset loads, routes per-note, and
/// streams in priority order (kick/snare first, china last).
#[test]
fn mm2_engine_preset_loads_with_priority() {
    use signal_sampler::bank::EnginePreset;
    let preset_path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Drum Kits/GGD Modern and Massive 2/Engines/Standard Kit.engine.styx",
    );
    if !preset_path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let preset = match EnginePreset::from_file(preset_path) {
        Ok(p) => p,
        Err(e) => panic!("preset parse failed: {e}"),
    };
    eprintln!(
        "loaded preset {:?}: {} slots",
        preset.name,
        preset.slots.len()
    );
    assert!(preset.slots.len() >= 10);

    let mut bank = SamplerBank::new(48_000);
    let preset_dir = preset_path.parent().unwrap();
    let ids = bank
        .load_engine_preset("mm2", &preset, preset_dir)
        .expect("load_engine_preset");
    eprintln!("registered {} sub-instrument ids", ids.len());

    // Wait briefly and check that the kick (priority 0) has progress
    // before china (priority 7). The kit preload uses ONE coordinator that
    // walks slots in priority order — so the highest-priority pack should
    // be loading or done while the lowest-priority pack hasn't started.
    std::thread::sleep(std::time::Duration::from_millis(800));
    let (kick_l, kick_t) = bank.preload_progress("mm2:36");
    let (china_l, china_t) = bank.preload_progress("mm2:52");
    eprintln!("after 800ms: kick {kick_l}/{kick_t}, china {china_l}/{china_t}");
    assert!(kick_l > 0, "kick should have started loading");
    assert!(
        kick_l > china_l,
        "kick (priority 0) should be ahead of china (priority 7): {kick_l} vs {china_l}"
    );

    // Fire a kick — note routing should dispatch to the kick slot.
    bank.note_on("mm2", 36, 100);
    let mut buf = vec![0.0f32; 4096 * 2];
    bank.render(&mut buf);
    let rms: f32 = (buf.iter().map(|s| s * s).sum::<f32>() / buf.len() as f32).sqrt();
    eprintln!("kick rms after note 36: {rms}");
    assert!(
        rms > 1e-6,
        "kick produced silent buffer (routing or preload broken)"
    );
}

/// Reproduce what the TUI does: load_pack via SamplerBank, fire note_on, mix.
/// Catches integration issues that don't show up loading via PlayerPatch alone.
#[test]
fn keyscape_via_bank_renders_audio() {
    let path = Path::new(
        "/run/media/AudioHaven/Signal/Libraries/Keys/Keyscape/Packs/Wurlitzer 200A.signalpack",
    );
    if !path.exists() {
        eprintln!("skip (missing)");
        return;
    }
    let mut bank = SamplerBank::new(48_000);
    bank.load_pack("tui", path).expect("load_pack via bank");
    bank.preload_instrument("tui").expect("preload"); // wait for streaming preload
    bank.note_on("tui", 60, 100);
    let mut buf = vec![0.0f32; 4096 * 2];
    bank.render(&mut buf);
    let rms: f32 = (buf.iter().map(|s| s * s).sum::<f32>() / buf.len() as f32).sqrt();
    eprintln!("bank rms: {rms}");
    assert!(rms > 1e-6, "bank produced silent buffer for Keyscape");
}

//! Host-simulation tests for the realtime transport scheduler (document
//! mode phase 2 — see `docs/plan/document-mode.md`, "Determinism").
//!
//! These drive the exact code `SignalSamplerClap::process()` delegates to
//! ([`RealtimeScheduler::process_block`]) with a fake host transport
//! advancing block-by-block, and assert the realtime walk reproduces the
//! offline reference walker (`document::render_schedule`) **bit-exactly** at
//! equal sample rate — the plan's hard determinism requirement. (The
//! nice-plug `Buffer`/`ProcessContext` types cannot be constructed outside
//! the host wrapper, so the thin process() adapter itself — transport
//! snapshot, event drain, de-interleave — is exercised in REAPER, while all
//! scheduling/rendering behavior is asserted here.)
//!
//! Fixture library: the same synthetic CSS-shaped legato patch as
//! `signal-sampler/tests/document_mode.rs`.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use signal_sampler::document::{DocumentRenderOptions, render_schedule};
use signal_sampler::{
    BlockTransport, DocNote, PlayMode, RealtimeScheduler, SamplerBank, Schedule, TrackDocument,
    annotate,
};

const SR: u32 = 48_000;
const ID: &str = "fixture";
/// Fixture legato pre-delay for velocities 0–64.
const SLOW_DELAY_FRAMES: u64 = 333 * SR as u64 / 1000;

// ── Fixture library (parity with signal-sampler/tests/document_mode.rs) ──────

fn write_sine_wav(path: &Path, frames: usize, freq: f64, amp: f32) {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: SR,
        bits_per_sample: 32,
        sample_format: hound::SampleFormat::Float,
    };
    let mut w = hound::WavWriter::create(path, spec).expect("create wav");
    for i in 0..frames {
        let t = i as f64 / SR as f64;
        let s = (t * freq * std::f64::consts::TAU).sin() as f32 * amp;
        w.write_sample(s).expect("write sample");
    }
    w.finalize().expect("finalize wav");
}

fn build_fixture(dir: &Path) -> PathBuf {
    std::fs::create_dir_all(dir).expect("mkdir");
    write_sine_wav(&dir.join("sus.wav"), SR as usize, 220.0, 0.5);
    let mut zones = String::from(
        "    { file sus.wav, key_min 0, key_max 127, root_key 60, articulation Sus }\n",
    );
    for rr in 0..4u32 {
        for dirn in ["up", "down"] {
            let f = format!("leg_{dirn}_{rr}.wav");
            write_sine_wav(&dir.join(&f), SR as usize, 300.0 + 40.0 * rr as f64, 0.4);
            zones.push_str(&format!(
                "    {{ file {f}, key_min 0, key_max 127, root_key 64, articulation Leg, direction {dirn}, rr_index {rr} }}\n"
            ));
        }
        let f = format!("stac_{rr}.wav");
        write_sine_wav(
            &dir.join(&f),
            SR as usize / 4,
            500.0 + 60.0 * rr as f64,
            0.4,
        );
        zones.push_str(&format!(
            "    {{ file {f}, key_min 0, key_max 127, root_key 60, articulation Stac, rr_index {rr} }}\n"
        ));
    }
    let styx = format!(
        r#"
name DocFixture
articulations (
    {{ id Sus,  label Sustain,  kind @Sustain, rr 1 }}
    {{ id Leg,  label Legato,   kind @Legato,  rr 4, directional true }}
    {{ id Stac, label Staccato, kind @Short,   rr 4 }}
)
legato_engine {{
    expressive {{
        zones (
            {{vel_range (0 64),   label slow, delay_ms 333}}
            {{vel_range (65 127), label fast, delay_ms 100}}
        )
    }}
    low_latency {{
        zones ( {{vel_range (0 127), label fast, delay_ms 100}} )
    }}
}}
short_note_timing {{ pre_delay_ms 60 }}
keyswitch {{
    cc58_map {{
        0-5   "Sustain: Low Latency Legato"
        21-25 Staccato
    }}
}}
zones (
{zones})
"#
    );
    let spec_path = dir.join("lib.styx");
    std::fs::write(&spec_path, styx).expect("write styx");
    spec_path
}

struct Guard(PathBuf);
impl Drop for Guard {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixture_bank(tag: &str) -> (SamplerBank, Guard) {
    let dir =
        std::env::temp_dir().join(format!("signal-clap-host-sim-{tag}-{}", std::process::id()));
    let spec_path = build_fixture(&dir);
    let mut bank = SamplerBank::new(SR);
    bank.load_instrument(ID, &spec_path, Some(&dir), "", "")
        .expect("load fixture instrument");
    bank.preload_instrument(ID).expect("preload");
    (bank, Guard(dir))
}

fn note(start_qn: f64, end_qn: f64, pitch: u8, vel: u8) -> DocNote {
    DocNote {
        start_qn,
        end_qn,
        chan: 0,
        pitch,
        vel,
    }
}

fn bits(audio: &[f32]) -> Vec<u32> {
    audio.iter().map(|s| s.to_bits()).collect()
}

/// Drive the realtime scheduler like a host: contiguous blocks of
/// `block_frames` starting at `start_frame`, transport playing, until
/// `total_frames` frames have been produced. Returns interleaved audio.
fn drive_playback(
    bank: &mut SamplerBank,
    scheduler: &mut RealtimeScheduler,
    sched: Arc<Schedule>,
    start_frame: i64,
    total_frames: usize,
    block_frames: usize,
    tempo_bpm: f64,
) -> Vec<f32> {
    scheduler.set_schedule(Some(sched));
    let mut audio = Vec::with_capacity(total_frames * 2);
    let mut buf = vec![0.0f32; block_frames * 2];
    let mut pos = start_frame;
    let mut produced = 0usize;
    while produced < total_frames {
        let n = block_frames.min(total_frames - produced);
        let t = BlockTransport {
            playing: true,
            pos_frame: pos,
            tempo_bpm: Some(tempo_bpm),
        };
        let doc = scheduler.process_block(bank, &t, &mut buf[..n * 2]);
        assert!(doc, "document mode must own every playback block");
        audio.extend_from_slice(&buf[..n * 2]);
        pos += n as i64;
        produced += n;
    }
    audio
}

fn test_doc() -> TrackDocument {
    TrackDocument {
        seed: 0xC1A9_5EED,
        notes: vec![
            note(0.0, 2.1, 60, 90),  // phrase start
            note(2.0, 4.1, 62, 30),  // legato, slow zone (333 ms prefire)
            note(4.0, 6.0, 65, 110), // legato, fast zone (100 ms prefire)
        ],
        ccs: vec![signal_sampler::DocCc {
            qn: 0.0,
            chan: 0,
            cc: 1,
            val: 96,
        }],
        ..Default::default()
    }
}

// ── 1. Realtime walk == offline walker, bit for bit ──────────────────────────

#[test]
fn realtime_walk_matches_offline_render_bit_exactly() {
    let doc = test_doc();
    let opts = DocumentRenderOptions {
        tail_sec: 1.0,
        ..Default::default()
    };

    // Offline reference (the canonical determinism baseline).
    let (mut bank_off, _g1) = fixture_bank("rt-off");
    let spec = bank_off.instrument_spec(ID).expect("spec");
    let sched = Arc::new(annotate(&doc, &spec, SR));
    let offline = render_schedule(&mut bank_off, ID, &sched, &opts);
    assert_eq!(offline.reactive_fallbacks, 0);
    assert!(offline.audio.iter().any(|s| *s != 0.0));
    let total_frames = offline.audio.len() / 2;

    // Realtime walk at the host's block size (512, same as the offline
    // chunk cap — chunking is deterministic either way).
    let (mut bank_rt, _g2) = fixture_bank("rt-rt");
    let mut scheduler = RealtimeScheduler::new(ID);
    let rt = drive_playback(
        &mut bank_rt,
        &mut scheduler,
        sched.clone(),
        0,
        total_frames,
        512,
        120.0,
    );
    assert_eq!(
        bits(&rt),
        bits(&offline.audio),
        "realtime transport walk must be byte-identical to the offline walker"
    );
    assert_eq!(scheduler.late_prefires(), 0);
    assert_eq!(
        bank_rt.reactive_legato_fires(ID),
        0,
        "a from-the-top playback never touches the reactive path"
    );

    // And it is block-size invariant: an odd host block produces the same
    // audio (events land at exact in-block offsets, not block heads).
    let (mut bank_rt2, _g3) = fixture_bank("rt-rt2");
    let mut scheduler2 = RealtimeScheduler::new(ID);
    let rt2 = drive_playback(
        &mut bank_rt2,
        &mut scheduler2,
        sched,
        0,
        total_frames,
        479,
        120.0,
    );
    assert_eq!(
        bits(&rt2),
        bits(&offline.audio),
        "realtime walk must not depend on the host block size"
    );
}

// ── 2. Seek mid-piece == offline start_frame render ──────────────────────────

#[test]
fn seek_mid_piece_matches_offline_mid_render() {
    // Two phrases separated by silence longer than any tail (same shape as
    // the offline mid-piece invariant test).
    let doc = TrackDocument {
        seed: 0xBEEF,
        notes: vec![
            note(0.0, 2.1, 60, 90),
            note(2.0, 3.0, 62, 30),
            note(11.0, 13.1, 64, 90),
            note(13.0, 14.0, 66, 30),
        ],
        ..Default::default()
    };
    let phrase2_frame: i64 = 264_000; // QN 11 at 120 BPM, 48 kHz

    let (mut bank_off, _g1) = fixture_bank("seek-off");
    let spec = bank_off.instrument_spec(ID).expect("spec");
    let sched = Arc::new(annotate(&doc, &spec, SR));
    let offline = render_schedule(
        &mut bank_off,
        ID,
        &sched,
        &DocumentRenderOptions {
            tail_sec: 1.0,
            start_frame: phrase2_frame as u64,
            ..Default::default()
        },
    );
    let total_frames = offline.audio.len() / 2;

    // Realtime: the transport simply STARTS at phrase 2 (a discontinuity —
    // the scheduler panics, binary-searches the cursor, resumes).
    let (mut bank_rt, _g2) = fixture_bank("seek-rt");
    let mut scheduler = RealtimeScheduler::new(ID);
    let rt = drive_playback(
        &mut bank_rt,
        &mut scheduler,
        sched,
        phrase2_frame,
        total_frames,
        512,
        120.0,
    );
    assert_eq!(
        bits(&rt),
        bits(&offline.audio),
        "a transport seek must reproduce the offline start_frame render"
    );
    assert_eq!(scheduler.late_prefires(), 0, "quiescent seek point");
}

// ── 3. Late start inside a prefire lead degrades gracefully ──────────────────

#[test]
fn late_start_inside_prefire_lead_fires_reactive_fallback_on_the_tick() {
    let doc = test_doc();
    let (mut bank, _g) = fixture_bank("late");
    let spec = bank.instrument_spec(ID).expect("spec");
    let sched = Arc::new(annotate(&doc, &spec, SR));

    // Note 2's tick is QN 2 = frame 48000; its prefire sits 333 ms earlier
    // (frame 32016). Start playback INSIDE that lead window.
    let tick: i64 = 48_000;
    let start = tick - (SLOW_DELAY_FRAMES as i64 / 2);
    let total = (SR as usize) * 2; // play 2 s

    let mut scheduler = RealtimeScheduler::new(ID);
    let audio = drive_playback(&mut bank, &mut scheduler, sched, start, total, 512, 120.0);

    assert_eq!(
        scheduler.late_prefires(),
        1,
        "the straddled prefire degrades to exactly one late note-on"
    );
    // The note still sounds, from its destination tick (not from the seek
    // point): the pre-tick region is silent (the first note's trigger is in
    // the past — v1 seek semantics), sound starts at the tick.
    let pre_tick = &audio[..((tick - start) as usize) * 2];
    let post_tick = &audio[((tick - start) as usize) * 2..];
    assert!(
        pre_tick.iter().all(|s| *s == 0.0),
        "before the missed note's tick: silence (mid-note sustains restart \
         at the next boundary)"
    );
    assert!(
        post_tick.iter().any(|s| *s != 0.0),
        "the legato note the transport started into must still sound at its tick"
    );
}

// ── 4. Stop / loop-back discontinuities recover cleanly ──────────────────────

#[test]
fn stop_relocates_cleanly_and_restores_strict_live() {
    let doc = test_doc();
    let (mut bank, _g) = fixture_bank("stop");
    let spec = bank.instrument_spec(ID).expect("spec");
    let sched = Arc::new(annotate(&doc, &spec, SR));

    let mut scheduler = RealtimeScheduler::new(ID);
    scheduler.set_schedule(Some(sched));
    let block = 512usize;
    let mut buf = vec![0.0f32; block * 2];

    // Play 1 s from the top.
    let mut pos: i64 = 0;
    while pos < SR as i64 {
        let t = BlockTransport {
            playing: true,
            pos_frame: pos,
            tempo_bpm: Some(120.0),
        };
        assert!(scheduler.process_block(&mut bank, &t, &mut buf));
        pos += block as i64;
    }
    assert_eq!(bank.play_mode(ID), Some(PlayMode::Lookahead));

    // Stop: block boundary hands the engine back to StrictLive.
    let t = BlockTransport {
        playing: false,
        pos_frame: pos,
        tempo_bpm: Some(120.0),
    };
    assert!(
        !scheduler.process_block(&mut bank, &t, &mut buf),
        "stopped transport = StrictLive block (caller renders live)"
    );
    assert!(!scheduler.document_active());
    assert_eq!(
        bank.play_mode(ID),
        Some(PlayMode::StrictLive),
        "leaving document mode must restore the strict zero-latency policy"
    );

    // Loop back: resume at 0.25 s (backwards jump = discontinuity). Note
    // 1's trigger (frame 0) is in the past — v1 semantics restart the line
    // at the NEXT boundary, note 2's prefire at frame 32016 — so playback
    // must go audible again once the walk crosses it.
    let mut pos: i64 = SR as i64 / 4;
    let mut heard = false;
    while pos < 60_000 {
        let t = BlockTransport {
            playing: true,
            pos_frame: pos,
            tempo_bpm: Some(120.0),
        };
        assert!(scheduler.process_block(&mut bank, &t, &mut buf));
        heard |= buf.iter().any(|s| *s != 0.0);
        pos += block as i64;
    }
    assert!(scheduler.document_active());
    assert_eq!(bank.play_mode(ID), Some(PlayMode::Lookahead));
    assert!(
        heard,
        "playback resumed after the loop-back (next boundary re-sounds)"
    );
}

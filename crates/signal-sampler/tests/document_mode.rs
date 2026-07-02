//! Document-mode phase-1 integration tests (see `docs/plan/document-mode.md`).
//!
//! Covers the three permanent guarantees:
//! 1. **Determinism** — same document + seed ⇒ byte-identical audio, and a
//!    mid-piece start reproduces the full render's tail exactly (RR slots are
//!    pure hashes of note identity, never counters).
//! 2. **Timing inversion** — a legato-followed note's transition fires
//!    `delay_ms` BEFORE its tick, so the arrival lands ON the tick.
//! 3. **Reactive vs document A/B** — the reactive path speaks `delay_ms`
//!    after the tick (the famous CSS latency); the document path arrives on
//!    the tick — earlier by exactly the configured delay.

use std::path::{Path, PathBuf};

use signal_sampler::SamplerRig;
use signal_sampler::document::{DocEvent, DocNote, DocumentRenderOptions, TrackDocument, annotate};

const SR: u32 = 48_000;
/// Fixture legato pre-delay for velocities 0–64 (see the styx below).
const SLOW_DELAY_MS: u64 = 333;
const SLOW_DELAY_FRAMES: u64 = SLOW_DELAY_MS * SR as u64 / 1000;

// ── Fixture library ───────────────────────────────────────────────────────────

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

/// Build a minimal CSS-shaped legato library in `dir`: a sustain body, 4-RR
/// directional legato transitions, and 4-RR shorts, with the CSS expressive
/// velocity→delay curve (333/100 ms).
fn build_fixture(dir: &Path) -> PathBuf {
    std::fs::create_dir_all(dir).expect("mkdir");
    write_sine_wav(&dir.join("sus.wav"), SR as usize, 220.0, 0.5);
    let mut zones = String::from(
        "    { file sus.wav, key_min 0, key_max 127, root_key 60, articulation Sus }\n",
    );
    for rr in 0..4u32 {
        for dirn in ["up", "down"] {
            let f = format!("leg_{dirn}_{rr}.wav");
            // Distinct frequency per RR slot so slot choice is audible.
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

fn fixture_rig(tag: &str) -> (SamplerRig, tempdir::Guard) {
    let dir =
        std::env::temp_dir().join(format!("signal-document-mode-{tag}-{}", std::process::id()));
    let spec_path = build_fixture(&dir);
    let rig = SamplerRig::new_offline(SR);
    rig.load_instrument("fixture", &spec_path, Some(&dir), "", "")
        .expect("load fixture instrument");
    rig.preload_instrument("fixture").expect("preload");
    (rig, tempdir::Guard(dir))
}

/// Tiny RAII cleanup for the fixture dir.
mod tempdir {
    pub struct Guard(pub std::path::PathBuf);
    impl Drop for Guard {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
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

fn audio_bits(audio: &[f32]) -> Vec<u32> {
    audio.iter().map(|s| s.to_bits()).collect()
}

// ── 1. Determinism ───────────────────────────────────────────────────────────

#[test]
fn document_render_is_byte_identical_across_runs() {
    let doc = TrackDocument {
        seed: 0xD0C_5EED,
        notes: vec![
            note(0.0, 2.1, 60, 90),  // phrase start
            note(2.0, 4.1, 62, 30),  // legato (slow, 333 ms)
            note(4.0, 6.0, 65, 110), // legato (fast, 100 ms)
        ],
        ccs: vec![signal_sampler::DocCc {
            qn: 0.0,
            chan: 0,
            cc: 1,
            val: 96,
        }],
        ..Default::default()
    };
    let opts = DocumentRenderOptions {
        tail_sec: 1.0,
        ..Default::default()
    };

    let (rig_a, _ga) = fixture_rig("det-a");
    let a = rig_a
        .render_offline_document("fixture", &doc, &opts)
        .expect("render a");
    let (rig_b, _gb) = fixture_rig("det-b");
    let b = rig_b
        .render_offline_document("fixture", &doc, &opts)
        .expect("render b");

    assert!(!a.audio.is_empty());
    assert!(a.audio.iter().any(|s| *s != 0.0), "render produced audio");
    assert_eq!(a.audio.len(), b.audio.len());
    assert_eq!(
        audio_bits(&a.audio),
        audio_bits(&b.audio),
        "same document + seed must render byte-identically"
    );
    assert_eq!(a.transitions, b.transitions);
    assert_eq!(
        a.transitions.len(),
        2,
        "both legato notes fired transitions"
    );
}

#[test]
fn mid_piece_start_matches_full_render_and_rr_slots() {
    // Two phrases separated by 4 s of silence (longer than any voice tail),
    // so the engine is quiescent at the phrase-2 boundary.
    let doc = TrackDocument {
        seed: 0xBEEF,
        notes: vec![
            // phrase 1
            note(0.0, 2.1, 60, 90),
            note(2.0, 3.0, 62, 30), // legato
            // phrase 2 — starts at QN 11 (120 BPM ⇒ 5.5 s ⇒ frame 264000)
            note(11.0, 13.1, 64, 90),
            note(13.0, 14.0, 66, 30), // legato
        ],
        ..Default::default()
    };
    let phrase2_frame: u64 = 264_000; // qn 11 at 120 BPM, 48 kHz

    let (rig_full, _gf) = fixture_rig("mid-full");
    let full = rig_full
        .render_offline_document(
            "fixture",
            &doc,
            &DocumentRenderOptions {
                tail_sec: 1.0,
                ..Default::default()
            },
        )
        .expect("full render");

    let (rig_mid, _gm) = fixture_rig("mid-mid");
    let mid = rig_mid
        .render_offline_document(
            "fixture",
            &doc,
            &DocumentRenderOptions {
                tail_sec: 1.0,
                start_frame: phrase2_frame,
                ..Default::default()
            },
        )
        .expect("mid render");

    // Position independence: the notes played from bar N use the same RR
    // slots as the full render — the schedule pins slots from note identity,
    // so the annotated slots are identical by construction; assert the
    // *audio* proves the engine obeyed them.
    let offset = (phrase2_frame * 2) as usize;
    let full_tail = &full.audio[offset..];
    assert_eq!(full_tail.len(), mid.audio.len());
    assert!(mid.audio.iter().any(|s| *s != 0.0), "mid render audible");
    assert_eq!(
        audio_bits(full_tail),
        audio_bits(&mid.audio),
        "starting mid-piece must reproduce the full render's tail exactly"
    );

    // The phrase-2 transition fired at the same absolute frame in both runs.
    let full_p2: Vec<_> = full
        .transitions
        .iter()
        .filter(|t| t.frame >= phrase2_frame)
        .collect();
    assert_eq!(full_p2.len(), 1);
    assert_eq!(mid.transitions.len(), 1);
    assert_eq!(full_p2[0], &mid.transitions[0]);

    // And the annotated schedule's RR pins are identical whether or not the
    // playback starts mid-piece (pure hash — no counters to advance).
    let spec = rig_full.instrument_spec("fixture").expect("spec");
    let s1 = annotate(&doc, &spec, SR);
    let s2 = annotate(&doc, &spec, SR);
    let slots = |s: &signal_sampler::Schedule| -> Vec<u32> {
        s.events
            .iter()
            .filter_map(|e| match e.kind {
                DocEvent::NoteOn { rr, .. }
                | DocEvent::NoteOff { rr, .. }
                | DocEvent::LegatoPrefire { rr, .. } => Some(rr),
                DocEvent::Cc { .. } => None,
            })
            .collect()
    };
    assert_eq!(slots(&s1), slots(&s2));
}

// ── 2. Timing inversion ──────────────────────────────────────────────────────

#[test]
fn transition_fires_before_tick_and_arrives_on_tick() {
    // B (vel 30 ⇒ slow zone, 333 ms) starts at QN 2 = 1.0 s = frame 48000.
    let tick: u64 = 48_000;
    let doc = TrackDocument {
        seed: 1,
        notes: vec![note(0.0, 2.1, 60, 90), note(2.0, 4.0, 62, 30)],
        ..Default::default()
    };

    let (rig, _g) = fixture_rig("inversion");
    // The annotated schedule places the prefire exactly delay_ms early.
    let spec = rig.instrument_spec("fixture").expect("spec");
    let sched = annotate(&doc, &spec, SR);
    let prefire = sched
        .events
        .iter()
        .find(|e| matches!(e.kind, DocEvent::LegatoPrefire { .. }))
        .expect("prefire scheduled");
    assert_eq!(prefire.frame, tick - SLOW_DELAY_FRAMES);

    // And the engine actually fired the transition at that frame.
    let res = rig
        .render_offline_document(
            "fixture",
            &doc,
            &DocumentRenderOptions {
                tail_sec: 0.5,
                ..Default::default()
            },
        )
        .expect("render");
    assert_eq!(res.transitions.len(), 1);
    let fire = &res.transitions[0];
    assert_eq!(fire.to_note, 62);
    assert_eq!(
        fire.frame,
        tick - SLOW_DELAY_FRAMES,
        "transition sample starts delay_ms BEFORE the destination tick"
    );
    assert_eq!(
        fire.frame + SLOW_DELAY_FRAMES,
        tick,
        "destination arrival lands exactly on the tick"
    );
}

// ── 3. Reactive vs document A/B ──────────────────────────────────────────────

#[test]
fn reactive_vs_document_arrival_differs_by_configured_delay() {
    let tick: u64 = 48_000;
    let block: usize = 64;

    // Document path: prefire ⇒ arrival on the tick (asserted above); the
    // transition voice spawns at tick − delay.
    let doc = TrackDocument {
        seed: 1,
        notes: vec![note(0.0, 2.1, 60, 90), note(2.0, 4.0, 62, 30)],
        ..Default::default()
    };
    let (doc_rig, _gd) = fixture_rig("ab-doc");
    let doc_res = doc_rig
        .render_offline_document(
            "fixture",
            &doc,
            &DocumentRenderOptions {
                tail_sec: 0.5,
                ..Default::default()
            },
        )
        .expect("document render");
    assert_eq!(doc_res.transitions.len(), 1);
    let doc_fire = doc_res.transitions[0].frame;
    let doc_arrival = doc_fire + SLOW_DELAY_FRAMES;
    assert_eq!(doc_arrival, tick);

    // Reactive path: the same two notes played live at their ticks. The
    // engine arms the countdown at note-on and the transition speaks
    // delay_ms AFTER the tick (the famous latency every DAW compensates
    // with negative track delay).
    let (live_rig, _gl) = fixture_rig("ab-live");
    live_rig.set_legato_mode("fixture", true, true); // same expressive curve
    live_rig.set_legato_fire_log_enabled("fixture", true);
    let mut cur: u64 = 0;
    let render_until = |target: u64, cur: &mut u64| {
        while *cur < target {
            let frames = ((target - *cur) as usize).min(block);
            let mut buf = vec![0.0f32; frames * 2];
            live_rig.render_offline(&mut buf).expect("render");
            *cur += frames as u64;
        }
    };
    live_rig.note_on("fixture", 60, 90);
    render_until(tick, &mut cur);
    live_rig.note_on("fixture", 62, 30); // vel 30 ⇒ 333 ms countdown
    render_until(tick + 2 * SLOW_DELAY_FRAMES, &mut cur);
    let live_log = live_rig.legato_fire_log("fixture");
    assert_eq!(live_log.len(), 1);
    let live_fire = live_log[0].frame;

    // Reactive speaks late: fire ≈ tick + delay (block-quantised — the
    // countdown fires at the head of the block in which it elapses).
    assert!(
        live_fire >= tick + SLOW_DELAY_FRAMES - block as u64
            && live_fire <= tick + SLOW_DELAY_FRAMES,
        "reactive transition speaks delay_ms after the tick (got {live_fire}, tick {tick})"
    );

    // THE INVERSION, measured: the document arrival is earlier than the
    // reactive speak point by exactly the configured delay (± one live block).
    let delta = live_fire - doc_arrival;
    assert!(
        (SLOW_DELAY_FRAMES - block as u64..=SLOW_DELAY_FRAMES).contains(&delta),
        "document arrival must lead the reactive one by the configured delay \
         (delay {SLOW_DELAY_FRAMES}, measured {delta})"
    );
}

//! Document mode — lookahead playback for the sampler (phase 1: offline).
//!
//! A [`TrackDocument`] is the full note/CC content of one track in musical
//! time (quarter notes) plus its tempo map. [`annotate`] turns it into a
//! [`Schedule`] of engine events in **absolute frames from the document
//! epoch**, inverting the classic legato-latency problem: every
//! legato-followed note is triggered `delay_ms` (the spec's expressive
//! velocity→delay curve) *before* its tick via
//! [`LegatoPrefire`](DocEvent::LegatoPrefire), so the transition's audible
//! arrival lands exactly on the grid — no negative track delay, no mirrored
//! timing copy. Short notes are pre-rolled by the spec's
//! `short_note_timing.pre_delay_ms` the same way.
//!
//! See `docs/plan/document-mode.md` for the full design, including the hard
//! **determinism** requirement this module implements: same document + same
//! parameters + same seed → byte-identical audio. Every stochastic choice
//! (round-robin) is a pure hash of `(seed, note identity, purpose)` — never a
//! mutable counter — so playback is position-independent (starting mid-piece
//! picks the same RR slot for every note) and edit-stable (inserting a note
//! re-rolls only that note).
//!
//! The articulation / legato-edge / re-bow inference rules are ported from —
//! and must stay in parity with — `keyflow-orchestra/src/mirror.rs`
//! (`mirror_part`, tested against the CSS reference engine in keyflow's
//! `tests/mirror_parity.rs`). That crate stays independent for phase 1;
//! TODO: unify the inference into one shared crate once document mode
//! stabilises.

use crate::spec::{ArticulationKind, LibrarySpec};

const EPS: f64 = 1e-6;

/// Same-pitch abutment tolerance (QN) under which two connectable notes read
/// as a re-bow. Mirrors keyflow's `Config::break_gap_qn` default (1/64) used
/// as `gap.abs() <= break_gap_qn * 2` — i.e. gaps up to ~1/32 QN connect.
const BREAK_GAP_QN: f64 = 1.0 / 64.0;

/// Engine fallback when the spec has no matching velocity zone (matches
/// `SampleEngine::legato_timing`).
const LEGATO_DELAY_FALLBACK_MS: u32 = 100;

// ── Document types ────────────────────────────────────────────────────────────

/// One tempo point: piecewise-constant BPM from `qn` onward.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TempoPoint {
    pub qn: f64,
    pub bpm: f64,
}

/// One note in the document (QN domain, 0-based channel).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DocNote {
    pub start_qn: f64,
    pub end_qn: f64,
    pub chan: u8,
    pub pitch: u8,
    pub vel: u8,
}

/// One CC event in the document (QN domain, 0-based channel).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DocCc {
    pub qn: f64,
    pub chan: u8,
    pub cc: u8,
    pub val: u8,
}

/// The full MIDI document of one track, given to the sampler ahead of time
/// (ARA-style) so it can *anticipate* instead of being compensated.
#[derive(Debug, Clone, Default)]
pub struct TrackDocument {
    /// Monotonic version; replaced wholesale on change (documents are small).
    pub version: u64,
    /// Determinism seed — all stochastic choices (RR) hash from this.
    /// Persisted with the project; re-roll to get a new "take".
    pub seed: u64,
    pub notes: Vec<DocNote>,
    pub ccs: Vec<DocCc>,
    /// Tempo map (piecewise-constant BPM). Empty ⇒ 120 BPM.
    pub tempo: Vec<TempoPoint>,
}

impl Default for TempoPoint {
    fn default() -> Self {
        Self {
            qn: 0.0,
            bpm: 120.0,
        }
    }
}

// ── Time conversion ───────────────────────────────────────────────────────────

/// Seconds from the document epoch (QN 0) to `qn`, integrating the
/// piecewise-constant tempo map. Before the first point, the first point's
/// BPM applies; an empty map means 120 BPM.
pub fn qn_to_sec(tempo: &[TempoPoint], qn: f64) -> f64 {
    let mut bpm = tempo.first().map(|t| t.bpm).unwrap_or(120.0);
    let mut sec = 0.0;
    let mut cur_qn = 0.0;
    for t in tempo {
        if t.qn >= qn {
            break;
        }
        if t.qn > cur_qn {
            sec += (t.qn - cur_qn) * 60.0 / bpm;
            cur_qn = t.qn;
        }
        bpm = t.bpm;
    }
    sec + (qn - cur_qn) * 60.0 / bpm
}

/// Absolute frame (from the document epoch) for a QN position.
pub fn qn_to_frame(tempo: &[TempoPoint], qn: f64, sample_rate: u32) -> i64 {
    (qn_to_sec(tempo, qn) * sample_rate as f64).round() as i64
}

fn ms_to_frames_i64(ms: u32, sample_rate: u32) -> i64 {
    (ms as i64 * sample_rate as i64) / 1000
}

// ── Deterministic round-robin ─────────────────────────────────────────────────

/// What an RR slot is being drawn for. Part of the hash so a note's body,
/// transition, and release each get an independent (but stable) slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum RrPurpose {
    /// The note's body trigger (sustain layers / short one-shot).
    Body = 1,
    /// The legato transition into the note.
    Transition = 2,
    /// The recorded release tail at note-off.
    Release = 3,
}

/// splitmix64 finalizer — a FIXED, documented mix function. Do NOT replace
/// with `std::hash::DefaultHasher` (unstable across Rust releases): rendered
/// projects must reproduce byte-identically forever.
fn splitmix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = x;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Number of abstract RR positions the stable hash draws from. The engine
/// reduces the pinned slot modulo the actual RR-group size at trigger time
/// (`select_zone_rr_slot` / `find_layer_zone`), so any group size divides in.
pub const RR_SLOT_SPACE: u32 = 4096;

/// The document-mode round-robin choice: a pure function of the seed and the
/// note's identity — **no mutable counter**. Consequences (per the design
/// doc's "Determinism"): position independence (starting playback at bar 17
/// gives every note the same RR as playing from the top) and edit stability
/// (inserting a note re-rolls only that note).
pub fn stable_rr_slot(seed: u64, start_qn: f64, pitch: u8, chan: u8, purpose: RrPurpose) -> u32 {
    let mut h = seed;
    for v in [
        start_qn.to_bits(),
        pitch as u64,
        chan as u64,
        purpose as u64,
    ] {
        h = splitmix64(h ^ v);
    }
    (h % RR_SLOT_SPACE as u64) as u32
}

// ── Schedule ──────────────────────────────────────────────────────────────────

/// One engine action in the schedule.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DocEvent {
    /// Plain note-on (first-of-phrase sustains, shorts — shorts arrive
    /// pre-rolled by `short_note_timing.pre_delay_ms`).
    NoteOn { note: u8, vel: u8, rr: u32 },
    /// Note-off (fires the release tail; `rr` pins its round-robin).
    NoteOff { note: u8, rr: u32 },
    /// CC pass-through.
    Cc { cc: u8, val: u8 },
    /// Fire the legato transition into `note` NOW — scheduled `delay_ms`
    /// before the note's tick so the arrival lands on the grid. Emitted
    /// INSTEAD of a `NoteOn` for legato-followed notes.
    LegatoPrefire { note: u8, vel: u8, rr: u32 },
}

/// One scheduled event at an absolute frame from the document epoch.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScheduledEvent {
    pub frame: u64,
    pub chan: u8,
    pub kind: DocEvent,
}

/// Annotated, frame-domain schedule for one document + library spec.
#[derive(Debug, Clone, Default)]
pub struct Schedule {
    pub sample_rate: u32,
    /// Sorted by `(frame, dispatch priority)`.
    pub events: Vec<ScheduledEvent>,
    /// Frame of the last event.
    pub end_frame: u64,
    /// The document's seed (carried through for render reports).
    pub seed: u64,
    pub note_count: usize,
    /// Notes that will arrive via `LegatoPrefire`.
    pub legato_count: usize,
    /// Notes pre-rolled as shorts.
    pub short_count: usize,
}

// ── Annotation (ported from keyflow-orchestra mirror.rs — keep in parity) ─────

/// CC58 band classification, ported verbatim from
/// `keyflow-orchestra/src/mirror.rs` (the parity source — tested against the
/// CSS reference engine there). Only these bands affect timing decisions.
fn ks_is_marcato(val: u8) -> bool {
    (66..=75).contains(&val) // marcato + marcato-with-overlay bands
}

fn ks_is_legato_toggle(val: u8) -> bool {
    (76..=85).contains(&val) // legato on / legato off
}

fn ks_is_con_sord(val: u8) -> bool {
    (86..=95).contains(&val) // con sordino on / off
}

/// Short-articulation bands (spiccato/staccatissimo/staccato/sfz/pizzicato)
/// plus tremolo — none of these connect, so a same-pitch abutment between
/// them is a break, not a re-bow. Marcato deliberately does NOT block
/// (fast-run tails flowing into held notes re-bow). Parity: `mirror.rs`.
fn ks_blocks_rebow(val: u8) -> bool {
    (11..=35).contains(&val) || (56..=60).contains(&val)
}

/// Per-channel step timeline of a CC's value (port of mirror.rs `CcState`).
struct CcState {
    /// (qn, val) sorted by qn.
    events: Vec<(f64, u8)>,
}

impl CcState {
    fn new(ccs: &[DocCc], chan: u8, cc: u8) -> Self {
        let mut events: Vec<(f64, u8)> = ccs
            .iter()
            .filter(|e| e.chan == chan && e.cc == cc)
            .map(|e| (e.qn, e.val))
            .collect();
        events.sort_by(|a, b| a.0.total_cmp(&b.0));
        Self { events }
    }

    /// Last value at or before `qn` (None if no event yet).
    fn at(&self, qn: f64) -> Option<u8> {
        let mut cur = None;
        for &(q, v) in &self.events {
            if q <= qn + EPS {
                cur = Some(v);
            } else {
                break;
            }
        }
        cur
    }
}

/// What the spec says a CC58 state plays. `None` label / no articulation
/// match (mode switches, bare state) ⇒ the engine's default long
/// articulation, i.e. sustain-like.
fn kind_for_ks(spec: &LibrarySpec, ks_val: Option<u8>) -> ArticulationKind {
    let Some(val) = ks_val else {
        return ArticulationKind::Sustain;
    };
    let Some(label) = spec.keyswitch.as_ref().and_then(|ks| ks.cc58_function(val)) else {
        return ArticulationKind::Sustain;
    };
    spec.articulations
        .iter()
        .find(|a| a.id == label || a.label == label)
        .map(|a| a.kind.clone())
        .unwrap_or(ArticulationKind::Sustain)
}

/// Working copy of a note through annotation.
struct ANote {
    src: DocNote,
    /// CC58 state at note-on (articulation identity) — filtered of
    /// legato-toggle / sordino presses, which are state, not articulation
    /// (parity: mirror.rs `MNote::ks_val`).
    ks_val: Option<u8>,
    kind: ArticulationKind,
    /// This note is reached by a legato transition (different-pitch overlap
    /// or same-pitch re-bow) → arrives via `LegatoPrefire`.
    legato_from: bool,
    /// This note flows into a same-pitch re-bow → its note-off is dropped
    /// (the transition into the next note replaces the release).
    re_bow_to: bool,
}

impl ANote {
    /// Parity: mirror.rs `MNote::blocks_rebow` (no notation hints here —
    /// the document is MIDI-domain only).
    fn blocks_rebow(&self) -> bool {
        self.ks_val.map(ks_blocks_rebow).unwrap_or(false)
    }

    fn is_marcato(&self) -> bool {
        self.ks_val.map(ks_is_marcato).unwrap_or(false)
    }

    fn is_sustain_like(&self) -> bool {
        matches!(
            self.kind,
            ArticulationKind::Sustain | ArticulationKind::Looped | ArticulationKind::Trill
        )
    }

    fn is_short(&self) -> bool {
        matches!(
            self.kind,
            ArticulationKind::Short | ArticulationKind::OneShot
        )
    }
}

/// Annotate a document against a library spec into a frame-domain
/// [`Schedule`].
///
/// Inference (ported from `keyflow-orchestra/src/mirror.rs::mirror_part`,
/// stage 1 — keep in parity):
/// - **articulation** = CC58 keyswitch state at each note-on
/// - **legato edge** = different-pitch overlap with the previous note on the
///   same channel
/// - **re-bow** = same-pitch abutment (gap ≤ ~1/32 QN) between connectable
///   (sustain-family) notes
///
/// Timing inversion (this module's contribution):
/// - legato-followed sustains → [`DocEvent::LegatoPrefire`] at
///   `start − expressive.delay_for_velocity(vel)`
/// - shorts → note-on pre-rolled by `short_note_timing.pre_delay_ms`
/// - marcato keyswitch state → no sampled pre-delay → no pull (mirror parity)
pub fn annotate(doc: &TrackDocument, spec: &LibrarySpec, sample_rate: u32) -> Schedule {
    let expressive = spec
        .legato_engine
        .as_ref()
        .and_then(|le| le.expressive.clone().or_else(|| le.primary_mode()));
    let porta_vel_max = spec
        .legato_engine
        .as_ref()
        .and_then(|le| le.portamento.as_ref())
        .map(|p| p.trigger_vel_max)
        .unwrap_or(0);
    let short_pre_frames = spec
        .short_note_timing
        .as_ref()
        .map(|s| ms_to_frames_i64(s.pre_delay_ms, sample_rate))
        .unwrap_or(0);
    let legato_capable = spec.legato_engine.is_some();

    // Working notes grouped per channel, sorted by source start.
    let mut notes: Vec<ANote> = doc
        .notes
        .iter()
        .map(|&src| ANote {
            src,
            ks_val: None,
            kind: ArticulationKind::Sustain,
            legato_from: false,
            re_bow_to: false,
        })
        .collect();
    let mut by_ch: std::collections::BTreeMap<u8, Vec<usize>> = std::collections::BTreeMap::new();
    for (i, n) in notes.iter().enumerate() {
        by_ch.entry(n.src.chan).or_default().push(i);
    }
    for list in by_ch.values_mut() {
        list.sort_by(|&a, &b| notes[a].src.start_qn.total_cmp(&notes[b].src.start_qn));
    }

    // Stage 1 — articulation state + legato/re-bow edges (mirror.rs parity).
    for (&ch, list) in &by_ch {
        let ks = CcState::new(&doc.ccs, ch, 58);
        for &ni in list {
            notes[ni].ks_val = ks.at(notes[ni].src.start_qn).filter(|v| {
                // legato-toggle / sordino presses are state, not articulation
                !ks_is_legato_toggle(*v) && !ks_is_con_sord(*v)
            });
            notes[ni].kind = kind_for_ks(spec, notes[ni].ks_val);
        }
        for w in 0..list.len().saturating_sub(1) {
            let (ai, bi) = (list[w], list[w + 1]);
            let a = &notes[ai];
            let b = &notes[bi];
            let gap = b.src.start_qn - a.src.end_qn;
            if a.src.pitch != b.src.pitch {
                // different-pitch overlap = legato transition
                if gap < -EPS {
                    notes[bi].legato_from = true;
                }
            } else if legato_capable
                && !a.blocks_rebow()
                && !b.blocks_rebow()
                && gap.abs() <= BREAK_GAP_QN * 2.0 + EPS
            {
                // same-pitch abutment between sustains = re-bow
                notes[ai].re_bow_to = true;
                notes[bi].legato_from = true;
            }
        }
    }

    // Stage 2 — schedule emission with the timing inversion.
    let mut events: Vec<(u64, u8, u8, DocEvent)> = Vec::new(); // (frame, prio, chan, ev)
    let mut legato_count = 0usize;
    let mut short_count = 0usize;

    for e in &doc.ccs {
        let frame = qn_to_frame(&doc.tempo, e.qn, sample_rate).max(0) as u64;
        events.push((
            frame,
            0,
            e.chan,
            DocEvent::Cc {
                cc: e.cc,
                val: e.val,
            },
        ));
    }

    for list in by_ch.values() {
        // Previous trigger frame on this channel — keeps the mono line's
        // trigger order strict even when a pre-roll would cross it.
        let mut prev_trigger: i64 = -1;
        for &ni in list {
            let n = &notes[ni];
            let start = qn_to_frame(&doc.tempo, n.src.start_qn, sample_rate);
            let end = qn_to_frame(&doc.tempo, n.src.end_qn, sample_rate);
            let body_rr = stable_rr_slot(
                doc.seed,
                n.src.start_qn,
                n.src.pitch,
                n.src.chan,
                RrPurpose::Body,
            );

            let (trigger_frame, kind) = if n.is_short() {
                // Shorts: recorded pre-roll before the rhythmic peak.
                short_count += 1;
                (
                    start - short_pre_frames,
                    DocEvent::NoteOn {
                        note: n.src.pitch,
                        vel: n.src.vel,
                        rr: body_rr,
                    },
                )
            } else if n.legato_from && n.is_sustain_like() && !n.is_marcato() && legato_capable {
                // THE INVERSION: fire the transition `delay_ms` early so the
                // arrival lands on the tick. Document mode always uses the
                // full expressive curve; portamento (vel ≤ threshold) has no
                // sampled pre-delay (the glide itself is the transition).
                legato_count += 1;
                let vel = n.src.vel;
                let lead_ms = if porta_vel_max > 0 && vel <= porta_vel_max {
                    0
                } else {
                    expressive
                        .as_ref()
                        .and_then(|m| m.delay_for_velocity(vel))
                        .unwrap_or(LEGATO_DELAY_FALLBACK_MS)
                };
                let rr = stable_rr_slot(
                    doc.seed,
                    n.src.start_qn,
                    n.src.pitch,
                    n.src.chan,
                    RrPurpose::Transition,
                );
                (
                    start - ms_to_frames_i64(lead_ms, sample_rate),
                    DocEvent::LegatoPrefire {
                        note: n.src.pitch,
                        vel,
                        rr,
                    },
                )
            } else {
                (
                    start,
                    DocEvent::NoteOn {
                        note: n.src.pitch,
                        vel: n.src.vel,
                        rr: body_rr,
                    },
                )
            };

            let trigger_frame = trigger_frame.max(prev_trigger + 1).max(0);
            prev_trigger = trigger_frame;
            let prio = match kind {
                DocEvent::LegatoPrefire { .. } => 2,
                _ => 3,
            };
            events.push((trigger_frame as u64, prio, n.src.chan, kind));

            // Note-off: dropped for re-bow sources — the transition into the
            // next same-pitch note replaces the release (fading this note is
            // `fire_legato`'s job, not a release tail's).
            if !n.re_bow_to {
                let rr = stable_rr_slot(
                    doc.seed,
                    n.src.start_qn,
                    n.src.pitch,
                    n.src.chan,
                    RrPurpose::Release,
                );
                events.push((
                    end.max(trigger_frame + 1).max(0) as u64,
                    1,
                    n.src.chan,
                    DocEvent::NoteOff {
                        note: n.src.pitch,
                        rr,
                    },
                ));
            }
        }
    }

    // Cc(0) < NoteOff(1) < LegatoPrefire(2) < NoteOn(3) at equal frames, so
    // keyswitch state lands before the notes it governs and releases precede
    // re-triggers. Vec::sort_by is stable → equal keys keep emission order.
    events.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));

    let end_frame = events.last().map(|e| e.0).unwrap_or(0);
    Schedule {
        sample_rate,
        events: events
            .into_iter()
            .map(|(frame, _prio, chan, kind)| ScheduledEvent { frame, chan, kind })
            .collect(),
        end_frame,
        seed: doc.seed,
        note_count: doc.notes.len(),
        legato_count,
        short_count,
    }
}

// ── Offline schedule playback ─────────────────────────────────────────────────

/// Options for [`render_schedule`] /
/// [`SamplerRig::render_offline_document`](crate::SamplerRig::render_offline_document).
#[derive(Debug, Clone)]
pub struct DocumentRenderOptions {
    /// Render chunk size (frames). Chunking is deterministic (it depends only
    /// on the schedule), so this affects performance, not output.
    pub block_frames: usize,
    /// Extra tail rendered after the last event (release/reverb ring-out).
    pub tail_sec: f64,
    /// Start the render at this absolute frame (schedule cursor re-locate).
    /// Events before it are skipped; notes sounding across it are dropped
    /// until their next boundary (v1 seek semantics per the design doc).
    pub start_frame: u64,
}

impl Default for DocumentRenderOptions {
    fn default() -> Self {
        Self {
            block_frames: 512,
            tail_sec: 3.0,
            start_frame: 0,
        }
    }
}

/// Result of one offline document render.
#[derive(Debug, Clone, Default)]
pub struct DocumentRenderResult {
    /// Interleaved stereo audio, starting at `start_frame`.
    pub audio: Vec<f32>,
    pub sample_rate: u32,
    pub start_frame: u64,
    pub seed: u64,
    pub note_count: usize,
    /// Legato transitions actually fired by the engine, with frames relative
    /// to `start_frame`.
    pub transitions: Vec<crate::engine::LegatoFireEvent>,
}

/// Walk a [`Schedule`] through a bank instrument, rendering block-sized
/// chunks that split at every event's exact frame — events therefore land
/// sample-accurately regardless of `block_frames`. Round-robin is pinned per
/// event via the forced-RR path; the engine never consults a mutable counter.
pub fn render_schedule(
    bank: &mut crate::bank::SamplerBank,
    id: &str,
    schedule: &Schedule,
    opts: &DocumentRenderOptions,
) -> DocumentRenderResult {
    // Reset playback state; document mode always plays the full expressive
    // legato (the whole point is that latency no longer costs anything).
    bank.panic(id);
    bank.set_legato_mode(id, true, true);
    bank.set_legato_fire_log_enabled(id, true);

    let sr = schedule.sample_rate;
    let tail_frames = (opts.tail_sec * sr as f64).round() as u64;
    let end_frame = schedule.end_frame + tail_frames;
    let mut audio: Vec<f32> = Vec::new();
    let mut cursor = opts.start_frame;
    let base_engine_frame = engine_frames_rendered(bank, id);

    let render_until = |bank: &mut crate::bank::SamplerBank,
                        audio: &mut Vec<f32>,
                        cursor: &mut u64,
                        target: u64| {
        while *cursor < target {
            let frames = ((target - *cursor) as usize).min(opts.block_frames.max(1));
            let mut buf = vec![0.0f32; frames * 2];
            bank.render(&mut buf);
            audio.extend_from_slice(&buf);
            *cursor += frames as u64;
        }
    };

    for ev in &schedule.events {
        if ev.frame < opts.start_frame {
            continue; // v1 seek: skip material before the cursor
        }
        render_until(bank, &mut audio, &mut cursor, ev.frame);
        match ev.kind {
            DocEvent::Cc { cc, val } => {
                bank.cc_instrument(id, cc, val);
                // Document mode owns the legato mode: a low-latency CC58
                // press (0–5) must not demote the expressive curve the
                // schedule's prefire leads were computed from.
                if cc == 58 && val <= 5 {
                    bank.set_legato_mode(id, true, true);
                }
            }
            DocEvent::NoteOn { note, vel, rr } => {
                bank.set_forced_rr(id, Some(rr));
                bank.note_on_instrument(id, note, vel);
            }
            DocEvent::NoteOff { note, rr } => {
                bank.set_forced_rr(id, Some(rr));
                bank.note_off_instrument(id, note);
            }
            DocEvent::LegatoPrefire { note, vel, rr } => {
                bank.set_forced_rr(id, Some(rr));
                bank.legato_prefire(id, note, vel);
            }
        }
    }
    render_until(bank, &mut audio, &mut cursor, end_frame);
    bank.set_forced_rr(id, None);

    // Fire log frames are engine-relative; re-base to this render's start.
    let transitions = bank
        .legato_fire_log(id)
        .into_iter()
        .map(|mut e| {
            e.frame = (e.frame - base_engine_frame) + opts.start_frame;
            e
        })
        .collect();
    bank.set_legato_fire_log_enabled(id, false);

    DocumentRenderResult {
        audio,
        sample_rate: sr,
        start_frame: opts.start_frame,
        seed: schedule.seed,
        note_count: schedule.note_count,
        transitions,
    }
}

fn engine_frames_rendered(bank: &crate::bank::SamplerBank, id: &str) -> u64 {
    bank.engine_frames_rendered(id).unwrap_or(0)
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn spec_with_legato() -> LibrarySpec {
        LibrarySpec::from_styx(
            r#"
name DocTest
articulations (
    { id Sus,  label Sustain,  kind @Sustain, rr 1 }
    { id Leg,  label Legato,   kind @Legato,  rr 4, directional true }
    { id Stac, label Staccato, kind @Short,   rr 4 }
)
legato_engine {
    expressive {
        zones (
            {vel_range (0 64),    label slow, delay_ms 333}
            {vel_range (65 127),  label fast, delay_ms 100}
        )
    }
    portamento { trigger_vel_max 10, volume_controller CC5 }
}
short_note_timing { pre_delay_ms 60 }
keyswitch {
    cc58_map {
        0-5   "Sustain: Low Latency Legato"
        21-25 Staccato
    }
}
"#,
        )
        .expect("parse test spec")
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

    const SR: u32 = 48_000;

    #[test]
    fn qn_to_sec_integrates_tempo_map() {
        let tempo = vec![
            TempoPoint {
                qn: 0.0,
                bpm: 120.0,
            },
            TempoPoint { qn: 4.0, bpm: 60.0 },
        ];
        assert!((qn_to_sec(&tempo, 0.0) - 0.0).abs() < 1e-12);
        assert!((qn_to_sec(&tempo, 4.0) - 2.0).abs() < 1e-12);
        assert!((qn_to_sec(&tempo, 6.0) - 4.0).abs() < 1e-12);
        // empty map = 120 BPM
        assert!((qn_to_sec(&[], 2.0) - 1.0).abs() < 1e-12);
    }

    /// Golden values — this hash is FROZEN (rendered projects must reproduce
    /// forever). If this test fails, the hash function changed: revert it.
    #[test]
    fn stable_rr_slot_is_frozen() {
        let a = stable_rr_slot(42, 1.5, 62, 0, RrPurpose::Body);
        let b = stable_rr_slot(42, 1.5, 62, 0, RrPurpose::Transition);
        let c = stable_rr_slot(43, 1.5, 62, 0, RrPurpose::Body);
        // Distinct across purpose and seed…
        assert_ne!(a, b);
        assert_ne!(a, c);
        // …and identical across calls / positions (pure function).
        assert_eq!(a, stable_rr_slot(42, 1.5, 62, 0, RrPurpose::Body));
        // Frozen golden values (computed once at introduction).
        assert_eq!(a, 3847);
        assert_eq!(b, 3851);
        assert_eq!(c, 488);
    }

    #[test]
    fn legato_followed_note_becomes_prefire_with_expressive_lead() {
        let doc = TrackDocument {
            seed: 7,
            notes: vec![note(0.0, 2.1, 60, 90), note(2.0, 4.0, 62, 30)],
            ..Default::default()
        };
        let sched = annotate(&doc, &spec_with_legato(), SR);
        assert_eq!(sched.legato_count, 1);

        // 120 BPM ⇒ QN 2.0 = 1.0 s = 48000 frames; vel 30 ⇒ 333 ms lead.
        let tick = 48_000i64;
        let lead = ms_to_frames_i64(333, SR);
        let prefire = sched
            .events
            .iter()
            .find(|e| matches!(e.kind, DocEvent::LegatoPrefire { .. }))
            .expect("second note arrives via prefire");
        assert_eq!(prefire.frame as i64, tick - lead);

        // The first note is a plain note-on at frame 0; only one NoteOn total.
        let note_ons: Vec<_> = sched
            .events
            .iter()
            .filter(|e| matches!(e.kind, DocEvent::NoteOn { .. }))
            .collect();
        assert_eq!(note_ons.len(), 1);
        assert_eq!(note_ons[0].frame, 0);
    }

    #[test]
    fn rebow_drops_source_note_off_and_prefires_target() {
        let doc = TrackDocument {
            seed: 7,
            // Same pitch, abutting within 1/32 QN ⇒ re-bow.
            notes: vec![note(0.0, 2.0, 60, 90), note(2.01, 4.0, 60, 90)],
            ..Default::default()
        };
        let sched = annotate(&doc, &spec_with_legato(), SR);
        assert_eq!(sched.legato_count, 1, "re-bow target arrives via prefire");
        let offs: Vec<_> = sched
            .events
            .iter()
            .filter(|e| matches!(e.kind, DocEvent::NoteOff { .. }))
            .collect();
        assert_eq!(offs.len(), 1, "re-bow source's note-off is dropped");
    }

    #[test]
    fn shorts_preroll_and_do_not_prefire() {
        let doc = TrackDocument {
            seed: 7,
            // CC58 = 23 (Staccato band) before both notes.
            ccs: vec![DocCc {
                qn: 0.0,
                chan: 0,
                cc: 58,
                val: 23,
            }],
            // Overlapping shorts must NOT read as legato.
            notes: vec![note(1.0, 2.1, 60, 90), note(2.0, 3.0, 62, 90)],
            ..Default::default()
        };
        let sched = annotate(&doc, &spec_with_legato(), SR);
        assert_eq!(sched.short_count, 2);
        assert_eq!(sched.legato_count, 0);
        // QN 1.0 = 24000 frames, minus 60 ms (2880 frames) pre-roll.
        let first_on = sched
            .events
            .iter()
            .find(|e| matches!(e.kind, DocEvent::NoteOn { .. }))
            .expect("note-on");
        assert_eq!(first_on.frame, 24_000 - 2_880);
    }

    #[test]
    fn same_pitch_break_beyond_gap_is_not_rebow() {
        let doc = TrackDocument {
            seed: 7,
            notes: vec![note(0.0, 1.0, 60, 90), note(1.5, 2.0, 60, 90)],
            ..Default::default()
        };
        let sched = annotate(&doc, &spec_with_legato(), SR);
        assert_eq!(sched.legato_count, 0);
    }

    #[test]
    fn annotation_is_deterministic() {
        let doc = TrackDocument {
            seed: 99,
            notes: vec![
                note(0.0, 2.1, 60, 90),
                note(2.0, 4.0, 62, 30),
                note(4.0, 5.0, 64, 110),
            ],
            ccs: vec![DocCc {
                qn: 3.9,
                chan: 0,
                cc: 1,
                val: 80,
            }],
            ..Default::default()
        };
        let spec = spec_with_legato();
        let a = annotate(&doc, &spec, SR);
        let b = annotate(&doc, &spec, SR);
        assert_eq!(a.events, b.events);
    }
}

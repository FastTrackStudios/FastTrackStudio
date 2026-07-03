//! Document mode — the REALTIME transport-driven schedule walker (phase 2,
//! see `docs/plan/document-mode.md`).
//!
//! [`RealtimeScheduler`] is the same walk as the offline
//! [`render_schedule`](crate::document::render_schedule) walker, driven by
//! the HOST transport instead of a loop: each audio block covers the window
//! `[playhead, playhead + block_frames)` in **absolute frames from the
//! document epoch** (= project time zero), and every scheduled event whose
//! frame falls inside the window is dispatched at its exact in-block offset —
//! including [`LegatoPrefire`](DocEvent::LegatoPrefire)s ahead of their
//! destination ticks. Given the same sample rate, the realtime walk produces
//! the SAME engine call sequence as the offline walker, so the audio is
//! byte-identical (asserted by `signal-sampler-clap/tests/host_sim.rs`).
//!
//! ## Transport / tempo mapping policy
//!
//! The schedule's frames were baked from the DOCUMENT tempo map by
//! [`annotate`](crate::document::annotate). The host playhead
//! ([`BlockTransport::pos_frame`], the song position in samples) is trusted
//! as-is — **REAPER is the tempo authority**. If the host-reported tempo at
//! the current position disagrees with the document tempo map, the two
//! timelines have diverged (the document is stale); we log a warning ONCE
//! and keep following the host playhead. The fix is upstream: rebuild the
//! document from the host's tempo map (the phase-3 self-sourced document
//! does this automatically).
//!
//! ## Discontinuities (seek / loop / stop / late start)
//!
//! A block whose playhead is not contiguous with the previous block is a
//! discontinuity. The scheduler then:
//! 1. kills all pending voice/transition state (`panic`, like any sampler on
//!    seek) and re-asserts the document legato mode,
//! 2. re-locates the schedule cursor by binary search,
//! 3. v1 seek semantics: notes SOUNDING across the seek point restart at
//!    their next scheduled boundary (their trigger events are in the past
//!    and are skipped) — same rule as the offline walker's `start_frame`,
//! 4. **late start**: a [`LegatoPrefire`] whose trigger frame is already in
//!    the past but whose destination tick is still ahead (the transport
//!    started closer than `delay_ms` to a legato note) degrades gracefully
//!    to a plain note-on **at the destination tick** — on a silent line
//!    that's a fresh attack on the grid; on a sounding line it goes down the
//!    engine's reactive path (StrictLive-style late transition). These are
//!    counted in [`RealtimeScheduler::late_prefires`]; unlike the offline
//!    determinism suite (which requires exactly 0 reactive fallbacks), they
//!    are allowed in realtime — a human pressing play mid-phrase prefers a
//!    late transition over a missing note. Note this is the one place the
//!    realtime walk intentionally diverges from the offline `start_frame`
//!    render, which drops such notes entirely.
//!
//! ## Mode arbitration (block boundaries only)
//!
//! `transport playing && schedule present` ⇒ document mode owns the engine
//! (Lookahead + expressive legato). Anything else ⇒ StrictLive: the caller
//! (the CLAP plugin) dispatches incoming live MIDI through the normal bank
//! path. Transitions happen exclusively at block boundaries: entering kills
//! live voices (`panic`) and relocates; leaving releases scheduled notes
//! (`all_notes_off`) and restores [`PlayMode::StrictLive`]. While a document
//! is playing, incoming live MIDI is IGNORED by the plugin for phase 2
//! (overdub arbitration is phase 3+).
//!
//! ## Threading
//!
//! Everything here runs on the audio thread and only WALKS: schedule
//! building (`annotate`) happens off-thread and arrives as a pre-built
//! `Arc<Schedule>` via [`RealtimeScheduler::set_schedule`] (the plugin swaps
//! it in at a block boundary). The scheduler pre-allocates its small
//! late-prefire queue; the steady walk path performs no allocation. (Known
//! engine-side exception, pre-existing: some engine dispatch paths build
//! short strings — e.g. `play_direction` — on trigger.)

use std::sync::Arc;

use crate::bank::SamplerBank;
use crate::document::{DocEvent, Schedule, TempoPoint};
use crate::engine::LineId;

/// One block's host-transport snapshot (from the CLAP process context, or a
/// fake host in tests).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BlockTransport {
    /// Transport rolling?
    pub playing: bool,
    /// Song position of the FIRST frame of this block, in samples from
    /// project time zero (= the document epoch). May be negative (count-in).
    pub pos_frame: i64,
    /// Host-reported tempo at this block, if available (BPM). Only used for
    /// the stale-document diagnostic — the playhead is the authority.
    pub tempo_bpm: Option<f64>,
}

/// A prefire missed by a seek/late start, degraded to a note-on at its
/// destination tick.
#[derive(Debug, Clone, Copy)]
struct LateNote {
    tick: u64,
    line: u8,
    note: u8,
    vel: u8,
    rr: u32,
}

/// Realtime schedule walker for ONE bank instrument. See the module docs.
pub struct RealtimeScheduler {
    /// Bank instrument this scheduler drives.
    id: String,
    schedule: Option<Arc<Schedule>>,
    /// Index of the next un-dispatched schedule event.
    cursor: usize,
    /// Playhead frame the next block must start at to be contiguous.
    /// `None` forces a relocate.
    expect_frame: Option<i64>,
    /// Document mode currently owns the engine.
    active: bool,
    /// Missed prefires pending as late note-ons (sorted by tick).
    late: Vec<LateNote>,
    late_head: usize,
    late_prefires: u64,
    tempo_warned: bool,
}

/// Bound on the late-prefire queue: one straddling prefire per engine line
/// is the structural maximum (a mono line has at most one pending
/// transition), padded generously.
const LATE_CAP: usize = crate::engine::MAX_LINES * 2;

impl RealtimeScheduler {
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            schedule: None,
            cursor: 0,
            expect_frame: None,
            active: false,
            late: Vec::with_capacity(LATE_CAP),
            late_head: 0,
            late_prefires: 0,
            tempo_warned: false,
        }
    }

    /// Swap the schedule (block boundary; `None` clears document mode).
    /// A changed schedule mid-playback is treated as a discontinuity: the
    /// next block relocates into the new schedule (v1: sounding notes
    /// restart at their next boundary, same as a seek).
    pub fn set_schedule(&mut self, schedule: Option<Arc<Schedule>>) {
        let same = match (&self.schedule, &schedule) {
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            (None, None) => true,
            _ => false,
        };
        if same {
            return;
        }
        self.schedule = schedule;
        self.expect_frame = None; // force relocate (or clean exit) next block
        self.tempo_warned = false;
    }

    /// Whether document mode owned the engine after the last block.
    pub fn document_active(&self) -> bool {
        self.active
    }

    /// Prefires that were missed by a seek/late start and degraded to
    /// reactive-style note-ons at their tick (allowed in realtime; the
    /// offline determinism suite requires 0).
    pub fn late_prefires(&self) -> u64 {
        self.late_prefires
    }

    /// Process one audio block. `out` is interleaved stereo (len =
    /// `frames * 2`) and is cleared first.
    ///
    /// Returns `true` when document mode consumed the block (the schedule
    /// was walked and `out` rendered). Returns `false` when the engine is in
    /// StrictLive for this block — `out` is left CLEARED and untouched; the
    /// caller dispatches live MIDI and renders. Mode transitions (including
    /// the release/panic bookkeeping) happen inside this call, at the block
    /// boundary only.
    pub fn process_block(
        &mut self,
        bank: &mut SamplerBank,
        t: &BlockTransport,
        out: &mut [f32],
    ) -> bool {
        out.fill(0.0);
        let frames = out.len() / 2;

        let want = t.playing && self.schedule.is_some();
        if want != self.active {
            if want {
                // Enter document mode: relocate() below does the panic +
                // legato-mode assertion.
                self.active = true;
                self.expect_frame = None;
            } else {
                self.exit(bank);
            }
        }
        if !self.active {
            return false;
        }

        let sched = self.schedule.clone().expect("active implies schedule");
        if self.expect_frame != Some(t.pos_frame) {
            self.relocate(bank, &sched, t.pos_frame);
        }
        self.check_tempo(&sched, t);

        let start = t.pos_frame;
        let end = start + frames as i64;
        let mut off = 0usize; // frames of this block already rendered

        loop {
            let ev_frame = sched.events.get(self.cursor).map(|e| e.frame as i64);
            let late_frame = self.late.get(self.late_head).map(|l| l.tick as i64);
            let next = match (ev_frame, late_frame) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (a, b) => a.or(b),
            };
            let Some(nf) = next else { break };
            if nf >= end {
                break;
            }
            // Render up to the event's exact in-block offset. (Events whose
            // frame precedes the block — count-in, clamp artifacts — fire at
            // offset 0.)
            let target = (nf.max(start) - start) as usize;
            if target > off {
                bank.render(&mut out[off * 2..target * 2]);
                off = target;
            }
            // Dispatch everything at this frame; scheduled events first so
            // schedule order (Cc < NoteOff < Prefire < NoteOn) is preserved,
            // late note-ons (priority-3-equivalent) last.
            if ev_frame == Some(nf) {
                let ev = sched.events[self.cursor];
                self.cursor += 1;
                self.dispatch(bank, ev.line as LineId, ev.kind);
            } else {
                let l = self.late[self.late_head];
                self.late_head += 1;
                self.late_prefires += 1;
                bank.set_forced_rr(&self.id, Some(l.rr));
                bank.note_on_instrument_line(&self.id, l.line as LineId, l.note, l.vel);
            }
        }
        if frames > off {
            bank.render(&mut out[off * 2..frames * 2]);
        }
        self.expect_frame = Some(end);
        true
    }

    fn dispatch(&mut self, bank: &mut SamplerBank, line: LineId, kind: DocEvent) {
        // Keep in parity with the offline walker (`document::walk_schedule`)
        // — the determinism guarantee is exactly this parity.
        match kind {
            DocEvent::Cc { cc, val } => {
                bank.cc_instrument_line(&self.id, line, cc, val);
                // Document mode owns the legato mode: a low-latency CC58
                // press must not demote the expressive curve the schedule's
                // prefire leads were computed from.
                if cc == 58 && val <= 5 {
                    bank.set_legato_mode(&self.id, true, true);
                }
            }
            DocEvent::NoteOn { note, vel, rr } => {
                bank.set_forced_rr(&self.id, Some(rr));
                bank.note_on_instrument_line(&self.id, line, note, vel);
            }
            DocEvent::NoteOff { note, rr } => {
                bank.set_forced_rr(&self.id, Some(rr));
                bank.note_off_instrument_line(&self.id, line, note);
            }
            DocEvent::LegatoPrefire { note, vel, rr, .. } => {
                bank.set_forced_rr(&self.id, Some(rr));
                bank.legato_prefire_line(&self.id, line, note, vel);
            }
        }
    }

    /// Discontinuity handling: kill pending state, binary-search the cursor,
    /// and queue missed prefires whose destination tick is still ahead as
    /// late note-ons (see module docs, "Discontinuities").
    fn relocate(&mut self, bank: &mut SamplerBank, sched: &Schedule, pos_frame: i64) {
        bank.panic(&self.id);
        // Document playback always runs the full expressive legato — that is
        // the whole point of lookahead. This also flips the engine into
        // PlayMode::Lookahead.
        bank.set_legato_mode(&self.id, true, true);

        let pos = pos_frame.max(0) as u64;
        self.cursor = sched.events.partition_point(|e| e.frame < pos);
        self.late.clear();
        self.late_head = 0;

        // Back-scan (bounded by the schedule's largest prefire lead) for
        // prefires straddling the seek point.
        let horizon = pos.saturating_sub(sched.max_prefire_lead);
        let mut i = self.cursor;
        while i > 0 {
            i -= 1;
            let e = &sched.events[i];
            if e.frame < horizon {
                break;
            }
            if let DocEvent::LegatoPrefire {
                note,
                vel,
                rr,
                lead,
            } = e.kind
            {
                let tick = e.frame + lead as u64;
                if tick >= pos && self.late.len() < LATE_CAP {
                    self.late.push(LateNote {
                        tick,
                        line: e.line,
                        note,
                        vel,
                        rr,
                    });
                }
            }
        }
        self.late.sort_by_key(|l| l.tick);
    }

    /// Leave document mode: release scheduled notes and hand the engine back
    /// to the strict zero-latency live policy.
    fn exit(&mut self, bank: &mut SamplerBank) {
        bank.set_forced_rr(&self.id, None);
        bank.all_notes_off(&self.id);
        bank.set_play_mode(&self.id, crate::engine::PlayMode::StrictLive);
        self.active = false;
        self.expect_frame = None;
        self.late.clear();
        self.late_head = 0;
    }

    /// Stale-document diagnostic: compare the host tempo against the
    /// document tempo map at the current position; warn ONCE on divergence
    /// and keep trusting the host playhead (see module docs).
    fn check_tempo(&mut self, sched: &Schedule, t: &BlockTransport) {
        if self.tempo_warned {
            return;
        }
        let Some(host_bpm) = t.tempo_bpm else { return };
        let sec = t.pos_frame.max(0) as f64 / sched.sample_rate as f64;
        let doc_bpm = bpm_at_sec(&sched.tempo, sec);
        if (host_bpm - doc_bpm).abs() > 0.01 {
            self.tempo_warned = true;
            tracing::warn!(
                host_bpm,
                doc_bpm,
                pos_sec = sec,
                "document tempo map diverges from host tempo — following the \
                 HOST playhead (REAPER is the tempo authority); rebuild the \
                 document from the host tempo map to realign scheduled frames"
            );
        }
    }
}

/// BPM of the piecewise-constant document tempo map at `sec` seconds from
/// the document epoch (inverse-integration counterpart of
/// [`qn_to_sec`](crate::document::qn_to_sec)).
fn bpm_at_sec(tempo: &[TempoPoint], sec: f64) -> f64 {
    let mut bpm = tempo.first().map(|t| t.bpm).unwrap_or(120.0);
    let mut cur_sec = 0.0;
    let mut cur_qn = 0.0;
    for t in tempo {
        let seg_sec = (t.qn - cur_qn).max(0.0) * 60.0 / bpm;
        if cur_sec + seg_sec > sec {
            return bpm;
        }
        cur_sec += seg_sec;
        cur_qn = t.qn.max(cur_qn);
        bpm = t.bpm;
    }
    bpm
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bpm_at_sec_follows_the_map() {
        let map = vec![
            TempoPoint {
                qn: 0.0,
                bpm: 120.0,
            },
            TempoPoint { qn: 4.0, bpm: 60.0 }, // switch at 2.0 s
        ];
        assert_eq!(bpm_at_sec(&map, 0.0), 120.0);
        assert_eq!(bpm_at_sec(&map, 1.9), 120.0);
        assert_eq!(bpm_at_sec(&map, 2.1), 60.0);
        assert_eq!(bpm_at_sec(&[], 5.0), 120.0);
    }
}

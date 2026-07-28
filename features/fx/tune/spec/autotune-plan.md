# Autotune / pitch plan — realtime correction + offline Melodyne-style editor

Survey date 2026-07-28. Companion (pending): oss-pitch-editing research
incl. melonix deep-dive.

## What exists (file:line refs from survey)

- **tune-dsp**: clean-room YIN (detect.rs, window 2048, thr 0.12,
  parabolic interp; O(n²) — needs FFT autocorr for small buffers;
  aperiodicity computed but unused). Offline note segmentation
  (note.rs), tiny snapping (correct.rs: Scale{root_pc, u16 mask},
  only chromatic/major/minor; integer-MIDI snap, ties break down, no
  hysteresis, no A4 ref). **dna.rs is NOT a phase vocoder** — it's
  harmonic-mask source separation (offline only, no shifting; the
  DNA→shift bridge is prose; test fakes the shift). tracker.rs greedy
  note tracker. `basic-pitch` feature is DANGLING (onnx model shipped,
  module doesn't exist).
- **pitch-dsp** (features/fx/pitch): 8 shifters via PitchChain — Psola
  (1024 latency, used by tune plugin), Wsola, Granular, Rubberband
  (only formant-capable path, system GPL lib, non-default feature —
  falls back silently to WSOLA), + zero-latency octave toys. Quality
  harness exists (benches/quality.rs: SI-SDR/MCD/F0-cents vs
  Rubberband) — hang autotune regressions here.
- **keyflow-proto**: FULL music theory — Key, 21 ScaleMode's w/
  interval_pattern() (fold to u16 mask), KeySpelling (per-key
  enharmonics for PC buttons), MusicalNote, Melody w/ pitch_class(),
  harmonize_scale (chord-aware). THREE parallel scale enums exist
  (tune_dsp::Scale, keyflow ScaleMode, audio_controls ScaleType) — 
  unify on keyflow.
- **apps/plugins/tune**: the shipped realtime path. 5 params. BUGS:
  slew coeff is per-sample applied per-BLOCK (retune time scales with
  buffer size!); formant_linked=true is a NO-OP (chain.rs:231 discards
  formant ratio); no MIDI_INPUT declared; detection once per block.
- **No NativeTune in signal-fx** (MIDI arrives free via
  PluginEvents.midi once added). BlockType::Tuner UI is a placeholder.
- **No formant code in-tree** beyond the dead Rubberband FFI bools. No
  WORLD, no LPC/cepstral envelope anywhere.

## Plan

### Track 1 — realtime `signal.fx.tune` (world-class autotune)
1. **NativeTune block**: YIN (FFT-accelerated) + keyflow-scale snap +
   per-sample retune slew (fix the block-rate bug) over PitchChain
   PSOLA. MIDI target mode via PluginEvents.midi (Scale | MidiLatch |
   MidiGate; held-note stack). Params: key/scale (keyflow 21 modes),
   per-PC tri-state masks (snap/bypass/remove), retune_ms (0 = hard
   tune), flex-tune (only-correct-when-close), humanize (LP-split the
   f0 contour: correct drift, keep vibrato), transition/onset re-lock
   (reuse trigger-dsp SuperFlux), reference pitch A4, strength, mix.
   Snap hysteresis at note boundaries. Confidence gating from
   aperiodicity (stop octave-jump artifacts).
2. **Formant preservation**: spectral-envelope estimation (cepstral
   lifter or WORLD CheapTrick port — BSD) + envelope reimposition in
   the shift path; "throat" control (formant shift decoupled).
3. Quality regressions in pitch-dsp's harness (cents accuracy, SI-SDR).

### Track 2 — offline Melodyne-style editor (melonix-seeded)
- Analysis pass: YIN/pYIN track + tracker.rs segmentation (+ optional
  basic-pitch ONNX for polyphonic — build the missing module).
- Note-blob model: per-note pitch curve split into center + drift +
  vibrato (LP/HP of contour); edits = transpose/flatten-drift/scale-
  vibrato/retune-speed per note; time ops later.
- Resynthesis: WORLD-style (harvest/CheapTrick/D4C port or binding,
  BSD) for vocals; PSOLA fallback; DNA separation feeds polyphonic
  per-note editing (the missing bridge).
- Lives as a DAW clip editor surface (keyflow types for the grid).

### Hygiene
- Kill the dangling basic-pitch feature or build the module.
- Unify scale types on keyflow-proto; delete tune_dsp::Scale dupe.
- Wire tuner UI (audio-controls tuner_graph) to real detection.
- Write oss-pitch-editing.md research doc (melonix, WORLD, CREPE,
  PSOLA licenses) — pending research agent.

## Status 2026-07-28 — first implementation pass

- **model.rs landed**: NoteBlob center/drift/vibrato decomposition
  (zero-phase 3 Hz split), PitchDoc with melonix warp/bend markers,
  target curves + preview shift ratios. 5 tests.
- **correct.rs upgraded**: Scale::from_intervals (keyflow bridge),
  contains_pc, snap_hysteresis with per-PC bypass mask.
- **NativeTune landed** (`signal.fx.tune`): fixes all three shipped
  bugs — buffer-size-INDEPENDENT retune slew (settles by real block
  duration; 0 ms = hard jump), MIDI target modes (latch/gate) via
  PluginEvents.midi, per-PC bypass params, flex-tune, A4 reference,
  hysteresis, detected-midi/cents readback for the tuner UI.
- ⚠️ **PSOLA is broken at small ratios** (measured: 452 Hz at
  −0.467 st came out 469 Hz — wrong direction; ±2 st off by ~10–30
  cents). Granular is also inaccurate (−2 st → 367 Hz vs 402.7).
  **WSOLA is cents-accurate at all tested shifts** and is now
  NativeTune's engine. TODO: fix PsolaShifter's synthesis epoch
  spacing (integer-period rounding suspected) and switch the shipped
  apps/plugins/tune off PSOLA too — it has this bug in production.

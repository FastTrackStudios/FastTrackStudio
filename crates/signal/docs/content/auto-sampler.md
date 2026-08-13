+++
title = "Auto-sampler"
description = "Drive a MIDI instrument across a note/velocity grid, capture its audio, emit a zone-mapped .signalpack"
+++

# Auto-sampler

`features/sampler/signal-auto-sampler` — sample an **external hardware
instrument** into a `.signalpack`. MIDI notes go out to the instrument, its
audio returns on interface inputs, and the result is a zone-mode pack.

```text
  grid ── note/velocity cells ──▶ MIDI out ──▶ instrument
                                                  │ audio
  pack ◀── library.styx ◀── WAV ◀── capture ◀──────┘
```

Mounted on the existing CLI as `fts signal sample`.

Because one process owns both the MIDI port and the capture stream, note-on and
record-arm need no cross-process sync — which is why this is a plain crate plus
CLI rather than the generator/recorder plugin pair a plugin host would force.

## Commands

| Command | Purpose |
|---|---|
| `devices` | List MIDI in/out ports and audio in/out devices |
| `plan` | Print the note/velocity grid without touching hardware |
| `run` | Sample one patch |
| `batch` | Sample many patches unattended, selecting each by set-list slot |
| `play` | Play a pack live from a MIDI keyboard |
| `compare` | A/B a pack against the hardware it came from |
| `reloop` | Recompute loop points on an already-sampled folder (seconds, not a re-run) |
| `export-decent` | Write a DecentSampler `.dspreset` beside the samples |

## Why the pack is always zone-mode

The sampler *chose* every note and velocity, so each sample's mapping is stated
outright rather than parsed back out of a filename. Convention-mode parsing —
the source of the Keyscape note/mic/release bugs — is never involved.

Every zone is tagged with one declared `@Sustain` articulation. Zone-mode
resolves sample paths without consulting the articulation list, but the engine
still picks a default articulation at construction and fires only zones matching
it, so a zone tagged with an undeclared articulation loads and is silent.

## Design decisions, and the measurements behind them

Each of these was a bug found by ear and then pinned down by measurement. They
are recorded because the reasoning is not recoverable from the code alone.

### Adaptive note length

Timing values are **limits, not fixed waits**. The sampler watches the input
level and stops as soon as the note has decayed, so a percussive patch produces
short samples and a sustaining one gets its full release — without per-patch
configuration.

A fixed 500 ms release tail was truncating roughly two-thirds of the release on
a patch whose tail ran ~1.4 s. Pack sizes now vary 113–499 MB across patches
with identical zone counts, because the recordings are the instrument's actual
decay.

### Round-trip latency is measured, not assumed

Between `note_on` returning and the first sample arriving sit the OS MIDI stack,
the instrument's own note-on latency, its converters, the interface, and one
capture buffer — 12–21 ms in practice, and not knowable in advance. It is
measured once with a percussive strike and trimmed from every take.

Per-sample onset detection would be worse: a slow pad has no detectable
transient, so detection fails exactly where the note is quietest.

### Noise floor: quietest of N windows, after a full panic

Measuring "silence" while the instrument is still ringing inflates the floor,
which raises the onset threshold above anything the calibration note can reach —
so the run aborts claiming the instrument is unpatched or muted. Two guards:

- A full **panic** first (sustain pedal release + All Notes Off + All Sound Off,
  all 16 channels), since a latched pedal holds notes through All Notes Off.
- The floor is the **minimum** of 5 short windows, not one long one. Sound is
  always additive, so the lowest observation is the least contaminated.

### Reverb bleed between notes

A fixed 250 ms settle let one note's tail land in the *head* of the next
sample — audible as frequencies that are not in the patch, because the foreign
pitch then plays on every press of that key and transposes with it.

Caught by checking pitch, not level: `Whistle_A0_021` (root 27.5 Hz) had 328 Hz
and 334 Hz in its first 10 ms — the calibration note (E4 = 329.6 Hz). Fixed by
waiting for the input to fall below −66 dBFS, including after the calibration
strike. A sweep of all 180 samples then showed zero bleed.

### Loop points by cross-correlation

Snapping the loop to a whole number of cycles of the fundamental is not enough:
chorus, detune and LFOs have slower periodicities unrelated to pitch, so a loop
that is a perfect whole number of pitch cycles can still cut an LFO mid-sweep.

`loopfind.rs` searches for the loop length whose seam windows actually match,
scored by **normalized cross-correlation** (scale-invariant, so a decaying note
is not penalised for being quieter later). Coarse-to-fine: decimate 16×, find
the peak, refine at full rate — 2.8 s for 180 samples.

### Crossfade defaults to **0**

This inverts the usual advice, so it is worth stating plainly: with
correlation-chosen loop points, the raw join measured **smoother than the
material's own sample-to-sample motion** (0.76× the 99.9th-percentile
first-difference), while enabling a 150 ms crossfade produced an audible burst
of noise in two independent samplers.

Blending two near-identical copies is not free — any residual phase offset
combs. Leave it off unless a specific loop needs it.

### Seam threshold (`--min-seam`)

Not every sound loops. A decaying, inharmonic note has no steady region that
repeats, so the best available join is still audible. Below the threshold a zone
is left **unlooped** and plays its full recorded decay, which the adaptive tail
already captured.

At `--min-seam 0.97` the looped-zone count reads as a physics summary of each
instrument: Whistle (static synth) 528/528, Don't Let Organ 408, the pianos
~190, Limelight Synth 140.

### `release_start` records the note-off frame

Written to every zone. The engine ignores this field — see
[the ZoneSpec note](#known-engine-gaps) — but without it a zone left unlooped
loses the information needed to ever loop it again, because note-off could only
be inferred from a loop that no longer exists.

With it, re-looping is idempotent and reversible: 0.97 → 188 zones, 0.90 → 318,
back to 0.97 → exactly 188.

### Velocity interpolation in the DecentSampler export

`ampVelTrack="1"` scales amplitude by `velocity/127`. A zone recorded at
velocity *V* already sounds like *V*, so it is pre-boosted by `127/V` to make
the two cancel exactly at *V*. The recorded velocity reproduces its recorded
level, and velocities between layers ramp smoothly instead of stepping.

Six layers then behave like a continuum, which is why sampled instruments do not
need dozens of layers.

## Batch mode

Selection is by **set-list slot**, not bank/program. In Set List mode a Program
Change selects a slot, and the slot carries its own bank, program, transpose and
effects.

This avoids reproducing the Kronos Bank Select MSB/LSB table — the part of
driving a synth over MIDI most likely to be silently wrong — and samples the
patch *as the set list actually uses it*. The GM bank is exactly the trap it
avoids: `PRG G 079 Whistle` needs a different Bank Select from `PRG U-G 079`,
and the two share a program number.

Verified by audio, not by exit code: three slots sampled in sequence produced
spectra with 0.21–0.63 cross-similarity (identical patches would score >0.95).
A Program Change the instrument ignores fails silently — every pack would build,
validate, and contain the wrong sound.

**Resume is by pack existence**, not a progress file. A crashed run picks up
where it stopped; re-doing one patch is `rm` on its pack. This survived a real
test: 7 GB of source WAVs were deleted between runs and the batch still counted
correctly, because the resume key is the pack, not the samples.

`--limit N` takes a long job in sittings.

## Known engine gaps

- **`ZoneSpec.release_start` is not implemented by the engine.** Only
  `loop_start`/`loop_end`/`loop_xfade`, `sample_start`/`sample_end` and
  `playback_mode` affect playback. A zone relying on `release_start` to play a
  key-up tail does nothing; use a separate `trigger_mode "release"` zone.
- **`with_forward_loop` requires `loop_end > loop_start + 1`.** `0/0` means *no
  loop* — a held note stops when the sample runs out. There is no fall-back to
  looping the whole file.
- **Uncached zones are dropped, not queued.** The engine returns without
  spawning a voice on a cache miss, so a freshly-opened pack silently loses
  notes until preload catches up. `play` calls `preload_samples()` before
  starting the stream; anything else driving a pack live must do the same.
- **signal-sampler sounds worse than DecentSampler on identical data.** Same
  WAVs, same loop points, same velocity mapping. Confirmed by ear, unresolved.
  Given the crossfade finding above, the engine's crossfade handling is the
  first place to look. `compare` exists to isolate this kind of question.

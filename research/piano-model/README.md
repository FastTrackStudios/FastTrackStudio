# piano-model — Keyscape C7 physical-model research

**Personal, non-shipping.** Turns the owned Keyscape *LA Custom C7 Grand*
sample set into physical-model parameters, then trains a neural residual so a
headless physical piano engine plays *that* instrument — with continuous
velocity, real sympathetic resonance, and real pedal instead of static layers.

Detached from the root monorepo workspace on purpose (own `[workspace]`,
own lockfile/target) — this is research tooling, not a shipping crate.

## One engine, many models

The engine is component-based: each physical block (unison strings, per-mode
jitter, two-stage decay, hammer attack, soundboard) is toggled and parameterized
by a `ModelConfig` (`src/model.rs`, serialized to JSON). A trained *model* =
its measured param table + a voicing config. Different instruments in the
Keyscape set (grand, Rhodes, Wurli, Clavinet, toy pianos) reuse this one engine
with different blocks enabled — e.g. a Rhodes turns `unison` off.

```
pm config --out out/grand_c7.json      # dump the default grand voicing to edit
pm synth --note 57 --vel 70            # uses built-in grand_c7
pm synth --config my_rhodes.json ...   # any voicing; None blocks are disabled
```

Ablation (proves the blocks): grand (all on) envelope cos 0.95 → bare modal
(all off) 0.78.

## Method (the hybrid: physics + neural residual)

Physics gets ~90% and generalizes; a small per-note net learns the residual
against the samples. Base-engine quality gates the whole thing — if the
physics is weak the net overfits into a glorified sampler.

## Pipeline stages

1. **Index** ✅ — parse the 5718 FLACs → structured sample set.
   `pm index` : 88 notes (MIDI 21–108), 24 velocity layers,
   pedal-up / pedal-down / release / pedal-noise articulations. 0 rejects.
2. **Analyze** ✅ (v0) — per-note spectral parameter extraction.
   `pm analyze --note N --vel V` : measured f0 (stretched tuning),
   inharmonicity B (asymmetric sharp-side partial search, fit on low-mid
   partials), decay T60, per-velocity peak RMS.
   - Verified physical: A3 B≈2.6e-4, wound-bass A1 B≈3e-5 (low, correct),
     stretched octaves. Real physics off real audio.
   - **Next refinement**: multi-frame median picking + explicit string-pair
     (two-string beating) detection to stabilize high-partial B; per-partial
     decay rates (two-stage decay), not just total-energy T60.
3. **Sweep** ✅ — `pm sweep` analyzes every (note, velocity) pedal-up sample
   in parallel → `out/c7_table.json` (316 records: f0, B, decay, peak RMS,
   full modal partial set with per-partial amp+decay). The physical
   fingerprint of the C7.
4. **Engine v0 (modal resynth)** ✅ — `pm synth` renders a note purely from
   its modal (freq, amp, decay) triples — no sample played back — and writes
   model+real WAV plus objective A/B (spectral + envelope cosine).
   - Result: envelope cos ~0.72–0.90 (decay physics captured); spectral cos
     ~0.29–0.60, falling with pitch (missing hammer attack, twin-string
     beating, inter-partial soundboard energy). Confirms base is sound and
     localizes the residual.
5. **Physics++** 🟡 — techniques ported from openwurli (studied in
   `scratchpad/openwurli`, GPL, patterns only):
   - ✅ Hammer attack-noise burst (bandpass, vel²-scaled) — openwurli
     `hammer.rs`. Adds the strike "knock" modal synth can't make.
   - ✅ Per-mode Ornstein-Uhlenbeck frequency jitter (~4 cents, τ=20ms) —
     openwurli `reed.rs`. Kills the static/metallic sustain.
   - Result: spectral cos A3 0.60→0.67, C5 0.45→0.60, C6 0.29→0.53.
   - ⬜ Still to add: twin detuned strings (beating / two-stage decay);
     soundboard body IR; quadrature oscillator + renorm (realtime form).
5b. **Twin/triple strings + two-stage decay** ✅ — register-correct unison
   count (1/2/3 strings), symmetric detuning → beating; per-partial
   double-exponential decay peeled from the sample (prompt sound + aftersound).
   - Envelope cos jumped: A3 0.79→0.95, C5 0.90→0.97, C6 0.76→0.98,
     A1 0.75→0.92. Extracted physics is correct (A3 k1: 83% decays in 0.9s
     prompt, 17% rings 20s aftersound).
5c. **Parametric soundboard** 🟡 — `soundboard.rs`: a tunable modal resonator
   bank (freq/Q/gain per mode), NOT an IR. Driven by hammer strike + string
   tap. Correct-but-subtle: the measured partials already carry the sampled
   board's balance, so a board-on-top can only add body without
   double-counting. **A truly controllable board needs de-embedding** (divide
   the board response out of the samples → bare-string source + explicit board
   filter = full source/filter split). That's the next real step for "control
   every aspect".
### Quality investigation (City Grand) — where the remaining gap is

A `spectral_diag` metric decomposes model-vs-real into **harmonic cos** (balance
at the partial frequencies) and **broadband ratio** (real ÷ model inter-partial
energy). Findings, measured across the keyboard:

- **Harmonic balance is already good**: cos 0.93–0.97 in mid/treble. Only the
  bass (A1 ≈ 0.77) needs better dense-partial extraction (wound strings).
- **The entire spectral gap is broadband body**: the real recording has
  **12–165× more inter-partial energy** than the model.
- **A linear resonator bank driven by the harmonic string cannot fix this** —
  proven: dense (72-mode) continuous board moved the ratio 93→101, i.e. not at
  all. Linear filtering can't create energy where the source has none.
- **A generative source (noise/nonlinearity) is required.** Note-enveloped noise
  into the board *does* fill broadband (ratio 101→51) — but hand-guessed shape
  makes the overall match *worse* (cos 0.65→0.46). **The board's broadband
  response must be FIT from data or learned, not hand-tuned.**

Conclusion → the next quality lever is one of:
   (a) **De-embed** the board response from the samples (divide harmonics out →
       measured inter-partial spectrum → shape the generative body per note), or
   (b) **Neural residual** (openwurli-style, in parameter/spectral-envelope
       space) trained on model-vs-sample.
Both need the same infra; (a) is interpretable/controllable, (b) is higher-ceiling.

6. **Sympathetic** ⬜ — PD−PU difference per note isolates sympathetic
   resonance + soundboard coupling; release samples → damper model.
7. **Engine** ⬜ — promote to a headless `no_std`+alloc Rust engine (no heap
   on the hot path, no threads) so it can drop into `features/fx` as a real
   instrument. Candidate reference: qiano (waveguide) architecture.
8. **Residual** ⬜ — small per-note MLP (candle) learns engine-output →
   sample difference; realtime inference via the in-tree `neural-amp-modeler`
   pattern. Training target = the sample set; A/B loss against held-out
   velocities.
7. **A/B harness** ⬜ — null-test engine vs sample per (note, vel); spectral
   + perceptual diff to drive iteration.

## Data

`/run/media/AudioHaven/Sampled/Keys/Keyscape/LA Custom C7 Grand/`
— 5718 FLAC, 44.1 kHz, plus `library.styx` (articulation/velocity map).

## Run

```
cargo build --release
./target/release/pm index                 # coverage + optional --out manifest.json
./target/release/pm analyze --note 57 --vel 70
./target/release/pm sweep --vels 24,46,64,82,100,115,124,127 \
    --out out/city_grand_table.json       # build the playable param table
```

## Play it live (MIDI)

Realtime polyphonic engine (`src/realtime.rs`): cpal audio out + midir MIDI in,
quadrature oscillators + exp-multiplier envelopes (the `no_std`-ready per-sample
form of the offline synth). Loads the swept table; nearest velocity-layer lookup
+ note transposition for the full MIDI range.

```
pm play --list                            # list audio + MIDI ports
pm play --selftest                        # render a test chord to WAV (no hardware)
pm play --midi-port <N>                   # play from MIDI port N (default 0)
pm play --midi-port <N> --config my.json  # play a specific voicing
```

Not yet wired into signal-engine — this is a standalone player for testing. The
same voice DSP is the thing that later drops into `features/fx` / signal-engine
as a headless vox-served instrument.

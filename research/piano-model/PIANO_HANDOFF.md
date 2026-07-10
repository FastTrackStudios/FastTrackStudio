# City Grand — Piano Physical-Model Handoff

Everything a new agent needs to continue building a **WASM-capable, coupled
physically-modeled piano** that is **trained/tuned per sample-library**. Read
this top to bottom once, then keep `PIANOTEQ_BLUEPRINT.md` open as the design
spec.

---

## 1. The goal (locked with the user)

Build ONE physical piano engine that:
- runs **real-time on CPU and in WASM** (browser) — so **no big samples**, it
  must be compact synthesis;
- is **tuned to match a target sample library** (start: Keyscape "LA Custom C7
  Grand"; then the user's other owned libraries — one trained param-set per
  piano);
- **training/tuning runs on GPU** (RTX 4080 present; candle CUDA), the **engine
  runs on CPU**.

The proof-of-capability target is "sound like Keyscape / Pianoteq." The value of
a model over sampling: continuous velocity, real pedal, infinite sustain, tiny
footprint, WASM.

---

## 2. THE key findings (read these — they cost the most to learn)

1. **Additive resynthesis of fitted static params has a perceptual ceiling.**
   We built a full analysis→modal-resynth→DDSP-fit pipeline. DDSP fitting
   *works* and is robust (see §5), improving log-spectral distance ~**-4.5 dB**
   across notes×velocities. **But it still sounds synthetic** — because it's
   essentially a sophisticated sinusoidal model: static per-note recipes, no
   dynamics, no coupling, no nonlinearity.

2. **The "broadband body" we chased for a long time was a GHOST = recording
   noise floor + room.** We measured Keyscape as having 12–165× more
   inter-partial energy than our model and spent weeks trying to add it (noise
   residual, sympathetic-as-noise, attack-as-noise). It always sounded like
   HISS and made the metric worse. Then we black-box-RE'd Pianoteq:
   **Pianoteq's broadband/harmonic ratio is ~0.001 — CLEANER than our model
   (0.010), and cleaner than Keyscape (0.12 at soft velocities).** A superb
   physical model has essentially no broadband floor. **Conclusion: the sample's
   broadband is mic/room noise, not the instrument. STOP adding broadband
   noise.** The model was right to reject it.

3. **The real quality driver is DYNAMICS, not spectrum shape:**
   - **Nonlinear hammer** → velocity→brightness. Measured: soft→loud gives
     **4.6× (Pianoteq) to 8× (Keyscape)** increase in high/low partial energy,
     while inharmonicity stays fixed. This is what makes a piano feel alive.
   - **Coupled strings** → two-stage decay (prompt/aftersound) + beating.
   - **Sympathetic resonance** → pedal-down bloom (measured 5× broadband PD vs
     PU — and it's TONAL, other strings ringing, not noise).
   These are exactly what a *static additive* model cannot produce and what a
   *coupled nonlinear real-time simulation* produces for free.

4. **Pianoteq's method (from RE + manual):** "computed in real-time… alive, not
   static… parameters control the PHYSICAL PROPERTIES, not the sound." A
   **coupled, nonlinear, real-time physical simulation**. This is the paradigm
   to build. See `PIANOTEQ_BLUEPRINT.md` for the full param→DSP map.

**Net: the project PIVOTED from sample-fitting to a coupled waveguide physical
model.** The analysis/DDSP/metric tooling is NOT wasted — it now measures targets
and tunes the physical model's params.

---

## 3. Where the code lives

**Research crate** (all the DSP + tooling): `research/piano-model/` — a DETACHED
cargo workspace (own lockfile/target; empty `[workspace]` so it doesn't perturb
the 160-member monorepo). Binary = `pm`. Pure Rust.

Build: `cd research/piano-model && cargo build --release`

Worktree: `/run/media/Development/FastTrackStudio-piano` (branch `piano-model`).

### `pm` subcommands (the toolbox)
- `index` — scan the Keyscape library → coverage (5718 samples, 88 notes, 24
  vels, articulations PU/PD/Rel/pedal-noise).
- `analyze --note N --vel V` — per-note spectral params: f0, inharmonicity **B**,
  two-stage decay, partials. THE physical-param extractor.
- `probe --path <wav> --note N` — analyze ANY wav (Pianoteq/Keyscape/ours):
  f0, B, decay, **brightness** (partials>5/first5), **broadband/harmonic** ratio.
  The black-box RE tool.
- `wg --note N --vel V [--inharm --n-disp --t60 --brightness --strings --detune
  --coupling]` — render the **waveguide engine** (the new direction). 
- `decompose --note --vel` — split a note into components (attack/tone/release/
  sympathetic/pedal) as WAVs.
- `analyze`/`sweep`/`synth`/`config`/`play` — the OLD additive pipeline (modal
  resynth). `synth` prints the **ACCURACY LSD** line (the metric).
- `train`/`train-set`/`fit` — DDSP fitting (candle). `train-set` batch-fits
  notes×velocities with early-stopping (see §5).
- `attack` — measured hammer-noise component (small, tonal-safe).

### Modules (`src/`)
- `sample.rs` — Keyscape filename parser + library scan.
- `audio.rs` — FLAC (claxon) + WAV (hound) load → mono f32. `load_any` dispatches
  by extension.
- `analyze.rs` — spectral analysis; `accuracy_lsd` (THE metric), `broadband_energy`,
  `extract_residual`, `attack_noise_model`, `avg_mag`.
- `synth.rs` — additive modal render + `add_residual` (noise, now off by default).
- `model.rs` — `ModelConfig` (toggleable voicing blocks) for the additive engine.
- `soundboard.rs` — parametric resonator bank (additive engine).
- `table.rs`, `realtime.rs` — additive realtime player (`pm play`, standalone).
- `ddsp.rs` — **DDSP training**: differentiable additive+noise synth in candle,
  multi-res STFT loss, Adam, `QuickVal` cheap validator, early stopping.
- `waveguide.rs` — **THE NEW ENGINE**: `StringWaveguide` (stiff-string digital
  waveguide) + `CoupledStrings` (unison bridge coupling).

### signal-engine integration (playable in the real rig)
Both play through the tree's keys rig (`just piano` / `just wurli` from worktree
root; TUI with meters, pipewire audio, MIDI). NOTE the ALSA gotcha in §7.
- **City Grand** (additive engine): `features/sampler/signal-sampler/src/native/
  modal.rs` (`NativeModal`, a `PluginInstance` on `BlockType::Harmonic`). Loads
  `~/.config/signal/city-grand/table.json`. Has sympathetic bank + residual (off).
- **City Wurli** (vendored openwurli, GPL, personal use): `features/rigs/wurli/
  openwurli-dsp/` (git dep removed → zero external deps), wrapped in
  `signal-sampler/src/native/wurli.rs` (`NativeWurli`, `BlockType::Formant`).
- Presets in `signal-sampler/src/nord.rs` (`city_grand_preset`,
  `city_wurli_preset`), registered in `preset_registry.rs::with_builtins`.
- Registry: `signal-sampler/src/native/registry.rs`.

The waveguide engine is NOT yet wired into signal-engine (still in the research
crate). When it's ready, it replaces/augments `NativeModal`'s voice.

---

## 4. Data & tools on disk

- **Keyscape samples** (already decrypted to FLAC, 157 GB):
  `/run/media/AudioHaven/Sampled/Keys/Keyscape/` — grand at `LA Custom C7 Grand/`
  (5718 FLAC + `library.styx`). Filenames encode `RR{rr}_SL{layer}LACP{PU|PD}r{..}
  _{note}-{vel}.flac` (PU=pedal-up, PD=pedal-down w/ sympathetic, Rel=release).
  Keyscape is Spectrasonics STEAM, NOT Kontakt — but it's already extracted.
- **Pianoteq 9** (RE target + oracle): standalone CLI at
  `~/Downloads/Pianoteq 9/x86-64bit/Pianoteq 9` (headless render:
  `"…/Pianoteq 9" --headless --preset "NY Steinway Model D" --midi x.mid
  --wav y.wav --rate 44100 --mono`). DSP core (stripped, 53MB):
  `~/.vst3/Pianoteq 9.vst3/Contents/x86_64-linux/Pianoteq 9.so`. Docs (full
  manual, physical model detail): `~/Downloads/Pianoteq 9/Documentation/
  pianoteq-english.html` (bodies collapsed; the ONLINE manual
  https://www.modartt.com/user_manual?product=pianoteq&lang=en parses better).
  It's the TRIAL — fine for analysis. MIDI probes + renders in
  `<scratchpad>/ptq/`.
- **Installed City Grand table**: `~/.config/signal/city-grand/table.json`
  (currently the DDSP-trained-C–G banked table; 742 records).

---

## 5. The metric & the DDSP training (still useful for tuning)

- **Metric**: `analyze::accuracy_lsd` — multi-resolution (1024/4096/16384) log-
  spectral distance in **dB**, onset-aligned, peak-normalized, -80 dB floor,
  phase-invariant. **0 dB = perfect null; lower = better.** Physical-model target
  is perceptual transparency (~single digits), NOT 0 (only a sampler nulls a
  recording). Print via `pm synth`/`pm train`; compute on any wav vs sample.
- **DDSP training lessons (baked into `ddsp.rs`, reusable to tune the physical
  model's params):**
  1. **RMS-normalize model AND target** in the loss, else Adam just gain-matches.
  2. **Early-stop on the accuracy metric**, not the STFT loss — naive DDSP
     overfits (loss ↓ but transferable output ↑; some cells diverged +12 dB).
     Keep best-scoring params starting from init → a cell can never regress.
  3. **Validation must be CHEAP** (`QuickVal`: one pre-planned 2048 FFT, 0.6 s
     window) — the full multi-res LSD in the inner loop is ~50× too slow.
  4. GPU: candle is device-agnostic; `Device::Cpu` → `cuda_if_available` + the
     `cuda` feature (needs CUDA toolkit/nvcc). Worth it for batch fitting on the
     4080. Physical-model params are mostly analytic though.
- **Proven result** (why we trust the fitter): C4–G4 × 23 vels (114 cells)
  25.77 → 21.28 dB, every cell improved. Banked into the playable table.

---

## 6. THE WAVEGUIDE ENGINE — current state & roadmap (the live work)

`src/waveguide.rs`. A note is a delay-line loop:
`loop = loop_gain · dispersion( loss_lp( delay(P) ) )` + fractional tuning
allpass + velocity hammer (contact pulse, harder=shorter=brighter) + strike-
point comb. `CoupledStrings` = N detuned strings sharing a bridge.

**Working:** single stiff string — tuned A3 (+0.9 cents), controllable
inharmonicity (B 2.2e-4 vs 3e-4 target, via `n_disp`), physical decay (11 s),
clean spectrum (0.006, like Pianoteq), and **velocity→brightness emerges**
(soft 0.000 → loud 0.008). Beating from detuned coupled strings works.

**Not yet working / the next tasks (in order):**
1. **Bridge admittance (the two-stage decay).** Current coupling is a crude
   scalar gain → doesn't cleanly split prompt/aftersound and goes UNSTABLE at
   coupling ≥ 0.1. Replace with a proper **bridge filter/resonator** (the string
   termination reflection depends on a bridge admittance) + stability guards.
   This is where the piano's signature bloom comes from. Ref: Weinreich 1977;
   Smith PASP (string-bridge coupling); Bensa 2003.
2. **Dispersion filter design + per-note tuning.** Getting B=3e-4 needs ~40
   allpasses (heavy). Use the proper **Rauhala-Välimäki dispersion filter design**
   (target B → few optimized allpass coeffs). Fix per-note tuning precisely
   (current +7.5 c constant offset is trimmed empirically with `-0.14`; proper
   fix = precise allpass group-delay accounting per note — Smith PASP).
3. **Hammer nonlinearity** — replace the contact-pulse heuristic with a proper
   nonlinear felt model (force = k·compression^p, hysteretic). Ref: Stulov 1995;
   Chaigne & Askenfelt 1994. This is the biggest perceptual lever (the measured
   4.6–8× velocity→brightness).
4. **Sympathetic + duplex** — all undamped strings ring through the bridge
   (pedal/CC64 gated). Tonal, not noise. (An additive `SympatheticBank` exists
   in `native/modal.rs` as a first cut.)
5. **Soundboard coupling filter** — impedance magnitude (sustain) + cutoff +
   slope (a 3-param bridge admittance), tuned from `pm analyze`'s decay-vs-freq.
6. **Tune the whole thing per library** — `pm analyze` gives B (→string length),
   decay-vs-freq (→impedance), overtone nulls (→strike point), brightness-vs-vel
   (→hammer hardness) analytically; a gradient-free/GPU-loss pass refines the
   rest. Validate with `accuracy_lsd` vs the library AND vs Pianoteq renders.
7. **Port** to `no_std`+alloc (per CLAUDE.md processing-core rules: no heap on
   hot path, no threads) → a `features/fx` voice → signal-engine → WASM.

**How to test each step:** render with `pm wg …`, then `pm probe --path out/wg.wav
--note N` to check f0/B/decay/brightness; A/B against a matched Pianoteq render
(`Pianoteq … --midi <single-note> --wav`) and the Keyscape sample.

---

## 7. Gotchas

- **ALSA sequencer queues**: `--midi all` in the keys rigs allocates ONE ALSA
  seq queue PER MIDI PORT. This rig has ~24 ports (mioXM ×16, Axe-Fx, X-Touch,
  **KONTROL S88** ×3) → `all` exhausts ALSA's ~32-queue limit →
  `snd_seq_alloc_named_queue: Out of memory`. ALWAYS target ONE port by name:
  `just piano KONTROL` (the S88 is the playing keyboard). Recipes default to
  `KONTROL`.
- A stale `signal-engine`/`fasttrackstudio --engine` may hold ~22 queues — kill
  it if queues are exhausted.
- **Pianoteq is the TRIAL** — fine for analysis; may add noise on long renders.
- **candle CUDA** needs the CUDA toolkit installed to build with the `cuda`
  feature; CPU works out of the box.
- Background `nohup` waiters kept getting killed at task-notification boundaries;
  the actual training processes survived. Poll the log directly.

---

## 8. The papers (reading list, by build-usefulness)

1. **J.O. Smith III, *Physical Audio Signal Processing*** — the DSP bible for
   waveguides. FREE: https://ccrma.stanford.edu/~jos/pasp/ . Read: Digital
   Waveguides, Stiff String (dispersion allpass), Commuted Piano Synthesis,
   String-Bridge coupling. **This is the build reference.**
2. **Bensa, Bilbao, Kronland-Martinet & Smith, "The simulation of piano string
   vibration: From physical models to finite difference schemes and digital
   waveguides,"** JASA 114(2), 2003. The best physics→waveguide bridge (our task).
3. **Chabassier PhD thesis, "Modeling and numerical simulation of a piano by
   physical models"** (École Polytechnique/INRIA, 2012). FREE on HAL. The
   rigorous full coupled model (reference depth).
4. **Bank, Zambon & Fontana, "A Modal-Based Real-Time Piano Synthesizer,"** IEEE
   TASLP 18(4), 2010; + Bank MSc "Physics-Based Sound Synthesis of the Piano"
   (2000). PDFs on Balázs Bank's site (home.mit.bme.hu/~bank/). Real-time modal.
5. **Hammer felt (nonlinear velocity→brightness — the top lever):** Stulov,
   "Hysteretic model of the grand piano hammer felt," JASA 97(1), 1995; Chaigne
   & Askenfelt, "Numerical simulations of piano strings I & II," JASA 95(2)&(3),
   1994.
6. **String coupling (two-stage decay + sympathetic):** Weinreich, "Coupled
   piano strings," JASA 62(6), 1977.
7. **Dispersion filter design:** Rauhala & Välimäki, "Tunable dispersion filter
   design for piano synthesis" (search title) — maps target B → allpass coeffs.

`PIANOTEQ_BLUEPRINT.md` maps every Pianoteq parameter → the DSP each paper
describes.

---

## 9. Suggested first move for the next agent

Read `PIANOTEQ_BLUEPRINT.md` + Smith PASP (waveguide + string-bridge coupling),
then implement **task 6.1 (bridge admittance)** — replace the scalar coupling in
`CoupledStrings` with a proper bridge filter so the two-stage decay emerges and
the coupling is stable. Test: `pm wg --strings 3 …` then `pm probe` — the k1
two-stage fit should show a real prompt/aftersound split (fast then slow), and it
should stay stable. A/B against `<scratchpad>/ptq/a3_pu.wav` (Pianoteq A3).

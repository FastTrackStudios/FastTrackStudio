# Pianoteq's model, decoded — the blueprint for the coupled engine

Sources: RE of `Pianoteq 9.so` (parameter vocabulary in the stripped binary:
`coupling_curve`, `impedance_cutoff`, `duplex_scale_resonance`, `Aliquot
Strings`, `hammer_hardness_{piano,mezzo,forte}`, `damper_*`, `direct_sound`) +
the Pianoteq manual §3.5 + §7–8. The manual is explicit about the paradigm:

> "computed in real-time, as on a real piano, not just read from disk or memory.
>  This explains why the sound is alive, not static: it is not a simple
>  recording… the parameters do not control the sound directly (not filters or
>  envelopes, far from a classic ADSR), but the **physical properties** of the
>  piano."

That is the whole lesson: **a coupled, nonlinear, real-time physical simulation**
— not additive resynthesis of fitted params (which is what our DDSP path does,
and why it sounds static despite the metric gains).

## Signal flow

```
Hammer (nonlinear, velocity-dependent hardness)
  → Strike point (comb: which overtones the strike excites)
  → String  ×3 per note (stiff waveguide → inharmonicity; detuned by unison width)
      ⇄ Bridge/Soundboard mechanical IMPEDANCE  (bidirectional coupling)
  → Soundboard resonator (cutoff + Q)           → Direct sound
  + Sympathetic resonance (all undamped strings, coupled)
  + Duplex scale / Aliquot (undamped string segments)
  + Damper model (position, duration, noise, velocity threshold)
```

## Every Pianoteq parameter → its physical DSP (this IS the build spec)

| Pianoteq control | Manual's physical description | DSP realization |
|---|---|---|
| **String length** | "inharmonicity decreases very rapidly with string length"; short → bell-like | **dispersion allpass** in the string waveguide; allpass coefficient ← inharmonicity B (which `pm analyze` already measures per note) |
| **Strike point** | "position where the string is struck by the hammer" → overtone distribution | **feedforward comb** on the excitation (`1 - z^-D`, D = strike fraction) — nulls harmonics with a node at the strike |
| **Hammer hardness** (pp/mf/ff) | "the harder the felt, the more brilliant"; velocity-dependent | **nonlinear hammer–felt contact** force = k·compression^p (hysteretic); hardness (p,k) interpolated across pp/mf/ff by velocity → louder = brighter *timbre* |
| **Spectrum profile** | 8 sliders = "individual intensity of the first eight overtones" | excitation spectral shaping (8-band) |
| **Hammer noise** | "the weight of the hammer percussion sound" | short filtered-noise burst at contact |
| **Hammer tone** | "darken/brighten the attack — low-freq woody impact vs high-pitched string contact" | tilt filter on the excitation (woody-LF ↔ contact-HF balance) |
| **Unison width** | "frequency variation within three-string groups"; wide = honky-tonk | **3 detuned coupled strings** per note |
| **Soundboard mechanical impedance** | "the greater the impedance, the longer the sound" | **string↔bridge coupling loss** — impedance magnitude sets energy-loss rate (sustain length + two-stage decay) |
| **Impedance cutoff** | "higher → more high overtones present" | the coupling/loss filter's **corner frequency** |
| **Impedance slope** | "greater → faster the high overtones decrease" | the coupling/loss filter's **high-freq slope** (freq-dependent damping) |
| **Sympathetic resonance** | "undamped strings vibrate when other strings are excited" | **coupled resonator bank** over all undamped strings, driven through the bridge |
| **Duplex scale** | "undamped string parts between tuning pins and frame — enriches harmonic content" | extra resonators tuned to the **non-speaking (front/back) segment** lengths |
| **Blooming** (energy, inertia) | "nonlinearity of the mechanical response… during the attack, energy transfers from lower to higher overtones ('boiinng')" | **nonlinear energy transfer** low→high overtones during attack; energy = amount, inertia = speed. (This is the tension-modulation nonlinearity → phantom partials.) |
| **Octave stretching** | "octaves stretched because of string inharmonicity" | emerges from the dispersion/inharmonicity + tuning |
| **Damper** (pos/dur/noise/vel-thresh) | release | damping into the string waveguide on note-off, register-dependent |
| **Direct sound duration** | initial radiated sound before board coupling dominates | direct string radiation path |
| **Condition** | "freshly-tuned → completely worn-out" (randomizable) | per-string randomization (detune, damping, hardness) |

### Two details worth calling out (from the full manual)

- **The soundboard is a 3-parameter *coupling filter***, not a static resonator:
  impedance **magnitude** (sustain), **cutoff** (how much HF passes), **slope**
  (how fast HF dies). That's the frequency-dependent bridge admittance — it sets
  the whole decay-vs-frequency behavior our fitted model faked with per-partial
  decays.
- **Blooming = the nonlinear energy transfer** (low→high overtones during the
  attack). This is the *generative nonlinearity* — the same mechanism behind
  piano phantom/longitudinal partials. It's exactly the "tonal richness that
  emerges" our additive model structurally cannot make. A tension-modulated /
  nonlinearly-coupled string is what produces it.

## Why this sounds alive and ours doesn't

- **Nonlinear hammer**: soft vs loud differ in *timbre* (generated by the
  contact physics), not a crossfade between velocity layers.
- **Bidirectional coupling**: string → board → string, and string → string.
  Phantom (longitudinal) partials, beating, bloom **emerge** from the coupling —
  they aren't fitted.
- **Real-time waveguide**: every sample computed from interacting delay lines +
  filters. Continuous velocity, real pedal, infinite sustain — free.

## Build plan (the pivot)

1. **Nonlinear hammer** → excitation (force–compression contact model, velocity
   sets hardness).
2. **Stiff-string waveguide**: delay line + **dispersion allpass** (inharmonicity
   from `String length`) + loss filter; **strike-point comb** on input. ×3
   detuned strings (`unison width`).
3. **Bridge/soundboard coupling**: strings share a bridge; **impedance** sets the
   coupling gain (sustain) and the two-stage decay emerges from the coupled
   symmetric/antisymmetric modes.
4. **Soundboard resonator**: cutoff + Q; radiates the direct + coupled sound.
5. **Sympathetic bank**: all undamped strings ring through the bridge (we already
   built a first `SympatheticBank` — reuse/upgrade).

Starting point: **qiano / FigBug** (found at session start — a coupled
*waveguide* piano with string↔soundboard coupling + longitudinal/phantom
partials — i.e. exactly this paradigm, open source). Port DSP to Rust/`no_std`.

## What we keep from the DDSP work (not wasted)

- **Sample analysis** (`pm analyze`: inharmonicity B, decay, partials) → *tunes
  this model's physical params* from the Keyscape data (string length ← B,
  impedance ← decay, strike point ← overtone nulls, hardness ← brightness).
- **LSD metric + DDSP fitter** → auto-tune the physical params against samples
  (Pianoteq is hand-tuned; we can fit ours).
- **signal-engine integration + WASM path** → plays whatever voice we build.

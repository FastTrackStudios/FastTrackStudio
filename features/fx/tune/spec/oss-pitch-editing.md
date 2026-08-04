# OSS pitch-editing research — offline Melodyne-style editor

2026-07-28. Companion to autotune-plan.md. Note: per Cody's recorded
decision (2026-07-12), FTS is GPL + open source, so GPL libs are
linkable — the PORTABLE marks below matter only where dual-licensing
might matter later (plugin suite).

## License table

| Project | License | Verdict |
|---|---|---|
| **melonix** (mika314) | MIT | PORTABLE — the requested starting point |
| **WORLD** (mmorise) | modified BSD | PORTABLE — the mono resynth path |
| Basic Pitch (Spotify) | Apache-2 (code + ONNX) | PORTABLE — polyphonic notes |
| CREPE / RMVPE / FCPE | MIT / Apache-2 / MIT | PORTABLE — neural f0 upgrades |
| **stftPitchShift** (jurihock) | MIT (core) | PORTABLE — phase-vocoder + cepstral formant ("timbre") shifting, small enough to port |
| signalsmith-stretch | MIT | PORTABLE — polyphonic stretch/shift fallback |
| SPTK | Apache-2 | PORTABLE |
| sannawag/TD-PSOLA | MIT | PORTABLE — readable PSOLA reference |
| Praat / pYIN+Tony / aubio / Rubber Band | GPL | linkable; Tony = the note-segmentation (pYIN+HMM) reference; Praat's Manipulation model (audio + pitch tier + duration tier) = the right mental model |
| HachiTune / PitchNet | AGPL | architecture study only — neural vocoder resynth (its submodels RMVPE/FCPE are permissive; vocoder weights CC BY-NC — do not ship) |

## melonix deep-dive (read source; MIT)

NOT a Melodyne clone internally: no pitch detection, no note blobs.
- Display-only lazy spectrogram (32k FFT/column, LRU range cache,
  worker thread) over a piano strip; the USER reads notes.
- Edit model = flat `Marker { sample, note, dTime, pitchBend }` list
  defining two piecewise-linear curves: time-warp + pitch-bend
  (semitones), memoized sample↔time maps invalidated on edit.
- Render = grains ~1500 samples snapped to hysteresis zero-crossings,
  selection walks the warped timeline, each grain linearly resampled
  by 2^(bend/12), spliced at zero crossings. No OLA, no formants.
  Same process() drives realtime scrub AND offline export.
- Take: the marker/warp data model, the lazy spectrogram cache, the
  zero-crossing grain table as the cheap PREVIEW renderer, and the
  pull-based process(cursor) shape. Skip its DSP — tune-dsp is past it.

## Recommended architecture (ours)

1. **Analysis pass** (offline, cached by content hash): YIN → later
   Harvest/CREPE swap; run-segmenter → Tony-style HMM upgrade;
   WORLD analysis (f0/sp/ap) cached once = the resynth source.
2. **Note-blob model** (the Melodyne insight): per note
   `f0(t) = center + drift(t) + modulation(t)` — center is what drag
   edits (keyflow-snapped), drift and vibrato independently scalable
   0..100%. Plus amplitude env, formant shift, and a clip-level
   melonix-style warp-marker map for timing.
3. **Resynthesis tiers**: preview = pitch-dsp PSOLA or melonix grain
   resampler; committed mono = WORLD (f0 rewrite + frame remap =
   pitch/time, formants untouched by construction); polyphonic =
   Basic Pitch notes + DNA masks + signalsmith-stretch per stem.
4. **Project format**: styx doc per clip — media hash, edits stored as
   DELTAS from analysis (re-analysis preserves edits), warp markers.
   Caches regenerable, never in the doc. Renders as proxy/ assets
   (song-as-project colocation).
5. **Crate shape**: tune-dsp gains `model.rs` (blob doc) + `world`
   (FFI first, port later); editor = signal-ui piano-roll surface over
   architect RPC so the browser remote edits too.

Priority: blob model → WORLD render → editing surface w/ warp markers
→ Basic Pitch+DNA polyphony → neural f0 options.

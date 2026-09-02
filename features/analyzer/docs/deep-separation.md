# Deep separation: reference-mix analysis

## What this is for

Point the analyzer at a mix you admire and get back numbers you can mix
against: how loud the kick is relative to the snare, where the kick and
bass trade the low end, what the guitars actually occupy, how far the
lead sits above the backing vocals.

This is **not** a corpus-wide stage. The Billboard corpus gets two stems
(vocals / instrumental) because 14,000 songs of six-way drum separation
is days of GPU for questions nobody has asked yet. Deep separation runs
on a handful of named reference tracks, on demand.

The point is not the separation. It is the **relationships between
stems**, because those are what a mix engineer can act on.

## The cascade

```
mix
 ├─ BS-RoFormer ──────► vocals ──► Mel-Band karaoke ──► lead
 │   (base 4-stem)      │                              └─► backing
 │                      ├──────► drums ──► DrumSep MDX23C ──► kick
 │                      │                                     snare
 │                      │                                     toms
 │                      │                                     hihat
 │                      │                                     ride
 │                      │                                     crash
 │                      ├──────► bass
 │                      └──────► other
 └─ htdemucs_6s ──────► guitar, piano   (parallel, see below)
```

### Model registry

| stage | model | notes |
|---|---|---|
| base 4-stem | `BS-RoFormer` | 11.99 dB SDR, won SDX23. Better than htdemucs for the base split |
| drums → 6 | `aufr33-jarredou_DrumSep_model_mdx23c_ep_141_sdr_10.8059.ckpt` | MDX23C, SDR 10.81. Splits **ride from crash**, which matters: cymbal wash sits in the same 6–12 kHz region as vocal air |
| vocals → lead/backing | Mel-Band RoFormer karaoke | The current best for this split |
| guitar / piano | `htdemucs_6s` | The only widely-deployed option. Guitar is usable; **piano bleeds badly** — treat piano numbers as indicative only |
| synths | — | **No model exists.** Synths land in `other`. A real gap in what can be answered |

All of it runs through
[`python-audio-separator`](https://github.com/nomadkaraoke/python-audio-separator),
which handles model download and covers every architecture above.

`htdemucs_6s` runs on the mix in parallel rather than in the cascade,
because its `other` head is trained differently from BS-RoFormer's and
feeding one into the other compounds two different models' assumptions.

### Why not LarsNet

[LarsNet](https://github.com/polimi-ispl/larsnet) is the academic
option and was the first candidate, but it is trained entirely on
**StemGMD** — MIDI performances rendered through sampled kits. No bleed,
no bus compression, no layered samples, no parallel crush. Real records
are none of those things, and this repo already learned that lesson in
`expression-editor-corpus`: *"bleed between mics is the entire
difficulty — a synthesized fixture has none of it, so passing on one
proves nothing."*

The aufr33/jarredou model is community-trained on real audio, reports a
higher SDR, and gives six stems instead of five. It wins on every axis
that matters here.

## What gets measured

Every stem is measured with what `metrics.rs` already provides, so this
stage adds orchestration rather than new DSP:

- **Level** — integrated loudness, and the same figure relative to the
  full mix. "The kick is −7 LU below the mix" travels between songs;
  raw dBFS does not.
- **Crest factor** ([`crest_db`]) — how hard the stem is compressed.
- **Band spectrum** ([`band_spectrum`]) — the stem's frequency profile,
  in logarithmic hertz bands.
- **Band margin** ([`band_margin`]) — how far one stem stands above
  another, band by band.

### Mapping the questions to measurements

| question | measurement |
|---|---|
| How loud is the kick / snare / vocal? | stem loudness relative to mix |
| Where are the kick / snare / tom fundamentals? | peak band below ~200 Hz in that stem's spectrum |
| Kick vs bass — who wins where? | **band dominance** (below) between kick and bass |
| How loud are the guitars, what do they occupy? | guitar loudness + band spectrum |
| How far is the lead above the backing vocals? | loudness difference, plus band margin |

### Band dominance

The "which one wins in which area" question is not answered by comparing
two average spectra, because averages hide the fact that a kick and a
bass *take turns*. Instead, take short-time band spectra for both stems
and report, per band, the fraction of time each is louder.

That distinguishes the two mixes that matter:

- kick and bass **sharing** 60 Hz, both present, fighting — dominance
  near 50/50 and neither clearly ahead;
- kick and bass **carved apart**, one owning the sub while the other
  owns the upper bass — dominance near 100/0 in each band.

The second is what a good mix usually does, and only a time-resolved
measure shows it.

## Validation, before trusting any of it

Every number here is downstream of two lossy separations, and the
published SDR figures are averages over benchmark material, not over the
records you care about.

**We have ground truth nobody else does: our own multitracks.** The plan
is to take finished FTS mixes where the real stems exist, bounce the
mix, run the full cascade, and compare estimated stems against the
actual ones.

That yields a per-stem error budget on real, processed, bleeding audio,
and answers the only question that matters before building on top:
*which of these measurements can be trusted?* The plausible outcome is
something like "kick and snare level within 1 dB, toms unreliable,
cymbals hopeless" — which changes what is worth building.

Do this first. Absolute numbers from an uncalibrated cascade are
confident and wrong, and that is worse than having no numbers.

### Why relative numbers survive

The reassuring part: separation error partly cancels between two stems
measured the same way. "Kick is 4 dB above bass at 60 Hz" is far more
robust than either stem's absolute level, because whatever the model
smeared into the kick it likely smeared into the bass too.

Since reference matching is inherently relative, the tool should be
built around **ratios, margins and dominance** rather than absolute
loudness. That is a design constraint, not a limitation.

## Implementation notes

### The `separate()` trap

`audio-separator`'s high-level `separate()` returns only a
primary/secondary pair. For a six-output drum model that is useless —
it silently gives two stems where six were expected.

`MDXCSeparator.demix()` has to be called directly to get the per-part
outputs. STEMwerk-reaper hit this and documented it; it is the kind of
thing that costs a day if discovered by accident.

### Everything else that bites

The traps from the `vocal-corpus` skill all still apply, and two matter
especially here:

- **Decode to `ANALYSIS_SAMPLE_RATE` before measuring.** The cascade
  produces stems at whatever rate each model works at, so a deep run
  mixes rates more than the two-stem pipeline ever did.
- **Never trust an exit code.** demucs exits 0 when it cannot decode.
  Confirm each stage by the presence of its outputs, and confirm the
  *count* — six drum stems, not two.

### Storage

Deep stems live alongside the existing two, in the same per-song
directory, with fixed names so a script finds them without the database:

```
by-name/2020/#001 91wk Artist - Title [12108]/
    source.webm
    vocals.opus
    instrumental.opus
    deep/
        lead.opus  backing.opus
        kick.opus  snare.opus  toms.opus
        hihat.opus ride.opus   crash.opus
        bass.opus  guitar.opus piano.opus  other.opus
```

Opus 128k as before. At roughly 4 MB per stem this is ~50 MB per song —
fine for tens of reference tracks, which is all this is for.

## Prior art

[STEMwerk-reaper](https://github.com/flarkflarkflark/STEMwerk-reaper)
(MIT) does the closest thing: REAPER-integrated separation with a drum
kit split, built on `python-audio-separator`. It is where the DrumSep
model choice and the `demix()` workaround came from.

It differs from this design in two ways: its base separation is the
htdemucs family rather than BS-RoFormer, and its karaoke workflow is
vocals/instrumental rather than lead-versus-backing. It also stops at
separation — there is no measurement layer, which is the entire point
here.

## Sources

- [LarsNet](https://github.com/polimi-ispl/larsnet) ·
  [Toward Deep Drum Source Separation](https://arxiv.org/abs/2312.09663) ·
  [StemGMD](https://sdx-workshop.github.io/papers/Mezza.pdf)
- [BS-RoFormer](https://arxiv.org/abs/2309.02612) ·
  [Mel-Band RoFormer](https://arxiv.org/pdf/2310.01809)
- [python-audio-separator](https://github.com/nomadkaraoke/python-audio-separator) ·
  [MSST](https://github.com/ZFTurbo/Music-Source-Separation-Training) ·
  [Demucs](https://github.com/facebookresearch/demucs)
- [STEMwerk-reaper](https://github.com/flarkflarkflark/STEMwerk-reaper)

# Keyflow → REAPER project scaffold — RPP format reference

How a keyflow chart maps onto a REAPER `.RPP` project. This backs two emitters
that share one `ScaffoldPlan`:

- **Live** — `session::keyflow_scaffold` REAPER action, applies the plan via the
  `daw` service traits (`Tracks`, `Regions`, `Markers`). Cannot write key
  signatures (no REAPER API).
- **Offline** — CLI (`kf scaffold <chart> -o out.rpp`), serialises the plan to
  `.RPP` text via `dawfile_reaper::rpp_tree::{RChunk, write_rpp}` and therefore
  **can** emit the `<KEYSIG>` block and chord notation the live path can't.

Reference project (the target shape): `~/Downloads/Key Signatures in Reaper/`.
The full RPP model to build on lives in the legacy repo
`../FastTrackStudio-legacy/reaper-file/crates/reaper-project` (project / track /
item / marker_region serialisation); the in-tree serialiser is
`features/dawfile/dawfile-reaper/src/rpp_tree.rs` (`write_rpp(path, &RChunk)`).

## Top-level structure

```
<REAPER_PROJECT 0.1 "7.75/linux-x86_64" <unixtime> 2
  … header fields (see below) …
  <PROJBAY
  >
  <KEYSIG               ← project key signatures (per measure)
    …
  >
  <TRACK {guid}         ← Keyflow folder (ISBUS 1 1)
    NAME Keyflow
    ISBUS 1 1
  >
  <TRACK {guid}         ← KEY   (ISBUS 0 0), holds the KEY-MIDI item
  <TRACK {guid}         ← CHORD (chords as notation items)
  <TRACK {guid}         ← MELODY
  <TRACK {guid}         ← SCALE (ISBUS 2 -1 → last child closes the folder)
  <EXTENSIONS
  >
>
MARKER <id> <pos> <name> …    ← markers + regions are top-level, after the chunk
```

### Minimal header

Reproduce the reference header verbatim (constants), varying only the unix
timestamp in the `<REAPER_PROJECT` line and `SAMPLERATE`/`TEMPO`. Load-bearing
fields observed: `RIPPLE`, `AUTOXFADE`, `GRID`, `TIMEMODE`, `PANMODE 3`,
`CURSOR`, `ZOOM`, `RECORD_PATH "Media" ""`, `<RECORD_CFG>`/`<RENDER_CFG>` (base64
blobs), `SAMPLERATE 48000 1 1`, `TEMPO <bpm> <num> <den>`. Everything else can
be REAPER defaults. A round-trip test (write → open in REAPER → save → diff) is
the acceptance gate.

## `<KEYSIG>` block — project key signatures  (reverse-engineered)

One line per key change:

```
<KEYSIG
  <measure> <root> <accidental> <scalemask>
  0  0  1 0xAB5
  8  1 -1 0xAB5      # measure 8 → Db major
  56 8 -1 0xAB5      # measure 56 → Ab major
>
```

| field | meaning |
| --- | --- |
| `measure` | 0-based measure index where the key takes effect |
| `root` | pitch class 0–11 (0=C, 1=C♯/D♭, … 11=B) |
| `accidental` | spelling preference: `1` = sharp-ward, `-1` = flat-ward (so root 1 + `-1` renders as D♭, root 1 + `1` as C♯) |
| `scalemask` | 12-bit chromatic scale mask, LSB = C. **`0xAB5` = major** (C D E F G A B). Minor / modes = their own masks (derive by rotating the mask). |

Mapping from keyflow: each `Chart.key_changes[i]` (and `initial_key`) →
`(measure_of_change, pitch_class(key.root), sharp_or_flat(key), mask(key.mode))`.
Measure index comes from the section layout (`chart_import` already lays sections
on measures/seconds).

## Folder + tracks

Folder nesting is encoded on each `<TRACK>` via `ISBUS <isbus> <compact>`:

- Folder **open** (parent): `ISBUS 1 1`.
- Normal child: `ISBUS 0 0`.
- Folder **close** (last child): `ISBUS 2 -1`.

So the Keyflow folder = one `ISBUS 1 1` track named `Keyflow`, then `KEY`,
`CHORD`, `MELODY` as `ISBUS 0 0`, then `SCALE` as `ISBUS 2 -1`. `NAME <name>`,
`TRACKID {guid}`, `PEAKCOL <bgr>` for colour. (The live path does the same via
`Tracks::add` + `set_folder_depth(1 / 0 / -1)`.)

## Markers & regions (song sections)

Top-level lines after the project chunk. A **marker** is a single line; a
**region** is a start line + an end line sharing a negated id:

```
MARKER 1 <start_sec> "VS" 0 <bgr|0x1000000> 1 R { <guid> }   ← region start
MARKER 1 <end_sec>   "" 0 0 1 R { <guid> }                   ← region end (name empty)
```

Section names use `SectionType::abbreviation()` ("IN","VS","CH","BR","OUT",…);
colours via `section_type_color(kind)`. The live path already does this through
`Regions::add` + `normalize_section_regions`; offline emits the `MARKER` lines
directly from the same `ScaffoldPlan.regions`.

## Chords & melody as items

Chords render as **REAPER notation/text events** inside a MIDI item on the CHORD
track — base64-encoded meta events of the form `FF 0F TRAC …`:

```
<X <ppq> 0 <flags> -1
  <base64 of "\xff\x0fTRAC text \"Cmaj7\"">
>
```

(Decoded examples from the reference: `TRAC text "Something Here"`,
`TRAC dynamic mf`.) Chord symbols → `TRAC text "<symbol>"` at the chord's PPQ
position; melody → ordinary MIDI note events (`E <ppq> 9<ch> <pitch> <vel>` /
note-off). PPQ = 960 per quarter (`HASDATA 1 960 QN`).

## Build order

1. `ScaffoldPlan` (backend-agnostic): `folder`, `tracks[]`, `regions[]`,
   `markers[]`, `keysigs[]`, `chords[]`, `melody[]` — derived from
   `keyflow::parse_text_chart` + `chart_import::chart_to_layout`.
2. Refactor the live action onto `ScaffoldPlan` (apply subset REAPER supports).
3. Offline emitter: `ScaffoldPlan → RChunk → write_rpp`. Header from this doc,
   then KEYSIG, folder+tracks, MARKER lines, chord notation items.
4. `kf scaffold` CLI command.
5. REAPER round-trip test on the generated file.

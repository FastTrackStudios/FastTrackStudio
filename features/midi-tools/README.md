# midi-tools

MIDI note manipulation — notes in, edits out.

Two tools so far, both ported from
[mrtnz's ReaperScripts](https://github.com/ErrrikMarrrtinez/ReaperScripts)
off Lua + rtk and onto the `daw` API:

- **velocity shaping** — `mrtnz_MVelocity.lua`
- **arpeggiator** — `mrtnz_Arpeggiator(chord to arp).lua`
  ([forum thread](https://forums.cockos.com/showthread.php?t=283743))

## Crates

| crate            | depends on            | what it is                                      |
|------------------|-----------------------|-------------------------------------------------|
| `midi-tools`     | `rand`, `tracing`     | the arithmetic. No DAW, no UI, no async.        |
| `midi-tools-ui`  | `midi-tools`, dioxus  | the panel. Runs standalone or as a REAPER panel.|
| `midi-tools-daw` | `midi-tools`, `daw`   | reads/writes notes through `daw::service::Midi`.|

`midi-tools-ui` and `midi-tools-daw` are siblings — neither depends on
the other. They meet at `midi_tools::VelocitySink`, which lives in the
pure crate so a DAW backend never links Dioxus.

## The arpeggiator

`arp::group_chords` folds a take's notes into `Chord`s **by onset**, then
`Arp::arpeggiate` walks each chord from its own start to its own end,
picking pitches with a `Direction` cursor (up / down / up-down / down-up /
random). Controls: rate, gate, octave range, ratchet, and a cycling step
grid where each step carries its own rate, velocity, octave, ratchet and
gate.

Grouping by onset rather than by "has the previous chord ended" is
load-bearing: a block-chord progression holds each chord right up to the
next, so an end-based rule reads Am→F as one eight-note cluster. The
REAPER integration test caught exactly that.

## The four velocity engines

| module      | MVelocity section | what it does                            |
|-------------|-------------------|-----------------------------------------|
| `curve`     | the Bézier widget | draws a velocity ramp across the span   |
| `pattern`   | STEP VELOCITY     | cycles an N-slot accent pattern         |
| `randomize` | RANGE + UPDATE    | rolls a per-note target, then blends in |
| `dynamics`  | COMPRESS / EXPAND | pulls toward / pushes away from a pivot |

`velocity::Session` composes them over a held baseline:

```text
baseline ─→ curve ─→ pattern ─→ randomize ─→ dynamics ─→ edits
            shape    accent     humanize     glue
```

Every control is a **parameter, not an action**: the result is recomputed
from the baseline on every change, so returning a slider to neutral
restores exactly what you started with, and `Session::edits()` reports
only the notes that actually moved.

## Running it

```sh
# The panel, standalone, on a synthetic take (no DAW needed).
cargo run -p midi-tools-ui --example panel

# The arithmetic.
cargo test -p midi-tools

# The arpeggiator, standalone.
cargo run -p midi-tools-ui --example arp

# The write paths, against live headless REAPER.
just reaper daw-test velocity_
just reaper daw-test arp_
```

## Deliberate divergences from the Lua

Behaviour changes, not refactors — each fixes something the original gets
wrong, and each is pinned by a test.

### MArpeggiator

Three of these are outright bugs:

1. **Every arp note was emitted twice.** The multi-note branch inserts the
   gated note and *then* unconditionally inserts it again at full length,
   so every step lands as a stacked pair. It's why gate appears not to
   work upstream.
2. **Ratchet duplicated its remainder.** `splitNote` puts the
   leftover-tail insert inside the per-ratchet loop, emitting it `ratchet`
   times at one position.
3. **Chord sorting read a nil.** `gatherChords` sorts with `current_mode`,
   a global still nil at gather time (the real one is a local declared
   later inside `insertArpeggios`), so chords always sorted descending
   regardless of direction.

And two design changes:

4. **Steps cycle.** Upstream scans its step list in reverse and takes the
   first whose counter divides by its `step` field. Every step ships with
   `step = 1`, which the *last* entry matches every time — so in the
   default configuration the other three never fire.
5. **Octaves are a range, not a dice roll.** Upstream shifts each note by
   `math.random(-octave, octave) * 12`, so the same settings give a
   different part every run. `Arp::octaves` is the standard control: the
   pitch pool repeats an octave up.

### MVelocity

6. **The curve's height is the velocity.** Upstream maps the curve twice
   (endpoint heights become a velocity range, then `y(t)` blends between
   them), which is only exact when the curve is anchored at 0 and 1 — as
   all ten shipped presets are. Draw a curve from 0.5 to 0.8 and the
   first note comes out at 82 instead of the 63 the widget is showing.
   The double mapping is also why upstream needs its auto-`invert` hack.
   Here `velocity(t) = y(t) * 127`, and inverting is something you ask
   for.

7. **Compress and expand are labelled for what they do.** Upstream draws
   "Expand" above the slider midpoint but runs `base + (target - base)*t`
   there, which moves notes *toward* the target — that's compression.

8. **FACTOR does something.** Upstream draws a FACTOR button next to
   TARGET and toggles a border on it, but no code path reads the mode.
   Here it's `Pivot::Mean`: compress toward the selection's own average
   rather than toward a number you had to guess.

Two smaller ones: results round rather than floor (flooring biases every
engine half a step low), and the baseline is owned in exactly one place
(upstream re-snapshots it inside two slider handlers, so "back to
neutral" depends on which section you touched last).

## What this needed from the tree

**`daw-reaper`'s `Midi` mutation half was all stubs.** `add_notes` worked
— which is why chord-tool passed and the gap went unnoticed — but every
setter and every delete was a `readonly_warn` no-op. Implemented now:

- `set_note_*` over `MIDI_SetNote` (`safe_wrappers::midi::set_note`, with
  per-field `Option`s for REAPER's null-means-don't-touch convention).
- `delete_note{,s}` and `delete_selected_notes` over `MIDI_DeleteNote`,
  deleting highest-index-first because removal renumbers everything above
  it.

Still stubs: `transpose_notes`, `quantize_notes`, `humanize_notes`,
`select_all_notes`.

**`Midi::add_notes` does not round-trip with `Midi::notes`.** `notes()`
returns raw take PPQ; `add_notes` reinterprets `start_ppq` as a project
*quarter-note* position. Anything echoing back what it read lands 960×
off. Rather than change existing semantics (guide_track and chord-tool
depend on them), there's now an explicit `Midi::add_notes_ppq` for the
raw-PPQ case, implemented on both backends. The arp uses it.

## UI notes

The sliders wrap `dioxus_primitives::slider` rather than hand-rolling
pointer arithmetic. The first cut hand-rolled it and flickered, for two
reasons worth remembering: a slider that feeds its *clamped* value back in
as its displayed value oscillates when the pointer sits between steps, and
one that measures its track only at mount drifts as soon as the panel
scrolls. The primitive keeps unclamped granular thumb state through a
drag, re-measures on `pointerdown` and on resize, and picks the active
thumb once at drag start (which is what made the two-thumb range slider
swap its thumbs mid-drag).

The two hand-rolled widgets that no primitive covers — the step-velocity
bar editor and the curve editor — take the same two lessons: re-measure at
the start of each gesture, and never cancel a drag on `pointerleave`.

Styling stays local and inline rather than using `fts_ui::components::
Slider`, which is the same primitive dressed in Tailwind and collapses
without it. Colours are `var(--token, fallback)` throughout. Note that
`--secondary` and `--muted` are the *same value* in the fts dark theme, so
data marks must not use one against the other.

## Not done yet

The REAPER action + panel registration in
`apps/extensions/reaper-fts-extensions`. The crates and both standalone
panels are complete; wiring the cdylib is the next step, and
`chord-tool` is in the same state.

## Reference checkout

The upstream scripts are cloned (gitignored) at `.reference/ReaperScripts`
for comparison while porting. Other tools in that repo worth porting land
here as sibling modules — an arpeggiator, a note splitter, a chopper are
all "notes in, notes out".

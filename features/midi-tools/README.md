# midi-tools

MIDI note manipulation — notes in, edits out.

The first tool here is **velocity shaping**, a port of
[mrtnz's MVelocity](https://github.com/ErrrikMarrrtinez/ReaperScripts)
(`MIDI Editor/mrtnz_MVelocity.lua`) off Lua + rtk and onto the `daw` API.

## Crates

| crate            | depends on            | what it is                                      |
|------------------|-----------------------|-------------------------------------------------|
| `midi-tools`     | `rand`, `tracing`     | the arithmetic. No DAW, no UI, no async.        |
| `midi-tools-ui`  | `midi-tools`, dioxus  | the panel. Runs standalone or as a REAPER panel.|
| `midi-tools-daw` | `midi-tools`, `daw`   | reads/writes notes through `daw::service::Midi`.|

`midi-tools-ui` and `midi-tools-daw` are siblings — neither depends on
the other. They meet at `midi_tools::VelocitySink`, which lives in the
pure crate so a DAW backend never links Dioxus.

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

# The write path, against live headless REAPER.
just reaper daw-test velocity_
```

## Deliberate divergences from the Lua

These are behaviour changes, not refactors — each fixes something the
original gets wrong. All three are pinned by unit tests.

1. **The curve's height is the velocity.** Upstream maps the curve twice
   (endpoint heights become a velocity range, then `y(t)` blends between
   them), which is only exact when the curve is anchored at 0 and 1 — as
   all ten shipped presets are. Draw a curve from 0.5 to 0.8 and the
   first note comes out at 82 instead of the 63 the widget is showing.
   The double mapping is also why upstream needs its auto-`invert` hack.
   Here `velocity(t) = y(t) * 127`, and inverting is something you ask
   for.

2. **Compress and expand are labelled for what they do.** Upstream draws
   "Expand" above the slider midpoint but runs `base + (target - base)*t`
   there, which moves notes *toward* the target — that's compression.

3. **FACTOR does something.** Upstream draws a FACTOR button next to
   TARGET and toggles a border on it, but no code path reads the mode.
   Here it's `Pivot::Mean`: compress toward the selection's own average
   rather than toward a number you had to guess.

Two smaller ones: results round rather than floor (flooring biases every
engine half a step low), and the baseline is owned in exactly one place
(upstream re-snapshots it inside two slider handlers, so "back to
neutral" depends on which section you touched last).

## What this needed from the tree

`daw-reaper`'s `Midi` note *setters* were all `readonly_warn` stubs —
`add_notes` worked, but `set_note_velocity` and friends silently did
nothing. They're implemented now over `MIDI_SetNote`
(`safe_wrappers::midi::set_note`, with per-field `Option`s for REAPER's
null-means-don't-touch convention).

## Not done yet

The REAPER action + panel registration in
`apps/extensions/reaper-fts-extensions`. The crates and the standalone
panel are complete; wiring the cdylib is the next step, and
`chord-tool` is in the same state.

## Reference checkout

The upstream scripts are cloned (gitignored) at `.reference/ReaperScripts`
for comparison while porting. Other tools in that repo worth porting land
here as sibling modules — an arpeggiator, a note splitter, a chopper are
all "notes in, notes out".

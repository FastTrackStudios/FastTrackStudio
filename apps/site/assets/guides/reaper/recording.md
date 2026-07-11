---
title: Recording
kind: input
type: input
mode: mode-record
---

# Recording

Record mode turns the number row into a take-ranking pad for comping while you track. Ranks drop a marker on the take so the comp pass later is just picking the smiley faces.

Activate the Record mode workflow to use these bindings — they layer over the base profile (see [[Input System|modes]]) and step aside when you leave the mode.

## Rank takes as they happen

While a take plays back, tap a number to rank it — the marker lands two seconds behind the play position, right where the phrase you just heard lives:

- `1` — Rank :) at the play position.
- `2` — Rank :)) at the play position.
- `3` — Rank :))) at the play position.
- `0` — Down-rank at the play position.

Hold Shift to rank the whole take instead of a moment — the marker sits at the item start:

- `<S-1>` — Rank :) item-wide (likewise `<S-2>`, `<S-3>`, `<S-0>`).

Point, don't select — rank the take under the mouse cursor:

- `f` — Favorite the take at the mouse.
- `b` — Down-rank the take at the mouse.

## Tracking controls

- `r` — Record (the base transport binding, unchanged).
- `<A-r>` — Toggle record-arm on the selected tracks.
- `i` — Toggle input monitoring on/off.
- `<S-i>` — Switch monitoring auto/tape and off.
- `p` — Toggle pre-roll on record.
- `e` — Restart recording: delete the bad take and roll again in one press.

The base [[Transport]] keys keep working underneath — `<space>` still stops, `,` and `.` still hop markers.

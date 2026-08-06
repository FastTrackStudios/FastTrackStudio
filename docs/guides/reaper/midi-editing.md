---
title: MIDI Editing
kind: input
type: input
category: editing
---

# MIDI Editing

The piano roll is the one place in FastTrackStudio where the mouse leads and the keyboard follows. Drawing notes is a pointer gesture; everything you do to notes *after* they exist is a key. `kbd:@40153` opens the selected item in the MIDI editor, and the same key closes it from inside.

## Reading the snap chip

Gestures that put something *at a position* carry a chip saying where it lands:

- `snap:grid` — the gesture snaps, so what you get depends on the current grid division. Set it with the `g` menu inside the editor: `kbd:@40197` for eighths, `kbd:@40192` for sixteenths.
- `snap:off` — the gesture ignores snapping and lands wherever the pointer is.

No chip means snapping doesn't enter into it — selecting, erasing, and scrubbing act on whatever is under the pointer regardless of the grid.

## The modifier map

Three modifiers, and each one means the same thing everywhere in the piano roll:

- **nothing** — select.
- **Ctrl** — draw.
- **Alt** — the preview-and-destroy half: scrub, erase, paint.

Shift is the qualifier that rides on top: it *adds* to a selection, and it *constrains* a draw.

### Selecting

- `mouse: ldrag` — marquee-select notes. The default drag does the thing you do most.
- `mouse:<S-> ldrag` — marquee **add** to the note selection, leaving what's already selected alone.

### Drawing

Hold Ctrl and the piano roll becomes a pencil — there is no separate tool to switch into and forget about.

- `mouse:<C-> lclick` `snap:grid` — insert a note at the grid slot under the pointer, one grid unit long.
- `mouse:<C-> ldrag` `snap:grid` — insert a note and keep dragging it. Drag right to set its length, up or down to change its pitch. The note isn't committed until you let go, so a wrong landing is a matter of keeping the button down and moving.
- `mouse:<S-C-> ldrag` `snap:grid` — the same gesture with pitch locked: drag only sets length. Use it when you're laying a run of notes on one pitch and don't want a stray vertical wobble to transpose them mid-drag.
- `mouse:<S-A-> ldrag` `snap:grid` — paint a straight line of notes: one drag lays a whole run at once, every note on the grid. The fast way to build a hi-hat pattern or a rolled arpeggio.

```gif
midi-editing-draw-notes
Ctrl+drag places a note and keeps it under the pointer — right for length, up/down for pitch.
- Let go to commit; the note lands on the grid
- `mouse:<S-C-> ldrag` is the length-only variant `snap:grid`
- Change the grid first with `kbd:@40192` and the same drag draws sixteenths
```

Because a click is just a drag that never moved, both Ctrl chords insert on release even if the pointer never left the spot.

### Erasing and auditioning

- `mouse:<C-A-> ldrag` — erase notes. Ctrl still means "the note tool"; adding Alt turns the pencil around, so you rub notes out by dragging across them.
- `mouse:<A-> ldrag` — scrub-preview the MIDI under the pointer. Drag across a passage to hear it at the speed you move, which is how you find the wrong note in a chord without soloing anything.

## Editing notes that already exist

Once notes are on the grid, the keyboard is faster than the mouse.

- `kbd:@40003` — select all notes. `kbd:@40214` clears the selection.
- `kbd:@40002` — delete the selected notes.
- `kbd:@40046` — split the selected notes at the cursor.
- `kbd:@40051` — insert a note at the edit cursor, no mouse at all.

### Length and position

- `kbd:@40446` / `kbd:@40447` — lengthen and shorten the selection by one grid unit.
- `kbd:@40184` / `kbd:@40183` — nudge the selection right and left by one grid unit.
- `kbd:@40469` — quantize note positions to the grid.

### Pitch and velocity

- `kbd:@40177` / `kbd:@40178` — transpose the selection by a semitone.
- `kbd:@40179` / `kbd:@40180` — transpose by an octave.
- `kbd:@40462` / `kbd:@40464` — velocity up and down by one.

```gif right
midi-editing-note-length
Length is a grid-unit verb, not a drag: select, then lengthen or shorten.
- `kbd:@40446` and `kbd:@40447` step by the current grid
- Change the grid mid-edit and the step size follows
- `kbd:@40469` snaps positions back to the grid afterwards
```

## Where the modifiers live

These gestures are not REAPER defaults — they come from the fasttrackstudio mouse profile, which the input layer applies to REAPER's mouse-modifier table on load. The MIDI piano roll rows behind this page are:

| Context | Modifier | REAPER behavior |
| --- | --- | --- |
| MIDI piano roll click | Ctrl | Insert note |
| MIDI piano roll click | Ctrl+Shift | Insert note |
| MIDI piano roll drag | *(none)* | Marquee select notes |
| MIDI piano roll drag | Shift | Marquee add to note selection |
| MIDI piano roll drag | Ctrl | Insert note, drag to extend or change pitch |
| MIDI piano roll drag | Ctrl+Shift | Insert note, drag to extend |
| MIDI piano roll drag | Alt | Scrub preview MIDI |
| MIDI piano roll drag | Shift+Alt | Paint a straight line of notes |
| MIDI piano roll drag | Ctrl+Alt | Erase notes |

Every behavior REAPER offers has a snapping twin ("… ignoring snap"). FastTrackStudio picks the snapping variant everywhere by default — the grid is the thing you change, not the gesture. See [[Input System|the input layer]] for how profiles and overlays compose.

Next: mark the arrangement up in [[markers-regions|Markers & regions]], or go back to audio items in [[editing|Editing]].

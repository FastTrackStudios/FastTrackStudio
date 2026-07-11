---
title: Tracks
kind: input
type: input
category: tracks
---

# Tracks

Track work is where the which-key layer shines: one plain chord for the raw insert, and two prefix menus — the track manager and the create-track family. (New to prefix menus? Read [[Input System|the input layer]] first.)

## The raw insert

- `<C-t>` — Insert a new empty track below the selection, exactly like stock REAPER.

## The track manager (`n` menu)

Press `n` to open the Track Manager menu. The overlay lists every follow-up key — each letter is mnemonic for the entity being added.

- `n n` — New blank track.
- `n d` — Duplicate the selected tracks.
- `n a` — Add an arrangement.
- `n c` — Add a channel.
- `n l` — Add a layer.
- `n m` — Add a multi-mic group.
- `n p` — Add a performer.

## Create categorized tracks (`<S-n>` menu)

Press `<S-n>` to create fully-configured session tracks — named, routed, and colored for their role. Keep Shift held and tap letters to create several in a row.

- `<S-n> d` — Drum kit.
- `<S-n> l` — Lead vocals.
- `<S-n> g` — Electric guitar.
- `<S-n> b` — Bass guitar.
- `<S-n> p` — Piano.

Branches nest: `<S-n> s` opens a synth submenu — arp, bass, lead, pad:

- `<S-n> s a` — Synth arp.

Next: get a take down with [[Transport]] and comp it in [[Recording]].

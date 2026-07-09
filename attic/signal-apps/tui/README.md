# tui-player

Kontakt-style terminal browser/player for `.signalpack` libraries. For
fast iteration while we build out the bigger engine.

## Run

```sh
nix develop --command cargo run -p tui-player -- <root-dir>
```

`<root-dir>` is any directory; the player walks it recursively looking
for folders that contain `library.signalpack` (or `library.styx`) and
treats each one as a patch. Parent directories form the library tree.

Example:
```sh
cargo run -p tui-player -- "/run/media/AudioHaven/Sampled/Drum Kits/Stylus RMX-fresh/"
```

## Keys

- `Tab` / `Shift+Tab` — cycle focus between Library / Patches panes
- `Up` / `Down` — move selection
- `Enter` — load highlighted patch
- `[` / `]` — shift octave down / up
- `q` / `Esc` — quit
- Tracker keyboard layout (current octave starts at C):
  - White keys: `z x c v b n m , .  /`  → C D E F G A B C D E
  - Black keys: `s d  g h j  l ;`       → C# D#  F# G# A#  C# D#
  - Upper octave white: `q w e r t y u i o p`
  - Upper octave black: `2 3  5 6 7  9 0`

## Status

Bottom status line shows: focused pane, selected patch, current octave,
voice count, last note, errors.

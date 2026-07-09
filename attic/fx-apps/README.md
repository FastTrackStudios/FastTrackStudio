# attic/fx-apps — parked FX plugin apps

Plugin-binary shells whose source has drifted from the current in-tree
library APIs. Not workspace members; reviving one means updating it to
the current API (and adding it back to root `members`).

| dir | what it was |
|---|---|
| `meter-plugin/` | FTS Meter — CLAP/VST3 metering plugin from the dissolved `Plugins/FTS-Audiocore` workspace. Its libraries (`meter-dsp`, `meter-ui`, `audiocore-*`) live on as root-workspace members under `crates/audiocore/`; the plugin shell itself no longer compiles against the current `audiocore-gui` widget API (`Dropdown` lost its `items` prop, import drift). |

Related: the eq/comp/reverb/delay/pitch/modulation plugin shells under
`features/fx/*/apps/` are likewise NOT workspace members (they reference
pre-move `../../crates/*` paths); they stayed beside their DSP crates
because those dirs moved wholesale in wave 4.

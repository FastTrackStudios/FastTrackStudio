# attic/signal-apps — parked signal apps (wave 4)

Parked when the `signal/` domain workspace dissolved into the root
workspace. None of these are workspace members; they reference the old
signal-workspace dep table and will need manifest surgery to revive.

| dir | what it was |
|---|---|
| `desktop/` | `signal-desktop` — Blitz/Dioxus standalone desktop app (nice-plug-dioxus `open_standalone_with_state`). Superseded by the detachable-GUI architecture: headless `apps/rigd` + remotes (`apps/signal-web`). Its `input.css`/`fts-theme.css` (the Tailwind build inputs) moved to `apps/signal-web/`. |
| `mobile/` | `signal-mobile` — Dioxus mobile shell experiment. |
| `native/` | `signal-native` — early native shell (pre-rigd). |
| `tui/` | `signal-tui` — terminal UI experiment. |
| `cli/` | `signal-cli` — CLI for the signal domain (lib + bin). |
| `amp/` | `signal-amp` — standalone neural-amp-modeler window (`just amp`). |
| `xtask/` | signal workspace xtask (nice-plug-xtask bundler + daw test runner). `bundler.toml` (CLAP/VST3 bundle names for signal-reaper-controller / signal-sampler-clap) parked alongside it. |

The live rig is `cargo build -p signal-rigd` from the repo root; remotes
connect over vox WebSocket (ws://:4040/vox).

# Target Layout — ONE workspace

One root Cargo workspace, one flake.nix, one lockfile, one target/.
Top level: `apps/` `crates/` `features/` `libs/`. Domain dirs
(signal/, daw/, session/…) dissolve into this shape. Every intra-repo
dep is declared ONCE in root `[workspace.dependencies]` (path) and
consumed as `x.workspace = true`.

## crates/ — domain cores (facade + proto + core logic)

| new | from |
|---|---|
| crates/daw/{daw,proto,control,module} | daw/crates/{daw,daw-proto,daw-control,daw-module} |
| crates/audiocore/* | Plugins/FTS-Audiocore/crates/* (audiocore-core, audiocore-dsp, audiocore-gui, fts-modulation) |
| crates/midicore/* | midicore/{crates,features}/* (midicore, midicore-proto, midicore-midir) |
| crates/session/{session,proto,ui} | session/crates/* |
| crates/signal/* | signal/crates/signal + signal/features/signal/* (signal-proto, signal-ui, signal-live, signal-storage, signal-controller, signal-import, signal-browser, signal-grid, signal-grid-ui, signal-daw-bridge) |
| crates/keyflow/* | keyflow/crates/{keyflow,keyflow-proto?,keyflow-text,keyflow-syntax,keyflow-chordpro,keyflow-live,keyflow-lsp,keyflow-daw-analysis,keyflow-orchestra,keyflow-ui} |
| crates/input/* | input_actions/crates/* (actions-proto, actions-registry, actions-keybindings, actions-standalone, input, input-dioxus; actions-reaper → features/reaper) |

## features/ — capabilities (cross-domain, feature-gated at root)

| new | from |
|---|---|
| features/audio/* | daw/features/audio/* (daw-audio-graph, daw-audio-io, daw-midi-io, daw-allocator, fts-audio-proto, audio-controls) |
| features/sync/* | daw/features/sync/* (daw-synchronization, daw-link, daw-network, daw-audio-sync) |
| features/dawfile/* | daw/features/backends/{protools,ableton,logic,aaf,dawproject}/dawfile-* + reaper/dawfile-reaper; plus dawfile-standalone, the native `.daw` format (new, #156) |
| features/reaper/* | daw/features/backends/reaper/* (daw-reaper, daw-bridge, daw-extension-runtime, daw-reaper-dioxus*, daw-reaper-embed, reaper-launcher) + daw/crates/reaper-config + signal/features/reaper/* + input_actions/crates/actions-reaper |
| features/standalone/ | daw/features/standalone/daw-standalone |
| features/surfaces/ | daw/features/surfaces/daw-csi |
| features/engraver/* | keyflow/crates/{engraver,engraver-proto} |
| features/guide/ | session/features/guide (session-guide) |
| features/nam/ | signal/features/nam (nam-manager) |
| features/plugin-host/ | signal/features/plugin-host |
| features/sampler/* | signal/features/sampler/* |
| features/rigs/* | signal/features/rigs/* (guitar+proto+ui, keys, drums, orchestra, synth) |
| features/fx/* | signal/features/fx/* (eq, reverb, delay, modulation, pitch, signal-fx + their dsp/ui/profiles/apps) + signal/features/macromod |
| features/daw-ui/* | daw/features/ui/* (daw-ui, daw-theme-reaper) |

## libs/ — UI + infrastructure libraries

| new | from |
|---|---|
| libs/architect/* | subtree import of FastTrackStudios/architect ({architect,macros/*,atom,form,auth/*,crdt/*}; examples/xtask dropped) |
| libs/fts-ui/* | fts-ui/crates/{fts-ui,fts-ui-audio,showcase} |
| libs/fts-story/* | fts-story/crates/* |
| libs/dock/* | dock-dioxus/crates/{dock-dioxus,dock-proto} |
| libs/nice-plug/* | FTS-Plugins/forks/fts-plug/crates/* |
| libs/utils/ | FastTrackStudio/crates/shared/utils |
| libs/vox-discover/ | FastTrackStudio/crates/shared/vox-discover |
| libs/installer-core/ | FastTrackStudio/crates/shared/installer-core |
| libs/neural-amp-modeler/ | neural-amp-modeler-rs (vendored C++ core stays inside) |
| libs/devtools/ | daw/crates/{fts-devtools,daw-test-macro} |
| libs/moire-trace-capture/ | daw/vendor/moire-trace-capture |

## apps/

| new | from |
|---|---|
| apps/fasttrackstudio | (already there) — THE app |
| (dissolved) apps/signal-engine | signal/apps/rigd (was `signal-rigd`) → folded into apps/fasttrackstudio (`fasttrackstudio --engine`, src/engine_main.rs) |
| (deleted) apps/signal-web | signal/apps/web → superseded by the fasttrackstudio web build (`dx build --platform web --no-default-features --features signal`) |
| apps/fasttrackstudio/cli | NEW — the one `fts` CLI (mounts daw::cli + keyflow-cli, manages engines) |
| (dissolved) apps/daw-cli | folded into crates/daw/daw as `daw::cli` behind the `cli` feature (thin `daw` bin kept) |
| (moved) apps/keyflow-cli | crates/keyflow/cli — lib-first (`keyflow_cli::cli_main`), thin `kf` bin kept |
| apps/installer | FastTrackStudio/apps/installer — rebuilt as `fts-installer`, a thin clap CLI that downloads/installs codeberg releases (old Dioxus wizard in git history) |
| apps/site | fasttrackstudio-site repo (subtree import; package renamed fts-site) — fasttrackstudio.app |
| apps/docs-site | unified docs site (dodeca + kf docs; NOT a cargo member) — docs.fasttrackstudio.app; keyflow docs content moved here from crates/keyflow/docs |
| (park/delete) | signal/apps/{desktop,mobile,native,tui,cli,amp}, session/apps/*, fts-ui/apps/*, daw/apps/{daw/native,example-*}, FastTrackStudio/apps/keyflow-playground — audit per-app: keep only what runs |

## Root workspace rules

- Root Cargo.toml: `[workspace]` members = every crate above;
  `[workspace.dependencies]` = merged dep table (one version per dep —
  the rc.5 fleet pins) + every internal crate as `{ path = … }`.
- Member manifests use `x.workspace = true` for EVERYTHING shared.
- Old domain workspace Cargo.tomls are deleted as their members move.
- architect lives in-tree at `libs/architect/` (subtree import, history
  preserved): `{architect,macros/*,atom,form,auth/*,crdt/*}` are root
  members; plain path deps in `[workspace.dependencies]`, no `[patch]`.
- Keep per-crate feature gates so cold builds stay lean (reaper,
  standalone, audio, tts, wasm UI, etc.).

## Dedup queue (after the move settles)

- keyflow-daw-analysis's daw types → daw-proto only
- audio-controls (vendored) → fold into features/daw-ui or delete after
  signal-ui migrates off it
- signal-audio remnants, duplicate wav/resampler helpers → libs/utils
  or audiocore-dsp
- ~~three CLIs → one `fts` CLI in apps/ (subcommands)~~ — done: apps/fasttrackstudio/cli

# FastTrackStudio

The shell repo: the website, the docs site, the installer, the unified
`fts` CLI, and the release machinery that ships the products.

The code moved out in August 2026. What used to be one 238-crate
monorepo is now:

| repo | holds |
|---|---|
| [daw](https://github.com/FastTrackStudios/daw) | the DAW platform + shared audio/MIDI/input substrate |
| [session](https://github.com/FastTrackStudios/session) | setlists, songs, keyflow, notation, the guide, the Session app |
| [signal](https://github.com/FastTrackStudios/signal) | the audio engine, the rigs, the sampler, the plugins, the Signal app |
| [Ignition](https://github.com/FastTrackStudios/Ignition) | the Bevy visuals engine |
| [patchbay](https://github.com/FastTrackStudios/patchbay) | PipeWire studio routing |
| [architect](https://github.com/FastTrackStudios/architect) | the framework (entity/RPC, atom, form, auth, permissions, crdt) |
| [task](https://github.com/FastTrackStudios/task) | the Task product + the Editor stack |
| [vendor](https://github.com/FastTrackStudios/vendor) | the phon / styx forks |
| [music-convention](https://github.com/FastTrackStudios/music-convention) | monarchy, music-catalog, color-palette |

Layer order is `daw -> session -> signal`, with cross-repo dependencies
pinned to tags. Note that this is the reverse of the *runtime* order:
Session coordinates Signal and Ignition over WebSocket while sitting
below Signal in the dependency graph. See `docs/split/PLAN.md`.

## Layout

```
apps/site/       fasttrackstudio.app (dioxus web)
apps/docs-site/  docs.fasttrackstudio.app
apps/installer/  the installer
apps/fasttrackstudio/cli/   the unified `fts` CLI — the one place
                 allowed to depend on daw, session and signal at once
apps/extensions/ the aggregate REAPER extension cdylib
```

## Licence

GPL-3.0-or-later.

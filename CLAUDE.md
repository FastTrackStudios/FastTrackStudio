# FastTrackStudio — Repo Instructions

**This repo is the shell.** The code moved out in August 2026. What used
to be one 238-crate monorepo is now a set of product repos, and this one
keeps the website, the docs site, the installer, the unified `fts` CLI,
the aggregate REAPER extension, and the release machinery.

| repo | holds | consumed as |
|---|---|---|
| **FastTrackStudio** (here) | site, docs-site, installer, the `fts` CLI, apps/extensions | — |
| [daw](https://github.com/FastTrackStudios/daw) | the DAW platform + all shared audio/MIDI/input substrate | git dep, tag |
| [session](https://github.com/FastTrackStudios/session) | setlists, songs, keyflow, notation, the guide, the Session app | git dep, tag |
| [signal](https://github.com/FastTrackStudios/signal) | the engine, the rigs, the sampler, the plugins, the Signal app | git dep, tag |
| [Ignition](https://github.com/FastTrackStudios/Ignition) | the Bevy visuals engine | separate product |
| [patchbay](https://github.com/FastTrackStudios/patchbay) | PipeWire studio routing | separate product |
| [architect](https://github.com/FastTrackStudios/architect) | the framework (entity/RPC, atom, form, auth, permissions, crdt) | git dep, tag |
| [task](https://github.com/FastTrackStudios/task) | the Task product + the Editor stack | git dep |
| [vendor](https://github.com/FastTrackStudios/vendor) | `phon`, `phon-jit`, `styx-format` forks | `[patch.crates-io]` |
| [music-convention](https://github.com/FastTrackStudios/music-convention) | monarchy, music-catalog, color-palette | git dep, tag |

## The layering, and why it looks backwards

```
daw  ->  session  ->  signal  ->  FastTrackStudio (here)
```

At **runtime** Session is the coordinator: the Session app opens and
syncs Signal and Ignition over WebSocket and depends on neither.

In **cargo** terms the arrow is reversed — Signal consumes Session's
crates (`signal-sampler` → `keyflow-orchestra`, `signal-synth` → `song`,
`signal-guitar-ui` → `session-ui`), because the rigs speak the musical
vocabulary Session defines. So `session` sits *below* `signal`: it
publishes the vocabulary AND ships the Session app, and Signal depends
only on the former. The graph stays acyclic because the coordination is
over the wire, not over cargo.

This repo is the only one allowed to depend on daw, session and signal at
once — that is what the `fts` CLI needs, and why it lives here.

## Rules learned the hard way in the split

- **Every repo must pin the SAME tag of a shared dependency.** This repo
  depends on both `daw` and `session`; when `session` was one daw tag
  behind, the build got two copies of `daw-proto` and their traits
  refused to unify. A cross-repo diamond is only safe when the shared
  edge is identical. Bumping daw means cascading: daw → session →
  signal → here, each tagged in turn.
- **Commit the lockfile, and never leave a git dep unpinned.** `clack`
  is a git dep with no rev, so a fresh resolve floats to its branch HEAD
  — which jumped `clack-host` 0.1.0 → 0.2.0 and broke `daw-standalone`
  the first time a repo resolved without the inherited lockfile.
- **`include_str!` / a build script's relative path cannot cross a repo
  boundary.** They are invisible to cargo's dependency graph, so they
  fail at compile time rather than resolution time. Export the bytes
  from the owning crate: that is what `reaper_input_config::PROFILES`
  is (the site's `/input` tutorial used to reach into
  `features/reaper/reaper-input/config`). This class of break has now
  bitten four times.
- **Never commit a local `[patch]` override.** The monorepo shipped a
  `[patch."…/architect"]` block pointing at `../architect`, against its
  own documented rule — it worked on one laptop and would break every
  CI run and every other checkout.
- **A feature-gated module needs a feature-gated dependency.**
  `signal-rigs-proto` was a hard dep used only under `#[cfg(feature =
  "signal")]`; it built in the full workspace and failed in a narrower
  graph. Same shape as architect's `chrono`-without-`serde` bug. Splitting
  a workspace is what exposes these.

## Layout

```
apps/site/                fasttrackstudio.app (dioxus web)
apps/docs-site/           docs.fasttrackstudio.app (NOT a cargo member)
apps/installer/           fts-installer
apps/fasttrackstudio/cli/ the unified `fts` CLI
apps/extensions/          the aggregate REAPER extension cdylib
docs/                     cross-domain guides + docs/split/PLAN.md
nix/                      the dendritic flake modules
```

## Build

```bash
nix develop            # or direnv
cargo check --workspace
```

Cross-repo development: override a tag with a local checkout rather than
pushing a tag to test.

```toml
[patch."https://github.com/FastTrackStudios/daw"]
daw = { path = "../daw/crates/daw/daw" }
```

**Never commit those overrides** — the paths are machine-specific.

## Licence

**GPL-3.0-or-later**, across every FTS-owned repo. `music-convention` is
deliberately the exception: `monarchy`, `music-catalog` and
`color-palette` are generic libraries kept `MIT OR Apache-2.0` so they
stay publishable. A GPL work may depend on MIT code, so this costs
nothing.

## Logging & tracing — wide events, ALWAYS

Before writing ANY log or debug output, load the
`logging-best-practices` skill. The span IS the wide event: enrich it
with `architect_telemetry::wide::set("namespace.field", value)`. Never
`println!`/`eprintln!`/`dbg!` in server or library code — not in
committed code, and not as debug scaffolding. Reproduce a bug in a
failing unit test instead.

## Agent skills

- **Issue tracker**: GitHub Issues on `FastTrackStudios/FastTrackStudio`,
  via the `gh` CLI. See `docs/agents/issue-tracker.md`.
- **Triage labels**: `needs-triage`, `needs-info`, `ready-for-agent`,
  `ready-for-human`, `wontfix`. See `docs/agents/triage-labels.md`.

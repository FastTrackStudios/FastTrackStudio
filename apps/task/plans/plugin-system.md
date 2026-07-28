# Plugin system — core Task stays, everything else becomes a plugin

Status: in progress (2026-07-28) — contract crate + manifest toggles landing
first; server/UI assembly follows once the realtime + widget branches merge.

## The idea

The core Task model (tasks, projects, the vault, orgs, auth) is the
platform. Everything domain-flavoured — meal planning, fitness,
FastTrackStudio's setlist/song surfaces, email, forges, scripture — is a
**plugin**: a named bundle of feature slices that an org can turn on and
off, and that a build can compile out entirely.

Two independent switches, deliberately:

| Switch | Mechanism | What "off" means |
|---|---|---|
| Build-time | cargo features on the assembling apps | the plugin's crates are not compiled; smaller binaries, faster builds |
| Runtime, per org | `plugins` in the org's `org.toml` (`OrgManifest`) | services not mounted (wire calls fail with not-found), nav hidden, routes render a "plugin disabled" notice, widgets unregistered |

Runtime toggling is the product feature; build-time exclusion is the
engineering feature. Neither uses dynamic loading — `dlopen` is a
non-starter for the wasm target and a liability everywhere else. A
"plugin" is a statically linked crate that *registers* contributions;
the registry decides what is active.

## The contract

A plugin is identity + contributions. Identity lives in one wasm-clean
crate; contributions are per-surface, because the server and the UI are
different binaries with different targets:

- **`task-plugin`** (new, `crates/task/plugin`) — the shared vocabulary:
  `PluginId`, `PluginInfo { id, name, description, core }`, the catalog
  of known plugins, and `PluginSet` — the resolution from an org's
  manifest to the enabled set (core plugins are always on; unknown ids
  are warned about and ignored, so an org.toml written by a newer build
  still loads on an older one).
- **Server contributions** — each plugin exposes a
  `fn server_plugin() -> ServerPlugin` carrying its service mounts
  (descriptor + serve + permit table + stream layers). The org router
  assembles from the enabled set. `permits::mounts()` becomes the
  concatenation of plugin contributions — the existing
  `permits_cover_router` guard keeps holding, per plugin.
- **UI contributions** — each plugin's `-ui` crate exposes a
  `fn ui_plugin() -> UiPlugin` carrying nav entries, widget specs (the
  `task-widgets` registry), and store registrations. The shell's `Route`
  enum stays static (Dioxus's router is an enum — routes cannot be
  dynamic), so a disabled plugin's routes stay routable but render a
  standard "this plugin is off for this org" panel, and its nav entries
  disappear. Compile-time exclusion removes the routes for real.
- **CLI contributions** — clap's derive is also static; the CLI keeps
  its command enum but consults the org's `PluginSet` before running a
  plugin command, failing with "the <x> plugin is disabled for this org
  (enable with `task org plugins enable <x>`)".

Registration is **explicit**: the app roots call
`registry.register(mealplan::plugin())`. No linker collection, no
`inventory` — explicit is debuggable, wasm-safe, and makes the
build-time feature gates one-line (`#[cfg(feature = "plugin-mealplan")]`
around the registration).

## Proposed plugin grouping

Core (always on, not toggleable): task, project, goal, milestone,
workstream, inbox, vault, view, tag, label, links, prefs, org, identity,
share, attachments, media, timer.

| Plugin id | Slices | Notes |
|---|---|---|
| `mealplan` | mealplan, pantry, cookbook, shopping, substitutions, recipe-import | the worked example |
| `fitness` | body, exercises, workouts, intake | |
| `fasttrackstudio` | song embeds, setlist/session player (`task-player-ui`), keyflow chart surfaces | the FTS product tie-in |
| `wiki` | wiki-* | big enough to be its own toggle |
| `scripture` | scripture, scripture-ui | |
| `email` | email-* | already effectively optional |
| `forge` | git-*, issue/review surfaces | |
| `agent` | agent-* | |
| `scheduling` | scheduling, calendar/booking surfaces | |
| `finance` | finance, finance-db, invoicing | timer stays core; billing is the plugin |
| `contacts` | contacts | |
| `recall` | recall | |
| `home` | locations, inventory | physical-world ops |

Grouping is a product call — this table is the proposal, trivially
adjustable while everything registers through one catalog.

## Sequencing

1. **Now**: `task-plugin` crate (ids, catalog, `PluginSet`), and
   `OrgManifest.plugins` — additive, default "all enabled", so nothing
   changes behaviour until assembly wires up.
2. **After the realtime + widgets + apidocs branches merge**: server
   assembly (mounts from plugins), UI assembly (nav/widgets/stores from
   plugins), `task org plugins list|enable|disable`, the settings panel,
   and `/org/{slug}/api` gaining a `plugin` field per service.
3. **Then**: cargo features per plugin on `task-server`, the three app
   crates, and the CLI; CI matrix job compiling the minimal (core-only)
   and full sets.

## Non-goals

- Dynamic loading of third-party code. External crates join by being
  added to the workspace and registered — the contract makes that a
  small, documented step, not a runtime capability.
- Per-user toggles (v1 is per-org; the manifest is org state).
- Data migration on disable. A disabled plugin's vault files and tables
  stay; the plugin just stops being served. Re-enabling picks them up
  again. Deleting data is a separate, explicit operation.

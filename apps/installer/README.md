# fts-installer

Thin downloader/installer for FastTrackStudio releases — a small clap
CLI, no GUI (the old Dioxus wizard lives in git history).

**Platforms**: Linux (x86_64) and macOS (Apple Silicon only — the
airlock build machine that signs/notarizes macOS releases is arm64;
there's no Intel-mac build yet). Every command below works on both;
where the install shape differs it's called out under
[macOS specifics](#macos-specifics).

```text
fts-installer                     # = install (latest release)
fts-installer install [--version vX.Y.Z] [--prefix DIR] [--url TARBALL_URL]
fts-installer update  [--prefix DIR]
fts-installer uninstall [--prefix DIR]
fts-installer reaper    [--prefix DIR]   # REAPER + SWS + ReaPack + the 3 rigs
fts-installer bundle    [--prefix DIR] [--version vX.Y.Z]   # everything, one command
```

## `fts-installer bundle` — the full installer package

Everything a fresh machine needs, in one command: `reaper`, then
`install` (the app), then `plugins install`. Order matters on Linux —
running `reaper` first means the FastTrackStudio app install has rig
dirs to drop `reaper_fts_extensions.so` into
(`~/fasttrackstudio/UserPlugins/`, `~/fts-tracks/UserPlugins/`,
`~/fts-dev/UserPlugins/`); the extension is silently skipped for any rig
dir that doesn't exist yet, so running `install` standalone on a machine
with no REAPER env is still fine, and re-running `bundle` after `reaper`
sets it up going forward is a no-op for what's already current. (macOS
has no `reaper_fts_extensions.dylib` build yet, so this step is a no-op
there regardless of order — see [macOS specifics](#macos-specifics).)
`--version` (if given) pins both the app and plugin bundle downloads;
REAPER/SWS/ReaPack are always their pinned/latest official versions (see
below), independent of the FTS release tag.

- Releases are resolved via the codeberg (Gitea/Forgejo) API
  (`/api/v1/repos/FastTrackStudios/FastTrackStudio/releases/latest` or
  `/releases/tags/<tag>`); the platform tarball asset
  (`fasttrackstudio-v<ver>-x86_64-linux.tar.gz`) is downloaded with
  progress + retry (installer-core primitives), verified against the
  release's `SHA256SUMS` asset when present, extracted, and the layout
  applied in Rust.
- `--url` skips the API entirely and installs from a direct tarball URL
  (SHA256SUMS is looked for next to it, best-effort).
- `$CODEBERG_TOKEN`, if set, is sent as `Authorization: token <t>`
  (private repo / rate-limit cases); public access needs no token.
- `--prefix` (default `$HOME`) relocates the whole layout; with a
  non-home prefix the systemd / desktop-database refreshes are skipped —
  that is the test path.
- `update` no-ops when `<prefix>/.local/lib/fts/VERSION` already matches
  the latest release tag.

Installed layout (identical to the repo's `just install` and to the
tarball's `install.sh`, which is the no-Rust fallback):

```text
~/.local/lib/fts/{fasttrackstudio,fts,VERSION}
~/.local/bin/{fasttrackstudio,fts}                 # symlinks
~/.config/systemd/user/signal-engine.service       # installed, NOT enabled
~/.local/share/applications/fasttrackstudio.desktop
~/.local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg
```

`uninstall` reverses it; user data (`~/.config/fts`, `~/.config/signal`)
is always kept.

The release artifacts this consumes are assembled by
`just release-package` (→ `dist/`): the tarball above (binaries
patchelf'd to the standard `/lib64` loader, plus unit/desktop/icon
templates and `install.sh`/`uninstall.sh`), the standalone
`fts-installer-x86_64-linux` binary, and `SHA256SUMS` covering both.

## `fts-installer reaper` — one-shot REAPER environment

Sets up the complete REAPER stack on a fresh machine so nobody
downloads 80 things by hand:

```text
fts-installer reaper [--prefix DIR]
```

- Downloads the **latest REAPER** (resolved from cockos.com, official
  reaper.fm tarball — REAPER's license forbids us redistributing it)
  into a portable install at `~/.local/lib/fts/reaper/`.
- Downloads **SWS** (sws-extension.org) and **ReaPack** (cfillion's
  GitHub release).
- Creates THE three rigs — `~/fasttrackstudio` (fts-reaper, main
  recording), `~/fts-tracks` (live tracks playback), `~/fts-dev`
  (dev/testing) — each with SWS + ReaPack in `UserPlugins/`, the SWS
  python scripts, and a minimal `reaper.ini` (only written when
  missing; re-runs never clobber a configured rig).
- Writes the `launch.json` files the fts launchers use
  (`~/fasttrackstudio/FastTrackStudio/launch.json` +
  `~/fts-dev/launch.json`) and `fts-{reaper,tracks,dev}.desktop`
  entries (skipped with a non-home `--prefix`).
- Idempotent: an already-current REAPER version is not re-downloaded.

Still TODO here: themes/library content (ColorThemes, FXChains,
TrackTemplates from an fts-library release).

## macOS specifics

Every command (`install`/`update`/`uninstall`/`reaper`/`plugins`/`bundle`)
runs the same on macOS, but the underlying artifacts and install paths
differ from Linux:

```text
~/Applications/<Name>.app                          the app (whatever `dx build` named it)
~/.local/bin/fasttrackstudio                       symlink -> .app/Contents/MacOS/<binary>
~/.local/share/fts/{macos-app-version,macos-app-name}
~/Library/Audio/Plug-Ins/CLAP/FTS <Name>.clap
~/Library/Audio/Plug-Ins/VST3/FTS <Name>.vst3/
~/.local/lib/fts/reaper/REAPER.app                 portable REAPER (universal binary)
~/fasttrackstudio, ~/fts-tracks, ~/fts-dev          the 3 rigs (same as Linux)
```

- **App**: `install`/`update` download the notarized+stapled
  `FastTrackStudio-*-macos.dmg` release asset (built by
  `apps/fasttrackstudio/ios/deploy-macos.sh` on airlock), mount it with
  `hdiutil`, copy the `.app` into `~/Applications`, and symlink whatever
  binary `dx` put at `Contents/MacOS/` into `~/.local/bin`. No
  SHA256SUMS check — the Developer-ID signature + notarization ticket
  is the integrity/authenticity check.
- **REAPER env**: `reaper` downloads REAPER's/SWS's official universal
  `.dmg`s (mounted, contents copied out, detached) and ReaPack's `.dylib`
  release asset, and sets up the same three rig dirs as Linux. No dock
  entries yet (Linux gets `.desktop` files; macOS would need a wrapper
  `.app` with its own `Info.plist`, not built) — launch a rig from a
  terminal: `~/.local/lib/fts/reaper/REAPER.app/Contents/MacOS/REAPER
  -cfgfile ~/fasttrackstudio/reaper.ini -newinst`.
- **Plugins**: CLAP + VST3 only — **no AU build** yet. The release asset
  is a `.zip` (Apple notarization only accepts zip/pkg/dmg, not tar.gz),
  Developer-ID signed and notarized by
  `apps/fasttrackstudio/ios/deploy-macos-plugins.sh`, but **not stapled**
  (stapling only works on `.app`/`.pkg`/`.dmg`) — Gatekeeper falls back
  to an online notarization-ticket lookup on first load instead, which
  needs network access at that moment. `plugins install` strips any
  lingering `com.apple.quarantine` xattr after extracting, best-effort.
- **Known gaps**: no unified `fts` CLI ships for macOS yet (only the
  `fasttrackstudio` GUI/`--engine` binary — the app tarball's `fts`
  binary is Linux-only for now); no `reaper_fts_extensions.dylib` build
  exists yet, so unlike Linux the macOS app install never touches the
  REAPER rig `UserPlugins/` dirs.

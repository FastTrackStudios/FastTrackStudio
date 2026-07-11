# fts-installer

Thin downloader/installer for FastTrackStudio releases — a small clap
CLI, no GUI (the old Dioxus wizard lives in git history).

```text
fts-installer                     # = install (latest release)
fts-installer install [--version vX.Y.Z] [--prefix DIR] [--url TARBALL_URL]
fts-installer update  [--prefix DIR]
fts-installer uninstall [--prefix DIR]
fts-installer reaper    [--prefix DIR]   # REAPER + SWS + ReaPack + the 3 rigs
```

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
TrackTemplates from an fts-library release) and installing
`reaper_fts_extensions.so` from the app tarball into the rigs.

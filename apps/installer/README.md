# fts-installer

Thin downloader/installer for FastTrackStudio releases — a small clap
CLI, no GUI (the old Dioxus wizard lives in git history).

```text
fts-installer                     # = install (latest release)
fts-installer install [--version vX.Y.Z] [--prefix DIR] [--url TARBALL_URL]
fts-installer update  [--prefix DIR]
fts-installer uninstall [--prefix DIR]
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

Everything the installer used to do beyond this — REAPER setup,
extensions, presets — belongs to the app itself post-install (engine
supervisor + auto-update in `apps/fasttrackstudio`).

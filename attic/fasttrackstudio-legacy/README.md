# attic/fasttrackstudio-legacy — the dissolved FastTrackStudio/ repo (finale wave)

The last remnants of the old `FastTrackStudio/` domain workspace (the
LEGACY app repo, superseded by `apps/fasttrackstudio`). Everything that
was still alive moved out before this parkage:

- `crates/shared/{utils,vox-discover,installer-core}` → `libs/` (wave 1)
- `apps/installer` → `apps/installer` (root workspace member, finale wave)
- `docs/` (facet/styx/tracey guides, spec/) → root `docs/` (finale wave)

What is parked here (not workspace members; the `Cargo.toml` here is the
old domain workspace manifest, referencing a pre-monorepo dep table —
git-rev dioxus, facet 0.46 — so reviving anything means manifest surgery):

| dir | what it was |
|---|---|
| `apps/keyflow-playground/` | Dioxus playground for keyflow chart rendering (old git-rev dioxus). |
| `apps/tests/wasm/` | wasm-pack smoke tests for the legacy app. |
| `xtask/` | legacy repo xtask (bundling, asset pipeline). |
| `ci/ hooks/ scripts/` | legacy repo CI + git hooks + install scripts. |
| `tasks/` | old PRD markdown (allocator, setlist mode, signal variation switching, …). |
| `effects/` | FTS JSFX test effects. |
| `flake.nix` / `flake.lock` | the old repo flake — its avahi/pipewire dev-shell inputs were folded into the ROOT `flake.nix`. |
| dotfiles (`.beads`, `.claude`, `.config`, `.github`, `.cargo`, …) | per-repo tooling state, kept for reference. |
| `README.legacy.md` / `CLAUDE.md` / `AGENTS.md` / `Justfile` | old repo docs/commands (build paths are stale). |

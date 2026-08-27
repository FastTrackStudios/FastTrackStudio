# FastTrackStudio — the aggregate repo.
# Run commands: just <recipe-name>
#
# The product recipes moved with their code in the August 2026 split:
# rig-install / keys-* / plugins-* are in signal, the theme and REAPER
# recipes in daw, the extension build in fts-extensions. Look in git
# history here if you need the old versions.

# List recipes by default
default:
    @just --list

# ── The installer ────────────────────────────────────────────────────────

# Build the installer.
installer:
    cargo build --release -p fts-installer

# Run it from the working tree.
installer-run *ARGS:
    cargo run -p fts-installer -- {{ARGS}}

# ── Docs site (docs.fasttrackstudio.app) ─────────────────────────────────

docs-build:
    apps/docs-site/build.sh

docs-serve:
    apps/docs-site/serve.sh

docs-deploy:
    apps/docs-site/deploy.sh

# ── Checks ───────────────────────────────────────────────────────────────

check:
    cargo check --workspace

fmt:
    cargo fmt --all

fmt-check:
    cargo fmt --all --check

test:
    cargo nextest run --workspace

# ── Disk ─────────────────────────────────────────────────────────────────

# target/ size across every worktree of this repo.
disk:
    #!/usr/bin/env bash
    du -sh $(git worktree list --porcelain | awk '/^worktree /{print $2"/target"}') 2>/dev/null | sort -rh

# Reclaim stale build artifacts.
sweep:
    cargo sweep --time 7

#!/usr/bin/env bash
# Clone the FTS sibling-repo layout that fts-extensions and FastTrackStudio
# expect when their workspace Cargo.toml local-dev patches are active.
#
# Usage:
#   ./scripts/bootstrap-siblings.sh [target-parent-dir]
#
# If no target dir is given, defaults to one level above this repo:
#   ~/Development/FastTrackStudio/
#
# Idempotent: skips repos that already exist; pulls latest on the default
# branch otherwise. Missing-on-starcommand fallbacks (moire) clone from
# their canonical upstream.

set -euo pipefail

PARENT="${1:-$(cd "$(dirname "$0")/../.." && pwd)}"
mkdir -p "$PARENT"
cd "$PARENT"

STAR="https://git.starcommand.live/FastTrackStudios"

# Sibling -> "url|branch". Most live on starcommand; a couple use upstream.
# Branch hints exist because some workspaces expect crates that only live on
# feature branches (daw's daw-module is only on pt-rpp-track-order-midi, etc).
declare -A REPOS=(
  [blitz]="$STAR/blitz.git|main"
  [daw]="$STAR/daw.git|pt-rpp-track-order-midi"
  [dioxus-launcher]="$STAR/dioxus-launcher.git|main"
  [dock-dioxus]="$STAR/dock-dioxus.git|main"
  [dynamic-template]="$STAR/dynamic-template.git|main"
  [fts-launcher]="$STAR/fts-launcher.git|main"
  [fts-story]="$STAR/fts-story.git|main"
  [fts-ui]="$STAR/fts-ui.git|main"
  [input]="$STAR/input.git|main"
  [input_actions]="$STAR/input_actions.git|main"
  [keyflow]="$STAR/keyflow.git|forgejo-main"
  [monarchy]="$STAR/monarchy.git|main"
  [reaper-file]="$STAR/reaper-file.git|main"
  [reaper-lib]="$STAR/reaper-lib.git|main"
  [session]="$STAR/session.git|main"
  [moire]="https://github.com/bearcove/moire|main"
)

# Top-level workspaces the user will actually build.
declare -A TOPLEVELS=(
  [fts-extensions]="$STAR/fts-extensions.git|main"
  [FastTrackStudio]="$STAR/FastTrackStudio.git|migrate/new-patterns"
)

clone_or_update() {
  local name="$1" spec="$2"
  local url="${spec%|*}" branch="${spec#*|}"
  if [ -d "$name/.git" ]; then
    echo "==> $name (existing, fetching, checkout $branch)"
    # Pre-existing checkouts often point at github origin. Point origin at
    # the expected URL so subsequent fetches see all the branches we need.
    local current_origin
    current_origin="$(git -C "$name" remote get-url origin 2>/dev/null || true)"
    if [ "$current_origin" != "$url" ]; then
      if [ -n "$current_origin" ]; then
        git -C "$name" remote set-url origin "$url"
      else
        git -C "$name" remote add origin "$url"
      fi
    fi
    git -C "$name" fetch --quiet origin "$branch" || true
    git -C "$name" checkout --quiet "$branch" 2>/dev/null || \
      git -C "$name" checkout --quiet -b "$branch" "origin/$branch"
  else
    echo "==> $name (cloning $url, branch $branch)"
    git clone --quiet --branch "$branch" "$url" "$name"
  fi
}

for name in "${!REPOS[@]}"; do
  clone_or_update "$name" "${REPOS[$name]}"
done

for name in "${!TOPLEVELS[@]}"; do
  clone_or_update "$name" "${TOPLEVELS[$name]}"
done

echo
echo "Done. Workspace ready under: $PARENT"
echo "Build fts-extensions:    (cd $PARENT/fts-extensions && cargo build)"
echo "Build FastTrackStudio:   (cd $PARENT/FastTrackStudio && cargo build)"

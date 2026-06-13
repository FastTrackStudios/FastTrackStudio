#!/usr/bin/env bash
# Materialize the sibling repos the Task workspace path-depends on.
#
# The root Cargo.toml reaches outside this repo (`../Editor`,
# `../architect`, `../FastTrackStudio/*`) so any cargo build — the
# server image, the wasm web bundle — needs those checkouts in place.
# Pins live in siblings.lock (same directory).
#
# Usage:
#   siblings.sh clone <root>
#       Ensure each sibling exists under <root> at its pinned sha.
#       Prefers an existing local checkout (left untouched, sha
#       mismatch warned); clones + checks out the pin otherwise.
#       Use this on CI runners / fresh machines:
#         deploy/docker/siblings.sh clone "$(dirname "$(pwd)")"
#
#   siblings.sh context <ctx-dir>
#       Assemble a clean docker build context for server.Dockerfile:
#       `git archive HEAD` of this repo into <ctx-dir>/Task plus each
#       sibling into its layout path (local checkout preferred,
#       clone-at-pin fallback). Then:
#         docker build -f <ctx>/Task/deploy/docker/server.Dockerfile <ctx>
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$here/../.." && pwd)"
lock="$here/siblings.lock"

# Where local sibling checkouts are expected (next to this repo).
local_root="${TASK_SIBLINGS_ROOT:-$(dirname "$repo_root")}"

mode="${1:?usage: siblings.sh clone|context <dir>}"
dest="${2:?usage: siblings.sh clone|context <dir>}"
mkdir -p "$dest"
dest="$(cd "$dest" && pwd)"

each_pin() { grep -Ev '^[[:space:]]*(#|$)' "$lock"; }

# Retry a git command up to $RETRIES times with $BACKOFF backoff —
# codeberg intermittently resets packfile transfers, so every fetch
# here is wrapped. Generous budget: the degradation comes in waves.
RETRIES="${SIBLINGS_RETRIES:-8}"
BACKOFF="${SIBLINGS_BACKOFF:-15}"
retry_git() {
    local attempt=1
    until "$@"; do
        if [ "$attempt" -ge "$RETRIES" ]; then
            echo "!! \`$*\` failed after $RETRIES attempts" >&2
            return 1
        fi
        echo ">> attempt $attempt failed — retrying in ${BACKOFF}s: $*" >&2
        attempt=$((attempt + 1)); sleep "$BACKOFF"
    done
}

clone_at() { # url sha target
    local url="$1" sha="$2" target="$3"
    echo ">> clone $url @ ${sha:0:10} -> $target"
    mkdir -p "$(dirname "$target")"
    # SHALLOW clone — minimal transfer, which is the whole point under
    # codeberg's flapping git serving: less data per attempt = a
    # smaller window for the connection reset. (NOT --filter=blob:none:
    # that partial clone defers a promisor blob fetch to checkout time,
    # which flaked separately.) The pin is usually the default-branch
    # tip the shallow clone lands on; if not, fetch just that one commit
    # shallowly. Every network op is retried.
    rm -rf "$target"
    retry_git git clone --depth 1 --no-tags "$url" "$target" || {
        rm -rf "$target"; exit 1
    }
    if [ "$(git -C "$target" rev-parse HEAD)" != "$sha" ]; then
        retry_git git -C "$target" fetch --depth 1 --no-tags origin "$sha" || {
            echo "!! $sha not reachable on $url — push the sibling branch (see siblings.lock header)" >&2
            exit 1
        }
        git -C "$target" checkout --detach "$sha" \
            || { echo "!! checkout $sha failed on $url"; exit 1; }
    fi
}

archive_into() { # src-checkout target
    local src="$1" target="$2"
    mkdir -p "$target"
    git -C "$src" archive HEAD | tar -x -C "$target"
}

case "$mode" in
clone)
    each_pin | while read -r rel url sha; do
        target="$dest/$rel"
        if [ -e "$target" ]; then
            have="$(git -C "$target" rev-parse HEAD 2>/dev/null || echo none)"
            [ "$have" = "$sha" ] \
                && echo "ok $rel @ ${sha:0:10} (existing)" \
                || echo "!! $rel exists at ${have:0:10}, pin is ${sha:0:10} — using existing checkout as-is"
            continue
        fi
        clone_at "$url" "$sha" "$target"
    done
    ;;
context)
    if [ -n "$(git -C "$repo_root" status --porcelain)" ]; then
        echo "!! Task checkout is dirty — context uses committed HEAD only" >&2
    fi
    echo ">> archive Task @ $(git -C "$repo_root" rev-parse --short HEAD) -> $dest/Task"
    archive_into "$repo_root" "$dest/Task"
    each_pin | while read -r rel url sha; do
        target="$dest/$rel"
        local_copy="$local_root/$rel"
        if [ -d "$local_copy/.git" ] || [ -f "$local_copy/.git" ]; then
            have="$(git -C "$local_copy" rev-parse HEAD)"
            [ "$have" = "$sha" ] || echo "!! $rel local checkout ${have:0:10} != pin ${sha:0:10} — archiving local state" >&2
            echo ">> archive $rel @ ${have:0:10} -> $target"
            archive_into "$local_copy" "$target"
        else
            tmp="$dest/.clone-$$"
            clone_at "$url" "$sha" "$tmp"
            archive_into "$tmp" "$target"
            rm -rf "$tmp"
        fi
    done
    echo "context ready: $dest"
    ;;
*)
    echo "unknown mode: $mode (want clone|context)" >&2
    exit 1
    ;;
esac

#!/usr/bin/env bash
# Move airlock's build scratch off the internal disk onto an external volume.
#
# Airlock's 228 GB internal disk fills chronically: the CI runner's target/
# (~19 GB), the manual ~/fts target/ (~24 GB), and Xcode's module caches +
# IPA staging under $TMPDIR are the four heavy consumers, and when the volume
# hits 100% builds fail with "No space left on device" — sometimes before the
# runner can even write its own log.
#
# This points all four at <mount>/fts-build instead:
#
#   CARGO_TARGET_DIR   <mount>/fts-build/cargo-target
#   TMPDIR             <mount>/fts-build/tmp          (xcodebuild module cache,
#                                                      IPA Payload staging)
#   Xcode DerivedData  <mount>/fts-build/DerivedData
#
# for BOTH build roots: the self-hosted runner (via ~/actions-runner/.env,
# which the runner service exports into every job) and interactive/SSH shells
# (via ~/.zprofile, which is what `ssh rat@airlock.local` gets).
#
# Usage (on airlock, with the drive mounted):
#   bash apps/fasttrackstudio/ios/airlock-build-storage.sh /Volumes/YourDrive
#   bash apps/fasttrackstudio/ios/airlock-build-storage.sh --status
#   bash apps/fasttrackstudio/ios/airlock-build-storage.sh --revert
#
# After applying, restart the runner service so it picks up the new .env:
#   launchctl kickstart -k gui/$(id -u)/actions.runner.FastTrackStudios-FastTrackStudio.airlock
#
# NOTE: a fresh CARGO_TARGET_DIR means the next build is cold (~1h on this
# box). The old target/ dirs are left in place — delete them once you trust
# the new location. Prefer a Thunderbolt/USB-3.2 SSD: cargo's target/ is
# millions of small writes and a spinning or USB-2 disk will be slower than
# rebuilding.
set -euo pipefail

RUNNER_ENV="$HOME/actions-runner/.env"
ZPROFILE="$HOME/.zprofile"
MARK_BEGIN="# >>> fts build storage >>>"
MARK_END="# <<< fts build storage <<<"

# Remove a previously written block from a file (idempotent re-apply / revert).
strip_block() {
    local f="$1"
    [ -f "$f" ] || return 0
    # `sed -i ''` is the BSD/macOS in-place form.
    sed -i '' "/^${MARK_BEGIN}\$/,/^${MARK_END}\$/d" "$f"
}

status() {
    echo "=== current build storage ==="
    for f in "$RUNNER_ENV" "$ZPROFILE"; do
        if [ -f "$f" ] && grep -q "^${MARK_BEGIN}\$" "$f"; then
            echo "--- $f ---"
            sed -n "/^${MARK_BEGIN}\$/,/^${MARK_END}\$/p" "$f" | sed '1d;$d'
        else
            echo "--- $f: not redirected (internal disk) ---"
        fi
    done
    echo "=== disk ==="
    df -h /System/Volumes/Data | tail -1
}

case "${1-}" in
    --status)
        status
        exit 0
        ;;
    --revert)
        strip_block "$RUNNER_ENV"
        strip_block "$ZPROFILE"
        echo "reverted — builds go back to the internal disk on next shell/job."
        echo "restart the runner: launchctl kickstart -k gui/$(id -u)/actions.runner.FastTrackStudios-FastTrackStudio.airlock"
        exit 0
        ;;
    "")
        echo "usage: $0 <mount-point> | --status | --revert" >&2
        echo "  e.g. $0 /Volumes/BuildSSD" >&2
        exit 2
        ;;
esac

MOUNT="${1%/}"
[ -d "$MOUNT" ] || { echo "ERROR: $MOUNT does not exist — is the drive plugged in and mounted?" >&2; exit 1; }
# A path under /Volumes that exists but isn't a mount point is the classic
# footgun: the OS creates an empty dir there when the disk is absent, and
# everything silently lands on the internal disk again.
if [ "$MOUNT" != "${MOUNT#/Volumes/}" ] && ! diskutil info "$MOUNT" >/dev/null 2>&1; then
    echo "ERROR: $MOUNT is not a mounted volume (stale empty dir?)." >&2
    exit 1
fi
[ -w "$MOUNT" ] || { echo "ERROR: $MOUNT is not writable." >&2; exit 1; }

BASE="$MOUNT/fts-build"
mkdir -p "$BASE/cargo-target" "$BASE/tmp" "$BASE/DerivedData"

# Fail loudly now rather than mid-build: a read-only mount, a full drive, or
# an exFAT volume that chokes on the symlinks cargo/codesign create.
probe="$BASE/.write-probe"
: > "$probe" && ln -sf "$probe" "$probe.link" && rm -f "$probe" "$probe.link" \
    || { echo "ERROR: $BASE cannot hold files+symlinks (read-only, full, or a filesystem without symlink support)." >&2; exit 1; }

write_block() {
    local f="$1"
    mkdir -p "$(dirname "$f")"
    touch "$f"
    strip_block "$f"
    cat >> "$f" <<EOF
${MARK_BEGIN}
# Build scratch on the external volume — see
# apps/fasttrackstudio/ios/airlock-build-storage.sh. Re-run that script
# (or --revert) instead of editing this block by hand.
export CARGO_TARGET_DIR="$BASE/cargo-target"
export TMPDIR="$BASE/tmp"
export XCODE_DERIVED_DATA="$BASE/DerivedData"
${MARK_END}
EOF
}

write_block "$RUNNER_ENV"
write_block "$ZPROFILE"

# xcodebuild reads DerivedData location from Xcode's own prefs, not the env.
defaults write com.apple.dt.Xcode IDECustomDerivedDataLocation -string "$BASE/DerivedData"

echo "=== build scratch now at $BASE ==="
status
cat <<EOF

next:
  1. restart the runner so jobs inherit the new .env:
       launchctl kickstart -k gui/$(id -u)/actions.runner.FastTrackStudios-FastTrackStudio.airlock
  2. the next build is COLD (fresh CARGO_TARGET_DIR). Once it passes, reclaim
     the old trees:
       rm -rf ~/actions-runner/_work/FastTrackStudio/FastTrackStudio/target ~/fts/target
  3. if the drive is ever unplugged, run --revert before building, or cargo
     will write to an empty /Volumes stub on the internal disk.
EOF

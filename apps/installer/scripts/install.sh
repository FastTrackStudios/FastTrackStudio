#!/usr/bin/env bash
# FastTrackStudio install script (no-Rust fallback, shipped in the
# release tarball next to the binaries it installs).
#
#   curl -LO https://codeberg.org/FastTrackStudios/FastTrackStudio/releases/...
#   tar xzf fasttrackstudio-v*-x86_64-linux.tar.gz && ./install.sh
#
# Layout (same as `fts-installer install` and the repo's `just install`):
#   $PREFIX/.local/lib/fts/{fasttrackstudio,fts,VERSION}
#   $PREFIX/.local/bin/{fasttrackstudio,fts}          (symlinks)
#   $PREFIX/.config/systemd/user/signal-engine.service (installed, NOT enabled —
#       the app is the on/off switch)
#   $PREFIX/.local/share/applications/fasttrackstudio.desktop
#   $PREFIX/.local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg
#
# PREFIX defaults to $HOME; set PREFIX to test-install elsewhere (the
# systemd / desktop-database refreshes are then skipped).
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
PREFIX="${PREFIX:-$HOME}"
lib="$PREFIX/.local/lib/fts"
bin="$PREFIX/.local/bin"

mkdir -p "$lib" "$bin"

# Binaries: copy to .new then rename so a running binary is replaced
# atomically.
for b in fasttrackstudio fts; do
    install -m 755 "$here/$b" "$lib/$b.new"
    mv -f "$lib/$b.new" "$lib/$b"
    ln -sfn "$lib/$b" "$bin/$b"
done
[ -f "$here/VERSION" ] && install -m 644 "$here/VERSION" "$lib/VERSION"

# systemd user unit (installed but NOT enabled).
mkdir -p "$PREFIX/.config/systemd/user"
install -m 644 "$here/signal-engine.service" "$PREFIX/.config/systemd/user/"

# Icon + desktop entry (@BIN@ substituted).
mkdir -p "$PREFIX/.local/share/icons/hicolor/scalable/apps"
install -m 644 "$here/icon.svg" \
    "$PREFIX/.local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg"
mkdir -p "$PREFIX/.local/share/applications"
sed "s|@BIN@|$lib/fasttrackstudio|" "$here/fasttrackstudio.desktop" \
    > "$PREFIX/.local/share/applications/fasttrackstudio.desktop"

if [ "$PREFIX" = "$HOME" ]; then
    if command -v systemctl >/dev/null 2>&1; then
        systemctl --user daemon-reload 2>/dev/null || true
        systemctl --user try-restart signal-engine 2>/dev/null || true
    fi
    update-desktop-database "$PREFIX/.local/share/applications" 2>/dev/null || true
    gtk-update-icon-cache "$PREFIX/.local/share/icons/hicolor" 2>/dev/null || true
else
    echo "(custom PREFIX $PREFIX — systemd/desktop-database refresh skipped)"
fi

echo "installed: fasttrackstudio + fts in $bin (version $(cat "$lib/VERSION" 2>/dev/null || echo unknown))"
echo "engine service installed but not enabled — the app starts it, or:"
echo "  systemctl --user start signal-engine"

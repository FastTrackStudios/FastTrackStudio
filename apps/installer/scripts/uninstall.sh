#!/usr/bin/env bash
# FastTrackStudio uninstall script (shipped in the release tarball).
# Reverses install.sh. User data is untouched (~/.config/fts and
# ~/.config/signal — the rig config may be a symlink; never deleted).
#
# PREFIX defaults to $HOME; must match the PREFIX used at install time.
set -euo pipefail

PREFIX="${PREFIX:-$HOME}"

if [ "$PREFIX" = "$HOME" ] && command -v systemctl >/dev/null 2>&1; then
    systemctl --user stop signal-engine 2>/dev/null || true
    systemctl --user disable signal-engine 2>/dev/null || true
fi
rm -f "$PREFIX/.config/systemd/user/signal-engine.service"
if [ "$PREFIX" = "$HOME" ] && command -v systemctl >/dev/null 2>&1; then
    systemctl --user daemon-reload 2>/dev/null || true
fi

rm -f "$PREFIX/.config/REAPER/UserPlugins/reaper_fts_extensions.so"
rm -f "$PREFIX/.local/bin/fasttrackstudio" "$PREFIX/.local/bin/fts"
rm -rf "$PREFIX/.local/lib/fts"
rm -f "$PREFIX/.local/share/applications/fasttrackstudio.desktop"
rm -f "$PREFIX/.local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg"

if [ "$PREFIX" = "$HOME" ]; then
    update-desktop-database "$PREFIX/.local/share/applications" 2>/dev/null || true
    gtk-update-icon-cache "$PREFIX/.local/share/icons/hicolor" 2>/dev/null || true
fi

echo "uninstalled (user data in ~/.config/fts and ~/.config/signal kept)"

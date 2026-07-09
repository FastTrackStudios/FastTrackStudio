#!/bin/bash
# FastTrackStudio installer for Linux
# Portable installation — runs entirely within its own directory.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo ""
echo "  FastTrackStudio for Linux"
echo "  ─────────────────────────"
echo ""
echo "  This will install FastTrackStudio into a directory of your choice."
echo "  The installation is fully portable — everything runs from one folder."
echo ""

DEFAULT_DIR="$HOME/opt/FastTrackStudio"

read -rp "  Install to [$DEFAULT_DIR]: " INSTALL_DIR
INSTALL_DIR="${INSTALL_DIR:-$DEFAULT_DIR}"

# Expand ~ if the user typed it
INSTALL_DIR="${INSTALL_DIR/#\~/$HOME}"

SELF="$(realpath "$SCRIPT_DIR")"
TARGET="$(realpath -m "$INSTALL_DIR")"

if [ "$SELF" = "$TARGET" ]; then
  echo "  Already in install directory, skipping copy."
else
  mkdir -p "$INSTALL_DIR"
  echo "  Copying files to $INSTALL_DIR ..."
  cp -a "$SCRIPT_DIR"/bin "$INSTALL_DIR/"
  cp -a "$SCRIPT_DIR"/lib "$INSTALL_DIR/"
  [ -d "$SCRIPT_DIR/share" ] && cp -a "$SCRIPT_DIR"/share "$INSTALL_DIR/"
  cp "$SCRIPT_DIR/install.sh" "$INSTALL_DIR/"
  echo "  Done."
fi

# ── Desktop entry ──────────────────────────────────────────────────
echo ""
read -rp "  Create desktop entry? [Y/n]: " CREATE_DESKTOP
CREATE_DESKTOP="${CREATE_DESKTOP:-Y}"
if [[ "$CREATE_DESKTOP" =~ ^[Yy] ]]; then
  DESKTOP_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/applications"
  mkdir -p "$DESKTOP_DIR"
  cat > "$DESKTOP_DIR/fasttrackstudio.desktop" << EOF
[Desktop Entry]
Type=Application
Name=FastTrackStudio
Exec=$INSTALL_DIR/bin/fasttrackstudio-desktop
Terminal=false
Categories=AudioVideo;Audio;Music;
EOF
  echo "  Created $DESKTOP_DIR/fasttrackstudio.desktop"
fi

# ── PATH integration ──────────────────────────────────────────────
echo ""
read -rp "  Add to PATH via shell profile? [Y/n]: " ADD_PATH
ADD_PATH="${ADD_PATH:-Y}"
if [[ "$ADD_PATH" =~ ^[Yy] ]]; then
  LINE="export PATH=\"$INSTALL_DIR/bin:\$PATH\""

  # Pick the right profile file
  if [ -n "${FISH_VERSION:-}" ] || [ "$(basename "${SHELL:-}")" = "fish" ]; then
    PROFILE="${XDG_CONFIG_HOME:-$HOME/.config}/fish/conf.d/fasttrackstudio.fish"
    mkdir -p "$(dirname "$PROFILE")"
    LINE="fish_add_path $INSTALL_DIR/bin"
  elif [ -f "$HOME/.zshrc" ]; then
    PROFILE="$HOME/.zshrc"
  elif [ -f "$HOME/.bashrc" ]; then
    PROFILE="$HOME/.bashrc"
  else
    PROFILE="$HOME/.profile"
  fi

  if ! grep -qF "$INSTALL_DIR/bin" "$PROFILE" 2>/dev/null; then
    {
      echo ""
      echo "# FastTrackStudio"
      echo "$LINE"
    } >> "$PROFILE"
    echo "  Added to $PROFILE"
  else
    echo "  PATH entry already exists in $PROFILE"
  fi
fi

echo ""
echo "  Installation complete!"
echo ""
echo "  Run FastTrackStudio:"
echo "    $INSTALL_DIR/bin/fasttrackstudio-desktop"
echo ""
echo "  To uninstall, remove the directory:"
echo "    rm -rf $INSTALL_DIR"
echo ""

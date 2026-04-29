#!/usr/bin/env bash
# Wrapper that runs REAPER through the NixOS FHS environment.
# Used by the test harness which expects a single executable.
#
# For virtual display testing (Xvfb), the test harness clears DISPLAY
# (headless mode). FTS_VIRTUAL_DISPLAY carries the original Xvfb display.
if [ -n "$FTS_VIRTUAL_DISPLAY" ] && [ -z "$DISPLAY" ]; then
    export DISPLAY="$FTS_VIRTUAL_DISPLAY"
fi

exec /nix/store/57r638ymnzh23ni6a15mqv0h4ym6p937-reaper-env/bin/reaper-env \
    /nix/store/qjiyish1cqpbf5qjxpdm3xrz16lz4ii7-reaper-headless-7.59/bin/reaper "$@"

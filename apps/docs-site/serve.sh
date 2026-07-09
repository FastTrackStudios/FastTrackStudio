#!/usr/bin/env bash
# Dev server for the FTS docsite, with live reload.
#
# Runs two watchers: `kf docs --watch` regenerates .rendered whenever an
# authored page under content/ changes, and `ddc serve` watches .rendered
# and live-reloads the browser. Edit content/; the rendered charts and the
# page refresh on save.
set -euo pipefail
cd "$(dirname "$0")"

echo "==> initial render (content → .rendered)"
cargo run -q -p keyflow-cli -- docs --input content --output .rendered

echo "==> watching content for kf changes"
cargo run -q -p keyflow-cli -- docs --input content --output .rendered --watch &
WATCH_PID=$!
trap 'kill "$WATCH_PID" 2>/dev/null || true' EXIT

echo "==> ddc serve (http://localhost:8080)"
ddc serve

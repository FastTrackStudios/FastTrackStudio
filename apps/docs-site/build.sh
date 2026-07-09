#!/usr/bin/env bash
# Build the unified FTS docsite (docs.fasttrackstudio.app) for production.
#
# Two stages, both required: `kf docs` renders every ```kf``` fenced block in
# content/ into inline SVG under .rendered (a generated mirror tree), then
# stock dodeca (`ddc`) builds that into output/. dodeca is never pointed at
# the raw authored content — only at the rendered tree.
set -euo pipefail
cd "$(dirname "$0")"

echo "==> rendering kf blocks (content → .rendered)"
cargo run -q -p keyflow-cli -- docs --input content --output .rendered

echo "==> dodeca build"
ddc build

echo "==> done → output"

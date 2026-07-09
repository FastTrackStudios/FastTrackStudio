#!/usr/bin/env bash
# Build and deploy the docsite to fly.io (app: fts-docs).
#
# One-time setup:
#   fly apps create fts-docs
#   fly certs add docs.fasttrackstudio.app -a fts-docs
#   → then point Cloudflare DNS at fts-docs.fly.dev (see fly certs show)
#
# keyflow.fasttrackstudio.app (the old per-project docsite) should redirect
# here — see README.md.
set -euo pipefail
cd "$(dirname "$0")"

./build.sh
fly deploy

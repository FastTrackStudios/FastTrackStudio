#!/usr/bin/env bash
set -e
# Build the WASM plugin for browser/Obsidian (web target so init() accepts Uint8Array)
wasm-pack build --target web --out-dir pkg --out-name task_vault_core

#!/usr/bin/env bash
# Install architect's git hooks: pre-commit (fmt + tracey) and pre-push
# (clippy + nextest). Idempotent — re-run any time.

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
hooks_dir="$repo_root/.git/hooks"
src_dir="$repo_root/hooks"

mkdir -p "$hooks_dir"

for hook in pre-commit pre-push; do
    src="$src_dir/$hook"
    dst="$hooks_dir/$hook"
    if [[ ! -f "$src" ]]; then
        continue
    fi
    install -m755 "$src" "$dst"
    echo "installed $hook → $dst"
done

echo
echo "hooks installed. Bypass with --no-verify if needed."

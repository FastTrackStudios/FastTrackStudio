#!/usr/bin/env bash
# Sync docs/content/ → the Forgejo wiki repo for this project.
#
# - Strips the TOML frontmatter (`+++ ... +++` block) so the wiki shows
#   clean prose, while leaving the source files untouched for dodeca.
# - Mirrors the directory layout into the wiki (Forgejo wiki supports
#   subdirectories, so `architecture/pattern.md` → `Architecture/Pattern.md`).
# - `_index.md` becomes the section's landing page: top-level becomes
#   `Home.md`, nested becomes `<Section>/Home.md`.
#
# Auth: pass a Forgejo token via FORGEJO_TOKEN env var, OR rely on
# system git credentials. The CI workflow injects the token.
#
# Usage:
#   FORGEJO_TOKEN=... ./scripts/sync-wiki.sh
#   FORGEJO_TOKEN=... ./scripts/sync-wiki.sh --dry-run
#
# Configure WIKI_URL to point at a different wiki repo if needed.

set -euo pipefail

WIKI_URL="${WIKI_URL:-https://git.starcommand.live/codywright/architect.wiki.git}"
WIKI_USER="${WIKI_USER:-architect}"
SRC_DIR="${SRC_DIR:-docs/content}"
DRY_RUN=0
WORK_DIR="$(mktemp -d)"

trap 'rm -rf "$WORK_DIR"' EXIT

for arg in "$@"; do
    case "$arg" in
        --dry-run) DRY_RUN=1 ;;
        *) echo "unknown arg: $arg" >&2; exit 1 ;;
    esac
done

if [[ ! -d "$SRC_DIR" ]]; then
    echo "no $SRC_DIR — run from repo root" >&2
    exit 1
fi

# Clone the wiki. Use askpass when a token is provided.
if [[ -n "${FORGEJO_TOKEN:-}" ]]; then
    askpass="$WORK_DIR/askpass.sh"
    cat > "$askpass" <<EOF
#!/bin/sh
case "\$1" in
  Username*) echo "$WIKI_USER" ;;
  Password*) echo "\$ARCHITECT_WIKI_TOKEN_INNER" ;;
esac
EOF
    chmod 700 "$askpass"
    ARCHITECT_WIKI_TOKEN_INNER="$FORGEJO_TOKEN" GIT_ASKPASS="$askpass" \
        git clone "$WIKI_URL" "$WORK_DIR/wiki"
else
    git clone "$WIKI_URL" "$WORK_DIR/wiki"
fi

cd "$WORK_DIR/wiki"

# Wipe stale pages — anything not regenerated below is removed.
# Keep `.git` and Forgejo's special sidebar/footer files if they exist.
find . -mindepth 1 -maxdepth 1 \
    -name '.git' -prune -o \
    -name '_Sidebar.md' -prune -o \
    -name '_Footer.md' -prune -o \
    -exec rm -rf {} +

# Render: for each markdown file in SRC_DIR, strip frontmatter, place
# at the wiki's mirrored path.
src="$OLDPWD/$SRC_DIR"
while IFS= read -r -d '' file; do
    rel="${file#$src/}"
    # Convert path segments to title-case for prettier wiki URLs.
    # docs/content/_index.md       → Home.md
    # docs/content/getting-started/_index.md → Getting-Started/Home.md
    # docs/content/architecture/pattern.md   → Architecture/Pattern.md
    out=""
    IFS='/' read -ra segs <<< "$rel"
    n=${#segs[@]}
    for (( i=0; i<n; i++ )); do
        seg="${segs[i]}"
        # The leaf (last segment) gets capitalized; _index.md becomes Home.md
        if (( i == n - 1 )); then
            case "$seg" in
                _index.md) seg="Home.md" ;;
                *) seg="$(echo "${seg%.md}" | sed -E 's/(^|-)([a-z])/\1\u\2/g; s/-/-/g').md" ;;
            esac
        else
            # Section dirs: kebab-to-Title-Case
            seg="$(echo "$seg" | sed -E 's/(^|-)([a-z])/\1\u\2/g')"
        fi
        out+="${seg}"
        (( i < n - 1 )) && out+="/"
    done

    mkdir -p "$(dirname "$out")"
    # Strip leading `+++ ... +++` frontmatter (TOML block, dodeca format).
    awk '
        BEGIN { in_fm = 0; done = 0 }
        !done && NR == 1 && /^\+\+\+[[:space:]]*$/ { in_fm = 1; next }
        in_fm && /^\+\+\+[[:space:]]*$/ { in_fm = 0; done = 1; next }
        in_fm { next }
        # Rewrite dodeca @/section/page.md links to wiki-friendly paths.
        { gsub(/\(@\/([^)]+)\.md\)/, "(\\1)"); print }
    ' "$file" > "$out"
done < <(find "$src" -type f -name '*.md' -print0)

# Show what changed.
git add -A
if git diff --cached --quiet; then
    echo "wiki already in sync"
    exit 0
fi

if (( DRY_RUN )); then
    echo "--- dry run, would commit: ---"
    git diff --cached --stat
    exit 0
fi

git -c user.name="$WIKI_USER" -c user.email="$WIKI_USER@architect" \
    commit -m "sync: regenerate wiki from docs/content/"

if [[ -n "${FORGEJO_TOKEN:-}" ]]; then
    ARCHITECT_WIKI_TOKEN_INNER="$FORGEJO_TOKEN" GIT_ASKPASS="$WORK_DIR/askpass.sh" \
        git push origin HEAD
else
    git push origin HEAD
fi

echo "wiki updated"

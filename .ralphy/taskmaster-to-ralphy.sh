#!/usr/bin/env bash
# Converts Task Master tasks.json to Ralphy-compatible markdown
# Usage: .ralphy/taskmaster-to-ralphy.sh [output-file]

set -euo pipefail

TASKS_FILE=".taskmaster/tasks/tasks.json"
OUTPUT_FILE="${1:-.ralphy/tasks.md}"

if [[ ! -f "$TASKS_FILE" ]]; then
    echo "Error: $TASKS_FILE not found. Run 'taskmaster parse-prd' first."
    exit 1
fi

echo "# Tasks from Task Master" > "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "Generated from: $TASKS_FILE" >> "$OUTPUT_FILE"
echo "Date: $(date -Iseconds)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Parse tasks.json and convert to markdown checkboxes
jq -r '.tasks[] |
    if .status == "done" then
        "- [x] " + .title + "\n  " + .description
    else
        "- [ ] " + .title + "\n  " + .description
    end' "$TASKS_FILE" >> "$OUTPUT_FILE"

echo ""
echo "✓ Exported tasks to $OUTPUT_FILE"
echo "  Run: ralphy --prd $OUTPUT_FILE"

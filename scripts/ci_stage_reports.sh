#!/usr/bin/env bash
set -euo pipefail

SRC_DIR="${1:-build/reports}"
DEST_DIR="${2:-${CI_REPORT_DIR:-}}"

if [ -z "$DEST_DIR" ]; then
  echo "CI_REPORT_DIR is not set; skipping report staging."
  exit 0
fi

if [ ! -d "$SRC_DIR" ]; then
  exit 0
fi

mkdir -p "$DEST_DIR"

if command -v rsync >/dev/null 2>&1; then
  rsync -a \
    --exclude 'agda' \
    --exclude 'docs' \
    --exclude 'roadmap' \
    --exclude 'python' \
    "$SRC_DIR"/ "$DEST_DIR"/
else
  (cd "$SRC_DIR" && tar \
      --exclude='agda' \
      --exclude='docs' \
      --exclude='roadmap' \
      --exclude='python' \
      -cf - .) | tar -C "$DEST_DIR" -xf -
fi

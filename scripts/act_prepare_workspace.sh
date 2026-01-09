#!/usr/bin/env bash
set -euo pipefail

dest="${1:-/tmp/act-workdir}"
src="${2:-$(pwd)}"

rm -rf "$dest"
mkdir -p "$dest"
tar -cf - -C "$src" . | (cd "$dest" && tar -xf -)

if [ -n "${GITHUB_ENV:-}" ]; then
  echo "WORKDIR=$dest" >> "$GITHUB_ENV"
fi

#!/usr/bin/env bash
set -euo pipefail

dest="${1:-/tmp/act-workdir}"
src="${2:-$(pwd)}"

rm -rf "$dest"
mkdir -p "$dest"
tar -cf - \
  --exclude=.coverage \
  --exclude=.mypy_cache \
  --exclude=.pytest_cache \
  --exclude=.ruff_cache \
  --exclude=.venv \
  --exclude=__pycache__ \
  --exclude=*.pyc \
  --exclude=*.agdai \
  --exclude=node_modules \
  --exclude=build \
  -C "$src" . | (cd "$dest" && tar -xf -)

if [ -n "${GITHUB_ENV:-}" ]; then
  echo "BUILD_WORKDIR=$dest" >> "$GITHUB_ENV"
  echo "WORKDIR=$dest" >> "$GITHUB_ENV"
fi

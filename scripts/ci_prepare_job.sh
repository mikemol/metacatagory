#!/usr/bin/env bash
set -euo pipefail

WORKDIR="${BUILD_WORKDIR:-.}"

cd "$WORKDIR"

if [ "${CI_SKIP_REGEN_MAKEFILE:-0}" != "1" ]; then
  bash scripts/recipes/regen-makefile.sh
fi
python -m pip install --upgrade pip
pip install -r requirements.txt

test -f data/planning_index.json
test -f data/dependency_graph.json

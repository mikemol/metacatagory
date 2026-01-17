#!/usr/bin/env bash
set -euo pipefail

WORKDIR="${BUILD_WORKDIR:-.}"

cd "$WORKDIR"

bash scripts/recipes/regen-makefile.sh
python -m pip install --upgrade pip
pip install -r requirements.txt

test -f data/planning_index.json
test -f data/dependency_graph.json

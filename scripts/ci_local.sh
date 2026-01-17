#!/usr/bin/env bash
set -euo pipefail

WORKDIR="${BUILD_WORKDIR:-.}"
cd "$WORKDIR"

MUTATE_LEVEL=repo bash scripts/recipes/regen-makefile.sh
MUTATE_LEVEL=repo make check-makefile-generated

MUTATE_LEVEL=repo make \
  build/diagrams/agda-deps-full.dot \
  data/planning_index.json \
  data/dependency_graph.json \
  graph-assert-ok \
  makefile-validate

MUTATE_LEVEL=repo python3 -m scripts.makefile_coverage --run-targets

CI_SKIP_REGEN_MAKEFILE=1 bash scripts/ci_prepare_job.sh

MUTATE_LEVEL=repo make check-docs
MUTATE_LEVEL=repo make check-roadmap
MUTATE_LEVEL=repo make check-json
MUTATE_LEVEL=repo make check-python
MUTATE_LEVEL=repo make check-debt

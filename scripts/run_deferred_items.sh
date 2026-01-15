#!/usr/bin/env bash
set -euo pipefail

BUILD_WORKDIR="${BUILD_WORKDIR:-.}"
if [ -n "${WORKDIR:-}" ]; then BUILD_WORKDIR="$WORKDIR"; fi
if [ -n "${ACT_WORKDIR:-}" ]; then BUILD_WORKDIR="$ACT_WORKDIR"; fi

cd "$BUILD_WORKDIR"

AGDA_COMPILE_DIR="${AGDA_COMPILE_DIR:-build/agda}"
"$AGDA_COMPILE_DIR/DeferredItemsOrchestrationFFI"

REPORT_ROOT="build/reports"
CORE_LIST="$REPORT_ROOT/deferred-core-algebra-modules.md"
core_count="$(rg -l -g '*.agda' -e 'postulate|TODO|FIXME|PLANNED|DeviationLog' src/agda/Core | wc -l | tr -d ' ')"
alg_count="$(rg -l -g '*.agda' -e 'postulate|TODO|FIXME|PLANNED|DeviationLog' src/agda/Algebra | wc -l | tr -d ' ')"
{
  printf "%s\n\n" "# Deferred Core/Algebra Modules"
  printf "%s\n\n" "Generated from TODO/postulate/FIXME/PLANNED/DeviationLog markers."
  printf "## Core (%s)\n" "$core_count"
  rg -l -g '*.agda' -e 'postulate|TODO|FIXME|PLANNED|DeviationLog' src/agda/Core | sort | sed 's/^/ - /'
  printf "\n## Algebra (%s)\n" "$alg_count"
  rg -l -g '*.agda' -e 'postulate|TODO|FIXME|PLANNED|DeviationLog' src/agda/Algebra | sort | sed 's/^/ - /'
} > "$CORE_LIST"

if [ -n "${CI_REPORT_DIR:-}" ]; then
  mkdir -p "$CI_REPORT_DIR"
  for report in deferred-items.md deferred-summary.json deferred-core-algebra-modules.md; do
    if [ -f "$REPORT_ROOT/$report" ]; then
      cp "$REPORT_ROOT/$report" "$CI_REPORT_DIR/$report"
    fi
  done
fi

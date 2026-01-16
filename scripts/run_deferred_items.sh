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

search_agda_markers() {
  local target="$1"
  if command -v rg >/dev/null 2>&1; then
    rg -l -g '*.agda' -e 'postulate|TODO|FIXME|PLANNED|DeviationLog' "$target" || true
  else
    grep -R -l --include='*.agda' -E 'postulate|TODO|FIXME|PLANNED|DeviationLog' "$target" || true
  fi
}

core_files="$(search_agda_markers src/agda/Core)"
alg_files="$(search_agda_markers src/agda/Algebra)"
core_count="$(printf "%s\n" "$core_files" | sed '/^$/d' | wc -l | tr -d ' ')"
alg_count="$(printf "%s\n" "$alg_files" | sed '/^$/d' | wc -l | tr -d ' ')"
{
  printf "%s\n\n" "# Deferred Core/Algebra Modules"
  printf "%s\n\n" "Generated from TODO/postulate/FIXME/PLANNED/DeviationLog markers."
  printf "## Core (%s)\n" "$core_count"
  printf "%s\n" "$core_files" | sed '/^$/d' | sort | sed 's/^/ - /'
  printf "\n## Algebra (%s)\n" "$alg_count"
  printf "%s\n" "$alg_files" | sed '/^$/d' | sort | sed 's/^/ - /'
} > "$CORE_LIST"

if [ -n "${CI_REPORT_DIR:-}" ]; then
  mkdir -p "$CI_REPORT_DIR"
  for report in deferred-items.md deferred-summary.json deferred-core-algebra-modules.md; do
    if [ -f "$REPORT_ROOT/$report" ]; then
      cp "$REPORT_ROOT/$report" "$CI_REPORT_DIR/$report"
    fi
  done
fi

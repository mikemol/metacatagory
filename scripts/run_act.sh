#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ACT_BASE="${ACT_TMP_BASE:-${ACT_TMP:-$REPO_ROOT/build/act-workdir}}"
ACT_CACHE_DIR="${ACT_CACHE_DIR:-$REPO_ROOT/build/act-cache}"
ACT_TMP="$(mktemp -d "${ACT_BASE}.XXXXXX")"
ACT_TMP_HOST="${ACT_TMP_HOST:-$REPO_ROOT/build/act-tmp}"
ACT_TMP_CONTAINER="${ACT_TMP_CONTAINER:-/tmp}"

"$SCRIPT_DIR/act_prepare_workspace.sh" "$ACT_TMP" "$REPO_ROOT"

. "$SCRIPT_DIR/ensure_rootless_docker.sh"

mkdir -p "$ACT_CACHE_DIR" "$ACT_TMP_HOST"
TMPDIR="${TMPDIR:-$ACT_TMP_HOST}"
export TMPDIR

ACT_NOOP_ACTION="$REPO_ROOT/.github/actions/act-noop"
act_args=(
  --action-cache-path "$ACT_CACHE_DIR"
  --bind
  --container-options "-u 0:0"
  --local-repository "actions/upload-artifact@v4=$ACT_NOOP_ACTION"
  --local-repository "actions/download-artifact@v4=$ACT_NOOP_ACTION"
  --local-repository "actions/checkout@v4=$ACT_NOOP_ACTION"
)
if [ -n "${MUTATE_OK:-}" ]; then
  act_args+=(--env "MUTATE_OK=$MUTATE_OK")
fi
act_args+=(--env "TMPDIR=$ACT_TMP_CONTAINER")
act_args+=(--env "LANG=C.utf8")
act_args+=(--env "LC_ALL=C.utf8")

ACT_WORKDIR="$ACT_TMP" WORKDIR="$ACT_TMP" act -C "$ACT_TMP" "${act_args[@]}" "$@"

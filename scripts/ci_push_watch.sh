#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/ci_push_watch.sh [--workflow NAME] [--branch NAME]

Pushes current branch to origin, then watches the latest workflow run
using scripts/ci_watch.sh.
USAGE
}

workflow_name="CI"
branch_name=""
wait_seconds="${CI_WATCH_DELAY:-5}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --workflow)
      workflow_name="$2"
      shift 2
      ;;
    --branch)
      branch_name="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

if [[ -z "$branch_name" ]]; then
  branch_name=$(git rev-parse --abbrev-ref HEAD)
fi

if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "Refusing to push: working tree has uncommitted changes." >&2
  exit 1
fi

git push origin "$branch_name"
sleep "$wait_seconds"
exec scripts/ci_watch.sh --workflow "$workflow_name" --branch "$branch_name"

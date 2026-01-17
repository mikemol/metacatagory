#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/ci_watch.sh [--push] [--workflow NAME] [--branch NAME] [--run-id ID]

Automates: (optional) push, find latest workflow run, wait for completion,
and emit logs if the run fails.

Options:
  --push            Push current branch to origin before watching.
  --workflow NAME   Workflow name (default: CI).
  --branch NAME     Branch name (default: current branch).
  --run-id ID       Use a specific run ID instead of discovering one.
USAGE
}

push_first=false
workflow_name="CI"
branch_name=""
run_id=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --push)
      push_first=true
      shift
      ;;
    --workflow)
      workflow_name="$2"
      shift 2
      ;;
    --branch)
      branch_name="$2"
      shift 2
      ;;
    --run-id)
      run_id="$2"
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

if $push_first; then
  if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "Refusing to push: working tree has uncommitted changes." >&2
    exit 1
  fi
  git push origin "$branch_name"
fi

if [[ -z "$run_id" ]]; then
  run_id=$(gh run list \
    --workflow "$workflow_name" \
    --branch "$branch_name" \
    --limit 1 \
    --json databaseId \
    --jq '.[0].databaseId')
fi

if [[ -z "$run_id" || "$run_id" == "null" ]]; then
  echo "No run found for workflow '$workflow_name' on branch '$branch_name'." >&2
  exit 1
fi

echo "Watching run $run_id ($workflow_name on $branch_name)..."
if ! gh run watch "$run_id" --exit-status; then
  echo "Run failed. Fetching failed logs..." >&2
  gh run view "$run_id" --log-failed
  exit 1
fi

echo "Run succeeded."

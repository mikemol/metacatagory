#!/usr/bin/env bash
set -euo pipefail

# sync-roadmap-issues.sh
# Creates or updates GitHub issues for roadmap tasks defined in .github/roadmap/tasks.json
# Requires: env GITHUB_TOKEN; runs inside GitHub Actions or locally (set GITHUB_REPOSITORY=owner/repo)

TASKS_FILE="${TASKS_FILE:-.github/roadmap/tasks.json}"
REPO="${GITHUB_REPOSITORY:-}"
API_ROOT="https://api.github.com"
LABEL_ROADMAP="roadmap"

if [[ -z "${REPO}" ]]; then
  echo "GITHUB_REPOSITORY not set (expected owner/repo)" >&2
  exit 1
fi
if [[ -z "${GITHUB_TOKEN:-}" ]]; then
  echo "GITHUB_TOKEN not set" >&2
  exit 1
fi
if [[ ! -f "$TASKS_FILE" ]]; then
  echo "Tasks file not found: $TASKS_FILE" >&2
  exit 1
fi

auth_header="Authorization: Bearer ${GITHUB_TOKEN}"
accept_header="Accept: application/vnd.github+json"

# Ensure roadmap label exists
echo "Ensuring label '$LABEL_ROADMAP' exists"
curl -s -X POST -H "$auth_header" -H "$accept_header" \
  -H 'Content-Type: application/json' \
  "$API_ROOT/repos/$REPO/labels" \
  -d '{"name":"'$LABEL_ROADMAP'","color":"0e8a16","description":"MetaCategory roadmap task"}' >/dev/null || true

echo "Fetching existing roadmap issues"
existing=$(curl -s -H "$auth_header" -H "$accept_header" "$API_ROOT/repos/$REPO/issues?state=open&labels=$LABEL_ROADMAP")

create_issue() {
  local id="$1" title="$2" status="$3" source="$4" files_json="$5" tags_json="$6"
  local issue_title="[Roadmap] $id - $title"
  local body
  body=$(jq -r -n \
    --arg id "$id" \
    --arg title "$title" \
    --arg status "$status" \
    --arg source "$source" \
    --arg files "$files_json" \
    --arg tags "$tags_json" '"### Roadmap Task\n\nID: \($id)\nStatus: \($status)\nSource: \($source)\n\nFiles: \($files)\nTags: \($tags)\n\nThis issue was auto-generated. Update status by editing tasks.json and re-running sync."')
  curl -s -X POST -H "$auth_header" -H "$accept_header" -H 'Content-Type: application/json' \
    "$API_ROOT/repos/$REPO/issues" \
    -d "{\"title\":\"$issue_title\",\"body\":$(jq -Rs . <<< "$body"),\"labels\":[\"$LABEL_ROADMAP\"]}" >/dev/null
  echo "Created issue: $issue_title"
}

update_issue() {
  local number="$1" id="$2" title="$3" status="$4" source="$5" files_json="$6" tags_json="$7"
  local issue_title="[Roadmap] $id - $title"
  local body
  body=$(jq -r -n \
    --arg id "$id" \
    --arg title "$title" \
    --arg status "$status" \
    --arg source "$source" \
    --arg files "$files_json" \
    --arg tags "$tags_json" '"### Roadmap Task (Updated)\n\nID: \($id)\nStatus: \($status)\nSource: \($source)\n\nFiles: \($files)\nTags: \($tags)\n\nEdit tasks.json and re-run sync to change."')
  curl -s -X PATCH -H "$auth_header" -H "$accept_header" -H 'Content-Type: application/json' \
    "$API_ROOT/repos/$REPO/issues/$number" \
    -d "{\"title\":\"$issue_title\",\"body\":$(jq -Rs . <<< "$body")}" >/dev/null
  echo "Updated issue #$number: $issue_title"
}

echo "Processing tasks"
jq -c '.[]' "$TASKS_FILE" | while read -r task; do
  id=$(jq -r '.id' <<< "$task")
  title=$(jq -r '.title' <<< "$task")
  status=$(jq -r '.status' <<< "$task")
  source=$(jq -r '.source' <<< "$task")
  files_json=$(jq -r '.files | join(", ")' <<< "$task")
  tags_json=$(jq -r '.tags | join(", ")' <<< "$task")
  issue_title="[Roadmap] $id - $title"
  existing_number=$(jq -r --arg t "$issue_title" '.[] | select(.title == $t) | .number' <<< "$existing" | head -n1 || true)

  if [[ -z "$existing_number" || "$existing_number" == "null" ]]; then
    create_issue "$id" "$title" "$status" "$source" "$files_json" "$tags_json"
  else
    update_issue "$existing_number" "$id" "$title" "$status" "$source" "$files_json" "$tags_json"
  fi
done

echo "Roadmap sync complete"

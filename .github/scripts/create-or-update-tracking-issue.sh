#!/bin/bash
# create-or-update-tracking-issue.sh
# Creates or updates a GitHub issue to track deferred items

set -e

REPORT_FILE="${1:-deferred-items.md}"
SUMMARY_FILE="${2:-deferred-summary.json}"
ISSUE_TITLE="🔍 Deferred Items Tracking"
ISSUE_LABEL="deferred-tracking"

if [ ! -f "$REPORT_FILE" ] || [ ! -f "$SUMMARY_FILE" ]; then
    echo "Error: Report or summary file not found"
    exit 1
fi

# Read summary
TOTAL=$(jq -r '.total' "$SUMMARY_FILE")
DEVIATION_LOG=$(jq -r '.deviation_log' "$SUMMARY_FILE")
POSTULATES=$(jq -r '.postulates' "$SUMMARY_FILE")
TODO=$(jq -r '.todo' "$SUMMARY_FILE")
PLANNED=$(jq -r '.planned' "$SUMMARY_FILE")
FIXME=$(jq -r '.fixme' "$SUMMARY_FILE")
TIMESTAMP=$(jq -r '.timestamp' "$SUMMARY_FILE")

# Create issue body
ISSUE_BODY=$(cat <<EOF
This issue tracks deferred items in the codebase that need attention.

**Last Updated:** $TIMESTAMP  
**Commit:** $GITHUB_SHA

## Quick Stats

| Category | Count |
|----------|-------|
| DeviationLog | $DEVIATION_LOG |
| Postulates | $POSTULATES |
| TODO | $TODO |
| PLANNED | $PLANNED |
| FIXME | $FIXME |
| **Total** | **$TOTAL** |

## Actions Required

- Review DeviationLog entries and create targeted issues for resolution
- Evaluate postulates and replace with constructive proofs where possible
- Address TODO items based on priority
- Complete PLANNED work items according to phase schedule
- Resolve FIXME items as blocking issues

## Detailed Report

<details>
<summary>Click to expand full report</summary>

$(cat "$REPORT_FILE")

</details>

---

*This issue is automatically updated by CI. Do not close manually - it will reopen on next run if deferred items remain.*
EOF
)

# Check if tracking issue already exists
echo "Checking for existing tracking issue..."
EXISTING_ISSUE=$(gh issue list --label "$ISSUE_LABEL" --state open --json number,title --jq ".[] | select(.title == \"$ISSUE_TITLE\") | .number" || echo "")

if [ -n "$EXISTING_ISSUE" ]; then
    echo "Found existing issue #$EXISTING_ISSUE, updating..."
    gh issue comment "$EXISTING_ISSUE" --body "$ISSUE_BODY"
    echo "✅ Updated issue #$EXISTING_ISSUE"
else
    echo "No existing issue found, creating new one..."
    NEW_ISSUE=$(gh issue create \
        --title "$ISSUE_TITLE" \
        --body "$ISSUE_BODY" \
        --label "$ISSUE_LABEL,enhancement" \
        | grep -oP '(?<=issues/)\d+')
    echo "✅ Created new issue #$NEW_ISSUE"
fi

#!/bin/bash
# detect-deferred-items.sh
# Scans codebase for deferred items (DeviationLog, postulates, TODOs, PLANNED)
# and generates a report for GitHub Actions

set -e

OUTPUT_FILE="${1:-deferred-items.md}"
SUMMARY_FILE="deferred-summary.json"

# Color codes for terminal output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "# Deferred Items Report" > "$OUTPUT_FILE"
echo "Generated on: $(date -u +"%Y-%m-%d %H:%M:%S UTC")" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Initialize counters
DEVIATION_LOG_COUNT=0
POSTULATE_COUNT=0
TODO_COUNT=0
PLANNED_COUNT=0
FIXME_COUNT=0

# Function to search and format results
search_pattern() {
    local pattern="$1"
    local label="$2"
    local counter_var="$3"
    
    echo "## $label" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    
    # Search for pattern in .agda and .md files
    local results=$(grep -rn --include="*.agda" --include="*.md" "$pattern" src/ testing.md README.md 2>/dev/null || true)
    
    if [ -z "$results" ]; then
        echo "✅ No $label found." >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
        return 0
    fi
    
    local count=$(echo "$results" | wc -l)
    eval "$counter_var=$count"
    
    echo "Found **$count** instances:" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    
    # Format results as markdown list with file links
    echo "$results" | while IFS=: read -r file line content; do
        # Trim whitespace from content
        content=$(echo "$content" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        echo "- \`$file:$line\` — \`$content\`" >> "$OUTPUT_FILE"
    done
    
    echo "" >> "$OUTPUT_FILE"
}

# Search for different types of deferred items
echo -e "${YELLOW}Scanning for DeviationLog entries...${NC}"
search_pattern "DeviationLog" "DeviationLog Entries" "DEVIATION_LOG_COUNT"

echo -e "${YELLOW}Scanning for postulates...${NC}"
search_pattern "^[[:space:]]*postulate" "Postulates" "POSTULATE_COUNT"

echo -e "${YELLOW}Scanning for TODO items...${NC}"
search_pattern "TODO" "TODO Items" "TODO_COUNT"

echo -e "${YELLOW}Scanning for PLANNED items...${NC}"
search_pattern "PLANNED" "PLANNED Items" "PLANNED_COUNT"

echo -e "${YELLOW}Scanning for FIXME items...${NC}"
search_pattern "FIXME" "FIXME Items" "FIXME_COUNT"

# Add summary section
TOTAL=$((DEVIATION_LOG_COUNT + POSTULATE_COUNT + TODO_COUNT + PLANNED_COUNT + FIXME_COUNT))

echo "## Summary" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "| Category | Count |" >> "$OUTPUT_FILE"
echo "|----------|-------|" >> "$OUTPUT_FILE"
echo "| DeviationLog | $DEVIATION_LOG_COUNT |" >> "$OUTPUT_FILE"
echo "| Postulates | $POSTULATE_COUNT |" >> "$OUTPUT_FILE"
echo "| TODO | $TODO_COUNT |" >> "$OUTPUT_FILE"
echo "| PLANNED | $PLANNED_COUNT |" >> "$OUTPUT_FILE"
echo "| FIXME | $FIXME_COUNT |" >> "$OUTPUT_FILE"
echo "| **Total** | **$TOTAL** |" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Generate JSON summary for programmatic access
cat > "$SUMMARY_FILE" <<EOF
{
  "total": $TOTAL,
  "deviation_log": $DEVIATION_LOG_COUNT,
  "postulates": $POSTULATE_COUNT,
  "todo": $TODO_COUNT,
  "planned": $PLANNED_COUNT,
  "fixme": $FIXME_COUNT,
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
}
EOF

# Print summary to console
echo ""
echo -e "${GREEN}=== Deferred Items Summary ===${NC}"
echo -e "DeviationLog entries: ${YELLOW}$DEVIATION_LOG_COUNT${NC}"
echo -e "Postulates: ${YELLOW}$POSTULATE_COUNT${NC}"
echo -e "TODO items: ${YELLOW}$TODO_COUNT${NC}"
echo -e "PLANNED items: ${YELLOW}$PLANNED_COUNT${NC}"
echo -e "FIXME items: ${YELLOW}$FIXME_COUNT${NC}"
echo -e "Total: ${YELLOW}$TOTAL${NC}"
echo ""
echo -e "${GREEN}Report saved to: $OUTPUT_FILE${NC}"
echo -e "${GREEN}Summary saved to: $SUMMARY_FILE${NC}"

# Set GitHub Actions output if running in CI
if [ -n "$GITHUB_OUTPUT" ]; then
    echo "total=$TOTAL" >> "$GITHUB_OUTPUT"
    echo "deviation_log=$DEVIATION_LOG_COUNT" >> "$GITHUB_OUTPUT"
    echo "postulates=$POSTULATE_COUNT" >> "$GITHUB_OUTPUT"
    echo "todo=$TODO_COUNT" >> "$GITHUB_OUTPUT"
    echo "planned=$PLANNED_COUNT" >> "$GITHUB_OUTPUT"
    echo "fixme=$FIXME_COUNT" >> "$GITHUB_OUTPUT"
fi

exit 0

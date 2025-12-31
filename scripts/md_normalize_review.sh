#!/usr/bin/env bash
set -euo pipefail

# Interactive per-file markdown normalization with before/after diff review.
# For each file:
# - Stage current content (index = baseline)
# - Run normalizer on working tree
# - Show diff (index vs working tree)
# - Prompt to accept (stage new content) or revert (restore staged content)
# - Supports 'accept all' mode after first prompt

ROOT_DIR="$(pwd)"
NORMALIZER="${ROOT_DIR}/src/agda/MarkdownNormalize"

# Build normalizer if missing
if [[ ! -x "$NORMALIZER" ]]; then
  echo "Building normalizer..."
  make src/agda/MarkdownNormalize
fi

# Collect files: use args if provided, else discover *.md excluding node_modules
FILES=()
if [[ $# -gt 0 ]]; then
  for arg in "$@"; do
    if [[ -f "$arg" ]]; then
      FILES+=("$arg")
    else
      echo "Warning: not a file: $arg" >&2
    fi
  done
else
  mapfile -d '' FILES < <(find . -type f -name "*.md" -not -path "./node_modules/*" -print0 | sort -z)
fi

if [[ ${#FILES[@]} -eq 0 ]]; then
  echo "No markdown files to process."
  exit 0
fi

accept_all=0
for f in "${FILES[@]}"; do
  # Skip if binary or unreadable
  if [[ ! -r "$f" ]]; then
    echo "Skipping unreadable file: $f"
    continue
  fi

  echo "\n===== Normalizing: $f ====="
  # Stage current content as baseline
  git add -- "$f" || true

  # Run normalizer
  "$NORMALIZER" "$f"

  # Show diff (index vs working tree)
  echo "--- Diff (staged vs working): $f ---"
  git --no-pager diff -- "$f" || true

  # If no changes, continue
  if git --no-pager diff --quiet -- "$f"; then
    echo "No changes; moving on."
    continue
  fi

  if [[ $accept_all -eq 1 ]]; then
    git add -- "$f"
    echo "Accepted (auto)."
    continue
  fi

  # Prompt
  while true; do
    read -r -p "Accept changes? [y]es/[n]o/[a]ccept-all/[q]uit: " resp
    case "$resp" in
      y|Y)
        git add -- "$f"
        echo "Accepted."
        break
        ;;
      a|A)
        git add -- "$f"
        accept_all=1
        echo "Accepted and will auto-accept remaining files."
        break
        ;;
      n|N)
        # Restore working tree from index (pre-normalized)
        git checkout -- "$f"
        echo "Reverted to pre-normalized content."
        break
        ;;
      q|Q)
        echo "Quitting at user request."
        exit 0
        ;;
      *)
        echo "Please answer y/n/a/q."
        ;;
    esac
  done

done

echo "\nReview complete. Stage contains accepted changes."

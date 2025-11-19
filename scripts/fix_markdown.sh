#!/usr/bin/env bash
set -euo pipefail

# Auto-format markdown using prettier + remark lint (optional).
# Requires: node (>=18), prettier, remark-cli, remark-preset-lint-consistent.

if ! command -v node >/dev/null 2>&1; then
  echo "Node.js required for auto-formatting" >&2
  exit 1
fi

if [ ! -f package.json ]; then
  cat > package.json <<'JSON'
{
  "name": "metacatagory-markdown-tools",
  "private": true,
  "devDependencies": {
    "prettier": "^3.1.0",
    "remark-cli": "^11.0.0",
    "remark-preset-lint-consistent": "^5.1.2"
  },
  "scripts": {
    "fmt": "prettier --write '**/*.md'",
    "lint": "remark . --frail"
  }
}
JSON
  npm install --no-audit --no-fund
fi

echo "Running prettier formatting..."
npx prettier --write '**/*.md'

echo "Running remark lint (non-fix)..."
npx remark . --frail || {
  echo "Remark lint found issues (non-auto-fix)." >&2
  exit 1
}

echo "Markdown formatting complete." 

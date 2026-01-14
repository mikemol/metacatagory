
#!/bin/bash
# Export CNF grammar from Agda module to JSON file using IO main

set -euo pipefail

AGDA_MODULE="src/agda/Plan/CIM/GrammarBridge.agda"
OUTPUT_JSON="sample_cnf_grammar.json"

# Compile the Agda module (main will write the JSON file)
BASE_WORKDIR="${BUILD_WORKDIR:-${WORKDIR:-.}}"
XDG_DATA_HOME="${XDG_DATA_HOME:-${BASE_WORKDIR}/build/xdg-data}"
XDG_CACHE_HOME="${XDG_CACHE_HOME:-${BASE_WORKDIR}/build/xdg-cache}"
AGDA_COMPILE_DIR="${AGDA_COMPILE_DIR:-${BASE_WORKDIR}/build/agda}"
env XDG_DATA_HOME="$XDG_DATA_HOME" XDG_CACHE_HOME="$XDG_CACHE_HOME" \
  agda --compile-dir="$AGDA_COMPILE_DIR" --compile --include-path=src/agda "$AGDA_MODULE"

# Run the compiled executable to produce the JSON file
"$AGDA_COMPILE_DIR/Plan/CIM/GrammarBridge"

echo "CNF grammar exported to $OUTPUT_JSON via Agda IO main."

#!/usr/bin/env python3
"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: .github/copilot-instructions.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.
"""

import sys
import json
import subprocess
from pathlib import Path
from typing import Any

from scripts.shared.io import load_json_text, save_json
from scripts.shared.markdown import (
    is_json_input,
    fix_list_markers,
    remove_multiple_blank_lines,
    fix_horizontal_rules,
    fix_headings,
    postprocess_markdown,
)

# Read Pandoc JSON from stdin
# pandoc_json = json.load(sys.stdin)

# Helper to recursively fix list marker spacing and style
# (asterisk, single space, indent=2)

# Pandoc AST: fix unordered list style, spacing, indentation
def fix_lists(obj: Any, indent: int = 0) -> Any:
    if isinstance(obj, dict):
        # Fix unordered lists
        if obj.get("t") == "BulletList":
            # Each item is a list of blocks
            obj["c"] = [fix_lists(item, indent + 2) for item in obj["c"]]
        else:
            for k, v in obj.items():
                obj[k] = fix_lists(v, indent)
    elif isinstance(obj, list):
        return [fix_lists(v, indent) for v in obj]
    return obj

# Apply fixes to Pandoc AST
# pandoc_json = fix_lists(pandoc_json)

# Convert back to markdown
def get_markdown_from_ast(ast: Any) -> str:
    # Dump AST to temp file, convert to markdown using Pandoc
    import tempfile

    with tempfile.NamedTemporaryFile("w+", delete=False, suffix=".json") as tmp:
        tmp_path = Path(tmp.name)
    save_json(tmp_path, ast)
    result = subprocess.run(
        ["pandoc", str(tmp_path), "-f", "json", "-t", "markdown"],
        capture_output=True,
        text=True,
    )
    return result.stdout

def main() -> None:
    input_text: str = sys.stdin.read()
    if is_json_input(input_text):
        # Pandoc JSON AST: process and output JSON
        pandoc_json: Any = load_json_text(
            input_text,
            required=True,
            error_msg="Invalid Pandoc JSON input",
        )
        pandoc_json = fix_lists(pandoc_json)
        print(json.dumps(pandoc_json))
    else:
        # Markdown: post-process and output markdown
        print(postprocess_markdown(input_text))

if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: COPILOT_SYNERGY.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.
"""

import sys
import json
import re
import subprocess
from typing import Any

def is_json_input(text: str) -> bool:
    text = text.lstrip()
    return text.startswith("{") or text.startswith("[")

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

def fix_list_markers(md: str) -> str:
    # Replace dash list markers with asterisks, fix spacing and indentation
    def repl(match: re.Match) -> str:
        indent = match.group(1) or ""
        content = match.group(3)
        # Always use asterisk, single space, indent=2 per level
        indent_level = len(indent) // 2 if indent else 0
        return ("  " * indent_level) + "* " + content

    # Match lines starting with optional indent, dash or asterisk, spaces, then content
    pattern = re.compile(r"^( *)([-*]) +(.+)$", re.MULTILINE)
    md = pattern.sub(repl, md)
    return md

# Remove multiple blank lines in markdown output
def remove_multiple_blank_lines(md: str) -> str:
    # Remove all leading blank lines so the first line is non-blank
    md = re.sub(r"^(\s*\n)+", "", md)
    lines = md.splitlines()
    while lines and lines[0].strip() == "":
        lines.pop(0)
    md = "\n".join(lines)
    # Replace 3+ blank lines anywhere with a single blank line
    md = re.sub(r"\n{3,}", "\n\n", md)
    return md

def fix_horizontal_rules(md: str) -> str:
    # Replace any line of dashes or mixed with standard 79-dash rule
    return re.sub(r"^[- ]{5,}$", "-" * 79, md, flags=re.MULTILINE)

def fix_headings(md: str) -> str:
    # Convert setext (underlined) headings to atx
    lines = md.split("\n")
    out = []
    i = 0
    while i < len(lines):
        if i + 1 < len(lines) and re.match(r"^(=+|-+)$", lines[i + 1]):
            level = 1 if lines[i + 1].startswith("=") else 2
            out.append("#" * level + " " + lines[i].strip())
            i += 2
        else:
            out.append(lines[i])
            i += 1
    return "\n".join(out)

# Apply fixes to Pandoc AST
# pandoc_json = fix_lists(pandoc_json)

# Convert back to markdown
def get_markdown_from_ast(ast: Any) -> str:
    # Dump AST to temp file, convert to markdown using Pandoc
    import tempfile

    with tempfile.NamedTemporaryFile("w+", delete=False, suffix=".json") as tmp:
        json.dump(ast, tmp)
        tmp.flush()
        result = subprocess.run(
            ["pandoc", tmp.name, "-f", "json", "-t", "markdown"],
            capture_output=True,
            text=True,
        )
        return result.stdout

def postprocess_markdown(md: str) -> str:
    md = remove_multiple_blank_lines(md)
    md = fix_list_markers(md)
    md = fix_horizontal_rules(md)
    md = fix_headings(md)
    return md

def main() -> None:
    input_text: str = sys.stdin.read()
    if is_json_input(input_text):
        # Pandoc JSON AST: process and output JSON
        pandoc_json: Any = json.loads(input_text)
        pandoc_json = fix_lists(pandoc_json)
        print(json.dumps(pandoc_json))
    else:
        # Markdown: post-process and output markdown
        print(postprocess_markdown(input_text))

if __name__ == "__main__":
    main()

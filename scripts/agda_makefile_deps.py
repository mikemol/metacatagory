#!/usr/bin/env python3
"""Generate Agda makefile dependency rules using shared Agda parser."""

from pathlib import Path
import sys

# Ensure repo root importable for scripts.shared
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.shared.agda import AgdaParser


def main() -> int:
    src_dir = Path("src/agda")
    parser = AgdaParser(agda_src_dir=src_dir)
    parser.parse_directory(src_dir)
    rules = parser.generate_makefile_rules()
    print(rules)
    return 0


if __name__ == "__main__":
    sys.exit(main())

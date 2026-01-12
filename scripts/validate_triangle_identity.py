#!/usr/bin/env python3
"""
Triangle identity validation entry point.

Validates consistency between Agda, JSON, and Markdown representations.
Uses modular validators from shared_validators for composability and reusability.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared_validators import run_all_validations

if __name__ == "__main__":
    success = run_all_validations()
    sys.exit(0 if success else 1)

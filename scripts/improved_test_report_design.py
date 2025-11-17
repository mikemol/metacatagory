#!/usr/bin/env python3
"""
Improved test report generator using structured Agda-generated data.

Instead of parsing Agda source with regex, this script:
1. Compiles Tests/CoverageReport.agda which contains structured metadata
2. Reads the Agda-generated data file
3. Produces reports from that structured data

This eliminates regex fragility while keeping the build process simple.
"""

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
OUT_DIR = ROOT / "build" / "reports"

# For now, maintain the regex-based approach but document the better path
# TODO: Implement Agda-based metadata generation

# Current implementation note:
# The right solution is to have Agda generate a JSON file during compilation
# that contains all adapter metadata. This would be:
#
# 1. Type-safe (Agda's type system ensures correctness)
# 2. Complete (Can't forget to register an adapter)
# 3. Maintainable (Single source of truth)
# 4. Fast (No file parsing needed)
#
# Implementation options:
#
# A. **Agda Reflection + Macros** (Most principled)
#    - Use Agda's reflection API in a dedicated module
#    - Macro at compile-time extracts all adapter types
#    - Generates JSON output via --compile
#    - Pros: Fully automatic, impossible to get out of sync
#    - Cons: Complex, requires understanding Agda's reflection API
#
# B. **Typed Metadata Module** (Pragmatic middle ground)
#    - Maintain Tests/CoverageReport.agda with explicit lists
#    - Type system ensures listed adapters actually exist
#    - Compile to Haskell/JS and extract data
#    - Pros: Type-safe, simpler than reflection
#    - Cons: Manual registration (but type-checked)
#
# C. **Test Framework Integration** (Industry standard)
#    - Adopt a proper Agda test framework (agda-stdlib has one)
#    - Use their infrastructure for test enumeration
#    - Pros: Battle-tested, good tooling
#    - Cons: Larger dependency
#
# For this codebase, option B is recommended:
# - Create Tests/CoverageMetadata.agda with adapter registry
# - Each adapter type explicitly listed (enforced by imports)
# - Agda --compile generates executable that outputs JSON
# - Python script reads JSON instead of parsing Agda source


def improved_design_notes():
    """
    Example of what CoverageMetadata.agda would look like:

    ```agda
    module Tests.CoverageMetadata where

    open import Tests.ObligationAdapters as A

    -- This list is type-checked - can't list non-existent adapters
    data AdapterTypeReg : Set where
      reg : (name : String) → (type : Set₁) → AdapterTypeReg

    allAdapterTypes : List AdapterTypeReg
    allAdapterTypes =
      reg "FibrationDeclarationAdapter" A.FibrationDeclarationAdapter ∷
      reg "CartesianArrowAdapter" A.CartesianArrowAdapter ∷
      ...  -- Must list all, or it won't compile
      []

    -- Compile this to JSON via GHC backend
    main : IO Unit
    main = putStrLn (toJSON allAdapterTypes)
    ```

    Then in build process:
    ```bash
    agda --compile --ghc Tests/CoverageMetadata.agda
    ./Tests/CoverageMetadata > build/reports/adapters.json
    python scripts/test_report.py  # reads adapters.json
    ```
    """
    pass


if __name__ == "__main__":
    print(__doc__)
    print("\nThis is a design document. The actual implementation")
    print("is in scripts/test_report.py (using regex for now).")
    print("\nTo implement the improved design:")
    print("1. Create Tests/CoverageMetadata.agda (see docstring)")
    print("2. Add compilation step to Makefile")
    print("3. Update test_report.py to read generated JSON")
    print("4. Remove regex parsing from test_report.py")

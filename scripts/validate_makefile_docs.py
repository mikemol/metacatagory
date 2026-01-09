#!/usr/bin/env python3
"""
Validate the Makefile "triangle identity" among:
1) The Agda exporter (Source of Truth) -> docs/automation/makefile_targets_generated.md
2) The Checked-in Documentation -> docs/automation/MAKEFILE-TARGETS.md

The script enforces that the checked-in documentation matches the generated truth.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Dict, Set, TypedDict

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT = SCRIPT_DIR.parent
sys.path.insert(0, str(ROOT))

from scripts.audax_doc import (
    AUDAXBlock,
    AUDAXDoc,
    Field,
    Header,
    ListLike,
    Para,
    Raw,
    Str,
    Table,
    render_doc,
)

GENERATED_DOC = ROOT / "docs" / "automation" / "makefile_targets_generated.md"
CHECKED_IN_DOC = ROOT / "docs" / "automation" / "MAKEFILE-TARGETS.md"
REPORT_DOC = ROOT / "build" / "reports" / "makefile-docs.md"

class TargetDoc(TypedDict):
    description: str
    mutability: str


def parse_markdown_table(text: str) -> Dict[str, TargetDoc]:
    """Parse a Markdown table into a {Target: {description, mutability}} dictionary."""
    entries: Dict[str, TargetDoc] = {}
    lines = text.splitlines()
    for line in lines:
        # Matches row: | `target` | Description |
        if not line.strip().startswith("|") or "---" in line or "Target" in line:
            continue
        
        parts = [p.strip() for p in line.split("|")]
        if len(parts) >= 3:
            target_cell = parts[1]
            desc_cell = parts[2]
            mut_cell = parts[3] if len(parts) >= 4 else ""
            
            # Extract target name from backticks
            target = target_cell.strip("` ")
            entries[target] = {
                "description": desc_cell.strip(),
                "mutability": mut_cell.strip(),
            }
            
    return entries

def main() -> int:
    print("Makefile Documentation Triangle Identity")
    print("=" * 60)
    
    if not GENERATED_DOC.exists():
        print(f"✗ Generated docs not found at {GENERATED_DOC}")
        print("  Run 'make regen-makefile' first.")
        return 1
        
    if not CHECKED_IN_DOC.exists():
        print(f"✗ Checked-in docs not found at {CHECKED_IN_DOC}")
        return 1
        
    gen_data = parse_markdown_table(GENERATED_DOC.read_text())
    checked_data = parse_markdown_table(CHECKED_IN_DOC.read_text())
    
    gen_set = set(gen_data.keys())
    checked_set = set(checked_data.keys())
    
    valid = True
    
    # Check 1: Existence
    missing = gen_set - checked_set
    extra = checked_set - gen_set
    
    if missing:
        print(f"✗ Targets missing from documentation ({len(missing)}):")
        for t in sorted(missing):
            print(f"  - {t}")
        valid = False
        
    if extra:
        print(f"✗ Undocumented/Extra targets in documentation ({len(extra)}):")
        for t in sorted(extra):
            print(f"  - {t}")
        valid = False
        
    # Check 2: Content Identity
    mismatches: list[tuple[str, TargetDoc, TargetDoc]] = []
    for t in gen_set.intersection(checked_set):
        gen_doc = gen_data[t]
        checked_doc = checked_data[t]
        
        if (
            gen_doc["description"] != checked_doc["description"]
            or gen_doc["mutability"] != checked_doc["mutability"]
        ):
            mismatches.append((t, gen_doc, checked_doc))
            
    if mismatches:
        print(f"✗ Description Mismatches ({len(mismatches)}):")
        for t, gen, checked in mismatches:
            print(f"  Target: {t}")
            if gen["description"] != checked["description"]:
                print(f"    Description Gen: {gen['description']}")
                print(f"    Description Doc: {checked['description']}")
            if gen["mutability"] != checked["mutability"]:
                print(f"    Mutability Gen: {gen['mutability']}")
                print(f"    Mutability Doc: {checked['mutability']}")
        valid = False
        
    doc = build_validation_doc(
        valid=valid,
        total=len(gen_set),
        missing=missing,
        extra=extra,
        mismatches=mismatches,
    )
    write_validation_doc(doc)
    if valid:
        print(f"✓ Documentation is isomorphic to Source of Truth ({len(gen_set)} targets verified)")
        return 0
    else:
        return 1


def build_validation_doc(
    *,
    valid: bool,
    total: int,
    missing: Set[str],
    extra: Set[str],
    mismatches: list[tuple[str, TargetDoc, TargetDoc]],
) -> AUDAXDoc:
    blocks: list[AUDAXBlock] = [
        Header(1, ListLike([Str("Makefile Documentation Triangle Identity")])),
        Para(
            ListLike(
                [
                    Str(
                        f"Total targets described by the exporter: {total}. "
                        + ("Validation succeeded." if valid else "Discrepancies found.")
                    )
                ]
            )
        ),
    ]

    if missing:
        blocks.append(Header(2, ListLike([Str("Missing Targets")])))
        table_rows = [ListLike([Str(target)]) for target in sorted(missing)]
        blocks.append(Table(header=["Target"], rows=table_rows))

    if extra:
        blocks.append(Header(2, ListLike([Str("Undocumented/Extra Targets")])))
        table_rows = [ListLike([Str(target)]) for target in sorted(extra)]
        blocks.append(Table(header=["Target"], rows=table_rows))

    if mismatches:
        blocks.append(Header(2, ListLike([Str("Description Mismatches")])))
        for target, gen_doc, doc_doc in mismatches:
            blocks.append(Field("Target", f"`{target}`"))
            if gen_doc["description"] != doc_doc["description"]:
                blocks.append(Field("Generated description", gen_doc["description"]))
                blocks.append(Field("Document description", doc_doc["description"]))
            if gen_doc["mutability"] != doc_doc["mutability"]:
                blocks.append(Field("Generated mutability", gen_doc["mutability"]))
                blocks.append(Field("Document mutability", doc_doc["mutability"]))

    blocks.append(Raw("Validation log stored under build/reports/makefile-docs.md"))
    return AUDAXDoc(ListLike(blocks))


def write_validation_doc(doc: AUDAXDoc) -> None:
    REPORT_DOC.parent.mkdir(parents=True, exist_ok=True)
    REPORT_DOC.write_text(render_doc(doc), encoding="utf-8")
    print(render_doc(doc))

if __name__ == "__main__":
    raise SystemExit(main())

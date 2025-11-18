#!/usr/bin/env python3
"""
Adapter Migration Script
Automatically adds categorical adapter fields to existing adapter records.

Usage:
  python3 scripts/migrate_adapters.py <adapter_file.agda>

This script:
1. Parses existing adapter records
2. Identifies the primary declaration field
3. Adds a categorical field
4. Updates constructors to initialize the categorical field
"""

import re
import sys
from pathlib import Path
from dataclasses import dataclass
from typing import List, Optional


@dataclass
class AdapterRecord:
    """Represents an Agda adapter record definition."""

    name: str
    decl_field: str
    decl_type: str
    has_status: bool
    fields: List[str]
    constructor_name: str
    start_line: int
    end_line: int


def parse_adapter_record(lines: List[str], start_idx: int) -> Optional[AdapterRecord]:
    """Parse an adapter record definition from Agda source."""
    # Look for: record XxxAdapter : Set₁ where
    record_match = re.match(r"record\s+(\w+Adapter)\s*:", lines[start_idx])
    if not record_match:
        return None

    adapter_name = record_match.group(1)

    # Find fields
    fields = []
    decl_field = None
    decl_type = None
    has_status = False

    i = start_idx + 1
    while i < len(lines) and not lines[i].strip().startswith("mk"):
        line = lines[i].strip()

        # Parse field declarations
        field_match = re.match(r"(\w+)\s*:\s*(.+)", line)
        if field_match:
            field_name = field_match.group(1)
            field_type = field_match.group(2)
            fields.append((field_name, field_type))

            if field_name == "decl":
                decl_field = field_name
                decl_type = field_type
            elif field_name == "status":
                has_status = True

        i += 1

    # Find constructor
    constructor_name = f"mk{adapter_name}"

    return AdapterRecord(
        name=adapter_name,
        decl_field=decl_field or "decl",
        decl_type=decl_type or "Unknown",
        has_status=has_status,
        fields=fields,
        constructor_name=constructor_name,
        start_line=start_idx,
        end_line=i,
    )


def generate_categorical_field(adapter: AdapterRecord) -> str:
    """Generate the categorical adapter field definition."""
    return f"    categorical : CategoricalAdapter {adapter.decl_type}"


def generate_enhanced_constructor(
    adapter: AdapterRecord, original_lines: List[str]
) -> List[str]:
    """Generate an updated constructor with categorical initialization."""
    # Find the original constructor
    constructor_start = None
    for i, line in enumerate(original_lines):
        if adapter.constructor_name in line and "=" in line:
            constructor_start = i
            break

    if constructor_start is None:
        return []

    # Extract constructor signature and body
    new_lines = []

    # Add categorical parameter to constructor signature
    # Original: mk<Name> (d : DeclType) → <Name>Adapter
    # New:      mk<Name> (d : DeclType) (f : ⊤ → DeclType) → <Name>Adapter

    sig_line = original_lines[constructor_start]
    # Insert categorical parameter before the final arrow
    enhanced_sig = sig_line.replace(" → ", " (f : ⊤ → " + adapter.decl_type + ") → ")
    new_lines.append(enhanced_sig)

    # Update record construction to include categorical field
    i = constructor_start + 1
    while i < len(original_lines):
        line = original_lines[i]
        new_lines.append(line)

        # Insert categorical field before closing brace
        if "status = " in line and "categorical" not in line:
            indent = len(line) - len(line.lstrip())
            categorical_init = (
                " " * indent
                + f"; categorical = mkCategoricalAdapter {adapter.decl_type} f"
            )
            new_lines.append(categorical_init)

        if line.strip().startswith("}"):
            break
        i += 1

    return new_lines


def add_categorical_to_record(lines: List[str], adapter: AdapterRecord) -> List[str]:
    """Add categorical field to an adapter record definition."""
    new_lines = []

    for i, line in enumerate(lines):
        new_lines.append(line)

        # Add categorical field after status field (if present)
        if i >= adapter.start_line and i < adapter.end_line:
            if "status" in line and "Bool" in line:
                categorical_line = generate_categorical_field(adapter)
                new_lines.append(categorical_line)

    return new_lines


def migrate_adapter_file(filepath: Path) -> None:
    """Migrate all adapters in a file to include categorical adapters."""
    print(f"Migrating {filepath}...")

    with open(filepath, "r") as f:
        lines = f.readlines()

    # Parse all adapter records
    adapters = []
    i = 0
    while i < len(lines):
        adapter = parse_adapter_record(lines, i)
        if adapter:
            adapters.append(adapter)
            print(f"  Found adapter: {adapter.name}")
            i = adapter.end_line
        else:
            i += 1

    if not adapters:
        print("  No adapters found.")
        return

    # Add import for CategoricalAdapter if not present
    has_import = any("Core.CategoricalAdapter" in line for line in lines)
    if not has_import:
        # Find import section and add
        for i, line in enumerate(lines):
            if line.startswith("import ") or line.startswith("open import"):
                lines.insert(i, "open import Core.CategoricalAdapter\n")
                break

    # Migrate each adapter
    modified_lines = lines[:]
    for adapter in adapters:
        modified_lines = add_categorical_to_record(modified_lines, adapter)

    # Write back
    backup_path = filepath.with_suffix(".agda.bak")
    filepath.rename(backup_path)
    print(f"  Created backup: {backup_path}")

    with open(filepath, "w") as f:
        f.writelines(modified_lines)

    print(f"  Migration complete!")


def generate_migration_report(adapter_dir: Path) -> None:
    """Generate a report of which adapters need migration."""
    print("Scanning for adapters...")

    adapter_files = list(adapter_dir.glob("*Adapter*.agda"))

    report = []
    for filepath in adapter_files:
        with open(filepath, "r") as f:
            content = f.read()

        # Check if file has categorical adapters
        has_categorical = (
            "categorical :" in content or "Core.CategoricalAdapter" in content
        )

        # Count adapters
        adapter_count = len(re.findall(r"record\s+\w+Adapter\s*:", content))

        report.append(
            {
                "file": filepath.name,
                "adapters": adapter_count,
                "migrated": has_categorical,
            }
        )

    print("\nMigration Status Report:")
    print("-" * 60)
    for item in report:
        status = "✓ Migrated" if item["migrated"] else "✗ Needs migration"
        print(f"{item['file']:40} {item['adapters']:3} adapters  {status}")

    total = len(report)
    migrated = sum(1 for r in report if r["migrated"])
    print("-" * 60)
    print(f"Total: {migrated}/{total} files migrated")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 migrate_adapters.py <path>")
        print("  <path> can be a .agda file or directory")
        sys.exit(1)

    path = Path(sys.argv[1])

    if path.is_file():
        migrate_adapter_file(path)
    elif path.is_dir():
        generate_migration_report(path)
    else:
        print(f"Error: {path} not found")
        sys.exit(1)

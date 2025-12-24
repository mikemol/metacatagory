#!/usr/bin/env python3
"""
Validate the Makefile "triangle identity" among:
1) The Agda exporter (src/agda/Examples/ExporterMakefile.agda) - source of truth.
2) The generated Makefile (Makefile.generated) produced by the Agda exporter.
3) The checked-in Makefile (user-facing targets, copied from generated).
4) The witness document docs/automation/MAKEFILE-TARGETS.md.

The script checks that:
- All phony targets in Makefile are documented.
- All documented targets appear in Makefile phony.
- All documented targets appear in Makefile.generated phony.
- All Makefile phony targets have concrete definitions in Makefile.
- All documented targets have a meaningful description (No TODOs).

Exit code is non-zero if any discrepancies are found.
"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Set, Tuple

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_MAKEFILE = ROOT / "Makefile"
DEFAULT_GENERATED = ROOT / "Makefile.generated"
DEFAULT_DOC = ROOT / "docs" / "automation" / "MAKEFILE-TARGETS.md"

TARGET_RE = re.compile(r"^([A-Za-z0-9_.-]+):")
# Matches: | `target-name` | Description text |
DOC_ROW_RE = re.compile(r"\|\s*`([^`]+)`\s*\|\s*([^|]+?)\s*\|")


@dataclass(frozen=True)
class ValidationResult:
    missing_in_docs: Set[str]
    extra_in_docs: Set[str]
    phony_without_rule: Set[str]
    phony_mismatch_generated: Set[str]
    generated_missing: Set[str]
    documented_not_in_generated: Set[str]
    missing_descriptions: Set[str]

    def ok(self) -> bool:
        return not (self.missing_in_docs or self.extra_in_docs or self.phony_without_rule or 
                   self.phony_mismatch_generated or self.generated_missing or 
                   self.documented_not_in_generated or self.missing_descriptions)


def parse_phony_targets(text: str) -> Set[str]:
    for line in text.splitlines():
        if line.strip().startswith(".PHONY"):
            parts = line.split(":", 1)
            if len(parts) == 2:
                return {p.strip() for p in parts[1].split() if p.strip()}
    return set()


def parse_rule_targets(text: str) -> Set[str]:
    targets: Set[str] = set()
    for line in text.splitlines():
        if not line or line[0].isspace() or line.lstrip().startswith("#"):
            continue
        m = TARGET_RE.match(line)
        if m:
            targets.add(m.group(1))
    return targets


def parse_doc_targets_with_desc(text: str) -> Dict[str, str]:
    """Parse table rows to extract target names and descriptions."""
    targets = {}
    for line in text.splitlines():
        m = DOC_ROW_RE.search(line)
        if m:
            target = m.group(1).strip()
            desc = m.group(2).strip()
            # Filter out header separators like '---'
            if target and not target.startswith("-"):
                targets[target] = desc
    return targets


def validate(makefile: Path, generated: Path, doc: Path) -> ValidationResult:
    make_text = makefile.read_text()
    doc_text = doc.read_text()

    phony = parse_phony_targets(make_text)
    rules = parse_rule_targets(make_text)
    
    doc_entries = parse_doc_targets_with_desc(doc_text)
    documented = set(doc_entries.keys())

    missing_in_docs = phony - documented
    extra_in_docs = documented - phony
    phony_without_rule = {t for t in phony if t not in rules}

    # Strict Description Check
    missing_descriptions = set()
    for target, desc in doc_entries.items():
        if not desc or "TODO" in desc or desc == "Description":
            missing_descriptions.add(target)

    generated_missing: Set[str] = set()
    phony_mismatch_generated: Set[str] = set()
    documented_not_in_generated: Set[str] = set()
    
    if generated.exists():
        gen_text = generated.read_text()
        gen_phony = parse_phony_targets(gen_text)
        generated_missing = gen_phony - phony
        phony_mismatch_generated = phony - gen_phony
        documented_not_in_generated = documented - gen_phony

    return ValidationResult(
        missing_in_docs=missing_in_docs,
        extra_in_docs=extra_in_docs,
        phony_without_rule=phony_without_rule,
        phony_mismatch_generated=phony_mismatch_generated,
        generated_missing=generated_missing,
        documented_not_in_generated=documented_not_in_generated,
        missing_descriptions=missing_descriptions,
    )


def render(result: ValidationResult) -> str:
    lines: List[str] = []
    lines.append("Makefile triangle identity validation")
    lines.append("=" * 60)
    lines.append("")
    lines.append("Triangle vertices:")
    lines.append("  1) Agda exporter → Makefile.generated (source of truth)")
    lines.append("  2) Makefile.generated → Makefile (copied)")
    lines.append("  3) Makefile ↔ docs/automation/MAKEFILE-TARGETS.md (witness)")
    lines.append("")
    
    if result.ok():
        lines.append("Status: ✓ OK - All three vertices agree and are documented")
    else:
        lines.append("Status: ✗ FAIL - Triangle identity or documentation broken")

    def dump(title: str, items: Set[str]) -> None:
        if not items:
            return
        lines.append("")
        lines.append(f"✗ {title} ({len(items)})")
        for item in sorted(items):
            lines.append(f"  - {item}")

    dump("Agda generator missing targets found in Docs", result.documented_not_in_generated)
    dump("Generated Makefile mismatch with Checked-in Makefile", result.phony_mismatch_generated)
    dump("Checked-in Makefile missing targets from Generated", result.generated_missing)
    dump("Targets defined in Makefile but NOT Documented", result.missing_in_docs)
    dump("Targets Documented but NOT in Makefile", result.extra_in_docs)
    dump("Phony targets missing Makefile rules", result.phony_without_rule)
    dump("Documented targets with Missing/TODO descriptions", result.missing_descriptions)
    
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate Makefile targets against documentation and generated Makefile")
    parser.add_argument("--makefile", type=Path, default=DEFAULT_MAKEFILE, help="Path to Makefile")
    parser.add_argument("--generated", type=Path, default=DEFAULT_GENERATED, help="Path to Makefile.generated")
    parser.add_argument("--doc", type=Path, default=DEFAULT_DOC, help="Path to documentation table")
    args = parser.parse_args()

    if not args.makefile.exists():
        sys.stderr.write(f"Makefile not found: {args.makefile}\n")
        return 2
    if not args.doc.exists():
        sys.stderr.write(f"Documentation not found: {args.doc}\n")
        return 2

    result = validate(args.makefile, args.generated, args.doc)
    output = render(result)
    print(output)
    return 0 if result.ok() else 1


if __name__ == "__main__":
    raise SystemExit(main())

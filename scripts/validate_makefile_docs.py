#!/usr/bin/env python3
"""
Validate the Makefile "triangle identity" among:
1) The checked-in Makefile (user-facing targets live here).
2) The generated Makefile (Makefile.generated) produced by the Agda exporter.
3) The witness document docs/automation/MAKEFILE-TARGETS.md.

The script checks that:
- All phony targets in Makefile are documented.
- Documented targets map back to Makefile phony targets.
- Phony targets have concrete definitions in Makefile.
- Makefile and Makefile.generated agree on phony targets (if generated exists).

Exit code is non-zero if any discrepancies are found.
"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Set

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_MAKEFILE = ROOT / "Makefile"
DEFAULT_GENERATED = ROOT / "Makefile.generated"
DEFAULT_DOC = ROOT / "docs" / "automation" / "MAKEFILE-TARGETS.md"

TARGET_RE = re.compile(r"^([A-Za-z0-9_.-]+):")
DOC_TARGET_RE = re.compile(r"\|\s*`([^`]+)`\s*\|")


@dataclass(frozen=True)
class ValidationResult:
    missing_in_docs: Set[str]
    extra_in_docs: Set[str]
    phony_without_rule: Set[str]
    phony_mismatch_generated: Set[str]
    generated_missing: Set[str]

    def ok(self) -> bool:
        return not (self.missing_in_docs or self.extra_in_docs or self.phony_without_rule or self.phony_mismatch_generated or self.generated_missing)


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


def parse_doc_targets(text: str) -> Set[str]:
    return {m.group(1).strip() for m in DOC_TARGET_RE.finditer(text)}


def validate(makefile: Path, generated: Path, doc: Path) -> ValidationResult:
    make_text = makefile.read_text()
    doc_text = doc.read_text()

    phony = parse_phony_targets(make_text)
    rules = parse_rule_targets(make_text)
    documented = parse_doc_targets(doc_text)

    missing_in_docs = phony - documented
    extra_in_docs = documented - phony
    phony_without_rule = {t for t in phony if t not in rules}

    generated_missing: Set[str] = set()
    phony_mismatch_generated: Set[str] = set()
    if generated.exists():
        gen_text = generated.read_text()
        gen_phony = parse_phony_targets(gen_text)
        generated_missing = gen_phony - phony
        phony_mismatch_generated = phony - gen_phony

    return ValidationResult(
        missing_in_docs=missing_in_docs,
        extra_in_docs=extra_in_docs,
        phony_without_rule=phony_without_rule,
        phony_mismatch_generated=phony_mismatch_generated,
        generated_missing=generated_missing,
    )


def render(result: ValidationResult) -> str:
    lines: List[str] = []
    lines.append("Makefile triangle identity validation")
    lines.append("")
    if result.ok():
        lines.append("Status: OK")
    else:
        lines.append("Status: FAIL")

    def dump(title: str, items: Set[str]) -> None:
        lines.append("")
        lines.append(f"{title} ({len(items)})")
        if items:
            for item in sorted(items):
                lines.append(f"- {item}")
        else:
            lines.append("- (none)")

    dump("Phony targets missing in docs", result.missing_in_docs)
    dump("Documented targets not in Makefile phony", result.extra_in_docs)
    dump("Phony targets without Makefile rule", result.phony_without_rule)
    dump("Phony targets missing from Makefile.generated", result.phony_mismatch_generated)
    dump("Makefile.generated phony not in Makefile", result.generated_missing)
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

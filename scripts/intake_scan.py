#!/usr/bin/env python3
"""
Scan intake/ markdown files for roadmap IDs and emit coverage reports.
"""
from __future__ import annotations

import json
import re
from collections import defaultdict
from pathlib import Path
from typing import Dict, Iterable, List, Set

ROOT = Path(__file__).resolve().parent.parent
INTAKE_DIR = ROOT / "intake"
CANONICAL_PATH = ROOT / "build" / "canonical_roadmap.json"
REPORT_DIR = ROOT / "build" / "reports"
REPORT_JSON = REPORT_DIR / "intake_coverage.json"
REPORT_MD = REPORT_DIR / "intake_coverage.md"

ID_PATTERN = re.compile(r"\b(?:PHASE-[A-Za-z0-9.\-]+|ROADMAP-MD-\d+|GP-[A-Za-z0-9.\-]+)\b")


def load_canonical_ids() -> Set[str]:
    if not CANONICAL_PATH.exists():
        CANONICAL_PATH.parent.mkdir(parents=True, exist_ok=True)
        CANONICAL_PATH.write_text("[]")
        return set()

    data = json.loads(CANONICAL_PATH.read_text())
    ids: Set[str] = set()
    for entry in data:
        value = entry.get("id") if isinstance(entry, dict) else None
        if isinstance(value, str):
            cleaned = value.strip()
            if cleaned:
                ids.add(cleaned)
    return ids


def iter_intake_files() -> Iterable[Path]:
    for path in INTAKE_DIR.rglob("*"):
        if path.is_file() and ".md" in path.name:
            yield path


def relative(path: Path) -> str:
    return path.resolve().relative_to(ROOT).as_posix()


def build_coverage(canonical_ids: Set[str], files: Iterable[Path]) -> Dict[str, List[str]]:
    coverage: Dict[str, List[str]] = defaultdict(list)
    file_hits: Dict[str, List[str]] = {}
    unknown_hits: Dict[str, List[str]] = defaultdict(list)
    files_without_ids: List[str] = []

    for path in files:
        text = path.read_text(errors="ignore")
        matches = set(ID_PATTERN.findall(text))
        canonical_matches = sorted(matches & canonical_ids)
        unknown_matches = sorted(matches - canonical_ids)

        rel = relative(path)
        if canonical_matches:
            file_hits[rel] = canonical_matches
            for id_value in canonical_matches:
                coverage[id_value].append(rel)
        else:
            files_without_ids.append(rel)

        if unknown_matches:
            unknown_hits[rel] = unknown_matches

    return {
        "coverage": {k: sorted(v) for k, v in coverage.items()},
        "file_hits": file_hits,
        "unknown_hits": unknown_hits,
        "files_without_ids": sorted(files_without_ids),
    }


def write_json_report(payload: dict) -> None:
    REPORT_DIR.mkdir(parents=True, exist_ok=True)
    REPORT_JSON.write_text(json.dumps(payload, indent=2))


def write_markdown_report(payload: dict, canonical_ids: Set[str]) -> None:
    coverage = payload["canonical_coverage"]
    file_hits = payload["files_with_ids"]
    unknown_hits = payload["unknown_ids"]
    files_without_ids = payload["files_without_ids"]

    missing_ids = payload["canonical_missing"]
    referenced_ids = sorted(coverage)
    lines: List[str] = []
    lines.append("# Intake coverage")
    lines.append("")
    lines.append(f"- Intake files scanned: {len(file_hits) + len(files_without_ids)}")
    lines.append(f"- Files with roadmap IDs: {len(file_hits)}")
    lines.append(f"- Files without roadmap IDs: {len(files_without_ids)}")
    lines.append(f"- Canonical IDs referenced: {len(referenced_ids)} / {len(canonical_ids)}")
    lines.append(f"- Unknown IDs encountered: {len(unknown_hits)}")

    lines.append("")
    lines.append("## Canonical coverage")
    lines.append("| Roadmap ID | Intake files |")
    lines.append("| --- | --- |")
    for id_value in referenced_ids:
        files = ", ".join(sorted(coverage[id_value]))
        lines.append(f"| {id_value} | {files} |")
    if not referenced_ids:
        lines.append("| _None_ | _None_ |")

    lines.append("")
    lines.append("## Canonical IDs not seen in intake/")
    if missing_ids:
        for id_value in missing_ids:
            lines.append(f"- {id_value}")
    else:
        lines.append("- _None_")

    lines.append("")
    lines.append("## Files without roadmap IDs")
    if files_without_ids:
        for path in files_without_ids:
            lines.append(f"- {path}")
    else:
        lines.append("- _None_")

    lines.append("")
    lines.append("## Unknown IDs in intake/")
    if unknown_hits:
        for path, ids in sorted(unknown_hits.items()):
            joined = ", ".join(ids)
            lines.append(f"- {path}: {joined}")
    else:
        lines.append("- _None_")

    REPORT_MD.write_text("\n".join(lines))


def main() -> None:
    canonical_ids = load_canonical_ids()
    files = list(iter_intake_files())
    payload = build_coverage(canonical_ids, files)

    summary = {
        "total_intake_files": len(payload["file_hits"]) + len(payload["files_without_ids"]),
        "files_with_ids": len(payload["file_hits"]),
        "files_without_ids": len(payload["files_without_ids"]),
        "canonical_ids_seen": len(payload["coverage"]),
        "canonical_ids_total": len(canonical_ids),
        "unknown_id_files": len(payload["unknown_hits"]),
    }

    report = {
        "summary": summary,
        "files_with_ids": payload["file_hits"],
        "canonical_coverage": payload["coverage"],
        "canonical_missing": sorted(id_value for id_value in canonical_ids if id_value not in payload["coverage"]),
        "unknown_ids": payload["unknown_hits"],
        "files_without_ids": payload["files_without_ids"],
    }

    write_json_report(report)
    write_markdown_report(report, canonical_ids)


if __name__ == "__main__":
    main()

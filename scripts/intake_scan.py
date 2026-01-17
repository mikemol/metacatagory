#!/usr/bin/env python3
"""
Scan intake/ markdown files for roadmap IDs and catalog shards.
Per integration-audit.md: intake is a "Ruminative Buffer" not a waste bin.
Reports:
- Roadmap ID coverage
- Raw shards (__(n).md files)
- Semi-structured candidates
- Contextual substrates
"""
from __future__ import annotations

import json
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.shared.paths import REPO_ROOT, REPORTS_DIR
from scripts import shared_data
from scripts.shared.io import save_json
from scripts.shared.intake import classify_intake_filename, find_roadmap_ids

INTAKE_DIR = REPO_ROOT / "intake"
CANONICAL_PATH = shared_data.resolve_planning_path(repo_root=REPO_ROOT)
REPORT_DIR = REPORTS_DIR
REPORT_JSON = REPORT_DIR / "intake_coverage.json"
REPORT_MD = REPORT_DIR / "intake_coverage.md"

def load_canonical_ids() -> set[str]:
    if not CANONICAL_PATH.exists():
        CANONICAL_PATH.parent.mkdir(parents=True, exist_ok=True)
        CANONICAL_PATH.write_text("[]")
        return set()

    data = shared_data.load_planning_index_validated_from(CANONICAL_PATH)
    ids: set[str] = set()
    for entry in data:
        value = entry.get("id") if isinstance(entry, dict) else None
        if isinstance(value, str):
            cleaned = value.strip()
            if cleaned:
                ids.add(cleaned)
    return ids

def iter_intake_files() -> list[Path]:
    """Return all intake markdown files"""
    return [p for p in INTAKE_DIR.rglob("*") if p.is_file() and ".md" in p.name]

def relative(path: Path) -> str:
    return path.resolve().relative_to(ROOT).as_posix()

def classify_intake_file(path: Path) -> str:
    """Classify intake file as shard, candidate, or substrate"""
    return classify_intake_filename(path.name)

def build_coverage(canonical_ids: set[str], files: list[Path]) -> dict[str, Any]:
    coverage: dict[str, list[str]] = defaultdict(list)
    file_hits: dict[str, list[str]] = {}
    unknown_hits: dict[str, list[str]] = defaultdict(list)
    files_without_ids: list[str] = []
    
    # Classify files
    shards: list[str] = []
    candidates: list[str] = []
    substrates: list[str] = []
    formalized: list[str] = []

    # Classify files
    shards: list[str] = []
    candidates: list[str] = []
    substrates: list[str] = []
    formalized: list[str] = []

    for path in files:
        text = path.read_text(errors="ignore")
        matches = find_roadmap_ids(text)
        canonical_matches = sorted(matches & canonical_ids)
        unknown_matches = sorted(matches - canonical_ids)

        rel = relative(path)
        
        # Classify file
        file_type = classify_intake_file(path)
        if file_type == "shard":
            shards.append(rel)
        elif file_type == "candidate":
            candidates.append(rel)
        elif file_type == "substrate":
            substrates.append(rel)
        else:
            formalized.append(rel)
        
        # Track ID coverage
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
        "shards": sorted(shards),
        "candidates": sorted(candidates),
        "substrates": sorted(substrates),
        "formalized": sorted(formalized),
    }

def write_json_report(payload: dict) -> None:
    save_json(REPORT_JSON, payload)

def write_markdown_report(payload: dict, canonical_ids: set[str]) -> None:
    coverage = payload["canonical_coverage"]
    file_hits = payload["files_with_ids"]
    unknown_hits = payload["unknown_ids"]
    files_without_ids = payload["files_without_ids"]
    
    # Metabolic Manifold classification (per integration-audit.md)
    shards = payload["shards"]
    candidates = payload["candidates"]
    substrates = payload["substrates"]
    formalized = payload["formalized"]

    missing_ids = payload["canonical_missing"]
    referenced_ids = sorted(coverage)
    lines: list[str] = []
    
    lines.append("# Intake Coverage & Metabolic Buffer Analysis")
    lines.append("")
    lines.append("*Per integration-audit.md: Intake is a Ruminative Buffer, not a waste bin.*")
    lines.append("")
    
    lines.append("## Summary")
    lines.append(f"- **Total intake files**: {len(file_hits) + len(files_without_ids)}")
    lines.append(f"- **Files with roadmap IDs**: {len(file_hits)}")
    lines.append(f"- **Files without roadmap IDs**: {len(files_without_ids)}")
    lines.append(f"- **Canonical IDs referenced**: {len(referenced_ids)} / {len(canonical_ids)}")
    lines.append(f"- **Unknown IDs encountered**: {len(unknown_hits)}")
    lines.append("")
    
    lines.append("## Metabolic Manifold (Latent Potentiality)")
    lines.append("")
    lines.append(f"### Atomic Shards (Unbound Energy)")
    lines.append(f"**Count:** {len(shards)}")
    lines.append("")
    if shards:
        for shard in shards:
            lines.append(f"- {shard}")
    else:
        lines.append("- _None_")
    lines.append("")
    
    lines.append(f"### Semi-Structured Candidates (Pending State)")
    lines.append(f"**Count:** {len(candidates)}")
    lines.append("")
    if candidates:
        for candidate in candidates:
            lines.append(f"- {candidate}")
    else:
        lines.append("- _None_")
    lines.append("")
    
    lines.append(f"### Contextual Substrates (Grounding History)")
    lines.append(f"**Count:** {len(substrates)}")
    lines.append("")
    if substrates:
        for substrate in substrates:
            lines.append(f"- {substrate}")
    else:
        lines.append("- _None_")
    lines.append("")
    
    lines.append(f"### Formalized GPs (Digested)")
    lines.append(f"**Count:** {len(formalized)}")
    lines.append("")
    if formalized:
        for gp in formalized[:20]:  # Limit to first 20
            lines.append(f"- {gp}")
        if len(formalized) > 20:
            lines.append(f"- _(and {len(formalized) - 20} more)_")
    else:
        lines.append("- _None_")
    lines.append("")

    lines.append("## Canonical Coverage")
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
    files = iter_intake_files()
    payload = build_coverage(canonical_ids, files)

    summary = {
        "total_intake_files": len(payload["file_hits"]) + len(payload["files_without_ids"]),
        "files_with_ids": len(payload["file_hits"]),
        "files_without_ids": len(payload["files_without_ids"]),
        "canonical_ids_seen": len(payload["coverage"]),
        "canonical_ids_total": len(canonical_ids),
        "unknown_id_files": len(payload["unknown_hits"]),
        "shards_count": len(payload["shards"]),
        "candidates_count": len(payload["candidates"]),
        "substrates_count": len(payload["substrates"]),
        "formalized_count": len(payload["formalized"]),
    }

    report = {
        "summary": summary,
        "files_with_ids": payload["file_hits"],
        "canonical_coverage": payload["coverage"],
        "canonical_missing": sorted(id_value for id_value in canonical_ids if id_value not in payload["coverage"]),
        "unknown_ids": payload["unknown_hits"],
        "files_without_ids": payload["files_without_ids"],
        "shards": payload["shards"],
        "candidates": payload["candidates"],
        "substrates": payload["substrates"],
        "formalized": payload["formalized"],
    }

    write_json_report(report)
    write_markdown_report(report, canonical_ids)
    
    # Print summary to stdout (as audit prescribes)
    print(f"✓ Intake scan complete")
    print(f"  • Formalized GPs: {len(payload['formalized'])}")
    print(f"  • Raw Shards: {len(payload['shards'])}")
    print(f"  • Candidates: {len(payload['candidates'])}")
    print(f"  • Substrates: {len(payload['substrates'])}")
    print(f"  • Reports: {REPORT_JSON}, {REPORT_MD}")

if __name__ == "__main__":
    main()

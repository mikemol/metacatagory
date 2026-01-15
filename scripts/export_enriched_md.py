#!/usr/bin/env python3
"""
Export enriched canonical roadmap to human-readable Markdown digest.

Reads build/canonical_enriched.json and outputs build/reports/tasks_enriched.md
(or CI_REPORT_DIR override).
with detailed semantic information for each task.
"""

import json
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import sys
from typing import Any, Dict, List, Tuple

REPO_ROOT = Path(__file__).parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.shared.paths import REPORTS_DIR
from scripts.shared_yaml import dump_yaml
from scripts.shared.parallel import get_parallel_settings

ENRICHED_JSON = REPO_ROOT / "build" / "canonical_enriched.json"
OUTPUT_MD = REPORTS_DIR / "tasks_enriched.md"

def format_list(items: List[str], indent: int = 0) -> str:
    """Format a list of items as Markdown bullet points."""
    if not items:
        return f"{'  ' * indent}*None*\n"
    
    result = []
    for item in items:
        result.append(f"{'  ' * indent}- {item}")
    return "\n".join(result) + "\n"

def format_evidence(evidence: List[Dict], indent: int = 0) -> str:
    """Format evidence snippets with source attribution."""
    if not evidence:
        return f"{'  ' * indent}*No evidence extracted*\n"
    
    result = []
    for ev in evidence:
        source = ev.get("source", "unknown")
        loc = ev.get("loc", "")
        text = ev.get("text", "")
        
        result.append(f"{'  ' * indent}- **{source}** ({loc})")
        result.append(f"{'  ' * (indent + 1)}> {text}")
    
    return "\n".join(result) + "\n"

def format_scope(scope: Dict, indent: int = 0) -> str:
    """Format scope in/out boundaries."""
    scope_in = scope.get("in", [])
    scope_out = scope.get("out", [])
    
    if not scope_in and not scope_out:
        return f"{'  ' * indent}Not specified.\n"
    
    result = []
    if scope_in:
        result.append(f"{'  ' * indent}**In scope:**")
        for item in scope_in:
            result.append(f"{'  ' * (indent + 1)}- {item}")
    
    if scope_out:
        result.append(f"{'  ' * indent}**Out of scope:**")
        for item in scope_out:
            result.append(f"{'  ' * (indent + 1)}- {item}")
    
    return "\n".join(result) + "\n"

def format_task_section(item: Dict, idx: int) -> str:
    """Format a single task as a Markdown section."""
    task_id = item.get("id", "unknown")
    title = item.get("title", "Untitled")
    status = item.get("status", "not-started")
    category = item.get("category", "")
    complexity = item.get("complexity", "unknown")
    
    # Build frontmatter with all structured metadata
    frontmatter = {
        'id': task_id,
        'title': title,
        'status': status,
        'category': category,
        'complexity': complexity
    }
    
    # Add optional fields
    if item.get('dependsOn'):
        frontmatter['dependencies'] = item['dependsOn']
    
    if item.get('derivedTags'):
        frontmatter['tags'] = item['derivedTags']
    
    if item.get('intent'):
        frontmatter['intent'] = item['intent']
    
    if item.get('deliverable'):
        frontmatter['deliverable'] = item['deliverable']
    
    if item.get('moduleAnchors'):
        frontmatter['module_anchors'] = item['moduleAnchors']
    
    if item.get('acceptance'):
        frontmatter['acceptance_criteria'] = item['acceptance']
    
    if item.get('inputs'):
        frontmatter['inputs'] = item['inputs']
    
    if item.get('outputs'):
        frontmatter['outputs'] = item['outputs']
    
    # Generate YAML frontmatter
    yaml_str = dump_yaml(frontmatter)
    
    # Header with frontmatter (unique heading to satisfy MD024)
    lines = [
        f"### Task {idx}: {task_id} — {title}",
        "",
        "```yaml",
        yaml_str.rstrip(),
        "```",
        "",
        f"**Status:** `{status}` | **Category:** {category} | **Complexity:** {complexity}",
        ""
    ]
    
    # Intent
    intent = item.get("intent", "")
    if intent:
        lines.extend([
            f"#### Intent — {task_id}",
            "",
            intent,
            ""
        ])
    
    # Deliverable
    deliverable = item.get("deliverable", "")
    if deliverable:
        lines.extend([
            f"#### Deliverable — {task_id}",
            "",
            deliverable,
            ""
        ])
    
    # Scope
    scope = item.get("scope", {})
    if scope:
        lines.extend([
            f"#### Scope — {task_id}",
            "",
            format_scope(scope),
            ""
        ])
    
    # Inputs/Outputs
    inputs = item.get("inputs", [])
    outputs = item.get("outputs", [])
    
    if inputs or outputs:
        lines.append(f"#### Inputs & Outputs — {task_id}")
        lines.append("")
        
        if inputs:
            lines.append("**Inputs:**")
            lines.append("")
            lines.append(format_list(inputs))
            lines.append("")
        
        if outputs:
            lines.append("**Outputs:**")
            lines.append("")
            lines.append(format_list(outputs))
            lines.append("")
    
    # Acceptance Criteria
    acceptance = item.get("acceptance", [])
    if acceptance:
        lines.extend([
            f"#### Acceptance Criteria — {task_id}",
            "",
            format_list(acceptance),
            ""
        ])
    
    # Module Anchors
    module_anchors = item.get("moduleAnchors", [])
    if module_anchors:
        lines.extend([
            f"#### Agda Modules — {task_id}",
            "",
            format_list(module_anchors),
            ""
        ])
    
    # Extracted Definitions
    definitions = item.get("definitions", [])
    if definitions:
        def_str = ", ".join(f"`{d}`" for d in definitions[:8])
        if len(definitions) > 8:
            def_str += f" (+ {len(definitions) - 8} more)"
        lines.extend([
            f"#### Key Definitions — {task_id}",
            "",
            def_str,
            ""
        ])
    
    # Tags
    tags = item.get("derivedTags", [])
    if tags:
        tag_str = ", ".join(f"`{t}`" for t in tags[:10])
        lines.extend([
            f"#### Tags — {task_id}",
            "",
            tag_str,
            ""
        ])
    
    # Dependencies & Relations
    depends_on = item.get("dependsOn", [])
    related = item.get("related", [])
    suggested_deps = item.get("suggestedDependencies", [])
    
    if depends_on or related or suggested_deps:
        lines.append(f"#### Dependencies & Relations — {task_id}")
        lines.append("")
        
        if depends_on:
            lines.append(f"**Depends on:** {', '.join(depends_on)}")
            lines.append("")
        
        if suggested_deps:
            suggested_str = ", ".join(suggested_deps[:5])
            if len(suggested_deps) > 5:
                suggested_str += f" (+ {len(suggested_deps) - 5} more)"
            lines.append(f"**Suggested dependencies (from imports):** {suggested_str}")
            lines.append("")
        
        if related:
            lines.append(f"**Related:** {', '.join(related)}")
            lines.append("")
    
    # Evidence
    evidence = item.get("evidence", [])
    if evidence:
        lines.extend([
            f"#### Evidence — {task_id}",
            "",
            format_evidence(evidence),
            ""
        ])
    
    # Provenance
    provenance = item.get("provenance", [])
    if provenance:
        prov_str = " → ".join(f"`{p}`" for p in provenance[:3])
        if len(provenance) > 3:
            prov_str += f" (+ {len(provenance) - 3} more)"
        
        lines.extend([
            f"#### Provenance — {task_id}",
            "",
            prov_str,
            ""
        ])
    
    lines.append("---")
    lines.append("")
    
    return "\n".join(lines)

def _format_task_entry(entry: Tuple[Dict, int]) -> str:
    item, idx = entry
    return format_task_section(item, idx)

def export_enriched_markdown() -> None:
    """Main export function."""
    if not ENRICHED_JSON.exists():
        print(f"Error: {ENRICHED_JSON} not found. Run 'make roadmap-enrich' first.")
        return
    
    with open(ENRICHED_JSON, "r", encoding="utf-8") as f:
        items = json.load(f)
    
    # Group by category
    by_category: Dict[str, List[Dict]] = {}
    for item in items:
        cat = item.get("category", "Other")
        by_category.setdefault(cat, []).append(item)
    
    # Generate markdown
    lines = [
        "# Enriched Roadmap Tasks",
        "",
        "Semantic details extracted from canonical roadmap sources.",
        "",
        f"**Total Tasks:** {len(items)}",
        "",
        "## Table of Contents",
        ""
    ]
    
    # TOC by category
    for category in sorted(by_category.keys()):
        count = len(by_category[category])
        lines.append(f"- [{category}](#category-{category.lower().replace(' ', '-')}) ({count} tasks)")
    
    lines.extend(["", "---", ""])
    
    # Generate sections
    parallel, workers = get_parallel_settings()
    executor = None
    if parallel and workers > 1:
        executor = ThreadPoolExecutor(max_workers=workers)

    task_idx = 1
    for category in sorted(by_category.keys()):
        lines.extend([
            f"## Category: {category}",
            "",
            f"**Tasks in this category:** {len(by_category[category])}",
            "",
            "---",
            ""
        ])
        
        entries: List[Tuple[Dict, int]] = []
        for item in by_category[category]:
            entries.append((item, task_idx))
            task_idx += 1

        if executor:
            sections = list(executor.map(_format_task_entry, entries))
        else:
            sections = [format_task_section(item, idx) for item, idx in entries]

        lines.extend(sections)

    if executor:
        executor.shutdown()
    
    # Statistics
    lines.extend([
        "## Statistics",
        "",
        f"- **Total tasks:** {len(items)}",
        f"- **Categories:** {len(by_category)}",
        ""
    ])
    
    # Status breakdown
    status_counts: Dict[str, int] = {}
    for item in items:
        status = item.get("status", "unknown")
        status_counts[status] = status_counts.get(status, 0) + 1
    
    lines.append("## Status Breakdown")
    lines.append("")
    for status in sorted(status_counts.keys()):
        count = status_counts[status]
        pct = (count / len(items)) * 100
        lines.append(f"- **{status}**: {count} ({pct:.1f}%)")
    
    lines.append("")
    
    # Complexity breakdown
    complexity_counts: Dict[str, int] = {}
    for item in items:
        complexity = item.get("complexity", "unknown")
        complexity_counts[complexity] = complexity_counts.get(complexity, 0) + 1
    
    lines.append("## Complexity Breakdown")
    lines.append("")
    for complexity in ["low", "medium", "high", "unknown"]:
        count = complexity_counts.get(complexity, 0)
        if count > 0:
            pct = (count / len(items)) * 100
            lines.append(f"- **{complexity}**: {count} ({pct:.1f}%)")
    
    lines.append("")
    
    # Write output
    OUTPUT_MD.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_MD, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    
    print(f"✓ Wrote enriched digest to {OUTPUT_MD} ({len(lines)} lines)")

if __name__ == "__main__":
    export_enriched_markdown()

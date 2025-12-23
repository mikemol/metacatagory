#!/usr/bin/env python3
"""
Export enriched canonical roadmap to human-readable Markdown digest.

Reads build/canonical_enriched.json and outputs build/reports/tasks_enriched.md
with detailed semantic information for each task.
"""

import json
from pathlib import Path
from typing import List, Dict, Any

REPO_ROOT = Path(__file__).parent.parent
ENRICHED_JSON = REPO_ROOT / "build" / "canonical_enriched.json"
OUTPUT_MD = REPO_ROOT / "build" / "reports" / "tasks_enriched.md"


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
        return f"{'  ' * indent}*Not specified*\n"
    
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
    
    # Header
    lines = [
        f"## {idx}. {task_id}: {title}",
        "",
        f"**Status:** `{status}` | **Category:** {category} | **Complexity:** {complexity}",
        ""
    ]
    
    # Intent
    intent = item.get("intent", "")
    if intent:
        lines.extend([
            "### Intent",
            "",
            intent,
            ""
        ])
    
    # Deliverable
    deliverable = item.get("deliverable", "")
    if deliverable:
        lines.extend([
            "### Deliverable",
            "",
            deliverable,
            ""
        ])
    
    # Scope
    scope = item.get("scope", {})
    if scope:
        lines.extend([
            "### Scope",
            "",
            format_scope(scope),
        ])
    
    # Inputs/Outputs
    inputs = item.get("inputs", [])
    outputs = item.get("outputs", [])
    
    if inputs or outputs:
        lines.append("### Inputs & Outputs")
        lines.append("")
        
        if inputs:
            lines.append("**Inputs:**")
            lines.append(format_list(inputs))
        
        if outputs:
            lines.append("**Outputs:**")
            lines.append(format_list(outputs))
    
    # Acceptance Criteria
    acceptance = item.get("acceptance", [])
    if acceptance:
        lines.extend([
            "### Acceptance Criteria",
            "",
            format_list(acceptance),
        ])
    
    # Module Anchors
    module_anchors = item.get("moduleAnchors", [])
    if module_anchors:
        lines.extend([
            "### Agda Modules",
            "",
            format_list(module_anchors),
        ])
    
    # Extracted Definitions
    definitions = item.get("definitions", [])
    if definitions:
        def_str = ", ".join(f"`{d}`" for d in definitions[:8])
        if len(definitions) > 8:
            def_str += f" (+ {len(definitions) - 8} more)"
        lines.extend([
            "### Key Definitions",
            "",
            def_str,
            ""
        ])
    
    # Tags
    tags = item.get("derivedTags", [])
    if tags:
        tag_str = ", ".join(f"`{t}`" for t in tags[:10])
        lines.extend([
            "### Tags",
            "",
            tag_str,
            ""
        ])
    
    # Dependencies & Relations
    depends_on = item.get("dependsOn", [])
    related = item.get("related", [])
    suggested_deps = item.get("suggestedDependencies", [])
    
    if depends_on or related or suggested_deps:
        lines.append("### Dependencies & Relations")
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
            "### Evidence",
            "",
            format_evidence(evidence),
        ])
    
    # Provenance
    provenance = item.get("provenance", [])
    if provenance:
        prov_str = " → ".join(f"`{p}`" for p in provenance[:3])
        if len(provenance) > 3:
            prov_str += f" (+ {len(provenance) - 3} more)"
        
        lines.extend([
            "### Provenance",
            "",
            prov_str,
            ""
        ])
    
    lines.append("---")
    lines.append("")
    
    return "\n".join(lines)


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
    task_idx = 1
    for category in sorted(by_category.keys()):
        lines.extend([
            f"# Category: {category}",
            "",
            f"**Tasks in this category:** {len(by_category[category])}",
            "",
            "---",
            ""
        ])
        
        for item in by_category[category]:
            lines.append(format_task_section(item, task_idx))
            task_idx += 1
    
    # Statistics
    lines.extend([
        "# Statistics",
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

#!/usr/bin/env python3
"""
Generate dependency graph visualization from enriched canonical roadmap.

Outputs:
- build/reports/dependency_graph.mmd (Mermaid)
- build/reports/dependency_graph.dot (GraphViz DOT)
"""

import json
from pathlib import Path
from typing import List, Dict, Set

REPO_ROOT = Path(__file__).parent.parent
ENRICHED_JSON = REPO_ROOT / "build" / "canonical_enriched.json"
MERMAID_OUTPUT = REPO_ROOT / "build" / "reports" / "dependency_graph.mmd"
DOT_OUTPUT = REPO_ROOT / "build" / "reports" / "dependency_graph.dot"


def escape_label(text: str) -> str:
    """Escape special characters for labels."""
    return text.replace('"', '\\"').replace("'", "\\'")


def generate_mermaid_graph(items: List[Dict]) -> str:
    """Generate Mermaid flowchart from task dependencies."""
    lines = [
        "```mermaid",
        "graph TB",
        "  %% Task Dependency Graph",
        ""
    ]
    
    # Track unique edges
    edges: Set[tuple] = set()
    
    # Node definitions with status coloring
    status_colors = {
        "completed": ":::completed",
        "planned": ":::planned",
        "not-started": ":::notstarted",
        "deferred": ":::deferred"
    }
    
    for item in items:
        task_id = item.get("id", "unknown")
        title = item.get("title", "")[:40]  # Truncate long titles
        status = item.get("status", "not-started")
        
        # Sanitize ID for Mermaid
        node_id = task_id.replace("-", "_").replace(".", "_")
        
        # Node with status class
        status_class = status_colors.get(status, "")
        lines.append(f'  {node_id}["{task_id}: {escape_label(title)}"]{status_class}')
        
        # Explicit dependencies
        for dep_id in item.get("dependsOn", []):
            dep_node_id = dep_id.replace("-", "_").replace(".", "_")
            edges.add((dep_node_id, node_id))
        
        # Suggested dependencies (dashed)
        for dep_id in item.get("suggestedDependencies", [])[:3]:  # Limit to avoid clutter
            dep_node_id = dep_id.replace("-", "_").replace(".", "_")
            # Use different edge style
            edges.add((dep_node_id, node_id, "suggested"))
    
    lines.append("")
    
    # Edge definitions
    for edge in sorted(edges):
        if len(edge) == 3 and edge[2] == "suggested":
            # Dashed edge for suggested
            lines.append(f"  {edge[0]} -.-> {edge[1]}")
        else:
            # Solid edge for explicit
            lines.append(f"  {edge[0]} --> {edge[1]}")
    
    lines.extend([
        "",
        "  %% Status styling",
        "  classDef completed fill:#90EE90,stroke:#2E8B57,stroke-width:2px",
        "  classDef planned fill:#87CEEB,stroke:#4682B4,stroke-width:2px",
        "  classDef notstarted fill:#FFE4B5,stroke:#DAA520,stroke-width:2px",
        "  classDef deferred fill:#D3D3D3,stroke:#808080,stroke-width:2px",
        "```"
    ])
    
    return "\n".join(lines)


def generate_dot_graph(items: List[Dict]) -> str:
    """Generate GraphViz DOT format from task dependencies."""
    lines = [
        "digraph TaskDependencies {",
        '  graph [rankdir=TB, bgcolor="white"];',
        '  node [shape=box, style="rounded,filled", fontname="sans-serif"];',
        '  edge [fontname="sans-serif"];',
        ""
    ]
    
    # Node definitions
    status_colors = {
        "completed": "#90EE90",
        "planned": "#87CEEB",
        "not-started": "#FFE4B5",
        "deferred": "#D3D3D3"
    }
    
    for item in items:
        task_id = item.get("id", "unknown")
        title = item.get("title", "")[:40]
        status = item.get("status", "not-started")
        category = item.get("category", "")
        
        color = status_colors.get(status, "#FFFFFF")
        label = f"{task_id}\\n{escape_label(title)}"
        
        # Escape ID for DOT
        node_id = f'"{task_id}"'
        
        lines.append(f'  {node_id} [label="{label}", fillcolor="{color}", tooltip="{category}"];')
    
    lines.append("")
    
    # Edges
    edges: Set[tuple] = set()
    
    for item in items:
        task_id = item.get("id", "unknown")
        node_id = f'"{task_id}"'
        
        # Explicit dependencies
        for dep_id in item.get("dependsOn", []):
            dep_node_id = f'"{dep_id}"'
            edges.add((dep_node_id, node_id, "explicit"))
        
        # Suggested dependencies
        for dep_id in item.get("suggestedDependencies", [])[:3]:
            dep_node_id = f'"{dep_id}"'
            edges.add((dep_node_id, node_id, "suggested"))
    
    for edge in sorted(edges):
        if edge[2] == "suggested":
            lines.append(f'  {edge[0]} -> {edge[1]} [style=dashed, color="#888888"];')
        else:
            lines.append(f'  {edge[0]} -> {edge[1]} [color="#000000"];')
    
    lines.extend([
        "",
        "  // Legend",
        '  subgraph cluster_legend {',
        '    label="Legend";',
        '    style=filled;',
        '    fillcolor="#F0F0F0";',
        '    legend_completed [label="Completed", fillcolor="#90EE90"];',
        '    legend_planned [label="Planned", fillcolor="#87CEEB"];',
        '    legend_notstarted [label="Not Started", fillcolor="#FFE4B5"];',
        '    legend_deferred [label="Deferred", fillcolor="#D3D3D3"];',
        "  }",
        "}"
    ])
    
    return "\n".join(lines)


def generate_dependency_summary(items: List[Dict]) -> Dict:
    """Generate summary statistics about dependencies."""
    total_tasks = len(items)
    tasks_with_explicit_deps = sum(1 for i in items if i.get("dependsOn", []))
    tasks_with_suggested_deps = sum(1 for i in items if i.get("suggestedDependencies", []))
    
    total_explicit = sum(len(i.get("dependsOn", [])) for i in items)
    total_suggested = sum(len(i.get("suggestedDependencies", [])) for i in items)
    
    # Find most connected tasks
    by_incoming = {}
    for item in items:
        task_id = item.get("id", "")
        # Count how many tasks depend on this one
        incoming = sum(
            1 for other in items
            if task_id in other.get("dependsOn", []) or task_id in other.get("suggestedDependencies", [])
        )
        if incoming > 0:
            by_incoming[task_id] = incoming
    
    most_depended_on = sorted(by_incoming.items(), key=lambda x: x[1], reverse=True)[:5]
    
    return {
        "total_tasks": total_tasks,
        "tasks_with_explicit_deps": tasks_with_explicit_deps,
        "tasks_with_suggested_deps": tasks_with_suggested_deps,
        "total_explicit_deps": total_explicit,
        "total_suggested_deps": total_suggested,
        "most_depended_on": most_depended_on
    }


def export_dependency_graphs() -> None:
    """Main export function."""
    if not ENRICHED_JSON.exists():
        print(f"Error: {ENRICHED_JSON} not found. Run 'make roadmap-enrich' first.")
        return
    
    with open(ENRICHED_JSON, "r", encoding="utf-8") as f:
        items = json.load(f)
    
    print(f"Generating dependency graphs for {len(items)} tasks...")
    
    # Generate Mermaid
    mermaid_content = generate_mermaid_graph(items)
    MERMAID_OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with open(MERMAID_OUTPUT, "w", encoding="utf-8") as f:
        f.write(mermaid_content)
    print(f"  ✓ Mermaid graph: {MERMAID_OUTPUT}")
    
    # Generate DOT
    dot_content = generate_dot_graph(items)
    with open(DOT_OUTPUT, "w", encoding="utf-8") as f:
        f.write(dot_content)
    print(f"  ✓ GraphViz DOT: {DOT_OUTPUT}")
    
    # Generate summary
    summary = generate_dependency_summary(items)
    
    print(f"\\nDependency Statistics:")
    print(f"  Total tasks: {summary['total_tasks']}")
    print(f"  Tasks with explicit dependencies: {summary['tasks_with_explicit_deps']}")
    print(f"  Tasks with suggested dependencies: {summary['tasks_with_suggested_deps']}")
    print(f"  Total explicit edges: {summary['total_explicit_deps']}")
    print(f"  Total suggested edges: {summary['total_suggested_deps']}")
    
    if summary['most_depended_on']:
        print(f"\\n  Most depended-on tasks:")
        for task_id, count in summary['most_depended_on']:
            print(f"    - {task_id}: {count} incoming dependencies")


if __name__ == "__main__":
    export_dependency_graphs()

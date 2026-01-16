#!/usr/bin/env python3

"""
SPPF-Composable Onboarding Header

Roadmap: src/agda/Plan/CIM/Utility.agda
Architecture: ARCHITECTURE.md
Onboarding: .github/copilot-instructions.md

Constructive Proof Semantics:
- This script participates in the composable SPPF model, mirroring Agda record patterns for protocol,
    witness, and universal property semantics.
- All logic should be traceable to roadmap nodes and architectural principles.
- For onboarding, review the architecture and roadmap, and recursively revisit related nodes for
    context and composability.

Phase Diagram Generator for Metacatagory Project

Analyzes test adapters to identify phase boundaries exercised:

Outputs:
"""

import re
import json
from pathlib import Path
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Any

# Use built-in generics for type hints (PEP 585, Python 3.9+)
import networkx as nx
from graphviz import Digraph
from rich.console import Console

from scripts.shared.io import save_json

console = Console()

@dataclass
class PhaseNode:
    """Represents a phase in the development (chapter/section/theorem)."""

    id: str
    label: str
    chapter: str
    section: str
    node_type: str
    properties: dict[str, Any] = field(default_factory=dict)

@dataclass
class PhaseEdge:
    """Represents a dependency/link between phases."""

    source: str
    target: str
    edge_type: str  # 'contains', 'depends_on', 'links_to', 'witness'
    properties: dict[str, Any] = field(default_factory=dict)

class PhaseDiagramGenerator:
    """Generates phase diagrams from test suite structure."""

    def __init__(self, test_dir: Path) -> None:
        self.test_dir = test_dir
        self.nodes: dict[str, PhaseNode] = {}
        self.edges: list[PhaseEdge] = []
        self.graph = nx.DiGraph()

    def parse_test_files(self) -> None:
        """Parse test files to extract phase structure."""
        for test_file in sorted(self.test_dir.glob("Chapter*Checklist.agda")):
            self._parse_chapter_file(test_file)

    def _parse_chapter_file(self, filepath: Path) -> None:
        """Parse a single chapter checklist file."""
        filename = filepath.stem
        chapter_match = re.match(r"(Chapter\d+)", filename)
        if not chapter_match:
            return

        chapter = chapter_match.group(1)
        chapter_id = f"{chapter}"

        # Add chapter node
        if chapter_id not in self.nodes:
            self.nodes[chapter_id] = PhaseNode(
                id=chapter_id,
                label=chapter,
                chapter=chapter,
                section="",
                node_type="chapter",
            )

        try:
            content = filepath.read_text(encoding="utf-8")

            # Find section boundaries (e.g., "-- Level1sub3")
            section_pattern = re.compile(r"^-+\s*\n--\s+Level\d+sub(\d+)", re.MULTILINE)
            sections = set()
            for match in section_pattern.finditer(content):
                subsection = match.group(1)
                sections.add(subsection)

            # Add section nodes and edges
            for section in sections:
                section_id = f"{chapter}.{section}"
                if section_id not in self.nodes:
                    self.nodes[section_id] = PhaseNode(
                        id=section_id,
                        label=f"§{section}",
                        chapter=chapter,
                        section=section,
                        node_type="section",
                    )
                    self.edges.append(
                        PhaseEdge(
                            source=chapter_id, target=section_id, edge_type="contains"
                        )
                    )

            # Find adapters
            adapter_pattern = re.compile(r"(\w+)-adapter\s*:\s*A\.(\w+)", re.MULTILINE)

            for match in adapter_pattern.finditer(content):
                adapter_name = match.group(1)
                adapter_type = match.group(2)

                # Infer section from context
                adapter_pos = match.start()
                preceding = content[:adapter_pos]
                section_num = self._infer_section(preceding)

                adapter_id = f"{chapter}.{section_num}.{adapter_name}"
                section_id = f"{chapter}.{section_num}"

                # Add adapter node
                if adapter_id not in self.nodes:
                    self.nodes[adapter_id] = PhaseNode(
                        id=adapter_id,
                        label=adapter_name,
                        chapter=chapter,
                        section=section_num,
                        node_type="adapter",
                        properties={"adapter_type": adapter_type},
                    )

                    # Link to section
                    if section_id in self.nodes:
                        self.edges.append(
                            PhaseEdge(
                                source=section_id,
                                target=adapter_id,
                                edge_type="contains",
                            )
                        )

            # Find link assertions to identify dependencies
            link_pattern = re.compile(
                r"(\w+)-\w+-link\s*:\s*.*?≡\s*([\w.]+)", re.MULTILINE
            )

            for match in link_pattern.finditer(content):
                source_adapter = match.group(1)
                target_ref = match.group(2)

                # Try to find corresponding adapter nodes
                source_nodes = [nid for nid in self.nodes if source_adapter in nid]
                if source_nodes:
                    for source_id in source_nodes:
                        self.edges.append(
                            PhaseEdge(
                                source=source_id,
                                target=f"witness.{target_ref}",
                                edge_type="links_to",
                                properties={"reference": target_ref},
                            )
                        )

        except Exception as e:
            console.print(f"[yellow]Warning: Error parsing {filepath}: {e}[/yellow]")

    def _infer_section(self, preceding_text: str) -> str:
        """Infer section number from preceding context."""
        # Look for Level markers
        level_match = re.search(r"Level\d+sub(\d+)", preceding_text[::-1])
        if level_match:
            return level_match.group(1)[::-1]

        # Look for chk markers
        chk_match = re.search(r"chk\d+s(\d+)", preceding_text[::-1])
        if chk_match:
            return chk_match.group(1)[::-1]

        return "0"

    def build_graph(self) -> None:
        """Build NetworkX graph from nodes and edges."""
        for node in self.nodes.values():
            self.graph.add_node(
                node.id,
                label=node.label,
                node_type=node.node_type,
                chapter=node.chapter,
                section=node.section,
            )

        for edge in self.edges:
            if edge.source in self.graph and edge.target in self.graph:
                self.graph.add_edge(edge.source, edge.target, edge_type=edge.edge_type)

    def generate_dot(self, output_path: Path, show_adapters: bool = True) -> Digraph:
        """Generate Graphviz DOT file."""
        dot = Digraph(comment="Metacatagory Phase Diagram")
        dot.attr(rankdir="TB", splines="ortho")
        dot.attr("node", shape="box", style="rounded,filled")

        # Color scheme
        colors = {
            "chapter": "#E3F2FD",
            "section": "#BBDEFB",
            "adapter": "#90CAF9",
            "witness": "#FFF9C4",
        }

        # Group by chapter
        chapters = defaultdict(list)
        for node in self.nodes.values():
            if node.node_type == "chapter":
                continue
            chapters[node.chapter].append(node)

        # Add chapter subgraphs
        for chapter, nodes_in_chapter in sorted(chapters.items()):
            # Set cluster attributes for chapter
            dot.attr("subgraph", name=f"cluster_{chapter}")
            dot.attr(label=chapter, style="rounded", color="#1976D2")

            # Group sections
            sections = defaultdict(list)
            for node in nodes_in_chapter:
                if node.node_type == "section":
                    dot.node(node.id, label=node.label, fillcolor=colors["section"])
                elif node.node_type == "adapter" and show_adapters:
                    sections[node.section].append(node)

            # Add adapters within sections
            if show_adapters:
                for section, adapters in sorted(sections.items()):
                    for adapter in adapters:
                        dot.node(
                            adapter.id,
                            label=adapter.label,
                            fillcolor=colors["adapter"],
                            shape="ellipse",
                        )

        # Add edges
        for edge in self.edges:
            if edge.source in self.graph and edge.target in self.graph:
                style = "solid"
                color = "black"

                if edge.edge_type == "contains":
                    style = "dashed"
                    color = "#666666"
                elif edge.edge_type == "links_to":
                    color = "#4CAF50"
                    style = "bold"

                dot.edge(edge.source, edge.target, style=style, color=color)

        # Write DOT file
        dot_content = dot.source
        output_path.write_text(dot_content, encoding="utf-8")
        console.print(f"[green]✓[/green] DOT file written to {output_path}")

        return dot

    def render_diagram(self, dot_file: Path, output_format: str = "svg") -> Path | None:
        """Render diagram to image using Graphviz."""
        try:
            from graphviz import Source

            source = Source.from_file(str(dot_file))
            output_path = dot_file.with_suffix(f".{output_format}")
            source.render(outfile=output_path, format=output_format, cleanup=True)
            console.print(
                f"[green]✓[/green] Rendered {output_format.upper()} to {output_path}"
            )
            return output_path
        except Exception as e:
            console.print(f"[yellow]Warning: Could not render diagram: {e}[/yellow]")
            console.print(
                "[dim]Install graphviz binary: sudo apt-get install graphviz[/dim]"
            )
            return None

    def export_json(self, output_path: Path) -> None:
        """Export phase structure as JSON."""
        data = {
            "nodes": [
                {
                    "id": node.id,
                    "label": node.label,
                    "chapter": node.chapter,
                    "section": node.section,
                    "type": node.node_type,
                    "properties": node.properties,
                }
                for node in self.nodes.values()
            ],
            "edges": [
                {
                    "source": edge.source,
                    "target": edge.target,
                    "type": edge.edge_type,
                    "properties": edge.properties,
                }
                for edge in self.edges
            ],
        }

        save_json(output_path, data)
        console.print(f"[green]✓[/green] JSON export written to {output_path}")

def main() -> None:
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(description="Generate phase boundary diagrams")
    parser.add_argument(
        "--test-dir",
        type=Path,
        default=Path("src/agda/Tests"),
        help="Directory containing test files (default: src/agda/Tests)",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("docs/automation"),
        help="Output directory for diagrams (default: docs/automation/)",
    )
    parser.add_argument(
        "--format",
        choices=["dot", "svg", "png", "pdf"],
        default="svg",
        help="Output format (default: svg)",
    )
    parser.add_argument(
        "--show-adapters",
        action="store_true",
        default=True,
        help="Include adapter nodes in diagram",
    )
    parser.add_argument(
        "--json", action="store_true", help="Also export JSON representation"
    )

    args = parser.parse_args()

    # Create output directory
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Generate diagram
    generator = PhaseDiagramGenerator(args.test_dir)

    console.print("[cyan]Parsing test files...[/cyan]")
    generator.parse_test_files()

    console.print(
        f"[cyan]Found {len(generator.nodes)} nodes, {len(generator.edges)} edges[/cyan]"
    )

    console.print("[cyan]Building graph...[/cyan]")
    generator.build_graph()

    # Generate DOT
    dot_path = args.output_dir / "phase_diagram.dot"
    console.print("[cyan]Generating DOT file...[/cyan]")
    generator.generate_dot(dot_path, show_adapters=args.show_adapters)

    # Render if requested
    if args.format != "dot":
        console.print(f"[cyan]Rendering {args.format.upper()}...[/cyan]")
        generator.render_diagram(dot_path, args.format)

    # Export JSON if requested
    if args.json:
        json_path = args.output_dir / "phase_structure.json"
        console.print("[cyan]Exporting JSON...[/cyan]")
        generator.export_json(json_path)

    console.print("[bold green]✓ Phase diagram generation complete![/bold green]")

if __name__ == "__main__":
    main()

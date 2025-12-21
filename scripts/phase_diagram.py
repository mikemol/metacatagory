#!/usr/bin/env python3
"""
Generate a coarse phase diagram from test suite adapters.

This script inspects src/agda/Tests/*.agda and builds a DOT graph of:
- Test modules
- Record types (e.g., KernelPairDeclaration)
- Adapter types (e.g., KernelPairAdapter)
with edges: Module -> Record -> Adapter.

Outputs DOT to build/diagrams/phases.dot; if graphviz 'dot' is available
and --render is passed, also writes SVG.
"""
from __future__ import annotations
import argparse
import shutil
import subprocess
import re
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
OUT_DIR = ROOT / "build" / "diagrams"

MODULE_RE = re.compile(r"^\s*module\s+([A-Za-z0-9_.]+)\s+where\s*$")
# E.g., `chk2s2B : S2.KernelPairDeclaration`
RECORD_DECL_RE = re.compile(r"^\s*([A-Za-z0-9_']+)\s*:\s*([A-Za-z0-9_.]+)\.([A-Za-z0-9_]+)\s*$")
# E.g., `foo : A.KernelPairAdapter`
ADAPTER_DECL_RE = re.compile(r"^\s*([A-Za-z0-9_']+)\s*:\s*A\.([A-Za-z0-9_]+)\s*$")


def parse_tests() -> dict[str, Any]:
    graph: dict[str, Any] = {
        "modules": {},  # mod -> {records:set(), adapters:set()}
    }
    for path in sorted(TESTS_DIR.glob("*.agda")):
        mod: str | None = None
        records: set[str] = set()
        adapters: set[str] = set()
        for line in path.read_text(encoding="utf-8").splitlines():
            m = MODULE_RE.match(line)
            if m:
                mod = m.group(1)
            m = RECORD_DECL_RE.match(line)
            if m:
                records.add(m.group(3))
            m = ADAPTER_DECL_RE.match(line)
            if m:
                adapters.add(m.group(2))
        key: str = mod or path.stem
        graph["modules"][key] = {
            "records": sorted(records),
            "adapters": sorted(adapters),
        }
    return graph


def to_dot(graph: dict[str, Any]) -> str:
    lines = ["digraph phases {", "  rankdir=LR;", "  node [shape=box, style=rounded];"]
    # Define clusters by module
    for mod, data in graph["modules"].items():
        cluster_name = "cluster_" + mod.replace(".", "_")
        lines.append(f"  subgraph {cluster_name} {{")
        lines.append(f"    label=\"{mod}\";")
        # module node
        mod_node = mod.replace('.', '_')
        lines.append(f"    {mod_node} [label=\"{mod}\", shape=folder, style=filled, fillcolor=lightgray];")
        for r in data["records"]:
            rnode = f"{mod_node}__R__{r}"
            lines.append(f"    {rnode} [label=\"{r}\", shape=component];")
            lines.append(f"    {mod_node} -> {rnode};")
        for a in data["adapters"]:
            anode = f"{mod_node}__A__{a}"
            lines.append(f"    {anode} [label=\"{a}\", shape=oval];")
            # Connect all records in module to adapter (coarse view)
            for r in data["records"]:
                rnode = f"{mod_node}__R__{r}"
                lines.append(f"    {rnode} -> {anode};")
        lines.append("  }")
    lines.append("}")
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", default=str(OUT_DIR))
    parser.add_argument(
        "--render", action="store_true", help="Render SVG using dot if available"
    )
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    graph = parse_tests()
    dot = to_dot(graph)
    dot_path = out_dir / "phases.dot"
    dot_path.write_text(dot, encoding="utf-8")
    print(f"Wrote {dot_path}")

    if args.render:
        if shutil.which("dot"):
            svg_path = out_dir / "phases.svg"
            subprocess.run(
                ["dot", "-Tsvg", str(dot_path), "-o", str(svg_path)], check=False
            )
            print(f"Rendered {svg_path}")
        else:
            print("dot not found; skipping render.")


if __name__ == "__main__":
    main()

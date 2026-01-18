#!/usr/bin/env python3
"""
Generate a coarse phase diagram from test suite adapters.

This script delegates to phase_diagram_new for parsing and DOT output.
Outputs DOT to build/diagrams/phases.dot; if graphviz 'dot' is available
and --render is passed, also writes SVG.
"""
from __future__ import annotations
import argparse
import shutil
import subprocess
from pathlib import Path
from scripts.phase_diagram_new import PhaseDiagramGenerator

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / "src" / "agda" / "Tests"
OUT_DIR = ROOT / "build" / "diagrams"

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", default=str(OUT_DIR))
    parser.add_argument(
        "--render", action="store_true", help="Render SVG using dot if available"
    )
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    dot_path = out_dir / "phases.dot"
    generator = PhaseDiagramGenerator(TESTS_DIR)
    generator.parse_test_files()
    generator.build_graph()
    generator.generate_dot(dot_path, show_adapters=True)
    print(f"Wrote {dot_path}")

    if args.render:
        if shutil.which("dot"):
            svg_path = generator.render_diagram(dot_path, output_format="svg")
            if svg_path:
                print(f"Rendered {svg_path}")
        else:
            print("dot not found; skipping render.")

if __name__ == "__main__":
    main()

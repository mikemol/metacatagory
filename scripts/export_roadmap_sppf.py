#!/usr/bin/env python3
"""
Export roadmap to SPPF JSON format for graph visualization.
Reads from the planning index (data preferred, build fallback).
"""
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts import shared_data
from scripts.shared.io import save_json

def main():
    # Read from planning index, not tasks.json
    repo_root = ROOT
    items = shared_data.load_planning_index(repo_root=repo_root)
    out_path = Path("build/gp_roadmap_sppf.json")

    nodes = []
    for it in items:
        nodes.append({
            'id': it.get('id', ''),
            'title': it.get('title', ''),
            'status': it.get('status', ''),
            'category': it.get('category', ''),
            'source': it.get('source', ''),
            'files': it.get('files', []),
            'tags': it.get('tags', []),
            'parents': [],
        })

    save_json(out_path, {'nodes': nodes})

if __name__ == '__main__':
    main()

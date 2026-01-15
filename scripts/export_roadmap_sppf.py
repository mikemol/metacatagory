#!/usr/bin/env python3
"""
Export roadmap to SPPF JSON format for graph visualization.
Reads from the planning index (data preferred, build fallback).
"""
import json
from pathlib import Path

from scripts import shared_data

def main():
    # Read from planning index, not tasks.json
    repo_root = Path.cwd()
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

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open('w') as f:
        json.dump({'nodes': nodes}, f, indent=2)

if __name__ == '__main__':
    main()

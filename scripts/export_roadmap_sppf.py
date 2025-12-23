#!/usr/bin/env python3
"""
Export roadmap to SPPF JSON format for graph visualization.
Reads from build/canonical_roadmap.json (canonical source)
"""
import json
from pathlib import Path

def main():
    # Read from canonical, not tasks.json
    in_path = Path('build/canonical_roadmap.json')
    out_path = Path('build/gp_roadmap_sppf.json')

    with in_path.open('r') as f:
        items = json.load(f)

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

#!/usr/bin/env python3
"""
Merge all roadmap sources into canonical RoadmapItem list.
Sources:
- .github/roadmap/tasks.json (GitHub issues)
- ROADMAP.md (structured markdown)
- src/agda/Plan/CIM/IngestedRoadmaps/*.agda (GP file extractions)
- roadmap-*.agda (legacy Agda schemas)
"""

import json
import re
from pathlib import Path
from typing import List, Dict

# --- Helpers --------------------------------------------------------------

def normalize_title(title: str) -> str:
    return re.sub(r"\W+", " ", title).strip().lower()

def ensure_provenance(item: Dict) -> None:
    """Ensure each item carries at least one provenance marker."""
    src = item.get("source", "")
    item.setdefault("provenance", [])
    if not item["provenance"]:
        item["provenance"] = [f"{item.get('id','')}|{src}"]

def load_tasks_json(path: Path) -> List[Dict]:
    """Load tasks.json as RoadmapItems."""
    if not path.exists():
        return []
    with open(path) as f:
        tasks = json.load(f)
    
    items = []
    for task in tasks:
        item = {
            "id": task.get("id", ""),
            "title": task.get("title", ""),
            "description": task.get("description", task.get("title", "")),
            "status": task.get("status", "not-started"),
            "category": task.get("category", ""),
            "source": task.get("source", ""),
            "files": task.get("files", []),
            "tags": task.get("tags", []),
            "dependsOn": task.get("dependsOn", []),
            "related": task.get("related", []),
            "provenance": task.get("provenance", [])
        }
        ensure_provenance(item)
        items.append(item)
    return items

def parse_roadmap_md(path: Path) -> List[Dict]:
    """Extract roadmap items from ROADMAP.md structured section."""
    if not path.exists():
        return []
    
    with open(path) as f:
        content = f.read()
    
    items = []
    # Pattern: * **Title** — Description [status: X]
    # Target: path(s)
    # Depends on: dep1, dep2
    
    pattern = r'\* \*\*(.+?)\*\* — (.+?) \[status: (.+?)\]\s*Target: `(.+?)`'
    matches = re.finditer(pattern, content, re.MULTILINE)
    
    for i, match in enumerate(matches):
        title, description, status, targets = match.groups()
        # Generate ID from title
        item_id = f"ROADMAP-MD-{i+1}"
        
        files = [t.strip() for t in targets.split(',')]
        
        item = {
            "id": item_id,
            "title": title.strip(),
            "description": description.strip(),
            "status": status.strip(),
            "category": "Roadmap",
            "source": "ROADMAP.md",
            "files": files,
            "tags": [],
            "dependsOn": [],
            "related": [],
            "provenance": []
        }
        
        # Try to extract dependencies from next line
        # Look for "Depends on:" pattern after this match
        end_pos = match.end()
        next_section = content[end_pos:end_pos + 500]
        dep_match = re.search(r'Depends on: `(.+?)`', next_section)
        if dep_match:
            deps = [d.strip() for d in dep_match.group(1).split(' — src/agda/')]
            item["dependsOn"] = [f"ROADMAP-MD-DEP-{d[:50]}" for d in deps if d]
        
        ensure_provenance(item)
        items.append(item)
    
    return items


def load_doclint_json(base_path: Path) -> List[Dict]:
    """Load doclint results (already shaped as RoadmapItems)."""
    path = base_path / "build" / "doclint_roadmap.json"
    if not path.exists():
        return []
    with open(path) as f:
        items = json.load(f)
    for item in items:
        item.setdefault("category", "Quality/DocLint")
        ensure_provenance(item)
    return items

def parse_ingested_agda(base_path: Path) -> List[Dict]:
    """Extract roadmap steps from IngestedRoadmaps/*.agda modules."""
    items = []
    ingested_dir = base_path / "src/agda/Plan/CIM/IngestedRoadmaps"
    
    if not ingested_dir.exists():
        return items
    
    for agda_file in ingested_dir.glob("*.agda"):
        with open(agda_file) as f:
            content = f.read()
        
        # Pattern: roadmapGpXXX : RoadmapStep
        # Extract: provenance, step, status, targetModule
        # Capture string literals that may contain escaped quotes.
        str_lit = r'"((?:[^"\\\\]|\\\\.)+)"'
        pattern = (
            rf'roadmap(Gp\d+) : RoadmapStep\s+roadmap\1 = record\s+\{{[^}}]+'
            rf'provenance\s+=\s+{str_lit}[^}}]+'
            rf'step\s+=\s+{str_lit}[^}}]+'
            rf'status\s+=\s+{str_lit}[^}}]+'
            rf'targetModule\s+=\s+{str_lit}'
        )
        
        matches = re.finditer(pattern, content, re.DOTALL)

        def unescape(s: str) -> str:
            try:
                return bytes(s, "utf-8").decode("unicode_escape")
            except Exception:
                return s

        for match in matches:
            gp_id, provenance, step, status, target = match.groups()
            provenance = unescape(provenance.strip())
            step = unescape(step.strip())
            status = unescape(status.strip())
            target = unescape(target.strip())

            item = {
                "id": f"GP-{gp_id}",
                "title": provenance,
                "description": step,
                "status": status,
                "category": "IngestedGP",
                "source": f"Plan/CIM/IngestedRoadmaps/{agda_file.name}",
                "files": [target],
                "tags": ["GP"],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
            ensure_provenance(item)
            items.append(item)
    
    return items

def parse_legacy_agda(base_path: Path) -> List[Dict]:
    """Extract items from legacy roadmap-*.agda files."""
    items = []
    
    # These files have various schemas, extract what we can
    for agda_file in base_path.glob("roadmap-*.agda"):
        # Simple extraction: look for record definitions or postulates
        # This is best-effort since schemas vary
        with open(agda_file) as f:
            content = f.read()
        
        # Skip very large files (likely generated)
        if len(content) > 100000:
            continue
        
        # Extract record type names as potential items
        type_pattern = r'data (\w+) : Set where'
        for match in re.finditer(type_pattern, content):
            type_name = match.group(1)
            item = {
                "id": f"LEGACY-{agda_file.stem}-{type_name}",
                "title": f"Type: {type_name} from {agda_file.name}",
                "status": "completed",  # Legacy items assumed done
                "category": "LegacyAgda",
                "source": agda_file.name,
                "files": [str(agda_file)],
                "tags": ["legacy", "agda"],
                "dependsOn": [],
                "related": [],
                "provenance": []
            }
            ensure_provenance(item)
            items.append(item)
    
    return items

def deduplicate_by_id(items: List[Dict]) -> List[Dict]:
    """Deduplicate items by ID, merging dependsOn and related lists."""
    seen: Dict[str, Dict] = {}
    
    for item in items:
        item_id = item["id"]
        if item_id in seen:
            # Merge dependencies and related
            existing = seen[item_id]
            existing["dependsOn"] = list(set(existing["dependsOn"] + item["dependsOn"]))
            existing["related"] = list(set(existing["related"] + item["related"]))
            existing["tags"] = list(set(existing["tags"] + item["tags"]))
            # Prefer non-empty values
            for key in ["title", "status", "category", "source"]:
                if not existing[key] and item[key]:
                    existing[key] = item[key]
        else:
            seen[item_id] = item
    
    return list(seen.values())

def merge_by_title(items: List[Dict]) -> List[Dict]:
    """Merge duplicates that share the same normalized title, preserving provenance.

    - Select a primary item (prefer non ROADMAP-MD-* IDs) to keep the stable id.
    - Union tags/files/dependsOn/related across variants.
    - Record provenance of all absorbed items in `provenance`.
    """

    groups: Dict[str, List[Dict]] = {}
    for item in items:
        ensure_provenance(item)
        groups.setdefault(normalize_title(item["title"]), []).append(item)

    merged: List[Dict] = []
    for _, group in groups.items():
        if len(group) == 1:
            merged.append(group[0])
            continue

        # Choose primary: first non ROADMAP-MD-* if any, else first
        primary = next((g for g in group if not g["id"].startswith("ROADMAP-MD-")), group[0])

        def union(field: str):
            seen = []
            for g in group:
                for v in g.get(field, []):
                    if v not in seen:
                        seen.append(v)
            return seen

        merged_item = {
            "id": primary["id"],
            "title": primary["title"],
            "description": primary.get("description", primary["title"]),
            "status": primary["status"],
            "category": primary["category"],
            "source": primary["source"],
            "files": union("files"),
            "tags": union("tags"),
            "dependsOn": union("dependsOn"),
            "related": union("related"),
            "provenance": union("provenance"),
        }

        merged.append(merged_item)

    return merged

def backfill_descriptions(items: List[Dict]) -> List[Dict]:
    """Ensure every item has a description; fall back to title if missing."""
    for item in items:
        desc = item.get("description")
        if not desc:
            item["description"] = item.get("title", "")
    return items

def merge_all_sources(base_path: Path) -> List[Dict]:
    """Merge all roadmap sources into unified list."""
    all_items = []
    
    # Load from each source
    print("Loading tasks.json...")
    all_items.extend(load_tasks_json(base_path / ".github/roadmap/tasks.json"))
    
    print("Parsing ROADMAP.md...")
    all_items.extend(parse_roadmap_md(base_path / "ROADMAP.md"))
    
    print("Parsing IngestedRoadmaps/*.agda...")
    all_items.extend(parse_ingested_agda(base_path))
    
    print("Parsing legacy roadmap-*.agda...")
    all_items.extend(parse_legacy_agda(base_path))

    print("Loading doclint roadmap...")
    all_items.extend(load_doclint_json(base_path))
    
    print(f"Total items before deduplication: {len(all_items)}")
    
    # Deduplicate
    merged = deduplicate_by_id(all_items)
    merged = merge_by_title(merged)
    merged = backfill_descriptions(merged)
    print(f"Total items after deduplication: {len(merged)}")

    return merged

def export_to_json(items: List[Dict], output_path: Path):
    """Export canonical list to JSON."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w') as f:
        json.dump(items, f, indent=2)
    print(f"Exported {len(items)} items to {output_path}")

def export_to_agda(items: List[Dict], output_path: Path):
    """Export canonical list as Agda syntax for CanonicalRoadmap module."""
    lines = [
        "-- Canonical Roadmap Items (Auto-generated by merge_roadmaps.py)",
        "-- DO NOT EDIT MANUALLY",
        "",
        "canonicalItems : List RoadmapItem",
        "canonicalItems ="
    ]
    
    for i, item in enumerate(items):
        is_last = (i == len(items) - 1)

        # Escape strings for Agda
        def agda_str(s):
            return '"' + s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n') + '"'
        
        def agda_list(lst):
            if not lst:
                return "[]"
            return "(" + " ∷ ".join(agda_str(x) for x in lst) + " ∷ [])"

        # Meaningful doc line for doc-lint: ID + title + status
        lines.append(f"  -- | {item['id']}: {item['title']} [status: {item['status']}]")
        lines.append("  record {")
        lines.append(f"    id = {agda_str(item['id'])}")
        lines.append(f"    ; title = {agda_str(item['title'])}")
        lines.append(f"    ; status = {agda_str(item['status'])}")
        lines.append(f"    ; category = {agda_str(item['category'])}")
        lines.append(f"    ; source = {agda_str(item['source'])}")
        lines.append(f"    ; files = {agda_list(item['files'])}")
        lines.append(f"    ; tags = {agda_list(item['tags'])}")
        lines.append(f"    ; dependsOn = {agda_list(item['dependsOn'])}")
        lines.append(f"    ; provenance = {agda_list(item.get('provenance', []))}")
        lines.append(f"    ; related = {agda_list(item['related'])}")
        lines.append("  }")
        
        if not is_last:
            lines.append("  ∷")
    
    lines.append("  ∷ []")
    
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w') as f:
        f.write('\n'.join(lines))
    
    print(f"Exported {len(items)} items to {output_path}")

if __name__ == "__main__":
    base = Path(__file__).resolve().parent.parent
    
    # Merge all sources
    canonical = merge_all_sources(base)
    
    # Export to JSON (intermediate format)
    export_to_json(canonical, base / "build/canonical_roadmap.json")
    
    # Export to Agda syntax
    export_to_agda(canonical, base / "build/canonical_roadmap.agda")
    
    print("\n✓ Merge complete. Next steps:")
    print("  1. Review build/canonical_roadmap.json")
    print("  2. Copy build/canonical_roadmap.agda content into src/agda/Plan/CIM/RoadmapIndex.agda (replace canonical definition)")
    print("  3. Run 'make roadmap-index' to verify compilation")

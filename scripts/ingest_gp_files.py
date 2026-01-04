#!/usr/bin/env python3
"""
Ingest GP files from intake directory and generate RoadmapStep records.
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple

ROOT = Path(__file__).resolve().parent.parent

def extract_structured_sections(content: str) -> Dict[str, str]:
    """Extract Insight/Gap/Fix sections from a GP Markdown snippet."""
    section_re = re.compile(r'^\s*\*\s+\*\*The (Insight|Gap|Fix):\*\s*(.*)', re.IGNORECASE)
    sections: Dict[str, List[str]] = {}
    current: str | None = None

    for line in content.splitlines():
        heading_match = section_re.match(line)
        if heading_match:
            label = heading_match.group(1).lower()
            sections[label] = [heading_match.group(2).strip()]
            current = label
            continue

        if current:
            if re.match(r'^\s*#{1,6}', line):
                current = None
                continue
            stripped = line.strip()
            if stripped.startswith('* *The '):
                current = None
                continue
            if stripped:
                cleaned = stripped.lstrip('> ').strip()
                sections.setdefault(current, []).append(cleaned)

    return {
        label: ' '.join(lines).replace('\n', ' ').strip()
        for label, lines in sections.items()
    }


def extract_metadata_from_md(filepath: str) -> Dict:
    """Extract title, summary, and key sections from a markdown file."""
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()

    # Extract first heading as title
    title_match = re.search(r'^#{1,3}\s+(.+?)$', content, re.MULTILINE)
    title = title_match.group(1).strip() if title_match else Path(filepath).stem

    # Get first 200 chars as summary
    clean_content = re.sub(r'[#\*_`\[\]]', '', content)
    lines = [l.strip() for l in clean_content.split('\n') if l.strip()]
    summary = ' '.join(lines[:3])[:150]

    # Extract any keywords or key phrases
    keywords = set()
    patterns = [
        r'(?:implementation|implement).*?(?:of|for)\s+([^.]+)',
        r'(?:design|define)\s+([^.]+)',
        r'(?:extends?|builds?|creates?)\s+([^.]+)',
    ]

    for pattern in patterns:
        matches = re.findall(pattern, content, re.IGNORECASE)
        keywords.update(m.strip()[:50] for m in matches if m.strip())

    sections = extract_structured_sections(content)

    return {
        'title': title,
        'summary': summary,
        'keywords': list(keywords)[:3],
        'insight': sections.get('insight', ''),
        'gap': sections.get('gap', ''),
        'fix': sections.get('fix', ''),
    }

def sanitize_string(value: str) -> str:
    """Prepare text to embed inside Agda string literals."""
    escaped = value.replace('"', "'")
    escaped = escaped.replace('\\', '\\\\')
    return ' '.join(escaped.split()).strip()


def generate_roadmap_step(gp_id: str, metadata: Dict, file_number: int) -> str:
    """Generate an Agda RoadmapStep record for a GP file."""

    safe_name = f"gp{gp_id.replace('/', '').lower()}"
    title = sanitize_string(metadata['title'])[:80]  # Truncate title
    summary = sanitize_string(metadata['summary'])

    insight = sanitize_string(metadata.get('insight', ''))
    gap = sanitize_string(metadata.get('gap', ''))
    fix = sanitize_string(metadata.get('fix', ''))

    implication_parts = []
    if insight:
        implication_parts.append(f"Insight: {insight}")
    if gap:
        implication_parts.append(f"Gap: {gap}")
    if fix:
        implication_parts.append(f"Fix: {fix}")
    implication = " | ".join(implication_parts) if implication_parts else "Implication TBD from intake."

    record = f'''example{safe_name.capitalize()}Roadmap : RoadmapStep
example{safe_name.capitalize()}Roadmap = record
    {{ provenance  = "{gp_id}: {title}"
    ; relatedNodes = []
    ; step        = "{summary}"
    ; implication = "{sanitize_string(implication)}"
    ; status      = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Polytopes.agda"
    ; next = []
    }}
'''
    return record

def process_gp_directory(intake_dir: str) -> Tuple[List[str], Dict]:
    """Process all GP files and generate roadmap steps."""
    gp_dir = os.path.join(intake_dir, 'GP')
    
    if not os.path.exists(gp_dir):
        print(f"GP directory not found: {gp_dir}")
        return [], {}
    
    roadmap_records = []
    metadata_map = {}
    
    gp_files = sorted([f for f in os.listdir(gp_dir) if f.endswith('.md')])
    print(f"Found {len(gp_files)} GP files")
    
    for idx, gp_file in enumerate(gp_files):
        filepath = os.path.join(gp_dir, gp_file)
        gp_id = gp_file.replace('.md', '')
        
        try:
            metadata = extract_metadata_from_md(filepath)
            metadata_map[gp_id] = metadata
            
            # Generate roadmap step
            record = generate_roadmap_step(gp_id, metadata, idx)
            roadmap_records.append(record)
            
            print(f"  ✓ {gp_id}: {metadata['title'][:50]}")
        except Exception as e:
            print(f"  ✗ {gp_id}: {e}")
    
    return roadmap_records, metadata_map

def generate_agda_module(records: List[str]) -> str:
    """Generate the complete Agda module with all roadmap steps."""
    
    module_header = '''-- Auto-generated Roadmap Module from GP Files
-- Generated from intake/GP/*.md files

module Plan.CIM.IngestedRoadmaps where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String
open import Plan.CIM.Utility using (RoadmapStep)

------------------------------------------------------------------------
-- Ingested Roadmap Steps from GP Files
------------------------------------------------------------------------
'''
    
    records_text = '\n'.join(records)
    
    return f"{module_header}\n{records_text}"

def main():
    """Main execution."""
    intake_dir = str(ROOT / 'intake')
    
    print("=" * 70)
    print("GP File Ingestion Script")
    print("=" * 70)
    
    # Process GP directory
    records, metadata = process_gp_directory(intake_dir)
    
    print(f"\n✓ Generated {len(records)} roadmap steps")
    
    # Generate Agda module
    agda_module = generate_agda_module(records)
    
    # Save to file
    output_path = str(ROOT / 'src/agda/Plan/CIM/IngestedRoadmaps.agda')
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    with open(output_path, 'w') as f:
        f.write(agda_module)
    
    print(f"\n✓ Saved Agda module to: {output_path}")
    
    # Save metadata for reference
    metadata_path = str(ROOT / 'build/ingested_metadata.json')
    os.makedirs(os.path.dirname(metadata_path), exist_ok=True)
    
    with open(metadata_path, 'w') as f:
        json.dump({
            'total_files': len(metadata),
            'files': metadata
        }, f, indent=2)
    
    print(f"✓ Saved metadata to: {metadata_path}")
    print("\n" + "=" * 70)

if __name__ == '__main__':
    main()

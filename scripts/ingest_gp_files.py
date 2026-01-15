#!/usr/bin/env python3
"""
Ingest GP files from intake directory and generate RoadmapStep records.
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple

from scripts.shared.gp_intake import extract_metadata_from_md, strip_base64_images
from scripts.shared.gp_roadmap_render import (
    build_implication,
    build_implication_from_concepts,
    build_step_summary,
    render_roadmap_step,
    sanitize_string,
)

ROOT = Path(__file__).resolve().parent.parent

def infer_target_module(content: str, title: str, keywords: List[str]) -> str:
    """Intelligently route GP content to appropriate Agda module.
    
    Uses pattern matching on content, title, and keywords to determine
    the best target module instead of defaulting to Polytopes.agda.
    """
    content_lower = content.lower()
    title_lower = title.lower()
    all_text = f"{content_lower} {title_lower} {' '.join(keywords).lower()}"
    
    # Category theory patterns
    if re.search(r'\b(category|functor|morphism|natural transformation)\b', all_text):
        return "src/agda/Core/CategoricalAdapter.agda"
    
    # Algebra patterns - Fields
    if re.search(r'\b(field|algebra|ring|group)\b', all_text):
        if re.search(r'\bf2\b|\bfinite field\b|\bgalois\b', all_text):
            return "src/agda/Algebra/Fields/F2.agda"
        return "src/agda/Algebra/Fields/GenericField.agda"
    
    # Polynomial patterns
    if re.search(r'\bpolynomial', all_text):
        if re.search(r'\bf2\b', all_text):
            return "src/agda/Core/PolynomialsF2.agda"
        return "src/agda/Algebra/Polynomials.agda"
    
    # Matrix/Linear algebra
    if re.search(r'\b(matrix|linear|vector|rotation)\b', all_text):
        return "src/agda/Algebra/LinearAlgebra.agda"
    
    # Storage/Infrastructure
    if re.search(r'\b(storage|persist|serialize|database)\b', all_text):
        return "src/agda/Infrastructure/Storage.agda"
    
    # Geometry/Visualization (only if explicitly mentioned)
    if re.search(r'\b(polytope|geometry|visual|diagram|render)\b', all_text):
        return "src/agda/Plan/CIM/Polytopes.agda"
    
    # Default: Planning/CIM for meta-tasks
    return "src/agda/Plan/CIM/Utility.agda"


def generate_roadmap_step(gp_id: str, metadata: Dict, file_number: int, full_content: str) -> str:
    """Generate an Agda RoadmapStep record for a GP file.
    
    Uses intelligent module routing (Protocol A) and semantic extraction (Protocol B).
    """

    title = sanitize_string(metadata['title'])[:80]
    summary = build_step_summary(metadata)
    implication = build_implication(metadata)
    concept_clause = build_implication_from_concepts(metadata.get("keywords", []))
    if concept_clause:
        implication = f"{implication} | {concept_clause}"
    
    # Intelligent module routing (Protocol A)
    target_module = infer_target_module(
        content=full_content,
        title=metadata['title'],
        keywords=metadata.get('keywords', [])
    )

    return render_roadmap_step(
        gp_id=gp_id,
        title=title,
        step=summary,
        implication=implication,
        target_module=target_module,
    )

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
            # Read full content for routing analysis
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                full_content = f.read()
            
            metadata = extract_metadata_from_md(filepath)
            metadata_map[gp_id] = metadata
            
            # Generate roadmap step with intelligent routing
            record = generate_roadmap_step(gp_id, metadata, idx, full_content)
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

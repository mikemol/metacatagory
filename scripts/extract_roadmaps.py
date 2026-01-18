#!/usr/bin/env python3
"""
GP Roadmap Extraction Script

Parses intake/GP/*.md files to generate properly structured Agda roadmap modules.
Preserves mathematical coherence and dependency relationships.
"""

from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict

from scripts.shared.gp_intake import (
    build_gp_metadata,
    infer_target_module,
    load_gp_text,
    load_concept_config,
)
from scripts.shared.gp_roadmap_render import (
    build_implication,
    build_implication_from_concepts,
    build_step_summary,
    render_roadmap_step,
    sanitize_string,
)
from scripts.shared.io import save_json

CONCEPT_CONFIG = load_concept_config(Path(__file__).parent / "extract-concepts-config.json")

@dataclass
class RoadmapEntry:
    """Represents a single GP roadmap entry with full context."""
    gp_number: str           # "GP01", "GP700", etc.
    title: str               # Main section header
    summary: str             # Extracted summary
    keywords: List[str]      # Metadata keywords
    insight: str             # Insight section
    gap: str                 # Gap section
    fix: str                 # Fix section
    category: str            # Foundation/Geometry/Polytopes/Analysis
    question: str            # The "Would you like..." actionable question
    formal_correction: str   # The "I. Formal Correction" section content
    key_concepts: List[str]  # Mathematical concepts mentioned
    related_gps: List[str]   # References to other GP files
    manifest_version: Optional[str]  # If this introduces a manifest (v2.0, etc.)
    target_modules: List[str]  # Agda/Python modules to implement
    target_module: str       # Routed primary module
def parse_gp_file(filepath: Path) -> RoadmapEntry:
    """Parse a single GP markdown file."""
    content = load_gp_text(filepath)
    gp_number = filepath.stem  # "GP01", "GP700", etc.
    metadata = build_gp_metadata(content, gp_number, CONCEPT_CONFIG)
    
    return RoadmapEntry(
        gp_number=gp_number,
        title=metadata['title'],
        summary=metadata.get('summary', ''),
        keywords=metadata.get('keywords', []),
        insight=metadata.get('insight', ''),
        gap=metadata.get('gap', ''),
        fix=metadata.get('fix', ''),
        category=metadata['category'],
        question=metadata['question'],
        formal_correction=metadata['formal_correction'],
        key_concepts=metadata['key_concepts'],
        related_gps=metadata['related_gps'],
        manifest_version=metadata['manifest_version'],
        target_modules=metadata['target_modules'],
        target_module=metadata['target_module'],
    )


def build_extraction_summary(entries_by_category: Dict[str, List[RoadmapEntry]]) -> Dict:
    """Build ingested_metadata-style summary payload."""
    files: Dict[str, Dict] = {}
    by_category: Dict[str, List[str]] = {}

    for category, entries in entries_by_category.items():
        by_category[category] = []
        for entry in entries:
            files[entry.gp_number] = {
                "title": entry.title,
                "summary": entry.summary,
                "keywords": entry.keywords,
                "insight": entry.insight,
                "gap": entry.gap,
                "fix": entry.fix,
                "target_module": entry.target_module,
                "category": entry.category,
                "question": entry.question,
                "formal_correction": entry.formal_correction,
                "related_gps": entry.related_gps,
                "manifest_version": entry.manifest_version,
                "target_modules": entry.target_modules,
                "key_concepts": entry.key_concepts,
            }
            by_category[category].append(entry.gp_number)

    return {
        "total_files": len(files),
        "files": files,
        "by_category": by_category,
    }

def generate_agda_module(category: str, entries: List[RoadmapEntry]) -> str:
    """Generate an Agda module for a category."""
    module_name = f"Plan.CIM.IngestedRoadmaps.{category}"
    
    agda_code = f"""-- {category} Roadmap Entries
-- Auto-generated from intake/GP/*.md files
-- Mathematical context preserved

module {module_name} where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String
open import Plan.CIM.Utility using (RoadmapStep)

------------------------------------------------------------------------
-- {category} Phase Roadmap Steps
------------------------------------------------------------------------

"""
    
    for entry in sorted(entries, key=lambda e: e.gp_number):
        implication = build_implication({
            "insight": entry.formal_correction,
            "gap": "",
            "fix": "",
        })
        concept_clause = build_implication_from_concepts(entry.key_concepts)
        if concept_clause:
            implication = f"{implication} | {concept_clause}"

        target_module = entry.target_module or infer_target_module(
            content=entry.formal_correction,
            title=entry.title,
            keywords=entry.key_concepts,
            config=CONCEPT_CONFIG,
        )

        agda_code += render_roadmap_step(
            gp_id=entry.gp_number,
            title=entry.title,
            step=build_step_summary({"summary": entry.question}),
            implication=implication,
            target_module=target_module,
        )
        agda_code += "\n"
    
    return agda_code

def main():
    """Main extraction process."""
    gp_dir = Path("intake/GP")
    output_dir = Path("src/agda/Plan/CIM/IngestedRoadmaps")
    
    if not gp_dir.exists():
        print(f"Error: {gp_dir} not found")
        return
    
    # Parse all GP files
    entries_by_category: Dict[str, List[RoadmapEntry]] = {
        "Foundation": [],
        "Geometry": [],
        "Corrections": [],
        "Polytopes": [],
        "Analysis": [],
    }
    
    gp_files = sorted(gp_dir.glob("GP*.md"))
    print(f"Found {len(gp_files)} GP files")
    
    for gp_file in gp_files:
        try:
            entry = parse_gp_file(gp_file)
            entries_by_category[entry.category].append(entry)
            print(f"✓ Parsed {entry.gp_number}: {entry.title[:50]}")
        except Exception as e:
            print(f"✗ Error parsing {gp_file.name}: {e}")
    
    # Generate Agda modules
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for category, entries in entries_by_category.items():
        if entries:
            module_code = generate_agda_module(category, entries)
            output_file = output_dir / f"{category}.agda"
            output_file.write_text(module_code, encoding='utf-8')
            print(f"✓ Generated {output_file} ({len(entries)} entries)")
    
    # Generate index module
    index_code = """-- Unified Roadmap Index
-- Re-exports all roadmap categories

module Plan.CIM.IngestedRoadmaps where

open import Plan.CIM.IngestedRoadmaps.Foundation public
open import Plan.CIM.IngestedRoadmaps.Geometry public
open import Plan.CIM.IngestedRoadmaps.Corrections public
open import Plan.CIM.IngestedRoadmaps.Polytopes public
open import Plan.CIM.IngestedRoadmaps.Analysis public
"""
    
    index_file = Path("src/agda/Plan/CIM/IngestedRoadmaps.agda")
    index_file.write_text(index_code, encoding='utf-8')
    print(f"✓ Generated {index_file}")
    
    # Generate JSON summary for documentation
    summary = build_extraction_summary(entries_by_category)
    
    summary_file = Path("build/roadmap_extraction_summary.json")
    save_json(summary_file, summary)
    print(f"✓ Generated {summary_file}")

if __name__ == "__main__":
    main()

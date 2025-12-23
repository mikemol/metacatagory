#!/usr/bin/env python3
"""
GP Roadmap Extraction Script

Parses intake/GP/*.md files to generate properly structured Agda roadmap modules.
Preserves mathematical coherence and dependency relationships.
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict

@dataclass
class RoadmapEntry:
    """Represents a single GP roadmap entry with full context."""
    gp_number: str           # "GP01", "GP700", etc.
    title: str               # Main section header
    category: str            # Foundation/Geometry/Polytopes/Analysis
    question: str            # The "Would you like..." actionable question
    formal_correction: str   # The "I. Formal Correction" section content
    key_concepts: List[str]  # Mathematical concepts mentioned
    related_gps: List[str]   # References to other GP files
    manifest_version: Optional[str]  # If this introduces a manifest (v2.0, etc.)
    target_modules: List[str]  # Agda/Python modules to implement
    
def extract_section(content: str, pattern: str) -> Optional[str]:
    """Extract a section matching the given pattern."""
    match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
    return match.group(1).strip() if match else None

def extract_title(content: str) -> str:
    """Extract the main title from section headers."""
    # Look for "I. Formal Correction:" or "I. Formal Analysis:" patterns
    patterns = [
        r'### \*\*I\. Formal (?:Correction|Analysis): (.+?)\*\*',
        r'### I\. Formal (?:Correction|Analysis): (.+?)$',
    ]
    for pattern in patterns:
        match = re.search(pattern, content, re.MULTILINE)
        if match:
            return match.group(1).strip()
    return "Unknown Title"

def extract_question(content: str) -> str:
    """Extract the actionable 'Would you like...' question."""
    # First line usually has the question
    lines = content.split('\n')
    for line in lines[:5]:  # Check first 5 lines
        if line.strip().startswith('Would you like'):
            # Clean up markdown
            question = line.strip()
            question = re.sub(r'\*\*(.+?)\*\*', r'\1', question)  # Remove bold
            return question[:500]  # Truncate if too long
    return "See full GP file for details"

def extract_formal_section(content: str) -> str:
    """Extract the formal correction/analysis section."""
    # Find the section starting with "I. Formal Correction" or "I. Formal Analysis"
    pattern = r'### \*?\*?I\. Formal (?:Correction|Analysis).*?\n\n(.+?)(?=\n###|\Z)'
    match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
    if match:
        section = match.group(1).strip()
        # Limit length but preserve mathematical content
        if len(section) > 1000:
            section = section[:1000] + "..."
        return section
    return "See full GP file"

def extract_concepts(content: str) -> List[str]:
    """Extract key mathematical concepts from the content."""
    concepts = set()
    
    # Look for capitalized mathematical terms
    concept_patterns = [
        r'\b(RoPE|SPPF|Stasheff|Associahedron|Polytope|Loday|Yoneda|Sheaf|Cohomology)',
        r'\b(Braid|Homotopy|Functor|Adjunction|Manifold|Topology|Geometry)',
        r'\b(Lie Group|Group Action|Category Theory|HoTT|Abelian)',
        r'\b(Quaternion|Octonion|Complex|Vector Space|Tensor)',
    ]
    
    for pattern in concept_patterns:
        matches = re.findall(pattern, content, re.IGNORECASE)
        concepts.update(m.title() for m in matches)
    
    return sorted(list(concepts))[:15]  # Limit to top 15

def extract_related_gps(content: str) -> List[str]:
    """Find references to other GP files."""
    # Look for "Nov 7", "Nov 26", "GP\d+" patterns
    related = set()
    
    # Direct GP references
    gp_refs = re.findall(r'GP(\d+)', content)
    related.update(f"GP{num}" for num in gp_refs)
    
    # Date references (map to GPs if we have metadata)
    # Nov 7 might be GP800, etc.
    
    return sorted(list(related))

def extract_manifest_version(content: str) -> Optional[str]:
    """Check if this GP introduces a new manifest version."""
    match = re.search(r'Manifest \(v([\d.]+)\)', content)
    return f"v{match.group(1)}" if match else None

def extract_target_modules(content: str) -> List[str]:
    """Find mentions of target implementation modules."""
    modules = set()
    
    # Look for .agda and .py file references
    agda_refs = re.findall(r'(\w+\.agda)', content)
    py_refs = re.findall(r'(\w+\.py)', content)
    
    modules.update(agda_refs)
    modules.update(py_refs)
    
    return sorted(list(modules))

def categorize_gp(gp_num: int) -> str:
    """Determine category based on GP number."""
    if gp_num <= 111:
        return "Foundation"
    elif 200 <= gp_num < 400:
        return "Geometry"
    elif 400 <= gp_num < 600:
        return "Corrections"
    elif 699 <= gp_num < 800:
        return "Polytopes"
    else:
        return "Analysis"

def parse_gp_file(filepath: Path) -> RoadmapEntry:
    """Parse a single GP markdown file."""
    content = filepath.read_text(encoding='utf-8')
    gp_number = filepath.stem  # "GP01", "GP700", etc.
    gp_num = int(re.search(r'\d+', gp_number).group())
    
    return RoadmapEntry(
        gp_number=gp_number,
        title=extract_title(content),
        category=categorize_gp(gp_num),
        question=extract_question(content),
        formal_correction=extract_formal_section(content),
        key_concepts=extract_concepts(content),
        related_gps=extract_related_gps(content),
        manifest_version=extract_manifest_version(content),
        target_modules=extract_target_modules(content),
    )

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
        # Escape strings properly
        def escape_agda_string(s: str) -> str:
            s = s.replace('\\', '\\\\')
            s = s.replace('"', '\\"')
            s = s.replace('\n', ' ')
            # Remove problematic characters
            s = re.sub(r'[^\x20-\x7E]', '', s)  # ASCII printable only
            return s
        
        title_safe = escape_agda_string(entry.title)
        question_safe = escape_agda_string(entry.question)
        concepts_str = ', '.join(entry.key_concepts[:5])  # Limit concepts
        related_str = ', '.join(entry.related_gps[:3])  # Limit related
        
        record_name = entry.gp_number.lower().replace('gp', 'gp')
        
        agda_code += f"""roadmap{record_name.capitalize()} : RoadmapStep
roadmap{record_name.capitalize()} = record
    {{ provenance   = "{entry.gp_number}: {title_safe}"
    ; relatedNodes = []  -- {related_str}
    ; step         = "{question_safe}"
    ; implication  = "Concepts: {concepts_str}"
    ; status       = "not-started"
    ; targetModule = "src/agda/Plan/CIM/Implementation.agda"
    ; next         = []
    }}

"""
    
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
    summary = {
        category: [asdict(e) for e in entries]
        for category, entries in entries_by_category.items()
    }
    
    summary_file = Path("build/roadmap_extraction_summary.json")
    summary_file.parent.mkdir(parents=True, exist_ok=True)
    summary_file.write_text(json.dumps(summary, indent=2), encoding='utf-8')
    print(f"✓ Generated {summary_file}")

if __name__ == "__main__":
    main()

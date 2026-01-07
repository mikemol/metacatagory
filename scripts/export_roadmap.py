#!/usr/bin/env python3
"""
Export ingested roadmap steps to structured Markdown ROADMAP.
"""

from pathlib import Path
from typing import Dict, List

# Import shared utilities
from scripts.shared.io import load_json, load_markdown, save_markdown
from scripts.shared.config import Config

def generate_markdown_section(metadata: Dict) -> str:
    """Generate a markdown section for each GP file."""
    
    md = "## Ingested Roadmap from GP Files\n\n"
    md += f"This section consolidates roadmap items extracted from {metadata['total_files']} GP files.\n\n"
    
    # Group files by category (using first part of filename)
    categories = {}
    for gp_id, meta in metadata['files'].items():
        # Extract category from GP ID (e.g., "GP01" -> "01", "GP830" -> "830")
        num = gp_id.replace('GP', '')
        if num.startswith('0'):
            cat = "Foundational (00-99)"
        elif num.startswith('1'):
            cat = "Structural (100-199)"
        elif num.startswith('2'):
            cat = "Geometric (200-299)"
        elif num.startswith('3'):
            cat = "Topological (300-399)"
        elif num.startswith('4'):
            cat = "Semantic (400-499)"
        elif num.startswith('5'):
            cat = "Polytope (500-599)"
        elif num.startswith('7'):
            cat = "Analysis (700-799)"
        elif num.startswith('8'):
            cat = "Unified (800-899)"
        else:
            cat = "Other"
        
        if cat not in categories:
            categories[cat] = []
        categories[cat].append((gp_id, meta))
    
    # Generate sections
    for category in sorted(categories.keys()):
        md += f"\n### {category}\n\n"
        
        for gp_id, meta in sorted(categories[category]):
            title = meta['title']
            summary = meta['summary']
            
            md += f"**{gp_id}**: {title}\n"
            md += f"> {summary}\n"
            
            if meta.get('keywords'):
                md += f"> *Keywords*: {', '.join(meta['keywords'])}\n"
            
            md += "\n"
    
    return md

def main(config: Config | None = None):
    """Main execution.
    
    Args:
        config: Optional Config instance for dependency injection (useful for testing).
    """
    if config is None:
        config = Config()
    
    roadmap_path = config.repo_root / 'ROADMAP.md'
    metadata_path = config.build_dir / 'ingested_metadata.json'
    
    print("Generating Markdown roadmap from ingested metadata...")
    
    # Load metadata using shared utility
    try:
        metadata = load_json(metadata_path, required=True)
    except (FileNotFoundError, SystemExit):
        print("Metadata file not found: " + str(metadata_path))
        return
    
    # Generate markdown
    markdown = generate_markdown_section(metadata)
    
    # Read existing ROADMAP using shared utility
    existing = load_markdown(roadmap_path, default="")
    
    if existing:
        # Find insertion point (before ## Implementation Status if it exists)
        insertion_point = existing.find("## Implementation Status")
        if insertion_point == -1:
            insertion_point = existing.find("## See Also")
        if insertion_point == -1:
            insertion_point = len(existing)
        
        # Insert new section
        updated = existing[:insertion_point] + "\n" + markdown + "\n" + existing[insertion_point:]
    else:
        updated = markdown
    
    # Save updated ROADMAP using shared utility
    save_markdown(roadmap_path, updated)
    
    print(f"✓ Updated {roadmap_path}")
    print(f"✓ Added {metadata['total_files']} ingested roadmap items")

if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""
Reconstruct corrupted markdown tables in intake files.
The tables have been collapsed into continuous lines with content run together.
"""

import re
import sys
from pathlib import Path

from scripts.shared.io import load_markdown, save_markdown
from scripts.shared.markdown import reconstruct_table_section

def process_file(filepath: Path) -> None:
    """Process a single markdown file to fix table corruption."""
    print(f"Processing {filepath}")
    
    content = load_markdown(filepath)
    lines = content.splitlines(keepends=True)
    
    new_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Detect start of corrupted table (lines starting with multiple spaces and dashes)
        if re.match(r'^\s{2,}---', line):
            reconstructed, new_i = reconstruct_table_section(lines, i)
            new_lines.extend(reconstructed)
            i = new_i
        else:
            new_lines.append(line)
            i += 1
    
    # Write back
    save_markdown(filepath, "".join(new_lines))
    
    print(f"  ✓ Processed {filepath}")

def list_intake_files(intake_dir: Path) -> list[Path]:
    """List numbered intake files."""
    return sorted(intake_dir.glob("__[0-9]*.md"))


def main() -> None:
    intake_dir = Path("intake")
    
    # Process all numbered intake files
    files = list_intake_files(intake_dir)
    
    for filepath in files:
        process_file(filepath)
    
    print(f"\nProcessed {len(files)} files")

if __name__ == "__main__":
    main()

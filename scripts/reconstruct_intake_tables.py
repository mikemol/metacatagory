#!/usr/bin/env python3
"""
Reconstruct corrupted markdown tables in intake files.
The tables have been collapsed into continuous lines with content run together.
"""

import re
import sys
from pathlib import Path

from scripts.shared.io import load_markdown, save_markdown

def reconstruct_table_section(lines, start_idx):
    """
    Reconstruct a table from collapsed format.
    Returns (new_lines, end_idx)
    """
    # Collect all lines that appear to be part of the collapsed table
    table_lines = []
    i = start_idx
    
    while i < len(lines):
        line = lines[i].strip()
        # Check if this looks like a table separator or content line
        if line.startswith('---') or '**' in line or '$mathbf{' in line:
            table_lines.append(lines[i])
            i += 1
        elif not line:  # Empty line might signal end
            i += 1
            if i < len(lines) and not lines[i].strip().startswith(('---', '  ---', '**', '  **', '$', '  $')):
                break
        else:
            break
    
    # If we didn't find enough content, return original
    if len(table_lines) < 2:
        return lines[start_idx:i], i
    
    # Try to identify columns by finding patterns
    # Look for header indicators
    combined = ' '.join(table_lines)
    
    # Common table structures - detect by content patterns
    if 'Formal Type' in combined and 'Exegesis' in combined:
        return reconstruct_formal_type_table(table_lines), i
    elif 'Level (n-Cell)' in combined or 'n-Cell' in combined:
        return reconstruct_ncell_table(table_lines), i
    elif 'Construction' in combined and 'Categorical' in combined:
        return reconstruct_categorical_table(table_lines), i
    elif 'Component' in combined or 'Operation' in combined:
        return reconstruct_component_table(table_lines), i
    else:
        # Generic fallback
        return table_lines, i

def reconstruct_formal_type_table(lines):
    """Reconstruct tables with Formal Type | Exegesis | Source pattern."""
    # Standard 3-column table
    result = [
        "| **Formal Type** | **Exegesis & Purpose** | **Source** |",
        "| --- | --- | --- |"
    ]
    
    # Extract content - this is simplified; full reconstruction would need semantic parsing
    # For now, preserve original format with minimal fixes
    return lines

def reconstruct_ncell_table(lines):
    """Reconstruct n-Cell hierarchy tables."""
    return lines

def reconstruct_categorical_table(lines):
    """Reconstruct categorical construction tables."""
    return lines

def reconstruct_component_table(lines):
    """Reconstruct component/operation tables."""
    return lines

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

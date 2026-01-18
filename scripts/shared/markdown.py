#!/usr/bin/env python3
"""
Markdown document parsing, manipulation, and generation.

Provides utilities for:
- Parsing frontmatter and document structure
- Extracting metadata (YAML, JSON)
- Rendering markdown with templates
- Validating markdown structure
"""

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Dict, Optional, Tuple, Any
import re
import json
import textwrap

try:
    from ..shared_yaml import (
        dump_yaml as shared_dump_yaml,
        safe_load as shared_safe_load,
    )
except ImportError:
    try:
        from scripts.shared_yaml import (  # type: ignore
            dump_yaml as shared_dump_yaml,
            safe_load as shared_safe_load,
        )
    except ImportError:
        shared_dump_yaml = None
        shared_safe_load = None
try:
    import yaml  # type: ignore
except ImportError:
    yaml = None

# Import enhanced validation patterns
try:
    from .validation import ValidationResult
    from .errors import ValidationError as ScriptValidationError, FileOperationError
except ImportError:
    # Fallback for standalone usage
    ValidationResult = None  # type: ignore
    ScriptValidationError = None  # type: ignore
    FileOperationError = None  # type: ignore


@dataclass
class FrontMatter:
    """Frontmatter metadata from markdown document.
    
    Source: Enhanced with validation patterns from validation.py
            Error handling from errors.py
    """
    raw: str = ""
    data: Dict[str, Any] = field(default_factory=dict)
    format: str = "yaml"  # yaml or json
    
    def __init__(self, data_or_raw: str | Dict[str, Any] | None = None, 
                 data: Dict[str, Any] | None = None, 
                 format: str = "yaml"):
        """Initialize FrontMatter.
        
        Source: Implication 1.1 (flexible initialization)
        
        Args:
            data_or_raw: Either raw string or data dict (for convenience)
            data: Explicit data dict (takes priority)
            format: Format ('yaml' or 'json')
        
        Examples:
            FrontMatter({"title": "Test"})  # Dict initialization
            FrontMatter(raw="...", data={...})  # Explicit
            FrontMatter(data={"title": "Test"})  # Named
        """
        from .errors import ValidationError
        
        # Handle first positional argument
        if isinstance(data_or_raw, dict):
            # Convenience: FrontMatter({"title": "Test"})
            self.data = data_or_raw
            self.raw = ""
        elif isinstance(data_or_raw, str):
            # Raw string provided
            self.raw = data_or_raw
            self.data = data if data is not None else {}
        elif data_or_raw is None:
            # Default initialization
            self.raw = ""
            self.data = data if data is not None else {}
        else:
            raise ValidationError(
                f"Invalid FrontMatter initialization: expected dict or str, got {type(data_or_raw).__name__}",
                field="data_or_raw",
                hint="Use FrontMatter({...}) or FrontMatter(raw='...', data={...})"
            )
        
        self.format = format
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get a value from frontmatter.
        
        Args:
            key: Key to retrieve
            default: Default value if key not found
            
        Returns:
            Value or default
        """
        return self.data.get(key, default)
    
    def set(self, key: str, value: Any) -> None:
        """Set a frontmatter value.
        
        Source: Implication 1.2 (mutable metadata)
        
        Args:
            key: Key to set
            value: Value to store
        """
        self.data[key] = value
    
    def __getitem__(self, key: str) -> Any:
        """Support dict-like access: fm['key']."""
        return self.data[key]
    
    def __setitem__(self, key: str, value: Any) -> None:
        """Support dict-like assignment: fm['key'] = value."""
        self.data[key] = value
    
    def __contains__(self, key: str) -> bool:
        """Support 'in' operator: 'key' in fm."""
        return key in self.data
    
    def to_yaml(self) -> str:
        """Render frontmatter as YAML.
        
        Source: Enhanced with error handling from errors.py
        
        Returns:
            YAML frontmatter block
        """
        from .errors import ConfigurationError
        
        if yaml is None:
            raise ConfigurationError(
                "PyYAML module not available",
                hint="Install with: pip install pyyaml"
            )
        
        content = yaml.dump(self.data, default_flow_style=False)
        return f"---\n{content}---\n"
    
    def to_json(self) -> str:
        """Render frontmatter as JSON.
        
        Returns:
            JSON frontmatter block
        """
        return f"```json\n{json.dumps(self.data, indent=2)}\n```\n"


class MarkdownParser:
    """Parser for markdown documents."""
    
    # More flexible to handle indented test strings
    FRONTMATTER_YAML_PATTERN = re.compile(
        r'^\s*---\s*\n(.+?)\n\s*---\s*\n',
        re.DOTALL
    )
    
    FRONTMATTER_JSON_PATTERN = re.compile(
        r'^\s*```json\s*\n(.*?)\n\s*```\s*\n',
        re.DOTALL
    )
    
    # Also support bare JSON frontmatter (for compatibility)
    FRONTMATTER_JSON_BARE_PATTERN = re.compile(
        r'^\s*(\{[^}]*"[^"]+"[^}]*\})',
        re.DOTALL
    )
    
    HEADING_PATTERN = re.compile(r'^(#{1,6})\s+(.+)$', re.MULTILINE)
    CODE_BLOCK_PATTERN = re.compile(r'```(\w*)\n(.*?)\n```', re.DOTALL)
    
    def __init__(self, content: str):
        """Initialize parser.
        
        Args:
            content: Markdown document content
        """
        self.content = content
        self._frontmatter = self._parse_frontmatter()
        self.body = self._extract_body()
    
    @property
    def frontmatter(self) -> Optional[FrontMatter]:
        """Get frontmatter.
        
        Returns:
            FrontMatter object or None if no frontmatter found
        """
        # Return None for empty frontmatter
        if not self._frontmatter.raw and not self._frontmatter.data:
            return None
        return self._frontmatter
    
    def _parse_frontmatter(self) -> FrontMatter:
        """Extract frontmatter from document.
        
        Source: Enhanced with error handling from errors.py
        
        Returns:
            FrontMatter object
        """
        # Try YAML frontmatter
        match = self.FRONTMATTER_YAML_PATTERN.match(self.content)
        if match:
            raw = match.group(1)
            # Dedent to handle indented test strings
            raw = textwrap.dedent(raw)
            try:
                if shared_safe_load is not None:
                    data = shared_safe_load(raw) or {}
                elif yaml is not None:
                    data = yaml.safe_load(raw) or {}
                else:
                    data = {}  # Fallback: no parsing without yaml module
                return FrontMatter(data_or_raw=raw, data=data, format="yaml")
            except Exception:
                pass  # Continue to try JSON
        
        # Try JSON frontmatter with code fence
        match = self.FRONTMATTER_JSON_PATTERN.match(self.content)
        if match:
            raw = match.group(1)
            raw = textwrap.dedent(raw)  # Dedent for indented content
            try:
                data = json.loads(raw)
                return FrontMatter(data_or_raw=raw, data=data, format="json")
            except Exception:
                pass
        
        # Try bare JSON frontmatter (for test compatibility)
        match = self.FRONTMATTER_JSON_BARE_PATTERN.match(self.content)
        if match:
            raw = match.group(1)
            raw = textwrap.dedent(raw)  # Dedent for indented content
            try:
                data = json.loads(raw)
                # Find where JSON ends to extract properly
                return FrontMatter(data_or_raw=raw, data=data, format="json")
            except Exception:
                pass
        
        return FrontMatter()
    
    def _extract_body(self) -> str:
        """Extract document body (content after frontmatter).
        
        Returns:
            Document body
        """
        # Remove YAML frontmatter
        match = self.FRONTMATTER_YAML_PATTERN.match(self.content)
        if match:
            return self.content[match.end():]
        
        # Remove JSON frontmatter with code fence
        match = self.FRONTMATTER_JSON_PATTERN.match(self.content)
        if match:
            return self.content[match.end():]
        
        # Remove bare JSON frontmatter
        match = self.FRONTMATTER_JSON_BARE_PATTERN.match(self.content)
        if match:
            # Find end of JSON and skip to next line
            json_end = match.end()
            remaining = self.content[json_end:]
            # Skip whitespace/newlines after JSON
            return remaining.lstrip('\n')
        
        return self.content
    
    def get_headings(self) -> List[Tuple[int, str]]:
        """Extract all headings from document.
        
        Returns:
            List of (level, text) tuples
        """
        headings = []
        for match in self.HEADING_PATTERN.finditer(self.body):
            level = len(match.group(1))
            text = match.group(2)
            headings.append((level, text))
        return headings
    
    def get_code_blocks(self) -> List[Tuple[str, str]]:
        """Extract all code blocks from document.
        
        Returns:
            List of (language, code) tuples
        """
        blocks = []
        for match in self.CODE_BLOCK_PATTERN.finditer(self.body):
            language = match.group(1) or "text"
            code = match.group(2)
            blocks.append((language, code))
        return blocks

    def get_links(self) -> List[Tuple[str, str]]:
        """Extract all links from document.
        
        Returns:
            List of (text, url) tuples
        """
        link_pattern = re.compile(r'\[([^\]]+)\]\(([^)]+)\)')
        links = []
        for match in link_pattern.finditer(self.body):
            text = match.group(1)
            url = match.group(2)
            links.append((text, url))
        return links

    def get_table_of_contents(self) -> str:
        """Generate table of contents from headings.
        
        Returns:
            Markdown formatted TOC
        """
        headings = self.get_headings()
        
        if not headings:
            return ""
        
        lines = ["## Table of Contents\n"]
        
        for level, text in headings:
            if level > 1:  # Skip h1
                indent = "  " * (level - 2)
                # Convert heading to anchor
                anchor = text.lower().replace(" ", "-").replace("`", "")
                lines.append(f"{indent}- [{text}](#{anchor})")
        
        return "\n".join(lines) + "\n"


FENCED_YAML_PATTERN = re.compile(r'```yaml\n(.*?)\n```', re.DOTALL)


def parse_yaml_fenced_blocks(content: str, include_errors: bool = False) -> List[Dict[str, Any]]:
    """Parse fenced ```yaml blocks into dicts.

    Args:
        content: Markdown content to scan.
        include_errors: When True, include parse errors as dicts with
            __parse_error__ keys instead of skipping.

    Returns:
        List of parsed dicts (and optional error dicts).
    """
    parsed: List[Dict[str, Any]] = []
    blocks = FENCED_YAML_PATTERN.findall(content)
    for block in blocks:
        try:
            if shared_safe_load is not None:
                data = shared_safe_load(block)
            elif yaml is not None:
                data = yaml.safe_load(block)
            else:
                data = None
            if isinstance(data, dict):
                parsed.append(data)
            elif include_errors:
                parsed.append({"__parse_error__": "non-dict yaml block"})
        except Exception as exc:
            if include_errors:
                parsed.append({"__parse_error__": str(exc)})
    return parsed


def parse_yaml_fenced_blocks_with_errors(content: str) -> List[Dict[str, Any]]:
    """Parse fenced YAML blocks and include parse errors as sentinel dicts."""
    return parse_yaml_fenced_blocks(content, include_errors=True)


def parse_simple_yaml_block(block: str) -> Dict[str, Any]:
    """Parse a minimal YAML block without a YAML parser."""
    entry: Dict[str, Any] = {}
    current_key: str | None = None
    for line in block.splitlines():
        raw = line.rstrip()
        if not raw.strip():
            continue
        if raw.lstrip().startswith("- "):
            if current_key is None:
                continue
            item = raw.lstrip()[2:].strip()
            if item.startswith('"') and item.endswith('"'):
                item = item[1:-1]
            entry.setdefault(current_key, []).append(item)
            continue
        if ":" in raw:
            key, value = raw.split(":", 1)
            key = key.strip()
            value = value.strip()
            if key.startswith('"') and key.endswith('"'):
                key = key[1:-1]
            if value.startswith('"') and value.endswith('"'):
                value = value[1:-1]
            if value == "":
                entry[key] = []
                current_key = key
            else:
                entry[key] = value
                current_key = None
    return entry


def parse_yaml_fenced_blocks_fallback(content: str) -> List[Dict[str, Any]]:
    """Parse fenced YAML blocks with a simple fallback parser."""
    parsed = parse_yaml_fenced_blocks(content, include_errors=False)
    if parsed:
        return parsed
    if shared_safe_load is not None or yaml is not None:
        return []
    fallback: List[Dict[str, Any]] = []
    for yaml_block in FENCED_YAML_PATTERN.findall(content):
        entry = parse_simple_yaml_block(yaml_block)
        if entry:
            fallback.append(entry)
    return fallback


def extract_bracketed_ids(content: str, pattern: str = r"\[([A-Z]+-\d+|GP\d+)\]") -> List[str]:
    """Extract bracketed IDs like [PHASE-123] from content."""
    ids = {match.group(1) for match in re.finditer(pattern, content)}
    return sorted(ids)


def extract_bold_list_item_titles(content: str) -> List[str]:
    """Extract bolded list item titles like '- **Title**' from content."""
    pattern = re.compile(r'[-*]\s+\*\*(.+?)\*\*')
    titles = {match.group(1).strip() for match in pattern.finditer(content)}
    return sorted(title for title in titles if title)


def extract_roadmap_frontmatter_and_ids(content: str) -> Tuple[List[str], List[Dict[str, Any]]]:
    """Extract roadmap frontmatter entries and referenced IDs from markdown."""
    frontmatter_items: List[Dict[str, Any]] = []
    for data in parse_yaml_fenced_blocks_fallback(content):
        if data and isinstance(data, dict):
            frontmatter_items.append(data)

    ids = set()
    for item in frontmatter_items:
        item_id = item.get("id")
        if isinstance(item_id, str):
            ids.add(item_id)

    ids.update(extract_bracketed_ids(content))
    return sorted(ids), frontmatter_items


def extract_markdown_section_from_content(
    content: str,
    heading_pattern: str,
) -> Dict[str, str] | None:
    """Extract a markdown section by heading substring (case-insensitive)."""
    heading = None
    heading_line = 0
    content_lines: list[str] = []
    in_section = False

    for i, line in enumerate(content.splitlines(), start=1):
        if re.match(r"^#+\s+", line):
            if heading_pattern.lower() in line.lower():
                heading = line.strip()
                heading_line = i
                in_section = True
                continue
            if in_section:
                break
        if in_section:
            stripped = line.strip()
            if stripped and not stripped.startswith("```"):
                content_lines.append(stripped)

    if heading and content_lines:
        text = " ".join(content_lines)
        return {
            "heading": heading,
            "text": text,
            "line_start": heading_line,
        }
    return None


def extract_markdown_section(
    md_path: Path,
    heading_pattern: str,
) -> Dict[str, str] | None:
    """Extract a markdown section from a file by heading substring."""
    if not md_path.exists():
        return None
    content = md_path.read_text(encoding="utf-8")
    return extract_markdown_section_from_content(content, heading_pattern)


def parse_markdown_table_rows(text: str) -> List[List[str]]:
    """Parse markdown table rows into cell lists.

    Skips separator rows composed of dashes/colons.
    """
    rows: List[List[str]] = []
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped.startswith("|"):
            continue
        # Drop leading/trailing pipes, then split cells.
        cells = [cell.strip() for cell in stripped.strip("|").split("|")]
        if not cells:
            continue
        # Skip separator rows like | --- | ---: |
        if all(re.fullmatch(r":?-{3,}:?", cell or "-") for cell in cells):
            continue
        rows.append(cells)
    return rows


class MarkdownBuilder:
    """Builder for constructing markdown documents."""
    
    def __init__(self, title: str = ""):
        """Initialize builder.
        
        Args:
            title: Document title
        """
        self.frontmatter: Dict[str, Any] = {}
        self.sections: List[Tuple[int, str, List[str]]] = []
        
        if title:
            self.frontmatter['title'] = title
    
    def set_frontmatter(self, data: Dict[str, Any]):
        """Set frontmatter metadata.
        
        Args:
            data: Frontmatter dictionary
        """
        self.frontmatter.update(data)
    
    def add_heading(self, text: str, level: int = 1):
        """Add a heading section.
        
        Args:
            text: Heading text
            level: Heading level (1-6), defaults to 1
        """
        self.sections.append((level, text, []))
    
    def add_paragraph(self, text: str):
        """Add a paragraph to the current section.
        
        Args:
            text: Paragraph text
        """
        if self.sections:
            self.sections[-1][2].append(text)
        else:
            self.sections.append((0, "", [text]))
    
    def add_code_block(self, code: str, language: str = ""):
        """Add a code block.
        
        Args:
            code: Code content
            language: Programming language
        """
        block = f"```{language}\n{code}\n```"
        self.add_paragraph(block)
    
    def add_list(self, items: List[str], ordered: bool = False):
        """Add a list.
        
        Args:
            items: List items
            ordered: If True, create ordered list
        """
        lines = []
        for i, item in enumerate(items):
            prefix = f"{i + 1}." if ordered else "-"
            lines.append(f"{prefix} {item}")
        
        self.add_paragraph("\n".join(lines))
    
    def add_table(self, headers: List[str], rows: List[List[str]]):
        """Add a table.
        
        Args:
            headers: Column headers
            rows: Table rows (each row is list of cell values)
        """
        lines = []
        lines.append("| " + " | ".join(headers) + " |")
        lines.append("|" + "|".join(["-" * 3 for _ in headers]) + "|")
        
        for row in rows:
            lines.append("| " + " | ".join(row) + " |")
        
        self.add_paragraph("\n".join(lines))
    
    def build(self) -> str:
        """Build the final markdown document.
        
        Returns:
            Complete markdown content
        """
        lines = []
        
        # Add frontmatter
        if self.frontmatter:
            if yaml is not None:
                if shared_dump_yaml is not None:
                    fm_content = shared_dump_yaml(self.frontmatter)
                else:
                    fm_content = yaml.dump(self.frontmatter, default_flow_style=False)
                lines.append("---")
                lines.append(fm_content.rstrip())
                lines.append("---")
            else:
                lines.append(f"```json\n{json.dumps(self.frontmatter, indent=2)}\n```")
        
        # Add sections
        for level, title, content in self.sections:
            if level > 0:
                lines.append(f"{'#' * level} {title}")
            
            if content:
                lines.extend(content)
            
            lines.append("")  # Blank line between sections
        
        return "\n".join(lines).rstrip() + "\n"


class MarkdownValidator:
    """Validates markdown document structure."""
    
    def __init__(self, content: str):
        """Initialize validator.
        
        Args:
            content: Markdown content
        """
        self.content = content
        self.parser = MarkdownParser(content)
        self.errors: List[str] = []
        self.warnings: List[str] = []
    
    def validate(self) -> Dict[str, Any]:
        """Run all validations.
        
        Source: Enhanced with ValidationResult pattern, returns dict for backward compatibility
        
        Returns:
            Dictionary with validation results (for backward compatibility)
        """
        self.errors = []
        self.warnings = []
        
        self._check_frontmatter()
        self._check_headings()
        self._check_code_blocks()
        self._check_links()
        
        headings_count = len(self.parser.get_headings())
        code_blocks_count = len(self.parser.get_code_blocks())
        links_count = len(self.parser.get_links())
        
        # Build ValidationResult if available, then convert to dict
        if ValidationResult is not None:
            result = ValidationResult()
            for error in self.errors:
                result.add_error("", error, hint="Review markdown structure")
            
            # Return dict representation for backward compatibility
            return {
                'valid': result.is_valid(),
                'errors': self.errors,
                'warnings': self.warnings,
                'headings_count': headings_count,
                'code_blocks_count': code_blocks_count,
                'links_count': links_count,
                'counts': {  # Test expects nested counts dict
                    'headings': headings_count,
                    'code_blocks': code_blocks_count,
                    'links': links_count,
                },
            }
        
        # Fallback without ValidationResult
        return {
            'valid': len(self.errors) == 0,
            'errors': self.errors,
            'warnings': self.warnings,
            'headings_count': headings_count,
            'code_blocks_count': code_blocks_count,
            'links_count': links_count,
            'counts': {
                'headings': headings_count,
                'code_blocks': code_blocks_count,
                'links': links_count,
            },
        }
    
    def _check_frontmatter(self):
        """Check frontmatter validity."""
        if self.parser.frontmatter and not self.parser.frontmatter.data:
            self.warnings.append("Document has frontmatter but it's empty or malformed")
    
    def _check_headings(self):
        """Check heading structure."""
        headings = self.parser.get_headings()
        
        if not headings:
            self.warnings.append("Document has no headings")
        
        if headings and headings[0][0] != 1:
            self.errors.append("First heading should be level 1 (h1)")
        
        # Check for proper nesting
        for i, (level, _) in enumerate(headings):
            if i > 0 and level > headings[i - 1][0] + 1:
                self.errors.append(
                    f"Heading level skipped: {headings[i - 1][0]} -> {level}"
                )
    
    def _check_code_blocks(self):
        """Check code block validity."""
        blocks = self.parser.get_code_blocks()
        
        for lang, code in blocks:
            if not code.strip():
                self.warnings.append("Found empty code block")
    
    def _check_links(self):
        """Check link validity."""
        links = self.parser.get_links()
        
        for text, url in links:
            if not text:
                self.warnings.append("Found link with empty text")
            if not url:
                self.errors.append("Found link with empty URL")
    
    def get_report(self) -> Dict[str, Any]:
        """Get validation report.
        
        Source: Enhanced to support ValidationResult pattern
        
        Returns:
            Dictionary with validation results
        """
        return self.validate()


def is_json_input(text: str) -> bool:
    """Check if input looks like JSON."""
    stripped = text.lstrip()
    return stripped.startswith("{") or stripped.startswith("[")


def fix_list_markers(md: str) -> str:
    """Normalize list markers to asterisks with consistent spacing."""
    def repl(match: re.Match) -> str:
        indent = match.group(1) or ""
        content = match.group(3)
        indent_level = len(indent) // 2 if indent else 0
        return ("  " * indent_level) + "* " + content

    pattern = re.compile(r"^( *)([-*]) +(.+)$", re.MULTILINE)
    return pattern.sub(repl, md)


def remove_multiple_blank_lines(md: str) -> str:
    """Collapse multiple blank lines and trim leading blanks."""
    md = re.sub(r"^(\s*\n)+", "", md)
    lines = md.splitlines()
    while lines and lines[0].strip() == "":
        lines.pop(0)
    md = "\n".join(lines)
    return re.sub(r"\n{3,}", "\n\n", md)


def fix_horizontal_rules(md: str) -> str:
    """Normalize horizontal rule lines."""
    return re.sub(r"^[- ]{5,}$", "-" * 79, md, flags=re.MULTILINE)


def fix_headings(md: str) -> str:
    """Convert setext headings to atx headings."""
    lines = md.split("\n")
    out = []
    i = 0
    while i < len(lines):
        if i + 1 < len(lines) and re.match(r"^(=+|-+)$", lines[i + 1]):
            level = 1 if lines[i + 1].startswith("=") else 2
            out.append("#" * level + " " + lines[i].strip())
            i += 2
        else:
            out.append(lines[i])
            i += 1
    return "\n".join(out)


def postprocess_markdown(md: str) -> str:
    """Apply standard markdown cleanup passes."""
    md = remove_multiple_blank_lines(md)
    md = fix_list_markers(md)
    md = fix_horizontal_rules(md)
    md = fix_headings(md)
    return md

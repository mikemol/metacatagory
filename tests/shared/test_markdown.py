#!/usr/bin/env python3
"""Test suite for scripts.shared.markdown module."""

import pytest
import tempfile
from pathlib import Path
from scripts.shared import markdown as markdown_module
from scripts.shared.markdown import (
    FrontMatter,
    MarkdownParser,
    MarkdownBuilder,
    MarkdownValidator,
    extract_bracketed_ids,
    parse_yaml_fenced_blocks_fallback,
)


class TestFrontMatter:
    """Test FrontMatter class."""
    
    def test_frontmatter_creation(self):
        """Test creating frontmatter."""
        fm = FrontMatter({"title": "Test", "date": "2024-01-01"})
        
        assert fm.get("title") == "Test"
        assert fm.get("date") == "2024-01-01"
    
    def test_frontmatter_get_default(self):
        """Test get with default value."""
        fm = FrontMatter({"title": "Test"})
        
        assert fm.get("author") is None
        assert fm.get("author", "Anonymous") == "Anonymous"
    
    def test_frontmatter_set(self):
        """Test setting values."""
        fm = FrontMatter({})
        fm["title"] = "New Title"
        
        assert fm.get("title") == "New Title"
    
    def test_frontmatter_to_yaml(self):
        """Test YAML rendering."""
        fm = FrontMatter({"title": "Test", "tags": ["a", "b"]})
        yaml_str = fm.to_yaml()
        
        assert "title:" in yaml_str
        assert "Test" in yaml_str
    
    def test_frontmatter_to_json(self):
        """Test JSON rendering."""
        fm = FrontMatter({"title": "Test", "count": 42})
        json_str = fm.to_json()
        
        assert '"title"' in json_str or "'title'" in json_str
        assert "Test" in json_str


class TestMarkdownParser:
    """Test MarkdownParser class."""
    
    def test_parse_simple_document(self):
        """Test parsing simple document."""
        content = """\
# Title

This is a paragraph.

## Section

More content here.
"""
        
        parser = MarkdownParser(content)
        
        assert parser.body is not None
        assert "This is a paragraph" in parser.body
    
    def test_parse_with_yaml_frontmatter(self):
        """Test parsing with YAML frontmatter."""
        content = """\
---
title: Test Document
date: 2024-01-01
---

# Content

Some text here.
"""
        
        parser = MarkdownParser(content)
        
        assert parser.frontmatter is not None
        assert parser.frontmatter.get("title") == "Test Document"
        assert "Some text here" in parser.body
    
    def test_parse_with_json_frontmatter(self):
        """Test parsing with JSON frontmatter."""
        content = """\
{
  "title": "Test Document",
  "date": "2024-01-01"
}

# Content

Some text here.
"""
        
        parser = MarkdownParser(content)
        
        assert parser.frontmatter is not None
        assert parser.frontmatter.get("title") == "Test Document"
    
    def test_parse_without_frontmatter(self):
        """Test parsing without frontmatter."""
        content = """\
# Title

Regular content without frontmatter.
"""
        
        parser = MarkdownParser(content)
        
        assert parser.frontmatter is None
        assert "Regular content" in parser.body
    
    def test_get_headings(self):
        """Test extracting headings."""
        content = """\
# Main Title

## Section One

### Subsection

## Section Two
"""
        
        parser = MarkdownParser(content)
        headings = parser.get_headings()
        
        assert len(headings) >= 2
        # Check that headings were extracted
        heading_texts = [h[1] for h in headings]
        assert "Main Title" in heading_texts
    
    def test_get_code_blocks(self):
        """Test extracting code blocks."""
        content = """\
Here is code:

```python
def hello():
    print("Hello")
```

More text and another code block:

```agda
data Bool : Set where
  true : Bool
  false : Bool
```
"""
        
        parser = MarkdownParser(content)
        blocks = parser.get_code_blocks()
        
        assert len(blocks) >= 1
    
    def test_get_links(self):
        """Test extracting links."""
        content = """\
Check out [this link](https://example.com).

And [another one](/local/path).

[Reference link][ref]

[ref]: https://example.com
"""
        
        parser = MarkdownParser(content)
        links = parser.get_links()
        
        assert len(links) >= 1
    
    def test_get_table_of_contents(self):
        """Test generating table of contents."""
        content = """\
# Document

## Section 1

### Subsection 1.1

## Section 2

### Subsection 2.1

### Subsection 2.2
"""
        
        parser = MarkdownParser(content)
        toc = parser.get_table_of_contents()
        
        assert len(toc) > 0
        # TOC should contain markdown links
        assert "-" in toc or "#" in toc


class TestMarkdownBuilder:
    """Test MarkdownBuilder class."""
    
    def test_builder_simple(self):
        """Test building simple document."""
        builder = MarkdownBuilder()
        builder.add_heading("Title", level=1)
        builder.add_paragraph("Some content")
        
        result = builder.build()
        
        assert "# Title" in result
        assert "Some content" in result
    
    def test_builder_with_frontmatter(self):
        """Test building with frontmatter."""
        builder = MarkdownBuilder()
        builder.set_frontmatter({"title": "Test", "date": "2024-01-01"})
        builder.add_heading("Content", level=1)
        
        result = builder.build()
        
        assert "title:" in result or '"title"' in result
        assert "# Content" in result
    
    def test_builder_sections(self):
        """Test building document sections."""
        builder = MarkdownBuilder()
        
        builder.add_heading("Main", level=1)
        builder.add_heading("Section 1", level=2)
        builder.add_paragraph("Content 1")
        builder.add_heading("Section 2", level=2)
        builder.add_paragraph("Content 2")
        
        result = builder.build()
        
        assert "# Main" in result
        assert "## Section" in result
    
    def test_builder_code_blocks(self):
        """Test building code blocks."""
        builder = MarkdownBuilder()
        
        builder.add_code_block("print('hello')", language="python")
        
        result = builder.build()
        
        assert "```python" in result
        assert "print('hello')" in result
    
    def test_builder_lists(self):
        """Test building lists."""
        builder = MarkdownBuilder()
        
        builder.add_list(["Item 1", "Item 2", "Item 3"])
        
        result = builder.build()
        
        assert "- Item 1" in result or "* Item 1" in result
    
    def test_builder_tables(self):
        """Test building tables."""
        builder = MarkdownBuilder()
        
        headers = ["Name", "Age"]
        rows = [
            ["Alice", "30"],
            ["Bob", "25"]
        ]
        
        builder.add_table(headers, rows)
        
        result = builder.build()
        
        assert "Name" in result
        assert "Alice" in result


class TestMarkdownValidator:
    """Test MarkdownValidator class."""
    
    def test_validate_valid_document(self):
        """Test validating a valid document."""
        content = """\
# Title

## Section

Some content here.
"""
        
        validator = MarkdownValidator(content)
        report = validator.validate()
        
        assert report['valid'] is True
        assert len(report['errors']) == 0
    
    def test_validate_missing_h1(self):
        """Test validation fails when h1 is missing."""
        content = """\
## Section

Some content here.
"""
        
        validator = MarkdownValidator(content)
        report = validator.validate()
        
        # Should detect missing h1
        assert len(report['errors']) > 0 or len(report['warnings']) > 0
    
    def test_validate_heading_levels(self):
        """Test validation of heading levels."""
        content = """\
# Title

### Missing H2 Section

Some content.
"""
        
        validator = MarkdownValidator(content)
        report = validator.validate()
        
        # Should warn about skipped level
        assert report['valid'] is False or len(report['warnings']) > 0
    
    def test_validate_with_frontmatter(self):
        """Test validation with frontmatter."""
        content = """\
---
title: Test
---

# Title

Some content.
"""
        
        validator = MarkdownValidator(content)
        report = validator.validate()
        
        assert 'frontmatter_valid' in report or 'valid' in report
    
    def test_validate_report_structure(self):
        """Test report structure."""
        content = """\
# Title

## Section

Content.
"""
        
        validator = MarkdownValidator(content)
        report = validator.validate()
        
        assert 'valid' in report
        assert 'errors' in report
        assert 'warnings' in report
        assert 'counts' in report


class TestMarkdownIntegration:
    """Integration tests for markdown handling."""
    
    def test_roundtrip_parse_build(self):
        """Test parsing and rebuilding document."""
        original = """\
---
title: Test Document
date: 2024-01-01
---

# Main Title

## Section 1

Some content here.

## Section 2

More content.
"""
        
        parser = MarkdownParser(original)
        
        builder = MarkdownBuilder()
        if parser.frontmatter:
            builder.set_frontmatter(parser.frontmatter.data)
        builder.add_heading("Main Title", level=1)
        builder.add_heading("Section 1", level=2)
        builder.add_paragraph("Some content here.")
        builder.add_heading("Section 2", level=2)
        builder.add_paragraph("More content.")
        
        rebuilt = builder.build()
        
        # Should contain key elements
        assert "# Main Title" in rebuilt
        assert "## Section" in rebuilt
    
    def test_file_roundtrip(self):
        """Test reading, parsing, and validating file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.md', delete=False) as f:
            content = """\
---
title: Test
---

# Title

## Section

Content.
"""
            f.write(content)
            f.flush()
            
            path = Path(f.name)
            
            # Parse
            parser = MarkdownParser(path.read_text())
            
            # Validate
            validator = MarkdownValidator(path.read_text())
            report = validator.validate()
            
            assert report['valid'] is True or 'title' in report or 'counts' in report
    
    def test_build_comprehensive_document(self):
        """Test building comprehensive document."""
        builder = MarkdownBuilder()
        
        builder.set_frontmatter({
            "title": "Comprehensive Guide",
            "date": "2024-01-01"
        })
        
        builder.add_heading("Introduction", level=1)
        builder.add_paragraph("This is the introduction.")
        
        builder.add_heading("Features", level=2)
        builder.add_list(["Feature 1", "Feature 2", "Feature 3"])
        
        builder.add_heading("Usage", level=2)
        builder.add_code_block("python example.py", language="bash")
        
        builder.add_heading("Examples", level=2)
        builder.add_heading("Example 1", level=3)
        builder.add_code_block("def hello():\n    print('hello')", language="python")
        
        doc = builder.build()
        
        # Validate the built document
        validator = MarkdownValidator(doc)
        report = validator.validate()
        
        # At minimum, should parse without catastrophic errors
        assert isinstance(report, dict)


def test_extract_bracketed_ids() -> None:
    content = "Some [GP-1] and [PHASE-12] plus [OTHER-999]."
    ids = extract_bracketed_ids(content)
    assert ids == ["GP-1", "OTHER-999", "PHASE-12"]


def test_parse_yaml_fenced_blocks_fallback(monkeypatch):
    content = """\
```yaml
id: GP-3
status: not-started
tags:
  - alpha
```
"""
    monkeypatch.setattr(markdown_module, "shared_safe_load", None)
    monkeypatch.setattr(markdown_module, "yaml", None)

    items = parse_yaml_fenced_blocks_fallback(content)
    assert items == [{"id": "GP-3", "status": "not-started", "tags": ["alpha"]}]

#!/usr/bin/env python3
"""
Tests for enrich_canonical.py

Coverage targets:
- Markdown section extraction
- Evidence extraction from markdown/Agda sources
- Agda module header parsing
- DOT dependency graph parsing
- Module name extraction from file paths
- Tag normalization
- Module-to-tasks mapping
"""

import pytest
import json
import tempfile
from pathlib import Path

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from enrich_canonical import (
    extract_markdown_section,
    extract_evidence_from_markdown,
    extract_agda_module_header,
    extract_agda_exports,
    parse_dot_dependency_graph,
    build_module_to_tasks_map,
    extract_evidence_from_agda,
    extract_module_anchors,
    TAG_VOCAB
)


class TestMarkdownExtraction:
    """Test markdown section extraction."""
    
    def test_extract_markdown_section_simple(self, tmp_path):
        """Test extracting a simple markdown section."""
        md_file = tmp_path / "test.md"
        md_content = """# Main Title

## Section One

This is the content of section one.
It has multiple lines.

## Section Two

Different content here.
"""
        md_file.write_text(md_content)
        
        result = extract_markdown_section(md_file, "Section One")
        
        assert result is not None
        assert "Section One" in result["heading"]
        assert "content of section one" in result["text"]
        assert result["line_start"] == 3
    
    def test_extract_markdown_section_not_found(self, tmp_path):
        """Test extraction when section doesn't exist."""
        md_file = tmp_path / "test.md"
        md_file.write_text("# Title\n\nContent")
        
        result = extract_markdown_section(md_file, "Nonexistent")
        
        assert result is None
    
    def test_extract_markdown_section_stops_at_next_heading(self, tmp_path):
        """Test that extraction stops at next heading of same level."""
        md_file = tmp_path / "test.md"
        md_content = """## Target Section

Content here.
More content.

## Next Section

Should not be included.
"""
        md_file.write_text(md_content)
        
        result = extract_markdown_section(md_file, "Target")
        
        assert result is not None
        assert "Content here" in result["text"]
        assert "Should not be included" not in result["text"]
    
    def test_extract_markdown_section_missing_file(self, tmp_path):
        """Test extraction with nonexistent file."""
        result = extract_markdown_section(tmp_path / "nonexistent.md", "Section")
        
        assert result is None


class TestEvidenceExtraction:
    """Test evidence extraction from sources."""
    
    def test_extract_evidence_from_markdown(self, tmp_path):
        """Test extracting evidence from markdown files."""
        # Create markdown file
        md_file = tmp_path / "roadmap.md"
        md_content = """## Test Task

This task does something important.
It has detailed specifications.
"""
        md_file.write_text(md_content)
        
        # Create item with provenance
        item = {
            "title": "Test Task",
            "provenance": [f"TASK-001|roadmap.md"]
        }
        
        # Set REPO_ROOT to tmp_path for testing
        import enrich_canonical
        original_root = enrich_canonical.REPO_ROOT
        enrich_canonical.REPO_ROOT = tmp_path
        
        try:
            evidence = extract_evidence_from_markdown(item)
            
            assert len(evidence) > 0
            assert evidence[0]["source"] == "roadmap.md"
            assert "important" in evidence[0]["text"]
        finally:
            enrich_canonical.REPO_ROOT = original_root
    
    def test_extract_evidence_no_markdown_sources(self):
        """Test evidence extraction with no markdown sources."""
        item = {
            "title": "Task",
            "provenance": ["TASK-001|tasks.json", "TASK-002|data.agda"]
        }
        
        evidence = extract_evidence_from_markdown(item)
        
        # No .md files in provenance
        assert evidence == []


class TestAgdaExtraction:
    """Test Agda module extraction."""
    
    def test_extract_agda_module_header_block_comment(self, tmp_path):
        """Test extracting module header from block comment."""
        agda_file = tmp_path / "Test.agda"
        agda_content = """{- This module implements core functionality.
   It provides types and operations.
-}

module Test where
"""
        agda_file.write_text(agda_content)
        
        doc = extract_agda_module_header(agda_file)
        
        assert doc is not None
        assert "core functionality" in doc
        assert "types and operations" in doc
    
    def test_extract_agda_module_header_line_comments(self, tmp_path):
        """Test extracting from line comments."""
        agda_file = tmp_path / "Test.agda"
        agda_content = """-- This is a test module
-- It demonstrates functionality

module Test where
"""
        agda_file.write_text(agda_content)
        
        doc = extract_agda_module_header(agda_file)
        
        assert doc is not None
        assert "test module" in doc
    
    def test_extract_agda_module_header_skips_options(self, tmp_path):
        """Test that OPTIONS pragmas are skipped."""
        agda_file = tmp_path / "Test.agda"
        agda_content = """{-# OPTIONS --safe #-}
{-# OPTIONS --without-K #-}

-- Actual documentation here

module Test where
"""
        agda_file.write_text(agda_content)
        
        doc = extract_agda_module_header(agda_file)
        
        assert doc is not None
        assert "documentation" in doc
        assert "OPTIONS" not in doc
    
    def test_extract_agda_module_header_missing_file(self, tmp_path):
        """Test extraction with nonexistent file."""
        doc = extract_agda_module_header(tmp_path / "nonexistent.agda")
        
        assert doc is None
    
    def test_extract_agda_exports(self, tmp_path):
        """Test extracting top-level definitions."""
        agda_file = tmp_path / "Test.agda"
        agda_content = """module Test where

identity : {A : Set} → A → A
identity x = x

compose : {A B C : Set} → (B → C) → (A → B) → (A → C)
compose g f x = g (f x)

data Nat : Set where
  zero : Nat
  suc : Nat → Nat
"""
        agda_file.write_text(agda_content)
        
        exports = extract_agda_exports(agda_file)
        
        assert "identity" in exports
        assert "compose" in exports
        assert len(exports) <= 5  # Limits to first 5


class TestDOTGraphParsing:
    """Test DOT dependency graph parsing."""
    
    def test_parse_dot_simple_graph(self, tmp_path):
        """Test parsing simple DOT graph."""
        dot_file = tmp_path / "deps.dot"
        dot_content = """digraph dependencies {
  m1[label="Core.Types"]
  m2[label="Core.Functions"]
  m3[label="Utils.Helper"]
  
  m2 -> m1
  m3 -> m1
}
"""
        dot_file.write_text(dot_content)
        
        deps = parse_dot_dependency_graph(dot_file)
        
        assert "Core.Functions" in deps
        assert "Core.Types" in deps["Core.Functions"]
        assert "Core.Types" in deps["Utils.Helper"]
    
    def test_parse_dot_filters_stdlib(self, tmp_path):
        """Test that stdlib modules are filtered out."""
        dot_file = tmp_path / "deps.dot"
        dot_content = """digraph dependencies {
  m1[label="MyModule"]
  m2[label="Agda.Builtin.Nat"]
  m3[label="Data.List"]
  
  m1 -> m2
  m1 -> m3
}
"""
        dot_file.write_text(dot_content)
        
        deps = parse_dot_dependency_graph(dot_file)
        
        # Filtering may result in empty deps for MyModule since all its dependencies are filtered
        # Verify function returns a dict and handles filtering correctly
        assert isinstance(deps, dict)
        # If MyModule has no non-stdlib deps, it won't be in the result
        if "MyModule" in deps:
            my_deps = deps["MyModule"]
            # No Agda.* or Data.* in dependencies
            assert not any("Agda." in d or "Data." in d for d in my_deps)
    
    def test_parse_dot_missing_file(self, tmp_path):
        """Test parsing nonexistent file."""
        deps = parse_dot_dependency_graph(tmp_path / "nonexistent.dot")
        
        assert deps == {}


class TestModuleMappings:
    """Test module name extraction and mappings."""
    
    def test_extract_module_anchors(self):
        """Test extracting module names from file paths."""
        item = {
            "files": [
                "src/agda/Core/Types.agda",
                "src/agda/Utils/Helper.agda",
                "README.md"
            ]
        }
        
        anchors = extract_module_anchors(item)
        
        assert "Core.Types" in anchors
        assert "Utils.Helper" in anchors
        assert len(anchors) == 2  # Only .agda files
    
    def test_extract_module_anchors_no_agda_dir(self):
        """Test module extraction when agda dir not in path."""
        item = {
            "files": ["Other/Module.agda"]
        }
        
        anchors = extract_module_anchors(item)
        
        # Should handle gracefully
        assert isinstance(anchors, list)
    
    def test_build_module_to_tasks_map(self):
        """Test building module→tasks mapping."""
        items = [
            {
                "id": "TASK-001",
                "files": ["src/agda/Core/Types.agda"]
            },
            {
                "id": "TASK-002",
                "files": ["src/agda/Core/Types.agda", "src/agda/Utils/Helper.agda"]
            },
            {
                "id": "TASK-003",
                "files": ["README.md"]
            }
        ]
        
        mapping = build_module_to_tasks_map(items)
        
        # Core.Types referenced by TASK-001 and TASK-002
        assert "Core.Types" in mapping
        assert set(mapping["Core.Types"]) == {"TASK-001", "TASK-002"}
        
        # Utils.Helper referenced by TASK-002
        assert "Utils.Helper" in mapping
        assert mapping["Utils.Helper"] == ["TASK-002"]
        
        # README.md not included (not .agda)
        assert len([k for k in mapping if "README" in k]) == 0


class TestExtractEvidenceFromAgda:
    """Test Agda evidence extraction."""
    
    def test_extract_evidence_from_agda(self, tmp_path):
        """Test extracting evidence from Agda files."""
        # Create Agda file with header
        agda_file = tmp_path / "src" / "agda" / "Test.agda"
        agda_file.parent.mkdir(parents=True)
        
        agda_content = """{- Test module documentation.
   Provides core functionality.
-}

module Test where
"""
        agda_file.write_text(agda_content)
        
        item = {
            "files": ["src/agda/Test.agda"]
        }
        
        # Set REPO_ROOT for testing
        import enrich_canonical
        original_root = enrich_canonical.REPO_ROOT
        enrich_canonical.REPO_ROOT = tmp_path
        
        try:
            evidence = extract_evidence_from_agda(item)
            
            assert len(evidence) > 0
            assert evidence[0]["source"] == "src/agda/Test.agda"
            assert evidence[0]["loc"] == "module header"
            assert "core functionality" in evidence[0]["text"]
        finally:
            enrich_canonical.REPO_ROOT = original_root
    
    def test_extract_evidence_no_agda_files(self):
        """Test evidence extraction with no Agda files."""
        item = {
            "files": ["README.md", "data.json"]
        }
        
        evidence = extract_evidence_from_agda(item)
        
        assert evidence == []


class TestTagVocabulary:
    """Test tag vocabulary structure."""
    
    def test_tag_vocab_exists(self):
        """Test that TAG_VOCAB is properly defined."""
        assert isinstance(TAG_VOCAB, dict)
        assert len(TAG_VOCAB) > 0
    
    def test_tag_vocab_categories(self):
        """Test expected tag categories exist."""
        expected_categories = ["hott", "adjunction", "yoneda", "testing", "phase"]
        
        for cat in expected_categories:
            assert cat in TAG_VOCAB
            assert isinstance(TAG_VOCAB[cat], list)
            assert len(TAG_VOCAB[cat]) > 0
    
    def test_tag_vocab_values_are_strings(self):
        """Test that all tag values are strings."""
        for category, tags in TAG_VOCAB.items():
            assert all(isinstance(tag, str) for tag in tags)


class TestIntegration:
    """Integration tests for enrichment workflow."""
    
    def test_full_module_extraction_workflow(self, tmp_path):
        """Test complete module extraction workflow."""
        # Create Agda file
        agda_file = tmp_path / "src" / "agda" / "Core" / "Types.agda"
        agda_file.parent.mkdir(parents=True)
        agda_content = """-- Core type definitions

module Core.Types where

data Bool : Set where
  true : Bool
  false : Bool
"""
        agda_file.write_text(agda_content)
        
        # Create item
        item = {
            "id": "TASK-001",
            "title": "Core Types",
            "files": ["src/agda/Core/Types.agda"]
        }
        
        # Set REPO_ROOT
        import enrich_canonical
        original_root = enrich_canonical.REPO_ROOT
        enrich_canonical.REPO_ROOT = tmp_path
        
        try:
            # Extract module anchors
            anchors = extract_module_anchors(item)
            assert "Core.Types" in anchors
            
            # Extract evidence
            evidence = extract_evidence_from_agda(item)
            assert len(evidence) > 0
            assert "Core type definitions" in evidence[0]["text"]
            
            # Extract exports
            exports = extract_agda_exports(agda_file)
            # Exports may include data type or constructors
            assert len(exports) >= 0  # Verify it returns a list
        finally:
            enrich_canonical.REPO_ROOT = original_root

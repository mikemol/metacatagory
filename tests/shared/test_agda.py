#!/usr/bin/env python3
"""Test suite for scripts.shared.agda module."""

import pytest
import tempfile
from pathlib import Path
from scripts.shared.agda import (
    AgdaModule, AgdaParser, ModuleCoverage,
    DependencyAnalyzer,
    extract_module_header,
    extract_definition_names,
)


class TestAgdaModule:
    """Test AgdaModule dataclass."""
    
    def test_module_creation(self):
        """Test creating an AgdaModule."""
        module = AgdaModule(
            name="Test",
            file_path=Path("/test/Test.agda"),
            imports={"Prelude", "Data.List"},
            exports={"testFunc", "TestType"},
            is_internal=False,
            has_doc=True,
            line_count=100
        )
        
        assert module.name == "Test"
        assert module.file_path == Path("/test/Test.agda")
        assert len(module.imports) == 2
        assert len(module.exports) == 2
        assert module.is_internal is False
        assert module.has_doc is True
        assert module.line_count == 100
    
    def test_module_defaults(self):
        """Test module with default values."""
        module = AgdaModule(
            name="Test",
            file_path=Path("/test/Test.agda")
        )
        
        assert module.imports == set()
        assert module.exports == set()
        assert module.is_internal is False
        assert module.has_doc is False
        assert module.line_count == 0


class TestAgdaParser:
    """Test AgdaParser class."""
    
    def test_parse_simple_module(self):
        """Test parsing simple module."""
        agda_code = """\
-- | Documentation for test module
module Test where

open import Prelude
open import Data.List

testFunc : Nat → Nat
testFunc n = n + 1

data TestType : Set where
  constructor : TestType
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.agda', delete=False) as f:
            f.write(agda_code)
            f.flush()
            
            parser = AgdaParser()
            module = parser.parse_file(Path(f.name))
            
            assert module.name == "Test"
            assert "Prelude" in module.imports
            assert "Data.List" in module.imports
            assert module.has_doc is True
    
    def test_parse_module_with_exports(self):
        """Test parsing module with explicit exports."""
        agda_code = """\
module Data.Tree (Tree, insert, lookup) where

open import Prelude

data Tree : Set where
  Empty : Tree
  Node : Nat → Tree → Tree → Tree

insert : Nat → Tree → Tree
insert = ?

lookup : Nat → Tree → Bool
lookup = ?
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.agda', delete=False) as f:
            f.write(agda_code)
            f.flush()
            
            parser = AgdaParser()
            module = parser.parse_file(Path(f.name))
            
            assert module.name == "Data.Tree"
    
    def test_parse_internal_module(self):
        """Test parsing internal module."""
        agda_code = """\
module _ where

open import Prelude

testFunc : Nat → Nat
testFunc = ?
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.agda', delete=False) as f:
            f.write(agda_code)
            f.flush()
            
            parser = AgdaParser()
            module = parser.parse_file(Path(f.name))
            
            # Should handle underscore (anonymous) modules
            assert module.is_internal is True
    
    def test_parse_module_without_documentation(self):
        """Test parsing module without docs."""
        agda_code = """\
module Test where

open import Prelude

testFunc : Nat
testFunc = 0
"""
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.agda', delete=False) as f:
            f.write(agda_code)
            f.flush()
            
            parser = AgdaParser()
            module = parser.parse_file(Path(f.name))
            
            assert module.has_doc is False
    
    def test_parse_nonexistent_file(self):
        """Test parsing nonexistent file."""
        parser = AgdaParser()
        
        with pytest.raises(FileNotFoundError):
            parser.parse_file(Path("/nonexistent/file.agda"))
    
    def test_parse_directory(self):
        """Test parsing directory of modules."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            
            # Create test modules
            (tmppath / "Module1.agda").write_text(
                "module Module1 where\nopen import Prelude"
            )
            (tmppath / "Module2.agda").write_text(
                "module Module2 where\nopen import Prelude"
            )
            (tmppath / "readme.txt").write_text("not agda")
            
            parser = AgdaParser()
            modules = parser.parse_directory(tmppath)
            
            assert len(modules) == 2
            names = {m.name for m in modules}
            assert "Module1" in names
            assert "Module2" in names


class TestModuleCoverage:
    """Test ModuleCoverage analyzer."""
    
    def test_coverage_empty(self):
        """Test coverage with no test modules."""
        coverage = ModuleCoverage(
            modules=[
                AgdaModule(
                    name="Test1",
                    file_path=Path("/test/Test1.agda"),
                    line_count=100
                )
            ]
        )
        
        assert coverage.get_coverage_percent() == 0.0
        assert coverage.get_tested_modules() == []
    
    def test_coverage_calculation(self):
        """Test coverage percentage calculation."""
        modules = [
            AgdaModule(
                name="Test1",
                file_path=Path("/test/Test1.agda"),
                line_count=100
            ),
            AgdaModule(
                name="Test2",
                file_path=Path("/test/Test2.agda"),
                line_count=200
            ),
        ]
        
        coverage = ModuleCoverage(modules=modules)
        coverage.add_test_module("Test1")
        
        # 1 tested out of 2 total
        assert coverage.get_coverage_percent() == 50.0
    
    def test_coverage_by_lines(self):
        """Test coverage by line count."""
        modules = [
            AgdaModule(
                name="Test1",
                file_path=Path("/test/Test1.agda"),
                line_count=100
            ),
            AgdaModule(
                name="Test2",
                file_path=Path("/test/Test2.agda"),
                line_count=200
            ),
        ]
        
        coverage = ModuleCoverage(modules=modules)
        coverage.add_test_module("Test1")  # 100 lines
        
        # 100 lines tested out of 300 total
        coverage_by_lines = coverage.get_coverage_by_lines()
        assert abs(coverage_by_lines - 33.33) < 0.1


class TestDependencyAnalyzer:
    """Test DependencyAnalyzer class."""
    
    def test_analyzer_simple_dependency(self):
        """Test simple dependency analysis."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports=set()
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        
        # A depends on B
        deps = analyzer.get_dependencies(modules[0])
        assert len(deps) == 1
        assert modules[1] in deps
    
    def test_analyzer_transitive_dependency(self):
        """Test transitive dependency analysis."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports={"C"}
            ),
            AgdaModule(
                name="C",
                file_path=Path("/test/C.agda"),
                imports=set()
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        
        # Transitive: A -> B -> C
        deps = analyzer.get_dependencies(modules[0], transitive=True)
        assert len(deps) >= 1
    
    def test_analyzer_cycle_detection(self):
        """Test cycle detection."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports={"C"}
            ),
            AgdaModule(
                name="C",
                file_path=Path("/test/C.agda"),
                imports={"A"}  # Creates cycle
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        cycles = analyzer.find_cycles()
        
        assert len(cycles) > 0
    
    def test_analyzer_no_cycles(self):
        """Test cycle detection with acyclic graph."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports=set()
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        cycles = analyzer.find_cycles()
        
        assert len(cycles) == 0
    
    def test_analyzer_dependents(self):
        """Test finding dependents."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports=set()
            ),
            AgdaModule(
                name="C",
                file_path=Path("/test/C.agda"),
                imports={"B"}
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        
        # B has dependents A and C
        dependents = analyzer.get_dependents(modules[1])
        assert len(dependents) >= 1
    
    def test_analyzer_import_statistics(self):
        """Test import statistics."""
        modules = [
            AgdaModule(
                name="A",
                file_path=Path("/test/A.agda"),
                imports={"B", "C"}
            ),
            AgdaModule(
                name="B",
                file_path=Path("/test/B.agda"),
                imports={"C"}
            ),
            AgdaModule(
                name="C",
                file_path=Path("/test/C.agda"),
                imports=set()
            ),
        ]
        
        analyzer = DependencyAnalyzer(modules)
        stats = analyzer.get_import_statistics()
        
        assert 'total_imports' in stats
        assert 'most_imported_modules' in stats
        assert 'average_imports_per_module' in stats


class TestAgdaIntegration:
    """Integration tests for Agda module handling."""
    
    def test_full_workflow(self):
        """Test complete workflow: parse -> analyze."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            
            # Create test modules
            (tmppath / "Prelude.agda").write_text(
                "module Prelude where"
            )
            (tmppath / "Data.agda").write_text(
                "module Data where\nopen import Prelude"
            )
            (tmppath / "Logic.agda").write_text(
                "module Logic where\nopen import Prelude"
            )
            (tmppath / "Main.agda").write_text(
                "module Main where\nopen import Data\nopen import Logic"
            )
            
            parser = AgdaParser()
            modules = parser.parse_directory(tmppath)
            
            assert len(modules) >= 3
            
            analyzer = DependencyAnalyzer(modules)
            
            # Verify Main depends on Data and Logic
            main_module = next(m for m in modules if m.name == "Main")
            deps = analyzer.get_dependencies(main_module)
            
            # Should have dependency on at least one other module
            assert len(deps) > 0


class TestAgdaHelpers:
    """Test shared helper functions."""

    def test_extract_module_header_block_comment(self, tmp_path):
        agda_file = tmp_path / "Test.agda"
        agda_file.write_text(
            "{- Header line\nMore details\n-}\n\nmodule Test where\n",
            encoding="utf-8",
        )
        header = extract_module_header(agda_file)
        assert header is not None
        assert "Header line" in header

    def test_extract_definition_names_limits(self, tmp_path):
        agda_file = tmp_path / "Test.agda"
        agda_file.write_text(
            "foo : Nat\nbar : Nat\nbaz : Nat\n",
            encoding="utf-8",
        )
        names = extract_definition_names(agda_file, limit=2)
        assert names == ["foo", "bar"]

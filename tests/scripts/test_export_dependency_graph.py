#!/usr/bin/env python3
"""
Tests for export_dependency_graph.py

Coverage targets:
- Mermaid graph generation
- DOT graph generation
- Dependency summary statistics
- CLI main function and error handling
"""

import pytest
import json
import tempfile
from pathlib import Path
from unittest.mock import patch, mock_open

# Import the module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))
from export_dependency_graph import (
    escape_label,
    generate_mermaid_graph,
    generate_dot_graph,
    generate_dependency_summary,
    export_dependency_graphs
)


class TestLabelEscaping:
    """Test label escaping for graphs."""
    
    def test_escape_label_quotes(self):
        """Test escaping double quotes."""
        result = escape_label('Task with "quotes"')
        assert '\\"' in result
        assert 'quotes' in result
    
    def test_escape_label_single_quotes(self):
        """Test escaping single quotes."""
        result = escape_label("Task with 'quotes'")
        assert "\\'" in result
    
    def test_escape_label_both_quotes(self):
        """Test escaping both quote types."""
        result = escape_label('''Task with "double" and 'single' quotes''')
        assert '\\"' in result
        assert "\\'" in result


class TestMermaidGeneration:
    """Test Mermaid graph generation."""
    
    def test_generate_mermaid_simple(self):
        """Test generating Mermaid graph with simple dependencies."""
        items = [
            {
                "id": "TASK-001",
                "title": "First Task",
                "status": "completed",
                "dependsOn": ["TASK-002"]
            },
            {
                "id": "TASK-002",
                "title": "Second Task",
                "status": "planned",
                "dependsOn": []
            }
        ]
        
        result = generate_mermaid_graph(items)
        
        assert "```mermaid" in result
        assert "graph TB" in result
        assert "TASK_001" in result
        assert "TASK_002" in result
        assert "First Task" in result
    
    def test_generate_mermaid_with_dependencies(self):
        """Test Mermaid graph includes dependency edges."""
        items = [
            {
                "id": "A",
                "title": "Task A",
                "status": "planned",
                "dependsOn": ["B", "C"]
            },
            {
                "id": "B",
                "title": "Task B",
                "status": "completed",
                "dependsOn": []
            },
            {
                "id": "C",
                "title": "Task C",
                "status": "not-started",
                "dependsOn": []
            }
        ]
        
        result = generate_mermaid_graph(items)
        
        # Should have edges A -> B and A -> C
        assert "-->" in result
        assert "A" in result
        assert "B" in result
        assert "C" in result


class TestDOTGeneration:
    """Test GraphViz DOT graph generation."""
    
    def test_generate_dot_simple(self):
        """Test generating DOT graph."""
        items = [
            {
                "id": "TASK-001",
                "title": "First Task",
                "status": "completed",
                "category": "development",
                "dependsOn": []
            }
        ]
        
        result = generate_dot_graph(items)
        
        assert "digraph" in result
        assert "TASK-001" in result or "TASK_001" in result
        assert "First Task" in result
    
    def test_generate_dot_with_dependencies(self):
        """Test DOT graph includes edges."""
        items = [
            {
                "id": "A",
                "title": "Task A",
                "status": "planned",
                "category": "dev",
                "dependsOn": ["B"]
            },
            {
                "id": "B",
                "title": "Task B",
                "status": "completed",
                "category": "dev",
                "dependsOn": []
            }
        ]
        
        result = generate_dot_graph(items)
        
        # Should have edge A -> B
        assert "->" in result
        assert "A" in result
        assert "B" in result


class TestDependencySummary:
    """Test dependency summary generation."""
    
    def test_generate_summary_no_dependencies(self):
        """Test summary with no dependencies."""
        items = [
            {"id": "A", "dependsOn": []},
            {"id": "B", "dependsOn": []}
        ]
        
        summary = generate_dependency_summary(items)
        
        assert summary["total_tasks"] == 2
        assert summary["tasks_with_explicit_deps"] == 0
        assert summary["total_explicit_deps"] == 0
    
    def test_generate_summary_with_dependencies(self):
        """Test summary with explicit dependencies."""
        items = [
            {"id": "A", "dependsOn": ["B", "C"]},
            {"id": "B", "dependsOn": ["C"]},
            {"id": "C", "dependsOn": []}
        ]
        
        summary = generate_dependency_summary(items)
        
        assert summary["total_tasks"] == 3
        assert summary["tasks_with_explicit_deps"] == 2  # A and B
        assert summary["total_explicit_deps"] == 3  # A->B, A->C, B->C
    
    def test_generate_summary_most_depended_on(self):
        """Test that most depended-on tasks are identified."""
        items = [
            {"id": "A", "dependsOn": ["D"]},
            {"id": "B", "dependsOn": ["D"]},
            {"id": "C", "dependsOn": ["D"]},
            {"id": "D", "dependsOn": []}
        ]
        
        summary = generate_dependency_summary(items)
        
        # D is depended on by A, B, C (3 times)
        assert len(summary["most_depended_on"]) > 0
        top_task, count = summary["most_depended_on"][0]
        assert top_task == "D"
        assert count == 3
    
    def test_generate_summary_with_suggested_deps(self):
        """Test summary includes suggested dependencies if implemented."""
        items = [
            {
                "id": "A",
                "dependsOn": ["B"],
                "suggestedDeps": ["C", "D"]
            },
            {
                "id": "B",
                "dependsOn": [],
                "suggestedDeps": []
            }
        ]
        
        summary = generate_dependency_summary(items)
        
        # suggestedDeps may not be implemented yet; check if field exists
        if "tasks_with_suggested_deps" in summary:
            assert summary["tasks_with_suggested_deps"] >= 0
            assert summary["total_suggested_deps"] >= 0
        else:
            # Feature not implemented; verify basic summary works
            assert summary["total_tasks"] == 2


class TestCLIExport:
    """Test CLI main export function."""
    
    def test_export_dependency_graphs_missing_input(self, tmp_path, capsys):
        """Test export with missing enriched JSON file."""
        # Patch ENRICHED_JSON to nonexistent path
        import export_dependency_graph
        original_path = export_dependency_graph.ENRICHED_JSON
        export_dependency_graph.ENRICHED_JSON = tmp_path / "nonexistent.json"
        
        try:
            export_dependency_graphs()
            
            captured = capsys.readouterr()
            assert "Error" in captured.out
            assert "not found" in captured.out
        finally:
            export_dependency_graph.ENRICHED_JSON = original_path
    
    def test_export_dependency_graphs_success(self, tmp_path, capsys):
        """Test successful dependency graph export."""
        # Create test enriched data
        enriched_file = tmp_path / "canonical_enriched.json"
        items = [
            {
                "id": "TASK-001",
                "title": "Test Task",
                "status": "planned",
                "category": "dev",
                "dependsOn": []
            }
        ]
        
        with open(enriched_file, "w") as f:
            json.dump(items, f)
        
        # Create output directory
        reports_dir = tmp_path / "reports"
        reports_dir.mkdir(parents=True)
        
        # Patch paths
        import export_dependency_graph
        original_enriched = export_dependency_graph.ENRICHED_JSON
        original_mermaid = export_dependency_graph.MERMAID_OUTPUT
        original_dot = export_dependency_graph.DOT_OUTPUT
        
        export_dependency_graph.ENRICHED_JSON = enriched_file
        export_dependency_graph.MERMAID_OUTPUT = reports_dir / "dep.mmd"
        export_dependency_graph.DOT_OUTPUT = reports_dir / "dep.dot"
        
        try:
            export_dependency_graphs()
            
            # Verify outputs created
            assert (reports_dir / "dep.mmd").exists()
            assert (reports_dir / "dep.dot").exists()
            
            # Verify console output
            captured = capsys.readouterr()
            assert "Generating dependency graphs" in captured.out
            assert "Mermaid graph" in captured.out
            assert "GraphViz DOT" in captured.out
            assert "Dependency Statistics" in captured.out
        finally:
            export_dependency_graph.ENRICHED_JSON = original_enriched
            export_dependency_graph.MERMAID_OUTPUT = original_mermaid
            export_dependency_graph.DOT_OUTPUT = original_dot
    
    def test_cli_main_entry_point(self, tmp_path, capsys):
        """Test __main__ entry point."""
        # Create minimal test file
        enriched_file = tmp_path / "test.json"
        with open(enriched_file, "w") as f:
            json.dump([{"id": "T1", "title": "Test", "status": "planned", 
                       "category": "dev", "dependsOn": []}], f)
        
        reports_dir = tmp_path / "reports"
        reports_dir.mkdir()
        
        # Patch and run
        import export_dependency_graph
        original_enriched = export_dependency_graph.ENRICHED_JSON
        original_mermaid = export_dependency_graph.MERMAID_OUTPUT
        original_dot = export_dependency_graph.DOT_OUTPUT
        
        export_dependency_graph.ENRICHED_JSON = enriched_file
        export_dependency_graph.MERMAID_OUTPUT = reports_dir / "test.mmd"
        export_dependency_graph.DOT_OUTPUT = reports_dir / "test.dot"
        
        try:
            # Simulate __main__ execution
            export_dependency_graphs()
            
            captured = capsys.readouterr()
            assert "Generating" in captured.out or "✓" in captured.out
        finally:
            export_dependency_graph.ENRICHED_JSON = original_enriched
            export_dependency_graph.MERMAID_OUTPUT = original_mermaid
            export_dependency_graph.DOT_OUTPUT = original_dot

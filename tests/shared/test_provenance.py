#!/usr/bin/env python3
"""Test suite for scripts.shared.provenance module."""

import pytest
import tempfile
from pathlib import Path
from datetime import datetime
from scripts.shared.provenance import (
    ProvenanceSource, ProvenanceRecord, ProvenanceTrail,
    ProvenanceTracker, ProvenanceAnalyzer,
)


class TestProvenanceSource:
    """Test ProvenanceSource enum."""
    
    def test_source_values(self):
        """Test source type values."""
        assert ProvenanceSource.SCRIPT.value == "script"
        assert ProvenanceSource.USER_INPUT.value == "user_input"
        assert ProvenanceSource.INGESTION.value == "ingestion"
        assert ProvenanceSource.SYNTHESIS.value == "synthesis"
        assert ProvenanceSource.TRANSFORMATION.value == "transformation"
        assert ProvenanceSource.MANUAL_ANNOTATION.value == "manual_annotation"
        assert ProvenanceSource.EXTERNAL_SOURCE.value == "external_source"


class TestProvenanceRecord:
    """Test ProvenanceRecord class."""
    
    def test_record_creation(self):
        """Test creating a record."""
        record = ProvenanceRecord(
            source_type=ProvenanceSource.SCRIPT,
            source_id="export_roadmap.py"
        )
        
        assert record.source_type == ProvenanceSource.SCRIPT
        assert record.source_id == "export_roadmap.py"
        assert record.timestamp is not None
    
    def test_record_with_metadata(self):
        """Test record with metadata."""
        record = ProvenanceRecord(
            source_type=ProvenanceSource.SCRIPT,
            source_id="process.py",
            source_location="line 42",
            metadata={"description": "Processed data", "version": "1.0"}
        )
        
        assert record.metadata["description"] == "Processed data"
        assert record.source_location == "line 42"
    
    def test_record_to_dict(self):
        """Test record serialization."""
        record = ProvenanceRecord(
            source_type=ProvenanceSource.USER_INPUT,
            source_id="user@example.com",
            metadata={"action": "created"}
        )
        
        d = record.to_dict()
        
        assert d['source_type'] == 'user_input'
        assert d['source_id'] == 'user@example.com'
        assert d['metadata']['action'] == 'created'
        assert d['timestamp'] is not None
    
    def test_record_to_string(self):
        """Test record string representation."""
        record = ProvenanceRecord(
            source_type=ProvenanceSource.TRANSFORMATION,
            source_id="normalize.py",
            source_location="line 10",
            metadata={"description": "Normalized data"}
        )
        
        s = record.to_string()
        
        assert "transformation:normalize.py" in s
        assert "line 10" in s


class TestProvenanceTrail:
    """Test ProvenanceTrail class."""
    
    def test_trail_creation(self):
        """Test creating a trail."""
        trail = ProvenanceTrail("artifact_001")
        
        assert trail.artifact_id == "artifact_001"
        assert len(trail.records) == 0
        assert len(trail.related_artifacts) == 0
    
    def test_trail_add_record(self):
        """Test adding records to trail."""
        trail = ProvenanceTrail("artifact_001")
        
        record1 = ProvenanceRecord(
            ProvenanceSource.INGESTION,
            "import.py"
        )
        record2 = ProvenanceRecord(
            ProvenanceSource.TRANSFORMATION,
            "process.py"
        )
        
        trail.add_record(record1)
        trail.add_record(record2)
        
        assert len(trail.records) == 2
    
    def test_trail_add_source_convenience(self):
        """Test adding source via convenience method."""
        trail = ProvenanceTrail("artifact_001")
        
        trail.add_source(
            ProvenanceSource.SCRIPT,
            "script.py",
            location="line 50",
            metadata={"version": "1.0"}
        )
        
        assert len(trail.records) == 1
        assert trail.records[0].source_id == "script.py"
    
    def test_trail_add_related(self):
        """Test adding related artifacts."""
        trail = ProvenanceTrail("artifact_001")
        
        trail.add_related("artifact_002")
        trail.add_related("artifact_003")
        
        assert "artifact_002" in trail.related_artifacts
        assert "artifact_003" in trail.related_artifacts
    
    def test_trail_get_primary_source(self):
        """Test getting primary source."""
        trail = ProvenanceTrail("artifact_001")
        
        record1 = ProvenanceRecord(ProvenanceSource.INGESTION, "source.json")
        record2 = ProvenanceRecord(ProvenanceSource.TRANSFORMATION, "process.py")
        
        trail.add_record(record1)
        trail.add_record(record2)
        
        primary = trail.get_primary_source()
        
        assert primary is record1
    
    def test_trail_get_transformation_chain(self):
        """Test getting transformation chain."""
        trail = ProvenanceTrail("artifact_001")
        
        trail.add_record(ProvenanceRecord(ProvenanceSource.INGESTION, "source"))
        trail.add_record(ProvenanceRecord(ProvenanceSource.TRANSFORMATION, "step1"))
        trail.add_record(ProvenanceRecord(ProvenanceSource.TRANSFORMATION, "step2"))
        trail.add_record(ProvenanceRecord(ProvenanceSource.SYNTHESIS, "synthesis"))
        
        chain = trail.get_transformation_chain()
        
        assert len(chain) == 2
    
    def test_trail_get_last_modified(self):
        """Test getting last modified time."""
        trail = ProvenanceTrail("artifact_001")
        
        trail.add_record(ProvenanceRecord(ProvenanceSource.INGESTION, "source"))
        trail.add_record(ProvenanceRecord(ProvenanceSource.TRANSFORMATION, "step"))
        
        last_mod = trail.get_last_modified()
        
        assert last_mod is not None
        assert isinstance(last_mod, datetime)
    
    def test_trail_to_dict(self):
        """Test trail serialization."""
        trail = ProvenanceTrail("artifact_001")
        trail.add_source(ProvenanceSource.SCRIPT, "script.py")
        trail.add_related("artifact_002")
        
        d = trail.to_dict()
        
        assert d['artifact_id'] == "artifact_001"
        assert len(d['records']) == 1
        assert 'artifact_002' in d['related_artifacts']
    
    def test_trail_to_markdown(self):
        """Test markdown representation."""
        trail = ProvenanceTrail("artifact_001")
        trail.add_source(
            ProvenanceSource.TRANSFORMATION,
            "process.py",
            location="line 10"
        )
        trail.add_related("artifact_002")
        
        md = trail.to_markdown()
        
        assert "artifact_001" in md
        assert "process.py" in md
        assert "artifact_002" in md


class TestProvenanceTracker:
    """Test ProvenanceTracker class."""
    
    def test_tracker_creation(self):
        """Test creating tracker."""
        tracker = ProvenanceTracker("system_v1")
        
        assert tracker.system_id == "system_v1"
        assert len(tracker.trails) == 0
    
    def test_tracker_create_trail(self):
        """Test creating trails."""
        tracker = ProvenanceTracker("system_v1")
        
        trail1 = tracker.create_trail("artifact_001")
        trail2 = tracker.create_trail("artifact_002")
        
        assert len(tracker.trails) == 2
        assert tracker.get_trail("artifact_001") is trail1
    
    def test_tracker_get_trail(self):
        """Test getting existing trail."""
        tracker = ProvenanceTracker("system_v1")
        tracker.create_trail("artifact_001")
        
        trail = tracker.get_trail("artifact_001")
        
        assert trail is not None
        assert trail.artifact_id == "artifact_001"
    
    def test_tracker_get_nonexistent_trail(self):
        """Test getting nonexistent trail."""
        tracker = ProvenanceTracker("system_v1")
        
        trail = tracker.get_trail("nonexistent")
        
        assert trail is None
    
    def test_tracker_add_provenance(self):
        """Test adding provenance."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.add_provenance(
            "artifact_001",
            ProvenanceSource.INGESTION,
            "data.json"
        )
        
        trail = tracker.get_trail("artifact_001")
        
        assert trail is not None
        assert len(trail.records) == 1
    
    def test_tracker_link_artifacts(self):
        """Test linking artifacts."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.link_artifacts("artifact_001", "artifact_002")
        
        trail = tracker.get_trail("artifact_002")
        
        assert "artifact_001" in trail.related_artifacts
    
    def test_tracker_get_lineage(self):
        """Test getting lineage."""
        tracker = ProvenanceTracker("system_v1")
        
        # Chain: A <- B <- C
        tracker.link_artifacts("artifact_a", "artifact_b")
        tracker.link_artifacts("artifact_b", "artifact_c")
        
        lineage = tracker.get_lineage("artifact_c")
        
        assert len(lineage) >= 1
    
    def test_tracker_get_dependents(self):
        """Test getting dependents."""
        tracker = ProvenanceTracker("system_v1")
        
        # A is source for B and C
        tracker.link_artifacts("artifact_a", "artifact_b")
        tracker.link_artifacts("artifact_a", "artifact_c")
        
        dependents = tracker.get_dependents("artifact_a")
        
        assert "artifact_b" in dependents
        assert "artifact_c" in dependents
    
    def test_tracker_get_transformation_path(self):
        """Test getting transformation path."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.add_provenance("artifact_001", ProvenanceSource.INGESTION, "source")
        tracker.add_provenance("artifact_001", ProvenanceSource.TRANSFORMATION, "step1")
        tracker.add_provenance("artifact_001", ProvenanceSource.TRANSFORMATION, "step2")
        
        path = tracker.get_transformation_path("artifact_001")
        
        assert len(path) == 2
    
    def test_tracker_generate_report(self):
        """Test report generation."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.add_provenance("artifact_001", ProvenanceSource.SCRIPT, "script.py")
        tracker.add_provenance("artifact_002", ProvenanceSource.USER_INPUT, "user@example.com")
        
        report = tracker.generate_report()
        
        assert report['system_id'] == "system_v1"
        assert report['total_artifacts'] == 2
        assert 'artifacts' in report
    
    def test_tracker_generate_markdown_report(self):
        """Test markdown report generation."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.add_provenance("artifact_001", ProvenanceSource.SCRIPT, "script.py")
        
        report = tracker.generate_markdown_report()
        
        assert "system_v1" in report
        assert "artifact_001" in report


class TestProvenanceAnalyzer:
    """Test ProvenanceAnalyzer class."""
    
    def test_analyzer_creation(self):
        """Test creating analyzer."""
        tracker = ProvenanceTracker("system_v1")
        analyzer = ProvenanceAnalyzer(tracker)
        
        assert analyzer.tracker is tracker
    
    def test_analyzer_source_statistics(self):
        """Test source statistics."""
        tracker = ProvenanceTracker("system_v1")
        tracker.add_provenance("artifact_001", ProvenanceSource.SCRIPT, "script1.py")
        tracker.add_provenance("artifact_002", ProvenanceSource.SCRIPT, "script2.py")
        tracker.add_provenance("artifact_003", ProvenanceSource.USER_INPUT, "user")
        
        analyzer = ProvenanceAnalyzer(tracker)
        stats = analyzer.get_source_statistics()
        
        assert stats.get('script', 0) == 2
        assert stats.get('user_input', 0) == 1
    
    def test_analyzer_top_sources(self):
        """Test getting top sources."""
        tracker = ProvenanceTracker("system_v1")
        
        for i in range(5):
            tracker.add_provenance(f"artifact_{i}", ProvenanceSource.SCRIPT, "common_script.py")
        
        tracker.add_provenance("artifact_x", ProvenanceSource.USER_INPUT, "unique_user")
        
        analyzer = ProvenanceAnalyzer(tracker)
        top = analyzer.get_top_sources(limit=3)
        
        assert len(top) <= 3
        assert top[0][0] == "common_script.py"  # Should be most frequent
    
    def test_analyzer_longest_lineage(self):
        """Test finding longest lineage."""
        tracker = ProvenanceTracker("system_v1")
        
        # Create chain A -> B -> C -> D
        tracker.link_artifacts("a", "b")
        tracker.link_artifacts("b", "c")
        tracker.link_artifacts("c", "d")
        
        analyzer = ProvenanceAnalyzer(tracker)
        artifact_id, depth = analyzer.get_longest_lineage()
        
        assert depth >= 1
    
    def test_analyzer_find_orphaned(self):
        """Test finding orphaned artifacts."""
        tracker = ProvenanceTracker("system_v1")
        
        # Create trail with no records
        tracker.create_trail("orphaned_001")
        
        # Create trail with records
        tracker.add_provenance("documented_001", ProvenanceSource.SCRIPT, "script.py")
        
        analyzer = ProvenanceAnalyzer(tracker)
        orphaned = analyzer.find_orphaned_artifacts()
        
        assert "orphaned_001" in orphaned
    
    def test_analyzer_analysis_report(self):
        """Test generating analysis report."""
        tracker = ProvenanceTracker("system_v1")
        
        tracker.add_provenance("artifact_001", ProvenanceSource.INGESTION, "data.json")
        tracker.add_provenance("artifact_002", ProvenanceSource.TRANSFORMATION, "process.py")
        tracker.link_artifacts("artifact_001", "artifact_002")
        
        analyzer = ProvenanceAnalyzer(tracker)
        report = analyzer.get_analysis_report()
        
        assert 'total_artifacts' in report
        assert 'total_records' in report
        assert 'source_statistics' in report
        assert 'top_sources' in report
        assert 'longest_lineage' in report


class TestProvenanceIntegration:
    """Integration tests for provenance system."""
    
    def test_complete_workflow(self):
        """Test complete provenance tracking workflow."""
        # Create tracker
        tracker = ProvenanceTracker("data_pipeline_v1")
        
        # Track data ingestion
        tracker.add_provenance(
            "raw_data",
            ProvenanceSource.EXTERNAL_SOURCE,
            "api.example.com",
            metadata={"endpoint": "/data"}
        )
        
        # Track transformation steps
        tracker.add_provenance(
            "cleaned_data",
            ProvenanceSource.TRANSFORMATION,
            "clean.py",
            metadata={"step": "1"}
        )
        tracker.link_artifacts("raw_data", "cleaned_data")
        
        tracker.add_provenance(
            "normalized_data",
            ProvenanceSource.TRANSFORMATION,
            "normalize.py",
            metadata={"step": "2"}
        )
        tracker.link_artifacts("cleaned_data", "normalized_data")
        
        # Analyze
        analyzer = ProvenanceAnalyzer(tracker)
        report = analyzer.get_analysis_report()
        
        assert report['total_artifacts'] == 3
        assert len(report['source_statistics']) > 0
    
    def test_report_generation_to_file(self):
        """Test generating reports to files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            
            tracker = ProvenanceTracker("test_system")
            tracker.add_provenance("artifact_001", ProvenanceSource.SCRIPT, "script.py")
            
            # Generate reports
            json_report = tracker.generate_report(tmppath / "report.json")
            md_report = tracker.generate_markdown_report(tmppath / "report.md")
            
            # Verify files exist and contain content
            assert (tmppath / "report.json").exists()
            assert (tmppath / "report.md").exists()
            
            json_content = (tmppath / "report.json").read_text()
            md_content = (tmppath / "report.md").read_text()
            
            assert "artifact_001" in json_content
            assert "artifact_001" in md_content

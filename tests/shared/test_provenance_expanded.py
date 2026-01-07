#!/usr/bin/env python3
"""Extended tests for provenance.py CHIP-N+1 enhancements."""

import pytest
import tempfile
from pathlib import Path
from datetime import datetime
from scripts.shared.provenance import (
    ProvenanceSource, ProvenanceRecord, ProvenanceTrail,
    ProvenanceTracker, ProvenanceAnalyzer
)


class TestProvenanceRecordSerialization:
    """Test ProvenanceRecord serialization symmetry (CHIP-N+1)."""
    
    def test_record_from_dict_basic(self):
        """Test ProvenanceRecord.from_dict() basic reconstruction."""
        data = {
            'source_type': 'script',
            'source_id': 'test.py',
            'source_location': '42',
            'timestamp': '2026-01-06T12:00:00',
            'metadata': {'author': 'test'}
        }
        
        record = ProvenanceRecord.from_dict(data)
        
        assert record.source_type == ProvenanceSource.SCRIPT
        assert record.source_id == 'test.py'
        assert record.source_location == '42'
        assert record.metadata == {'author': 'test'}
    
    def test_record_from_dict_missing_required(self):
        """Test ProvenanceRecord.from_dict() with missing fields."""
        data = {'source_id': 'test.py'}  # Missing source_type
        
        with pytest.raises(ValueError, match="Missing required field: source_type"):
            ProvenanceRecord.from_dict(data)
    
    def test_record_from_dict_invalid_source_type(self):
        """Test ProvenanceRecord.from_dict() with invalid source type."""
        data = {
            'source_type': 'invalid_type',
            'source_id': 'test.py'
        }
        
        with pytest.raises(ValueError, match="Invalid source_type"):
            ProvenanceRecord.from_dict(data)
    
    def test_record_roundtrip(self):
        """Test ProvenanceRecord serialization round-trip."""
        original = ProvenanceRecord(
            source_type=ProvenanceSource.TRANSFORMATION,
            source_id='transform.py',
            source_location='100',
            metadata={'step': 'parse'}
        )
        
        # Round-trip: to_dict -> from_dict
        data = original.to_dict()
        reconstructed = ProvenanceRecord.from_dict(data)
        
        assert reconstructed.source_type == original.source_type
        assert reconstructed.source_id == original.source_id
        assert reconstructed.source_location == original.source_location
        assert reconstructed.metadata == original.metadata
    
    def test_record_from_dict_optional_fields(self):
        """Test ProvenanceRecord.from_dict() with optional fields."""
        data = {
            'source_type': 'user_input',
            'source_id': 'user123'
            # No source_location, timestamp, or metadata
        }
        
        record = ProvenanceRecord.from_dict(data)
        
        assert record.source_type == ProvenanceSource.USER_INPUT
        assert record.source_id == 'user123'
        assert record.source_location is None
        assert record.metadata == {}


class TestProvenanceTrailSerialization:
    """Test ProvenanceTrail serialization symmetry (CHIP-N+1)."""
    
    def test_trail_from_dict_basic(self):
        """Test ProvenanceTrail.from_dict() basic reconstruction."""
        data = {
            'artifact_id': 'artifact1',
            'records': [
                {
                    'source_type': 'script',
                    'source_id': 'test.py',
                    'source_location': '10',
                    'timestamp': '2026-01-06T12:00:00',
                    'metadata': {}
                }
            ],
            'related_artifacts': ['artifact2', 'artifact3']
        }
        
        trail = ProvenanceTrail.from_dict(data)
        
        assert trail.artifact_id == 'artifact1'
        assert len(trail.records) == 1
        assert trail.records[0].source_id == 'test.py'
        assert len(trail.related_artifacts) == 2
        assert 'artifact2' in trail.related_artifacts
    
    def test_trail_from_dict_missing_artifact_id(self):
        """Test ProvenanceTrail.from_dict() with missing artifact_id."""
        data = {'records': []}
        
        with pytest.raises(ValueError, match="Missing required field: artifact_id"):
            ProvenanceTrail.from_dict(data)
    
    def test_trail_roundtrip(self):
        """Test ProvenanceTrail serialization round-trip."""
        original = ProvenanceTrail(artifact_id='test_artifact')
        original.add_source(
            ProvenanceSource.INGESTION,
            'data.json',
            location='field1',
            metadata={'validated': True}
        )
        original.add_related('related_artifact')
        
        # Round-trip: to_dict -> from_dict
        data = original.to_dict()
        reconstructed = ProvenanceTrail.from_dict(data)
        
        assert reconstructed.artifact_id == original.artifact_id
        assert len(reconstructed.records) == len(original.records)
        assert reconstructed.records[0].source_id == original.records[0].source_id
        assert reconstructed.related_artifacts == original.related_artifacts
    
    def test_trail_from_dict_empty(self):
        """Test ProvenanceTrail.from_dict() with minimal data."""
        data = {'artifact_id': 'empty_artifact'}
        
        trail = ProvenanceTrail.from_dict(data)
        
        assert trail.artifact_id == 'empty_artifact'
        assert len(trail.records) == 0
        assert len(trail.related_artifacts) == 0


class TestProvenanceTrackerSerialization:
    """Test ProvenanceTracker serialization symmetry (CHIP-N+1)."""
    
    def test_tracker_from_dict_basic(self):
        """Test ProvenanceTracker.from_dict() basic reconstruction."""
        data = {
            'system_id': 'test_system',
            'total_artifacts': 2,
            'artifacts': {
                'artifact1': {
                    'artifact_id': 'artifact1',
                    'records': [
                        {
                            'source_type': 'script',
                            'source_id': 'script.py',
                            'source_location': None,
                            'timestamp': '2026-01-06T12:00:00',
                            'metadata': {}
                        }
                    ],
                    'related_artifacts': []
                },
                'artifact2': {
                    'artifact_id': 'artifact2',
                    'records': [],
                    'related_artifacts': ['artifact1']
                }
            }
        }
        
        tracker = ProvenanceTracker.from_dict(data)
        
        assert tracker.system_id == 'test_system'
        assert len(tracker.trails) == 2
        assert 'artifact1' in tracker.trails
        assert 'artifact2' in tracker.trails
        assert len(tracker.trails['artifact1'].records) == 1
    
    def test_tracker_from_dict_missing_system_id(self):
        """Test ProvenanceTracker.from_dict() with missing system_id."""
        data = {'artifacts': {}}
        
        with pytest.raises(ValueError, match="Missing required field: system_id"):
            ProvenanceTracker.from_dict(data)
    
    def test_tracker_roundtrip(self):
        """Test ProvenanceTracker serialization round-trip."""
        original = ProvenanceTracker('test_system')
        original.add_provenance(
            'artifact1',
            ProvenanceSource.SYNTHESIS,
            'synthesizer.py',
            location='line 50',
            metadata={'version': '1.0'}
        )
        original.link_artifacts('artifact1', 'artifact2')
        
        # Round-trip: generate_report -> from_dict
        report = original.generate_report()
        reconstructed = ProvenanceTracker.from_dict(report)
        
        assert reconstructed.system_id == original.system_id
        assert len(reconstructed.trails) == len(original.trails)
        assert 'artifact1' in reconstructed.trails
        assert reconstructed.trails['artifact1'].records[0].source_id == 'synthesizer.py'
    
    def test_tracker_from_dict_empty(self):
        """Test ProvenanceTracker.from_dict() with no artifacts."""
        data = {
            'system_id': 'empty_system',
            'total_artifacts': 0,
            'artifacts': {}
        }
        
        tracker = ProvenanceTracker.from_dict(data)
        
        assert tracker.system_id == 'empty_system'
        assert len(tracker.trails) == 0


class TestProvenanceErrorHandling:
    """Test provenance error handling with CHIP-N+1 patterns."""
    
    def test_tracker_generate_report_file_error(self):
        """Test tracker handles file write errors gracefully."""
        tracker = ProvenanceTracker('test_system')
        tracker.add_provenance('artifact1', ProvenanceSource.SCRIPT, 'test.py')
        
        # Try to write to an invalid path
        invalid_path = Path('/nonexistent/directory/report.json')
        
        # Should raise FileOperationError if available, otherwise OSError
        with pytest.raises((OSError, Exception)):
            tracker.generate_report(output_path=invalid_path)
    
    def test_tracker_report_without_file(self):
        """Test tracker generates report without writing to file."""
        tracker = ProvenanceTracker('test_system')
        tracker.add_provenance('artifact1', ProvenanceSource.SCRIPT, 'test.py')
        
        # Should succeed without file path
        report = tracker.generate_report()
        
        assert 'system_id' in report
        assert report['system_id'] == 'test_system'
        assert 'artifacts' in report


class TestProvenanceIntegrationExpanded:
    """Extended integration tests for provenance tracking."""
    
    def test_full_serialization_workflow(self):
        """Test complete workflow: create -> report -> reconstruct -> verify."""
        # Create original tracker
        original = ProvenanceTracker('workflow_test')
        
        # Add complex provenance data
        original.add_provenance(
            'source_data',
            ProvenanceSource.INGESTION,
            'data.csv',
            metadata={'rows': 1000}
        )
        
        original.add_provenance(
            'cleaned_data',
            ProvenanceSource.TRANSFORMATION,
            'clean.py',
            location='line 42',
            metadata={'removed_nulls': 50}
        )
        original.link_artifacts('source_data', 'cleaned_data')
        
        original.add_provenance(
            'model',
            ProvenanceSource.SYNTHESIS,
            'train.py',
            metadata={'accuracy': 0.95}
        )
        original.link_artifacts('cleaned_data', 'model')
        
        # Generate report
        report = original.generate_report()
        
        # Reconstruct from report
        reconstructed = ProvenanceTracker.from_dict(report)
        
        # Verify reconstruction
        assert reconstructed.system_id == original.system_id
        assert len(reconstructed.trails) == 3
        
        # Verify lineage
        model_lineage = reconstructed.get_lineage('model')
        assert 'cleaned_data' in model_lineage
        assert 'source_data' in model_lineage or 'cleaned_data' in reconstructed.get_lineage('cleaned_data')
        
        # Verify dependents
        source_dependents = reconstructed.get_dependents('source_data')
        assert 'cleaned_data' in source_dependents
    
    def test_roundtrip_preserves_timestamps(self):
        """Test that timestamp information is preserved through serialization."""
        tracker = ProvenanceTracker('timestamp_test')
        
        # Add record with explicit timestamp
        fixed_time = datetime(2026, 1, 6, 12, 0, 0)
        record = ProvenanceRecord(
            source_type=ProvenanceSource.MANUAL_ANNOTATION,
            source_id='user',
            timestamp=fixed_time
        )
        
        trail = tracker.create_trail('artifact')
        trail.add_record(record)
        
        # Round-trip
        report = tracker.generate_report()
        reconstructed = ProvenanceTracker.from_dict(report)
        
        # Verify timestamp preserved
        reconstructed_trail = reconstructed.get_trail('artifact')
        assert reconstructed_trail is not None
        assert len(reconstructed_trail.records) == 1
        
        # Timestamps are serialized as ISO strings, so we compare them
        original_iso = fixed_time.isoformat()
        reconstructed_iso = reconstructed_trail.records[0].timestamp.isoformat()
        assert reconstructed_iso == original_iso

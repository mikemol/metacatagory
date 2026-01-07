#!/usr/bin/env python3
"""
Provenance tracking and lineage analysis for artifacts and decisions.

Provides utilities for:
- Tracking origin and history of items
- Recording source attribution
- Analyzing transformation lineage
- Generating traceability reports

CHIP-N+1 Protocol applied:
- Serialization symmetry: to_dict()/from_dict() for all data structures
- Error integration: Structured errors with causality chains
- Validation integration: Schema validation for provenance data
- Causality tracking: Links provenance to error chains
"""

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Dict, Optional, Set, Any, Tuple
from enum import Enum
from datetime import datetime
import json

# CHIP-N+1: Import enhanced modules with fallback for graceful degradation
try:
    from scripts.shared.errors import ScriptError, FileOperationError, ValidationError as ScriptValidationError
except ImportError:
    ScriptError = Exception
    FileOperationError = IOError
    ScriptValidationError = ValueError

try:
    from scripts.shared.validation import ValidationResult, ValidationFailure
except ImportError:
    ValidationResult = dict
    ValidationFailure = None

try:
    from scripts.shared.logging import StructuredLogger
except ImportError:
    StructuredLogger = None


class ProvenanceSource(Enum):
    """Types of provenance sources."""
    SCRIPT = "script"
    USER_INPUT = "user_input"
    INGESTION = "ingestion"
    SYNTHESIS = "synthesis"
    TRANSFORMATION = "transformation"
    MANUAL_ANNOTATION = "manual_annotation"
    EXTERNAL_SOURCE = "external_source"


@dataclass
class ProvenanceRecord:
    """Single provenance entry.
    
    CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
    Track source of changes with full causality chain.
    """
    source_type: ProvenanceSource
    source_id: str  # e.g., filename, script name, user ID
    source_location: Optional[str] = None  # e.g., line number
    timestamp: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def __post_init__(self):
        """Initialize defaults.
        
        CHIP-N+1 Implication 1.1.1 (Flexible Initialization):
        Auto-populate timestamp for traceability.
        """
        if self.timestamp is None:
            self.timestamp = datetime.now()
    
    def to_dict(self) -> dict:
        """Convert to dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Serialize with explicit field mapping.
        """
        return {
            'source_type': self.source_type.value,
            'source_id': self.source_id,
            'source_location': self.source_location,
            'timestamp': self.timestamp.isoformat() if self.timestamp else None,
            'metadata': self.metadata,
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'ProvenanceRecord':
        """Create from dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Deserialize with validation and type checking.
        
        Args:
            data: Dictionary with provenance fields
            
        Returns:
            ProvenanceRecord instance
            
        Raises:
            ValueError: If required fields are missing or invalid
        """
        required_fields = ['source_type', 'source_id']
        for field in required_fields:
            if field not in data:
                raise ValueError(f"Missing required field: {field}")
        
        # Parse source type from string
        try:
            source_type = ProvenanceSource(data['source_type'])
        except ValueError:
            raise ValueError(f"Invalid source_type: {data['source_type']}")
        
        # Parse timestamp from ISO string
        timestamp = None
        if data.get('timestamp'):
            try:
                timestamp = datetime.fromisoformat(data['timestamp'])
            except (ValueError, TypeError):
                pass  # Use None if parsing fails
        
        return cls(
            source_type=source_type,
            source_id=data['source_id'],
            source_location=data.get('source_location'),
            timestamp=timestamp,
            metadata=data.get('metadata', {}),
        )
    
    def to_string(self) -> str:
        """Convert to human-readable string.
        
        Returns:
            Formatted provenance string
        """
        parts = [f"{self.source_type.value}:{self.source_id}"]
        
        if self.source_location:
            parts.append(f"#{self.source_location}")
        
        if self.metadata:
            if 'description' in self.metadata:
                parts.append(f"({self.metadata['description']})")
        
        return "|".join(parts)


@dataclass
class ProvenanceTrail:
    """Complete provenance trail for an artifact.
    
    CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
    Track complete lineage and dependencies of artifacts.
    """
    artifact_id: str
    records: List[ProvenanceRecord] = field(default_factory=list)
    related_artifacts: Set[str] = field(default_factory=set)
    
    def add_record(self, record: ProvenanceRecord):
        """Add a provenance record.
        
        CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
        Append record to provenance chain.
        
        Args:
            record: ProvenanceRecord to add
        """
        self.records.append(record)
    
    def add_source(self, source_type: ProvenanceSource, source_id: str,
                   location: Optional[str] = None,
                   metadata: Optional[Dict] = None):
        """Add a provenance source (convenience method).
        
        CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
        Create and add provenance record in one step.
        
        Args:
            source_type: Type of source
            source_id: Source identifier
            location: Optional location (e.g., line number)
            metadata: Optional metadata dictionary
        """
        record = ProvenanceRecord(
            source_type=source_type,
            source_id=source_id,
            source_location=location,
            metadata=metadata or {}
        )
        self.add_record(record)
    
    def add_related(self, artifact_id: str):
        """Add a related artifact.
        
        Args:
            artifact_id: Related artifact ID
        """
        self.related_artifacts.add(artifact_id)
    
    def get_primary_source(self) -> Optional[ProvenanceRecord]:
        """Get the primary (first) source.
        
        Returns:
            First ProvenanceRecord or None
        """
        return self.records[0] if self.records else None
    
    def get_transformation_chain(self) -> List[ProvenanceRecord]:
        """Get all transformation records.
        
        Returns:
            List of transformation records
        """
        return [
            r for r in self.records
            if r.source_type == ProvenanceSource.TRANSFORMATION
        ]
    
    def get_last_modified(self) -> Optional[datetime]:
        """Get the timestamp of the last modification.
        
        Returns:
            Datetime of last change or None
        """
        if not self.records:
            return None
        return max(r.timestamp for r in self.records if r.timestamp)
    
    def to_dict(self) -> dict:
        """Convert to dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Serialize with complete trail metadata.
        """
        return {
            'artifact_id': self.artifact_id,
            'records': [r.to_dict() for r in self.records],
            'related_artifacts': sorted(self.related_artifacts),
            'last_modified': self.get_last_modified().isoformat() if self.get_last_modified() else None,
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'ProvenanceTrail':
        """Create from dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Deserialize with full trail reconstruction.
        
        Args:
            data: Dictionary with trail fields
            
        Returns:
            ProvenanceTrail instance
            
        Raises:
            ValueError: If required fields are missing
        """
        if 'artifact_id' not in data:
            raise ValueError("Missing required field: artifact_id")
        
        trail = cls(artifact_id=data['artifact_id'])
        
        # Reconstruct records
        for record_data in data.get('records', []):
            record = ProvenanceRecord.from_dict(record_data)
            trail.add_record(record)
        
        # Reconstruct related artifacts
        for artifact in data.get('related_artifacts', []):
            trail.add_related(artifact)
        
        return trail
    
    def to_markdown(self) -> str:
        """Convert to markdown representation.
        
        Returns:
            Markdown formatted provenance
        """
        lines = [f"## Provenance: {self.artifact_id}\n"]
        
        if self.records:
            lines.append("### Sources\n")
            for i, record in enumerate(self.records, 1):
                lines.append(f"{i}. {record.to_string()}")
                if record.metadata:
                    for key, value in record.metadata.items():
                        if key != 'description':
                            lines.append(f"   - {key}: {value}")
            lines.append("")
        
        if self.related_artifacts:
            lines.append("### Related Artifacts\n")
            for artifact in sorted(self.related_artifacts):
                lines.append(f"- {artifact}")
            lines.append("")
        
        return "\n".join(lines)


class ProvenanceTracker:
    """Centralized provenance tracking for a system.
    
    CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
    Centralized tracking of all artifact provenance.
    """
    
    def __init__(self, system_id: str):
        """Initialize tracker.
        
        CHIP-N+1 Implication 1.1.1 (Flexible Initialization):
        Initialize with system identifier for multi-system tracking.
        
        Args:
            system_id: System identifier
        """
        self.system_id = system_id
        self.trails: Dict[str, ProvenanceTrail] = {}
    
    def create_trail(self, artifact_id: str) -> ProvenanceTrail:
        """Create or get a provenance trail.
        
        CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
        Ensure trail exists for artifact tracking.
        
        Args:
            artifact_id: Artifact identifier
            
        Returns:
            ProvenanceTrail (new or existing)
        """
        if artifact_id not in self.trails:
            self.trails[artifact_id] = ProvenanceTrail(artifact_id)
        return self.trails[artifact_id]
    
    def get_trail(self, artifact_id: str) -> Optional[ProvenanceTrail]:
        """Get a provenance trail.
        
        Args:
            artifact_id: Artifact identifier
            
        Returns:
            ProvenanceTrail or None
        """
        return self.trails.get(artifact_id)
    
    def add_provenance(self, artifact_id: str, source_type: ProvenanceSource,
                       source_id: str, location: Optional[str] = None,
                       metadata: Optional[Dict] = None):
        """Add provenance information to an artifact.
        
        Args:
            artifact_id: Artifact ID
            source_type: Type of source
            source_id: Source identifier
            location: Optional location
            metadata: Optional metadata
        """
        trail = self.create_trail(artifact_id)
        trail.add_source(source_type, source_id, location, metadata)
    
    def link_artifacts(self, source_id: str, target_id: str):
        """Link a source artifact to a target artifact.
        
        Args:
            source_id: Source artifact ID
            target_id: Target artifact ID
        """
        trail = self.create_trail(target_id)
        trail.add_related(source_id)
    
    def get_lineage(self, artifact_id: str) -> List[str]:
        """Get the lineage (all ancestors) of an artifact.
        
        Args:
            artifact_id: Artifact ID
            
        Returns:
            List of ancestor artifact IDs
        """
        visited = set()
        lineage = []
        
        def traverse(aid: str):
            if aid in visited:
                return
            visited.add(aid)
            
            trail = self.get_trail(aid)
            if trail:
                for related in trail.related_artifacts:
                    lineage.append(related)
                    traverse(related)
        
        traverse(artifact_id)
        return lineage
    
    def get_dependents(self, artifact_id: str) -> Set[str]:
        """Get all artifacts that depend on this one.
        
        Args:
            artifact_id: Artifact ID
            
        Returns:
            Set of dependent artifact IDs
        """
        dependents = set()
        
        for aid, trail in self.trails.items():
            if artifact_id in trail.related_artifacts:
                dependents.add(aid)
        
        return dependents
    
    def get_transformation_path(self, artifact_id: str) -> List[ProvenanceRecord]:
        """Get the full transformation path for an artifact.
        
        Args:
            artifact_id: Artifact ID
            
        Returns:
            List of transformation records
        """
        trail = self.get_trail(artifact_id)
        if not trail:
            return []
        
        return trail.get_transformation_chain()
    
    def generate_report(self, output_path: Optional[Path] = None) -> Dict[str, Any]:
        """Generate a provenance report.
        
        CHIP-N+1 Implication 1.3.1 (Outcome-Metric Calibration):
        Generate comprehensive provenance report.
        
        Args:
            output_path: Optional path to write JSON report
            
        Returns:
            Report dictionary
        """
        report = {
            'system_id': self.system_id,
            'total_artifacts': len(self.trails),
            'artifacts': {
                aid: trail.to_dict()
                for aid, trail in sorted(self.trails.items())
            }
        }
        
        if output_path:
            try:
                output_path.write_text(json.dumps(report, indent=2, default=str))
            except (IOError, OSError) as e:
                if FileOperationError != IOError:
                    raise FileOperationError(f"Failed to write report: {e}", path=output_path)
                else:
                    raise
        
        return report
    
    @classmethod
    def from_dict(cls, data: dict) -> 'ProvenanceTracker':
        """Create from dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Deserialize complete tracker state.
        
        Args:
            data: Dictionary with tracker state
            
        Returns:
            ProvenanceTracker instance
            
        Raises:
            ValueError: If required fields are missing
        """
        if 'system_id' not in data:
            raise ValueError("Missing required field: system_id")
        
        tracker = cls(system_id=data['system_id'])
        
        # Reconstruct trails
        for artifact_id, trail_data in data.get('artifacts', {}).items():
            # Ensure artifact_id is in trail_data
            if 'artifact_id' not in trail_data:
                trail_data['artifact_id'] = artifact_id
            trail = ProvenanceTrail.from_dict(trail_data)
            tracker.trails[artifact_id] = trail
        
        return tracker
    
    def generate_markdown_report(self, output_path: Optional[Path] = None) -> str:
        """Generate a markdown provenance report.
        
        Args:
            output_path: Optional path to write markdown report
            
        Returns:
            Markdown formatted report
        """
        lines = [f"# Provenance Report: {self.system_id}\n"]
        
        lines.append(f"**Generated:** {datetime.now().isoformat()}\n")
        lines.append(f"**Total Artifacts:** {len(self.trails)}\n\n")
        
        for aid in sorted(self.trails.keys()):
            trail = self.trails[aid]
            lines.append(trail.to_markdown())
        
        content = "\n".join(lines)
        
        if output_path:
            output_path.write_text(content)
        
        return content


class ProvenanceAnalyzer:
    """Analyze provenance patterns and statistics."""
    
    def __init__(self, tracker: ProvenanceTracker):
        """Initialize analyzer.
        
        Args:
            tracker: ProvenanceTracker instance
        """
        self.tracker = tracker
    
    def get_source_statistics(self) -> Dict[str, int]:
        """Get statistics about source types.
        
        Returns:
            Dictionary with source type counts
        """
        stats = {}
        
        for trail in self.tracker.trails.values():
            for record in trail.records:
                source_type = record.source_type.value
                stats[source_type] = stats.get(source_type, 0) + 1
        
        return stats
    
    def get_top_sources(self, limit: int = 10) -> List[Tuple[str, int]]:
        """Get top source identifiers by frequency.
        
        Args:
            limit: Maximum number of results
            
        Returns:
            List of (source_id, count) tuples
        """
        sources = {}
        
        for trail in self.tracker.trails.values():
            for record in trail.records:
                sources[record.source_id] = sources.get(record.source_id, 0) + 1
        
        sorted_sources = sorted(sources.items(), key=lambda x: -x[1])
        return sorted_sources[:limit]
    
    def get_longest_lineage(self) -> Tuple[str, int]:
        """Find artifact with longest lineage.
        
        Returns:
            (artifact_id, lineage_depth)
        """
        longest_id = None
        longest_depth = 0
        
        for aid in self.tracker.trails.keys():
            lineage = self.tracker.get_lineage(aid)
            if len(lineage) > longest_depth:
                longest_depth = len(lineage)
                longest_id = aid
        
        return (longest_id or "", longest_depth)
    
    def find_orphaned_artifacts(self) -> List[str]:
        """Find artifacts with no provenance records.
        
        Returns:
            List of artifact IDs with no sources
        """
        orphaned = []
        
        for aid, trail in self.tracker.trails.items():
            if not trail.records:
                orphaned.append(aid)
        
        return orphaned
    
    def get_analysis_report(self) -> Dict[str, Any]:
        """Generate comprehensive analysis report.
        
        Returns:
            Dictionary with analysis results
        """
        return {
            'total_artifacts': len(self.tracker.trails),
            'total_records': sum(len(t.records) for t in self.tracker.trails.values()),
            'source_statistics': self.get_source_statistics(),
            'top_sources': dict(self.get_top_sources()),
            'longest_lineage': {
                'artifact_id': self.get_longest_lineage()[0],
                'depth': self.get_longest_lineage()[1],
            },
            'orphaned_artifacts': self.find_orphaned_artifacts(),
        }

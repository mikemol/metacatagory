#!/usr/bin/env python3
"""Tests for RecoveryPipeline composite module."""

import pytest
import time
from pathlib import Path
from typing import Any, Dict

from scripts.shared.recovery_pipeline import (
    RecoveryPipeline, RecoveryStrategy, RecoveryMetrics
)
from scripts.shared.errors import ScriptError, FileOperationError
from scripts.shared.logging import StructuredLogger
from scripts.shared.pipelines import Phase, PipelineContext, PhaseStatus


# Test fixtures

class FlakyPhase(Phase[int, int]):
    """Test phase that can fail on demand."""
    
    def __init__(self, name: str, fail_times: int = 0, raise_error: bool = False):
        super().__init__(name)
        self.fail_times = fail_times
        self.raise_error = raise_error
        self.call_count = 0
    
    def transform(self, input_data: int, context: Dict[str, Any]) -> int:
        self.call_count += 1
        
        if self.call_count <= self.fail_times:
            if self.raise_error:
                raise FileOperationError(
                    f"Phase {self.name} failed (attempt {self.call_count})",
                    self.name,
                    attempt=self.call_count
                )
            else:
                raise ValueError(f"Phase {self.name} failed (attempt {self.call_count})")
        
        return input_data + 1


@pytest.fixture
def logger():
    """Create test logger."""
    return StructuredLogger("test_recovery_pipeline")


@pytest.fixture
def default_strategy():
    """Create default recovery strategy."""
    return RecoveryStrategy(max_retries=3, backoff_factor=1.5)


# RecoveryStrategy Tests

class TestRecoveryStrategy:
    """Tests for RecoveryStrategy configuration."""
    
    def test_default_values(self):
        """Test default strategy values."""
        strategy = RecoveryStrategy()
        assert strategy.max_retries == 3
        assert strategy.backoff_factor == 2.0
        assert strategy.respect_recoverable is True
        assert strategy.log_recovery is True
        assert strategy.track_patterns is True
    
    def test_custom_values(self):
        """Test custom strategy configuration."""
        strategy = RecoveryStrategy(
            max_retries=5,
            backoff_factor=3.0,
            respect_recoverable=False,
            log_recovery=False,
            track_patterns=False
        )
        assert strategy.max_retries == 5
        assert strategy.backoff_factor == 3.0
        assert strategy.respect_recoverable is False
        assert strategy.log_recovery is False
        assert strategy.track_patterns is False
    
    def test_to_dict(self):
        """Test strategy serialization."""
        strategy = RecoveryStrategy(max_retries=5, backoff_factor=3.0)
        data = strategy.to_dict()
        
        assert data["max_retries"] == 5
        assert data["backoff_factor"] == 3.0
        assert data["respect_recoverable"] is True
        assert isinstance(data, dict)
    
    def test_from_dict(self):
        """Test strategy deserialization."""
        data = {
            "max_retries": 5,
            "backoff_factor": 3.0,
            "respect_recoverable": False,
            "log_recovery": False,
            "track_patterns": True,
        }
        strategy = RecoveryStrategy.from_dict(data)
        
        assert strategy.max_retries == 5
        assert strategy.backoff_factor == 3.0
        assert strategy.respect_recoverable is False
        assert strategy.log_recovery is False
        assert strategy.track_patterns is True
    
    def test_roundtrip(self):
        """Test strategy serialization roundtrip."""
        original = RecoveryStrategy(max_retries=7, backoff_factor=2.5)
        data = original.to_dict()
        restored = RecoveryStrategy.from_dict(data)
        
        assert restored.max_retries == original.max_retries
        assert restored.backoff_factor == original.backoff_factor


# RecoveryMetrics Tests

class TestRecoveryMetrics:
    """Tests for RecoveryMetrics tracking."""
    
    def test_initial_state(self):
        """Test initial metrics state."""
        metrics = RecoveryMetrics()
        assert metrics.total_attempts == 0
        assert metrics.successful_recoveries == 0
        assert metrics.failed_recoveries == 0
        assert metrics.success_rate() == 0.0
        assert metrics.avg_retry_time() == 0.0
    
    def test_add_successful_recovery(self):
        """Test recording successful recovery."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("test_phase", retries=2, duration=1.5, success=True)
        
        assert metrics.total_attempts == 1
        assert metrics.successful_recoveries == 1
        assert metrics.failed_recoveries == 0
        assert metrics.success_rate() == 1.0
        assert metrics.avg_retry_time() == 1.5
        assert "2_retries" in metrics.retry_distribution
    
    def test_add_failed_recovery(self):
        """Test recording failed recovery."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("test_phase", retries=3, duration=2.0, success=False)
        
        assert metrics.total_attempts == 1
        assert metrics.successful_recoveries == 0
        assert metrics.failed_recoveries == 1
        assert metrics.success_rate() == 0.0
        assert metrics.avg_retry_time() == 2.0
    
    def test_mixed_recoveries(self):
        """Test mixed success/failure tracking."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("phase1", retries=1, duration=1.0, success=True)
        metrics.add_recovery("phase2", retries=2, duration=2.0, success=False)
        metrics.add_recovery("phase3", retries=1, duration=1.5, success=True)
        
        assert metrics.total_attempts == 3
        assert metrics.successful_recoveries == 2
        assert metrics.failed_recoveries == 1
        assert metrics.success_rate() == pytest.approx(2/3)
        assert metrics.avg_retry_time() == pytest.approx(1.5)
    
    def test_retry_distribution(self):
        """Test retry count distribution tracking."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("phase1", retries=1, duration=1.0, success=True)
        metrics.add_recovery("phase2", retries=1, duration=1.0, success=True)
        metrics.add_recovery("phase3", retries=2, duration=1.0, success=True)
        
        assert metrics.retry_distribution["1_retries"] == 2
        assert metrics.retry_distribution["2_retries"] == 1
    
    def test_failure_patterns(self):
        """Test failure pattern recording."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("phase1", retries=2, duration=1.5, success=False)
        
        assert len(metrics.failure_patterns) == 1
        pattern = metrics.failure_patterns[0]
        assert pattern["phase"] == "phase1"
        assert pattern["retries"] == 2
        assert pattern["duration"] == 1.5
        assert pattern["success"] is False
        assert "timestamp" in pattern
    
    def test_to_dict(self):
        """Test metrics serialization."""
        metrics = RecoveryMetrics()
        metrics.add_recovery("phase1", retries=2, duration=1.5, success=True)
        
        data = metrics.to_dict()
        assert data["total_attempts"] == 1
        assert data["successful_recoveries"] == 1
        assert data["success_rate"] == 1.0
        assert "failure_patterns" in data
    
    def test_from_dict(self):
        """Test metrics deserialization."""
        data = {
            "total_attempts": 5,
            "successful_recoveries": 3,
            "failed_recoveries": 2,
            "total_retry_time": 10.0,
            "retry_distribution": {"1_retries": 2, "2_retries": 3},
            "failure_patterns": [
                {"phase": "test", "retries": 2, "duration": 1.0, "success": False}
            ],
        }
        metrics = RecoveryMetrics.from_dict(data)
        
        assert metrics.total_attempts == 5
        assert metrics.successful_recoveries == 3
        assert metrics.failed_recoveries == 2
        assert metrics.total_retry_time == 10.0
        assert len(metrics.failure_patterns) == 1


# RecoveryPipeline Tests

class TestRecoveryPipeline:
    """Tests for RecoveryPipeline execution."""
    
    def test_initialization(self, logger, default_strategy):
        """Test pipeline initialization."""
        pipeline = RecoveryPipeline(logger, default_strategy, name="test")
        
        assert pipeline.name == "test"
        assert pipeline.logger == logger
        assert pipeline.strategy == default_strategy
        assert len(pipeline.phases) == 0
    
    def test_initialization_defaults(self):
        """Test pipeline initialization with defaults."""
        pipeline = RecoveryPipeline()
        
        assert pipeline.name == "RecoveryPipeline"
        assert pipeline.logger is not None
        assert pipeline.strategy is not None
    
    def test_add_phase(self, logger, default_strategy):
        """Test adding phases to pipeline."""
        pipeline = RecoveryPipeline(logger, default_strategy)
        phase1 = FlakyPhase("phase1")
        phase2 = FlakyPhase("phase2")
        
        pipeline.add_phase(phase1).add_phase(phase2)
        
        assert len(pipeline.phases) == 2
        assert pipeline.phases[0] == phase1
        assert pipeline.phases[1] == phase2
    
    def test_execute_success_no_retries(self, logger, default_strategy):
        """Test successful execution without retries."""
        pipeline = RecoveryPipeline(logger, default_strategy)
        pipeline.add_phase(FlakyPhase("increment1"))
        pipeline.add_phase(FlakyPhase("increment2"))
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 2
        assert result.metrics.retry_count == 0
        assert len(result.context.get("recovery_log", [])) == 0
    
    def test_execute_with_recovery(self, logger):
        """Test execution with successful recovery."""
        strategy = RecoveryStrategy(max_retries=3, backoff_factor=1.0)
        pipeline = RecoveryPipeline(logger, strategy)
        
        # Phase fails twice then succeeds
        pipeline.add_phase(FlakyPhase("flaky", fail_times=2, raise_error=True))
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 1
        assert result.metrics.retry_count == 2
        assert len(result.context.get("recovery_log", [])) > 0
    
    def test_execute_failed_recovery(self, logger):
        """Test execution with failed recovery."""
        strategy = RecoveryStrategy(max_retries=2, backoff_factor=1.0)
        pipeline = RecoveryPipeline(logger, strategy)
        
        # Phase fails more than max_retries
        pipeline.add_phase(FlakyPhase("broken", fail_times=5, raise_error=True))
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.FAILURE
        assert result.output is None
        assert result.error is not None
        assert result.metrics.retry_count > 0
    
    def test_metrics_tracking(self, logger):
        """Test recovery metrics tracking."""
        strategy = RecoveryStrategy(max_retries=3, backoff_factor=1.0, track_patterns=True)
        pipeline = RecoveryPipeline(logger, strategy)
        
        pipeline.add_phase(FlakyPhase("flaky", fail_times=1, raise_error=True))
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.SUCCESS
        assert pipeline.metrics.total_attempts > 0
        assert pipeline.metrics.successful_recoveries > 0
    
    def test_execution_history(self, logger, default_strategy):
        """Test execution history tracking."""
        pipeline = RecoveryPipeline(logger, default_strategy)
        pipeline.add_phase(FlakyPhase("increment"))
        
        # Execute twice
        pipeline.execute(0)
        pipeline.execute(10)
        
        assert len(pipeline._execution_history) == 2
        assert pipeline._execution_history[0]["status"] == PhaseStatus.SUCCESS.value
        assert pipeline._execution_history[1]["status"] == PhaseStatus.SUCCESS.value
    
    def test_analyze_recovery_patterns_no_data(self, logger, default_strategy):
        """Test pattern analysis with no executions."""
        pipeline = RecoveryPipeline(logger, default_strategy)
        
        analysis = pipeline.analyze_recovery_patterns()
        
        assert analysis["total_executions"] == 0
        assert "No executions recorded" in analysis["recommendations"][0]
    
    def test_analyze_recovery_patterns_with_data(self, logger):
        """Test pattern analysis with execution data."""
        strategy = RecoveryStrategy(max_retries=3, backoff_factor=1.0, track_patterns=True)
        pipeline = RecoveryPipeline(logger, strategy, name="analysis_test")
        
        # Execute with some failures
        pipeline.add_phase(FlakyPhase("flaky", fail_times=1, raise_error=True))
        pipeline.execute(0)
        pipeline.execute(0)
        
        analysis = pipeline.analyze_recovery_patterns()
        
        assert analysis["pipeline_name"] == "analysis_test"
        assert analysis["total_executions"] == 2
        assert "execution_stats" in analysis
        assert "recommendations" in analysis
        assert analysis["execution_stats"]["success_rate"] > 0
    
    def test_analyze_recommendations_low_success(self, logger):
        """Test recommendations for low recovery success."""
        strategy = RecoveryStrategy(max_retries=1, backoff_factor=1.0, track_patterns=True)
        pipeline = RecoveryPipeline(logger, strategy)
        
        # Execute with consistent failures
        pipeline.add_phase(FlakyPhase("broken", fail_times=5, raise_error=True))
        for _ in range(3):
            pipeline.execute(0)
        
        analysis = pipeline.analyze_recovery_patterns()
        
        # Should recommend increasing max_retries
        recommendations = " ".join(analysis["recommendations"])
        assert "max_retries" in recommendations.lower() or "root causes" in recommendations.lower()


# Serialization Tests

class TestRecoveryPipelineSerialization:
    """Tests for RecoveryPipeline serialization."""
    
    def test_to_dict(self, logger, default_strategy):
        """Test pipeline serialization."""
        pipeline = RecoveryPipeline(logger, default_strategy, name="serialize_test")
        pipeline.add_phase(FlakyPhase("phase1"))
        pipeline.add_phase(FlakyPhase("phase2"))
        
        data = pipeline.to_dict()
        
        assert data["name"] == "serialize_test"
        assert data["phase_count"] == 2
        assert "strategy" in data
        assert "metrics" in data
        assert "execution_history" in data
    
    def test_from_dict_without_phases(self, logger):
        """Test pipeline deserialization without phases."""
        data = {
            "name": "restored",
            "strategy": {"max_retries": 5, "backoff_factor": 3.0},
            "phase_count": 2,
            "metrics": {"total_attempts": 3},
            "execution_history": [],
        }
        
        pipeline = RecoveryPipeline.from_dict(data, logger=logger)
        
        assert pipeline.name == "restored"
        assert pipeline.strategy.max_retries == 5
        assert len(pipeline.phases) == 0  # Phases not restored
    
    def test_from_dict_with_phases(self, logger):
        """Test pipeline deserialization with phases."""
        data = {
            "name": "restored",
            "strategy": {"max_retries": 5},
            "phase_count": 2,
            "metrics": {},
            "execution_history": [],
        }
        
        phases = [FlakyPhase("phase1"), FlakyPhase("phase2")]
        pipeline = RecoveryPipeline.from_dict(data, phases=phases, logger=logger)
        
        assert pipeline.name == "restored"
        assert len(pipeline.phases) == 2
    
    def test_roundtrip_preserves_state(self, logger):
        """Test serialization roundtrip preserves state."""
        original = RecoveryPipeline(logger, name="roundtrip_test")
        original.add_phase(FlakyPhase("phase1"))
        
        # Execute to create history
        original.execute(0)
        
        # Serialize and restore
        data = original.to_dict()
        phases = [FlakyPhase("phase1")]  # Must provide phases separately
        restored = RecoveryPipeline.from_dict(data, phases=phases, logger=logger)
        
        assert restored.name == original.name
        assert len(restored._execution_history) == len(original._execution_history)


# Integration Tests

class TestRecoveryPipelineIntegration:
    """Integration tests for RecoveryPipeline."""
    
    def test_multi_phase_with_mixed_recovery(self, logger):
        """Test multi-phase pipeline with mixed recovery needs."""
        strategy = RecoveryStrategy(max_retries=3, backoff_factor=1.0)
        pipeline = RecoveryPipeline(logger, strategy)
        
        # Phase 1: Always succeeds
        pipeline.add_phase(FlakyPhase("always_success"))
        
        # Phase 2: Fails once then succeeds
        pipeline.add_phase(FlakyPhase("flaky", fail_times=1, raise_error=True))
        
        # Phase 3: Always succeeds
        pipeline.add_phase(FlakyPhase("also_success"))
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 3  # All phases increment by 1
        assert result.metrics.retry_count == 1  # Only phase2 retried
    
    def test_early_failure_stops_pipeline(self, logger):
        """Test pipeline stops on unrecoverable failure."""
        strategy = RecoveryStrategy(max_retries=1, backoff_factor=1.0)
        pipeline = RecoveryPipeline(logger, strategy)
        
        # Phase 1: Fails too many times
        pipeline.add_phase(FlakyPhase("broken", fail_times=5, raise_error=True))
        
        # Phase 2: Never reached
        phase2 = FlakyPhase("unreached")
        pipeline.add_phase(phase2)
        
        result = pipeline.execute(0)
        
        assert result.status == PhaseStatus.FAILURE
        assert phase2.call_count == 0  # Phase 2 never executed
    
    def test_full_workflow_with_analysis(self, logger):
        """Test complete workflow: execute -> analyze -> recommendations."""
        strategy = RecoveryStrategy(max_retries=3, backoff_factor=1.0, track_patterns=True)
        pipeline = RecoveryPipeline(logger, strategy, name="full_workflow")
        
        pipeline.add_phase(FlakyPhase("phase1", fail_times=1, raise_error=True))
        pipeline.add_phase(FlakyPhase("phase2", fail_times=0))
        pipeline.add_phase(FlakyPhase("phase3", fail_times=2, raise_error=True))
        
        # Execute multiple times
        for _ in range(3):
            pipeline.execute(0)
        
        # Analyze patterns
        analysis = pipeline.analyze_recovery_patterns()
        
        assert analysis["total_executions"] == 3
        assert "execution_stats" in analysis
        assert "recovery_metrics" in analysis
        assert len(analysis["recommendations"]) > 0
        
        # Verify metrics tracked patterns
        assert pipeline.metrics.total_attempts > 0
        assert len(pipeline.metrics.failure_patterns) > 0

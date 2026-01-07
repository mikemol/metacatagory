#!/usr/bin/env python3
"""Test suite for scripts.shared.pipelines module."""

import pytest
import time
from typing import Any, Dict
from scripts.shared.pipelines import (
    Phase, PhaseStatus, PhaseMetrics, PhaseResult,
    ComposedPhase, Pipeline, PipelineContext,
)


class TestPhaseStatus:
    """Test PhaseStatus enum."""
    
    def test_phase_status_values(self):
        """Test phase status values."""
        assert PhaseStatus.PENDING.value == "pending"
        assert PhaseStatus.RUNNING.value == "running"
        assert PhaseStatus.SUCCESS.value == "success"
        assert PhaseStatus.FAILURE.value == "failure"
        assert PhaseStatus.SKIPPED.value == "skipped"


class TestPhaseMetrics:
    """Test PhaseMetrics class."""
    
    def test_metrics_creation(self):
        """Test creating metrics."""
        start = time.time()
        metrics = PhaseMetrics(start_time=start)
        assert metrics.start_time == start
        assert metrics.end_time is None
        assert metrics.error_count == 0
        assert metrics.warning_count == 0
    
    def test_metrics_duration(self):
        """Test calculating duration."""
        start = 1000.0
        end = 1005.5
        metrics = PhaseMetrics(start_time=start, end_time=end)
        metrics.finalize()
        assert abs(metrics.duration_seconds - 5.5) < 0.01
    
    def test_metrics_dict(self):
        """Test metrics to_dict method not available but data present."""
        start = 1000.0
        end = 1005.0
        metrics = PhaseMetrics(start_time=start, end_time=end)
        metrics.error_count = 1
        metrics.warning_count = 2
        
        metrics.finalize()
        assert metrics.start_time == 1000.0
        assert metrics.end_time == 1005.0
        assert metrics.duration_seconds == 5.0
        assert metrics.error_count == 1
        assert metrics.warning_count == 2


class TestPhaseResult:
    """Test PhaseResult class."""
    
    def test_result_creation(self):
        """Test creating results."""
        result = PhaseResult[str](
            status=PhaseStatus.SUCCESS,
            output="test_output"
        )
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == "test_output"
        assert result.error is None
    
    def test_result_failure(self):
        """Test creating failure results."""
        result = PhaseResult[str](
            status=PhaseStatus.FAILURE,
            error="test error"
        )
        assert result.status == PhaseStatus.FAILURE
        assert result.error == "test error"
    
    def test_result_dict(self):
        """Test result to_dict."""
        result = PhaseResult[int](
            status=PhaseStatus.SUCCESS,
            output=42,
            metrics=PhaseMetrics(start_time=100.0, end_time=105.0)
        )
        
        d = result.to_dict()
        assert d['status'] == 'success'
        assert d['has_output'] is True
        assert d['error'] is None
    
    def test_result_is_success(self):
        """Test is_success method."""
        success = PhaseResult[int](status=PhaseStatus.SUCCESS, output=42)
        failure = PhaseResult[int](status=PhaseStatus.FAILURE, error="failed")
        
        assert success.is_success() is True
        assert failure.is_success() is False
    
    def test_result_is_failure(self):
        """Test is_failure method."""
        success = PhaseResult[int](status=PhaseStatus.SUCCESS, output=42)
        failure = PhaseResult[int](status=PhaseStatus.FAILURE, error="failed")
        
        assert failure.is_failure() is True
        assert success.is_failure() is False


class SimpleDemoPhase(Phase):
    """Simple demo phase for testing."""
    
    def __init__(self, name: str, transform_fn):
        """Initialize with a transformation function."""
        super().__init__(name)
        self.transform_fn = transform_fn
    
    def transform(self, input_data: Any, context: Dict[str, Any]) -> Any:
        """Apply transformation."""
        return self.transform_fn(input_data)


class TestPhaseExecution:
    """Test Phase execution."""
    
    def test_phase_execute_success(self):
        """Test successful phase execution."""
        phase = SimpleDemoPhase("double", lambda x: x * 2)
        result = phase.execute(5)
        
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 10
        assert result.is_success() is True
    
    def test_phase_execute_failure(self):
        """Test failed phase execution."""
        def failing(x):
            raise ValueError("test error")
        
        phase = SimpleDemoPhase("fail", failing)
        result = phase.execute(5)
        
        assert result.status == PhaseStatus.FAILURE
        assert result.error is not None
        assert "test error" in result.error
        assert result.is_failure() is True
    
    def test_phase_with_context(self):
        """Test phase execution with context."""
        def use_context(x):
            return x
        
        phase = SimpleDemoPhase("ctx", use_context)
        context = {"key": "value"}
        result = phase.execute(42, context)
        
        assert result.context == context
        assert result.output == 42


class TestComposedPhase:
    """Test ComposedPhase."""
    
    def test_compose_two_phases(self):
        """Test composing two phases."""
        phase1 = SimpleDemoPhase("add", lambda x: x + 1)
        phase2 = SimpleDemoPhase("double", lambda x: x * 2)
        
        composed = ComposedPhase([phase1, phase2])
        result = composed.execute(5)
        
        # (5 + 1) * 2 = 12
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 12
    
    def test_compose_with_failure(self):
        """Test composition stops on failure."""
        phase1 = SimpleDemoPhase("add", lambda x: x + 1)
        phase2 = SimpleDemoPhase("fail", lambda x: 1 / 0)
        phase3 = SimpleDemoPhase("double", lambda x: x * 2)
        
        composed = ComposedPhase([phase1, phase2, phase3])
        result = composed.execute(5)
        
        assert result.status == PhaseStatus.FAILURE
        assert "failed" in result.error.lower()
    
    def test_compose_chain(self):
        """Test chaining via compose method."""
        phase1 = SimpleDemoPhase("add5", lambda x: x + 5)
        phase2 = SimpleDemoPhase("double", lambda x: x * 2)
        
        composed = phase1.compose(phase2)
        result = composed.execute(10)
        
        # (10 + 5) * 2 = 30
        assert result.status == PhaseStatus.SUCCESS
        assert result.output == 30


class TestPipelineContext:
    """Test PipelineContext."""
    
    def test_context_creation(self):
        """Test creating pipeline context."""
        ctx = PipelineContext("test_pipeline")
        assert ctx.pipeline_id == "test_pipeline"
        assert len(ctx.phases) == 0
        assert ctx.end_time is None
    
    def test_context_add_result(self):
        """Test adding phase results."""
        ctx = PipelineContext("test_pipeline")
        result1 = PhaseResult(status=PhaseStatus.SUCCESS, output=42)
        
        ctx.add_phase_result("phase1", result1)
        
        assert "phase1" in ctx.phases
        assert ctx.phases["phase1"] is result1
    
    def test_context_finalize(self):
        """Test context finalization."""
        ctx = PipelineContext("test_pipeline")
        assert ctx.end_time is None
        
        ctx.finalize()
        
        assert ctx.end_time is not None
    
    def test_context_total_duration(self):
        """Test duration calculation."""
        ctx = PipelineContext("test_pipeline")
        time.sleep(0.05)  # Wait 50ms
        
        duration = ctx.total_duration_seconds()
        
        assert duration >= 0.04  # At least ~40ms


class TestPipeline:
    """Test Pipeline orchestrator."""
    
    def test_pipeline_execution(self):
        """Test executing a pipeline."""
        phase1 = SimpleDemoPhase("step1", lambda x: x + 1)
        phase2 = SimpleDemoPhase("step2", lambda x: x * 2)
        
        pipeline = Pipeline("test", [phase1, phase2])
        context = pipeline.execute(5)
        
        # Pipeline returns PipelineContext, not PhaseResult
        assert context is not None
        assert context.pipeline_id == "test"
        assert len(context.phases) == 2
        
        # Last phase result
        last_result = context.phases["step2"]
        assert last_result.is_success()
        assert last_result.output == 12
    
    def test_pipeline_with_context(self):
        """Test pipeline tracking via context."""
        phase1 = SimpleDemoPhase("step1", lambda x: x + 1)
        phase2 = SimpleDemoPhase("step2", lambda x: x * 2)
        
        pipeline = Pipeline("test", [phase1, phase2])
        context = pipeline.execute(5)
        
        assert context.phases["step1"].output == 6
        assert context.phases["step2"].output == 12
    
    def test_pipeline_stop_on_error(self):
        """Test pipeline with stop_on_error."""
        phase1 = SimpleDemoPhase("step1", lambda x: x + 1)
        phase2 = SimpleDemoPhase("step2", lambda x: 1 / 0)
        phase3 = SimpleDemoPhase("step3", lambda x: x * 2)
        
        pipeline = Pipeline("test", [phase1, phase2, phase3], stop_on_error=True)
        
        # Should raise RuntimeError when stop_on_error=True
        with pytest.raises(RuntimeError):
            pipeline.execute(5)


class TestPipelineIntegration:
    """Integration tests for pipelines."""
    
    def test_complex_pipeline(self):
        """Test complex pipeline workflow."""
        phases = [
            SimpleDemoPhase("parse", lambda x: int(x) if isinstance(x, str) else x),
            SimpleDemoPhase("validate", lambda x: x if x > 0 else -1),
            SimpleDemoPhase("transform", lambda x: x * 10),
            SimpleDemoPhase("compute", lambda x: x ** 2),
        ]
        
        pipeline = Pipeline("complex", phases)
        context = pipeline.execute("5")
        
        # "5" -> 5 -> 5 -> 50 -> 2500
        assert context.phases["compute"].output == 2500
        assert context.phases["compute"].is_success()
    
    def test_pipeline_metrics(self):
        """Test that pipeline tracks metrics."""
        def slow_op(x):
            time.sleep(0.01)
            return x * 2
        
        phase = SimpleDemoPhase("slow", slow_op)
        result = phase.execute(5)
        
        assert result.metrics.start_time is not None
        assert result.metrics.end_time is not None
        assert result.metrics.duration_seconds >= 0.01
    
    def test_empty_pipeline(self):
        """Test handling empty pipeline."""
        pipeline = Pipeline("empty", [])
        context = pipeline.execute(5)
        
        # Empty pipeline should succeed with no phases
        assert context.pipeline_id == "empty"
        assert len(context.phases) == 0

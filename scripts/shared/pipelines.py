#!/usr/bin/env python3
"""
Pipeline orchestration and execution module for coordinating multi-step processes.

Provides composable pipeline primitives following SPPF model:
- Phase: Atomic transformation unit (A → B)
- Pipeline: Sequential or parallel composition of phases
- PipelineContext: Execution state tracking
- PipelineResult: Success/failure with metrics

Enhanced with error handling, logging, and recovery patterns (CHIP-N+1).
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Generic, List, Optional, TypeVar, Dict, Union
import time
import json
from datetime import datetime

# Import enhanced modules for integration
try:
    from .errors import ScriptError, FileOperationError, ValidationError as ScriptValidationError
    from .logging import StructuredLogger
except ImportError:
    ScriptError = None  # type: ignore
    FileOperationError = None  # type: ignore
    ScriptValidationError = None  # type: ignore
    StructuredLogger = None  # type: ignore

# Type variables for generic phases
A = TypeVar('A')
B = TypeVar('B')
T = TypeVar('T')


class PhaseStatus(Enum):
    """Status of a phase execution."""
    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILURE = "failure"
    SKIPPED = "skipped"


@dataclass
class PhaseMetrics:
    """Metrics for a phase execution.
    
    Source: CHIP-N+1 Implication 1.3.1 (outcome-metric calibration)
    Tracks execution performance, errors, and recovery attempts.
    """
    start_time: float
    end_time: Optional[float] = None
    duration_seconds: Optional[float] = None
    error_count: int = 0
    warning_count: int = 0
    retry_count: int = 0  # NEW: Track retry attempts
    recovery_attempts: int = 0  # NEW: Track recovery actions
    
    def finalize(self):
        """Calculate final metrics."""
        if self.end_time is None:
            self.end_time = time.time()
        self.duration_seconds = self.end_time - self.start_time
    
    def to_dict(self) -> dict:
        """Serialize metrics to dictionary (NEW: symmetry pattern).
        
        Source: Implication 1.2 (serialization symmetry)
        """
        self.finalize()
        return {
            'start_time': self.start_time,
            'end_time': self.end_time,
            'duration_seconds': self.duration_seconds,
            'error_count': self.error_count,
            'warning_count': self.warning_count,
            'retry_count': self.retry_count,
            'recovery_attempts': self.recovery_attempts,
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'PhaseMetrics':
        """Deserialize metrics from dictionary (NEW: symmetry pattern).
        
        Source: Implication 1.2 (serialization symmetry)
        """
        metrics = cls(start_time=data.get('start_time', time.time()))
        metrics.end_time = data.get('end_time')
        metrics.duration_seconds = data.get('duration_seconds')
        metrics.error_count = data.get('error_count', 0)
        metrics.warning_count = data.get('warning_count', 0)
        metrics.retry_count = data.get('retry_count', 0)
        metrics.recovery_attempts = data.get('recovery_attempts', 0)
        return metrics


@dataclass
class PhaseResult(Generic[B]):
    """Result of a phase execution.
    
    Source: CHIP-N+1 Implication 1.3.2 (outcome-driven prioritization)
    Enhanced with error causality and recovery semantics.
    """
    status: PhaseStatus
    output: Optional[B] = None
    error: Optional[str] = None
    error_type: Optional[str] = None  # NEW: Track error type for causality
    caused_by: Optional[str] = None  # NEW: Causality chain (from errors.py pattern)
    metrics: PhaseMetrics = field(default_factory=lambda: PhaseMetrics(start_time=time.time()))
    context: Dict[str, Any] = field(default_factory=dict)
    recovery_hint: Optional[str] = None  # NEW: Recovery suggestion
    
    def is_success(self) -> bool:
        """Check if phase succeeded."""
        return self.status == PhaseStatus.SUCCESS
    
    def is_failure(self) -> bool:
        """Check if phase failed."""
        return self.status == PhaseStatus.FAILURE
    
    def with_cause(self, cause: str, hint: str = "") -> 'PhaseResult':
        """Add causal error and recovery hint (from errors.py pattern).
        
        Source: Implication 2.3.1 (causality chain enhancement)
        
        Args:
            cause: Description of root cause
            hint: Recovery suggestion
            
        Returns:
            Self for chaining
        """
        self.caused_by = cause
        self.recovery_hint = hint
        return self
    
    def to_dict(self) -> dict:
        """Convert to dictionary with full causality chain.
        
        Source: Implication 1.2 (serialization symmetry)
        """
        self.metrics.finalize()
        return {
            'status': self.status.value,
            'duration_seconds': self.metrics.duration_seconds,
            'error_count': self.metrics.error_count,
            'warning_count': self.metrics.warning_count,
            'retry_count': self.metrics.retry_count,
            'recovery_attempts': self.metrics.recovery_attempts,
            'error': self.error,
            'error_type': self.error_type,
            'caused_by': self.caused_by,
            'recovery_hint': self.recovery_hint,
            'has_output': self.output is not None,
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'PhaseResult':
        """Deserialize result from dictionary.
        
        Source: Implication 1.2 (serialization symmetry)
        """
        result = cls(
            status=PhaseStatus(data.get('status', 'pending')),
            error=data.get('error'),
            error_type=data.get('error_type'),
            caused_by=data.get('caused_by'),
            recovery_hint=data.get('recovery_hint'),
            metrics=PhaseMetrics.from_dict(data.get('metrics', {})),
        )
        return result


class Phase(ABC, Generic[A, B]):
    """Abstract base class for a transformation phase.
    
    A Phase transforms input of type A to output of type B.
    Each phase is atomic, auditable, and composable.
    """
    
    def __init__(self, name: str, description: str = ""):
        """Initialize phase.
        
        Args:
            name: Unique phase identifier
            description: Human-readable description
        """
        self.name = name
        self.description = description
    
    @abstractmethod
    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        """Execute phase transformation.
        
        Args:
            input_data: Input of type A
            context: Execution context
            
        Returns:
            Output of type B
            
        Raises:
            Exception: On transformation failure
        """
        ...
    
    def execute(self, input_data: A, context: Optional[Dict[str, Any]] = None) -> PhaseResult[B]:
        """Execute phase with error handling and metrics.
        
        Source: CHIP-N+1 Implication 2.1.1 (protocol-implementation trace)
        Enhanced with error type tracking and causality chains.
        
        Args:
            input_data: Input data
            context: Optional execution context
            
        Returns:
            PhaseResult with status, output, and metrics
        """
        if context is None:
            context = {}
        
        result = PhaseResult[B](
            status=PhaseStatus.RUNNING,
            metrics=PhaseMetrics(start_time=time.time())
        )
        result.context = context
        
        try:
            output = self.transform(input_data, context)
            result.status = PhaseStatus.SUCCESS
            result.output = output
        except Exception as e:
            result.status = PhaseStatus.FAILURE
            result.error = str(e)
            result.error_type = type(e).__name__  # NEW: Track error type
            result.metrics.error_count = 1
            
            # NEW: Extract causality chain from ScriptError if available
            if ScriptError is not None and isinstance(e, ScriptError):
                if hasattr(e, 'caused_by') and e.caused_by:
                    result.caused_by = str(e.caused_by)
                if hasattr(e, 'hint') and e.hint:
                    result.recovery_hint = e.hint
        finally:
            result.metrics.finalize()
        
        return result
    
    def compose(self, other: Phase[B, T]) -> ComposedPhase[A, T]:
        """Compose this phase with another phase.
        
        Args:
            other: Phase to compose (takes output of self as input)
            
        Returns:
            ComposedPhase representing self >> other
        """
        return ComposedPhase([self, other])


class ComposedPhase(Phase[A, B]):
    """Composition of multiple phases executed sequentially."""
    
    def __init__(self, phases: List[Phase]):
        """Initialize composed phase.
        
        Args:
            phases: List of phases to execute in order
        """
        names = " >> ".join(p.name for p in phases)
        super().__init__(f"Composed[{names}]", "Sequential phase composition")
        self.phases = phases
    
    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        """Execute all phases sequentially.
        
        Args:
            input_data: Input for first phase
            context: Execution context
            
        Returns:
            Output of final phase
        """
        current_output = input_data
        
        for phase in self.phases:
            result = phase.execute(current_output, context)
            if result.is_failure():
                raise RuntimeError(
                    f"Phase '{phase.name}' failed: {result.error}"
                )
            current_output = result.output
        
        return current_output


@dataclass
class PipelineContext:
    """Execution context for a pipeline.
    
    Source: CHIP-N+1 Implication 3.1 (feedback-source attribution)
    Tracks all phase results and pipeline-level metrics.
    """
    pipeline_id: str
    start_time: datetime = field(default_factory=datetime.now)
    end_time: Optional[datetime] = None
    phases: Dict[str, PhaseResult] = field(default_factory=dict)
    config: Dict[str, Any] = field(default_factory=dict)
    recovery_log: List[str] = field(default_factory=list)  # NEW: Track recovery actions
    
    def add_phase_result(self, phase_name: str, result: PhaseResult):
        """Record phase result in context."""
        self.phases[phase_name] = result
    
    def log_recovery(self, action: str):
        """Log a recovery action taken.
        
        Source: Implication 3.2.2 (emergence-response protocol)
        """
        timestamp = datetime.now().isoformat()
        self.recovery_log.append(f"[{timestamp}] {action}")
    
    def finalize(self):
        """Finalize pipeline context."""
        self.end_time = datetime.now()
    
    def total_duration_seconds(self) -> float:
        """Get total pipeline duration."""
        if self.end_time is None:
            return (datetime.now() - self.start_time).total_seconds()
        return (self.end_time - self.start_time).total_seconds()
    
    def to_dict(self) -> dict:
        """Convert context to dictionary with full traceability.
        
        Source: Implication 1.2 (serialization symmetry)
        """
        self.finalize()
        return {
            'pipeline_id': self.pipeline_id,
            'start_time': self.start_time.isoformat(),
            'end_time': self.end_time.isoformat() if self.end_time else None,
            'total_duration_seconds': self.total_duration_seconds(),
            'phases': {
                name: result.to_dict()
                for name, result in self.phases.items()
            },
            'recovery_log': self.recovery_log,
            'recovery_count': len(self.recovery_log),
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'PipelineContext':
        """Deserialize context from dictionary.
        
        Source: Implication 1.2 (serialization symmetry)
        """
        start_time = datetime.fromisoformat(data.get('start_time', datetime.now().isoformat()))
        end_time = None
        if data.get('end_time'):
            end_time = datetime.fromisoformat(data['end_time'])
        
        context = cls(
            pipeline_id=data['pipeline_id'],
            start_time=start_time,
            end_time=end_time,
            config=data.get('config', {}),
            recovery_log=data.get('recovery_log', []),
        )
        # Reconstruct phases
        for name, phase_data in data.get('phases', {}).items():
            context.phases[name] = PhaseResult.from_dict(phase_data)
        
        return context


class Pipeline:
    """Multi-phase pipeline orchestrator.
    
    Coordinates execution of multiple phases with error handling,
    dependency management, and comprehensive tracking.
    """
    
    def __init__(self, pipeline_id: str, phases: List[Phase],
                 stop_on_error: bool = True):
        """Initialize pipeline.
        
        Args:
            pipeline_id: Unique pipeline identifier
            phases: List of phases to execute in order
            stop_on_error: If True, stop on first phase failure
        """
        self.pipeline_id = pipeline_id
        self.phases = phases
        self.stop_on_error = stop_on_error
        self.context: Optional[PipelineContext] = None
    
    def execute(self, initial_input: Any,
                config: Optional[Dict[str, Any]] = None) -> PipelineContext:
        """Execute entire pipeline.
        
        Args:
            initial_input: Input for first phase
            config: Optional configuration dictionary
            
        Returns:
            PipelineContext with execution results
        """
        self.context = PipelineContext(
            pipeline_id=self.pipeline_id,
            config=config or {}
        )
        
        current_output = initial_input
        
        for phase in self.phases:
            result = phase.execute(current_output, self.context.config)
            self.context.add_phase_result(phase.name, result)
            
            if result.is_failure():
                if self.stop_on_error:
                    self.context.finalize()
                    raise RuntimeError(
                        f"Pipeline '{self.pipeline_id}' failed at phase '{phase.name}': "
                        f"{result.error}"
                    )
                # Otherwise continue to next phase
            else:
                current_output = result.output
        
        self.context.finalize()
        return self.context
    
    def get_status(self) -> str:
        """Get overall pipeline status."""
        if self.context is None:
            return "not_started"
        
        failed = sum(1 for r in self.context.phases.values() if r.is_failure())
        if failed > 0:
            return "failed"
        
        success = sum(1 for r in self.context.phases.values() if r.is_success())
        if success == len(self.phases):
            return "success"
        
        return "in_progress"
    
    def report(self) -> Dict[str, Any]:
        """Generate execution report.
        
        Returns:
            Dictionary with comprehensive pipeline statistics
        """
        if self.context is None:
            return {"status": "not_started"}
        
        return {
            'pipeline_id': self.pipeline_id,
            'status': self.get_status(),
            'total_duration_seconds': self.context.total_duration_seconds(),
            'phase_count': len(self.phases),
            'success_count': sum(1 for r in self.context.phases.values() if r.is_success()),
            'failure_count': sum(1 for r in self.context.phases.values() if r.is_failure()),
            'skipped_count': sum(1 for r in self.context.phases.values() if r.status == PhaseStatus.SKIPPED),
            'phases': {
                name: result.to_dict()
                for name, result in self.context.phases.items()
            },
        }


class FunctionPhase(Phase[A, B]):
    """Phase wrapping a plain function.
    
    Adapts any function into a Phase for easy composition.
    """
    
    def __init__(self, name: str, func: Callable[[A, Dict[str, Any]], B],
                 description: str = ""):
        """Initialize function-based phase.
        
        Args:
            name: Phase name
            func: Transformation function (input, context) → output
            description: Human-readable description
        """
        super().__init__(name, description)
        self.func = func
    
    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        """Execute wrapped function.
        
        Args:
            input_data: Input data
            context: Execution context
            
        Returns:
            Function output
        """
        return self.func(input_data, context)


class ConditionalPhase(Phase[A, B]):
    """Phase that executes one of two branches based on condition."""
    
    def __init__(self, name: str, condition: Callable[[A, Dict], bool],
                 true_phase: Phase[A, B], false_phase: Phase[A, B]):
        """Initialize conditional phase.
        
        Args:
            name: Phase name
            condition: Predicate function (input, context) → bool
            true_phase: Phase to execute if condition is True
            false_phase: Phase to execute if condition is False
        """
        super().__init__(name, "Conditional phase branching")
        self.condition = condition
        self.true_phase = true_phase
        self.false_phase = false_phase
    
    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        """Execute appropriate branch.
        
        Args:
            input_data: Input data
            context: Execution context
            
        Returns:
            Output from executed branch
        """
        if self.condition(input_data, context):
            result = self.true_phase.execute(input_data, context)
            if result.is_failure():
                raise RuntimeError(f"True branch failed: {result.error}")
            return result.output
        else:
            result = self.false_phase.execute(input_data, context)
            if result.is_failure():
                raise RuntimeError(f"False branch failed: {result.error}")
            return result.output


class RetryPhase(Phase[A, B]):
    """Phase with automatic retry on failure."""
    
    def __init__(self, phase: Phase[A, B], max_retries: int = 3,
                 backoff_factor: float = 2.0):
        """Initialize retry phase.
        
        Args:
            phase: Phase to wrap
            max_retries: Maximum number of retries
            backoff_factor: Exponential backoff multiplier
        """
        super().__init__(
            f"Retry[{phase.name}]",
            f"Phase with up to {max_retries} retries"
        )
        self.phase = phase
        self.max_retries = max_retries
        self.backoff_factor = backoff_factor
    
    def transform(self, input_data: A, context: Dict[str, Any]) -> B:
        """Execute phase with retries and recovery tracking.
        
        Source: Implication 3.3.1 (correction-refinement loop)
        Tracks retry attempts and logs recovery actions.
        
        Args:
            input_data: Input data
            context: Execution context
            
        Returns:
            Phase output
            
        Raises:
            RuntimeError: If all retries exhausted
        """
        wait_time = 1.0
        last_error = None
        
        for attempt in range(self.max_retries + 1):
            result = self.phase.execute(input_data, context)
            
            if result.is_success():
                # Log recovery if we retried successfully
                if attempt > 0:
                    if hasattr(context, 'get') and 'pipeline_context' in context:
                        context['pipeline_context'].log_recovery(
                            f"Phase '{self.phase.name}' succeeded after {attempt} retries"
                        )
                    result.metrics.retry_count = attempt
                    result.metrics.recovery_attempts = 1
                return result.output
            
            last_error = result.error
            
            if attempt < self.max_retries:
                time.sleep(wait_time)
                wait_time *= self.backoff_factor
        
        raise RuntimeError(
            f"Phase '{self.phase.name}' failed after {self.max_retries + 1} attempts: {last_error}"
        )


class ConcurrentPhase(Phase[A, List[B]]):
    """Phase that executes multiple phases in parallel (stub for future use).
    
    Uses a thread pool when METACATAGORY_PARALLEL is enabled.
    """
    
    def __init__(self, name: str, phases: List[Phase[A, B]]):
        """Initialize concurrent phase.
        
        Args:
            name: Phase name
            phases: List of phases to execute (sequentially for now)
        """
        super().__init__(name, "Concurrent phase execution (sequential stub)")
        self.phases = phases
    
    def transform(self, input_data: A, context: Dict[str, Any]) -> List[B]:
        """Execute phases (sequentially or in parallel).
        
        Args:
            input_data: Input data for all phases
            context: Execution context
            
        Returns:
            List of outputs from all phases
        """
        from concurrent.futures import ThreadPoolExecutor

        parallel = False
        workers = 1
        try:
            from .config import get_config

            config = get_config()
            parallel = config.parallel
            workers = max(1, int(config.workers))
        except Exception:
            parallel = False
            workers = 1

        if not parallel or len(self.phases) <= 1 or workers <= 1:
            results = []
            for phase in self.phases:
                result = phase.execute(input_data, context)
                if result.is_failure():
                    raise RuntimeError(f"Concurrent phase failed: {result.error}")
                results.append(result.output)
            return results

        results: list[B] = [None] * len(self.phases)  # type: ignore[list-item]

        def run_phase(idx: int, phase: Phase[A, B]) -> tuple[int, PhaseResult[B]]:
            return idx, phase.execute(input_data, context)

        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = [executor.submit(run_phase, idx, phase) for idx, phase in enumerate(self.phases)]
            for future in futures:
                idx, result = future.result()
                if result.is_failure():
                    raise RuntimeError(f"Concurrent phase failed: {result.error}")
                results[idx] = result.output  # type: ignore[assignment]

        return results

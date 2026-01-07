#!/usr/bin/env python3
"""
RecoveryPipeline: Orchestrated error recovery with full observability.

Composite module integrating:
- errors.retry_on_error: Retry semantics with exponential backoff
- logging.StructuredLogger: Recovery event tracking
- pipelines.Pipeline: Multi-phase orchestration
- pipelines.PhaseResult: Recovery metrics aggregation

CHIP-N+1 Protocol applied:
- 1.1.1: Composable recovery strategies across phases
- 2.1.1: Temporal coordination of retries with backoff
- 3.1.1: Contextual adaptation to failure patterns

Design Principles:
- Automatic retry integration for all phases
- Full observability of recovery attempts
- Recovery pattern analysis and recommendations
- Backward compatible with existing Pipeline API
"""

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Generic, List, Optional, TypeVar, Dict
import time
from datetime import datetime

from .errors import retry_on_error, ScriptError, ErrorSeverity
from .logging import StructuredLogger
from .pipelines import (
    Phase, Pipeline, PipelineContext, PhaseResult, PhaseStatus,
    PhaseMetrics
)

A = TypeVar('A')
B = TypeVar('B')
T = TypeVar('T')


@dataclass
class RecoveryStrategy:
    """Configuration for recovery behavior.
    
    CHIP-N+1 Implication 1.1.1: Composable recovery strategies
    """
    max_retries: int = 3
    backoff_factor: float = 2.0
    respect_recoverable: bool = True
    log_recovery: bool = True
    track_patterns: bool = True
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize recovery strategy.
        
        CHIP-N+1 Implication 1.3.1: Outcome measurement via serialization
        """
        return {
            "max_retries": self.max_retries,
            "backoff_factor": self.backoff_factor,
            "respect_recoverable": self.respect_recoverable,
            "log_recovery": self.log_recovery,
            "track_patterns": self.track_patterns,
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> RecoveryStrategy:
        """Deserialize recovery strategy."""
        return cls(
            max_retries=data.get("max_retries", 3),
            backoff_factor=data.get("backoff_factor", 2.0),
            respect_recoverable=data.get("respect_recoverable", True),
            log_recovery=data.get("log_recovery", True),
            track_patterns=data.get("track_patterns", True),
        )


@dataclass
class RecoveryMetrics:
    """Metrics for recovery analysis.
    
    CHIP-N+1 Implication 2.1.1: Temporal coordination tracking
    """
    total_attempts: int = 0
    successful_recoveries: int = 0
    failed_recoveries: int = 0
    total_retry_time: float = 0.0
    retry_distribution: Dict[str, int] = field(default_factory=dict)
    failure_patterns: List[Dict[str, Any]] = field(default_factory=list)
    
    def add_recovery(self, phase_name: str, retries: int, duration: float, success: bool):
        """Record recovery attempt."""
        self.total_attempts += 1
        if success:
            self.successful_recoveries += 1
        else:
            self.failed_recoveries += 1
        self.total_retry_time += duration
        
        # Track retry distribution
        retry_key = f"{retries}_retries"
        self.retry_distribution[retry_key] = self.retry_distribution.get(retry_key, 0) + 1
        
        # Record pattern for analysis
        self.failure_patterns.append({
            "phase": phase_name,
            "retries": retries,
            "duration": duration,
            "success": success,
            "timestamp": datetime.now().isoformat(),
        })
    
    def success_rate(self) -> float:
        """Calculate recovery success rate."""
        if self.total_attempts == 0:
            return 0.0
        return self.successful_recoveries / self.total_attempts
    
    def avg_retry_time(self) -> float:
        """Calculate average retry duration."""
        if self.total_attempts == 0:
            return 0.0
        return self.total_retry_time / self.total_attempts
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize recovery metrics."""
        return {
            "total_attempts": self.total_attempts,
            "successful_recoveries": self.successful_recoveries,
            "failed_recoveries": self.failed_recoveries,
            "total_retry_time": self.total_retry_time,
            "success_rate": self.success_rate(),
            "avg_retry_time": self.avg_retry_time(),
            "retry_distribution": self.retry_distribution,
            "failure_patterns": self.failure_patterns,
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> RecoveryMetrics:
        """Deserialize recovery metrics."""
        metrics = cls()
        metrics.total_attempts = data.get("total_attempts", 0)
        metrics.successful_recoveries = data.get("successful_recoveries", 0)
        metrics.failed_recoveries = data.get("failed_recoveries", 0)
        metrics.total_retry_time = data.get("total_retry_time", 0.0)
        metrics.retry_distribution = data.get("retry_distribution", {})
        metrics.failure_patterns = data.get("failure_patterns", [])
        return metrics


class RecoveryPipeline(Generic[A, B]):
    """Pipeline with integrated error recovery and logging.
    
    CHIP-N+1 Implications:
    - 1.1.1: Composable recovery strategies across phases
    - 2.1.1: Temporal coordination of retries with backoff
    - 3.1.1: Contextual adaptation to failure patterns
    
    Example:
        >>> logger = StructuredLogger("recovery_pipeline")
        >>> strategy = RecoveryStrategy(max_retries=3, backoff_factor=2.0)
        >>> pipeline = RecoveryPipeline(logger, strategy)
        >>> 
        >>> # Add phases (same as standard Pipeline)
        >>> pipeline.add_phase(compile_phase)
        >>> pipeline.add_phase(validate_phase)
        >>> 
        >>> # Execute with automatic retry and logging
        >>> result = pipeline.execute(input_data)
        >>> 
        >>> # Analyze recovery patterns
        >>> analysis = pipeline.analyze_recovery_patterns()
        >>> print(f"Success rate: {analysis['success_rate']:.2%}")
    """
    
    def __init__(self,
                 logger: Optional[StructuredLogger] = None,
                 strategy: Optional[RecoveryStrategy] = None,
                 name: str = "RecoveryPipeline"):
        """Initialize recovery pipeline.
        
        Args:
            logger: StructuredLogger for recovery event tracking (creates default if None)
            strategy: RecoveryStrategy configuration (uses defaults if None)
            name: Pipeline name for logging context
        """
        self.logger = logger or StructuredLogger(name)
        self.strategy = strategy or RecoveryStrategy()
        self.name = name
        self.phases: List[Phase] = []
        self.metrics = RecoveryMetrics()
        
        # Track context for all executions
        self._execution_history: List[Dict[str, Any]] = []
    
    def add_phase(self, phase: Phase[A, B]) -> RecoveryPipeline[A, B]:
        """Add phase to pipeline.
        
        Args:
            phase: Phase to add to execution sequence
            
        Returns:
            Self for method chaining
        """
        self.phases.append(phase)
        return self
    
    def execute_phase_with_recovery(self,
                                     phase: Phase[A, B],
                                     input_data: A,
                                     context: Dict[str, Any]) -> PhaseResult[B]:
        """Execute single phase with automatic retry and logging.
        
        CHIP-N+1 Implication 3.1.1: Contextual adaptation to failures
        
        Args:
            phase: Phase to execute
            input_data: Input data for phase
            context: Pipeline execution context
            
        Returns:
            PhaseResult with recovery metrics
        """
        start_time = time.time()
        retry_count = 0
        recovery_log: List[str] = []
        
        # Log phase start
        self.logger.info(f"Starting phase: {phase.name}",
                        phase=phase.name,
                        strategy=self.strategy.to_dict())
        
        # Wrap phase execution with retry decorator
        @retry_on_error(
            max_attempts=self.strategy.max_retries,
            delay=1.0,
            backoff=self.strategy.backoff_factor,
            respect_recoverable=self.strategy.respect_recoverable
        )
        def execute_with_logging():
            nonlocal retry_count
            
            try:
                # Execute phase
                output_value = phase.transform(input_data, context)
                
                if retry_count > 0:
                    recovery_msg = f"Recovered after {retry_count} retries"
                    recovery_log.append(recovery_msg)
                    if self.strategy.log_recovery:
                        self.logger.info(recovery_msg,
                                       phase=phase.name,
                                       retry_count=retry_count)
                
                return output_value
                
            except Exception as e:
                retry_count += 1
                error_msg = f"Retry {retry_count}/{self.strategy.max_retries}: {str(e)}"
                recovery_log.append(error_msg)
                
                if self.strategy.log_recovery:
                    self.logger.warning(error_msg,
                                       phase=phase.name,
                                       retry_count=retry_count,
                                       error_type=type(e).__name__)
                raise
        
        # Execute with retry
        try:
            output = execute_with_logging()
            duration = time.time() - start_time
            
            # Track successful recovery
            if retry_count > 0 and self.strategy.track_patterns:
                self.metrics.add_recovery(phase.name, retry_count, duration, success=True)
            
            # Create successful result
            metrics = PhaseMetrics(start_time=start_time, retry_count=retry_count)
            metrics.end_time = start_time + duration
            
            result = PhaseResult(
                status=PhaseStatus.SUCCESS,
                output=output,
                error=None,
                metrics=metrics,
            )
            # Store recovery info in context
            result.context["recovery_log"] = list(recovery_log)
            result.context["retries"] = retry_count
            
            self.logger.info(f"Phase completed: {phase.name}",
                           phase=phase.name,
                           duration=duration,
                           retry_count=retry_count)
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            # Track failed recovery
            if self.strategy.track_patterns:
                self.metrics.add_recovery(phase.name, retry_count, duration, success=False)
            
            # Create failure result
            metrics = PhaseMetrics(start_time=start_time, retry_count=retry_count)
            metrics.end_time = start_time + duration
            
            result = PhaseResult(
                status=PhaseStatus.FAILURE,
                output=None,
                error=str(e),
                metrics=metrics,
            )
            result.context["recovery_log"] = list(recovery_log)
            result.context["retries"] = retry_count
            
            self.logger.error(f"Phase failed: {phase.name}",
                            phase=phase.name,
                            duration=duration,
                            retry_count=retry_count,
                            error=str(e))
            
            return result
    
    def execute(self, input_data: A) -> PhaseResult[B]:
        """Execute all phases with recovery.
        
        Args:
            input_data: Initial input for first phase
            
        Returns:
            Final PhaseResult with aggregated recovery metrics
        """
        execution_start = time.time()
        context = PipelineContext(
            pipeline_id=self.name,
            config={"strategy": self.strategy.to_dict(), "phase_count": len(self.phases)}
        )
        
        self.logger.info(f"Starting pipeline: {self.name}",
                        pipeline=self.name,
                        phase_count=len(self.phases))
        
        current_data = input_data
        phase_results: List[PhaseResult] = []
        
        # Execute each phase sequentially
        for i, phase in enumerate(self.phases):
            # Store phase index in config
            context.config["current_phase"] = i
            
            result = self.execute_phase_with_recovery(phase, current_data, context.config)
            phase_results.append(result)
            
            if result.status != PhaseStatus.SUCCESS:
                # Pipeline failed, stop execution
                break
            
            current_data = result.output
        
        execution_time = time.time() - execution_start
        
        # Determine overall status
        if all(r.status == PhaseStatus.SUCCESS for r in phase_results):
            overall_status = PhaseStatus.SUCCESS
            final_output = current_data
        else:
            overall_status = PhaseStatus.FAILURE
            final_output = None
        
        # Aggregate metrics
        total_retries = sum((r.metrics.retry_count if r.metrics else 0) for r in phase_results)
        all_recovery_logs = []
        for r in phase_results:
            all_recovery_logs.extend(r.context.get("recovery_log", []))
        
        final_result = PhaseResult(
            status=overall_status,
            output=final_output,
            error=next((r.error for r in phase_results if r.error), None),
            metrics=PhaseMetrics(start_time=execution_start, retry_count=total_retries),
        )
        final_result.metrics.end_time = execution_start + execution_time
        # Include aggregated recovery log in context
        final_result.context["recovery_log"] = all_recovery_logs
        
        # Record execution in history
        self._execution_history.append({
            "timestamp": datetime.now().isoformat(),
            "status": overall_status.value,
            "execution_time": execution_time,
            "total_retries": total_retries,
            "phase_count": len(self.phases),
            "phase_results": [r.to_dict() for r in phase_results],
        })
        
        self.logger.info(f"Pipeline completed: {self.name}",
                        pipeline=self.name,
                        status=overall_status.value,
                        execution_time=execution_time,
                        total_retries=total_retries)
        
        return final_result
    
    def analyze_recovery_patterns(self) -> Dict[str, Any]:
        """Analyze recovery success rates and failure patterns.
        
        CHIP-N+1 Implication 3.2.1: Emergence pattern recognition
        
        Returns:
            Analysis report with recovery statistics and recommendations
        """
        analysis = {
            "pipeline_name": self.name,
            "total_executions": len(self._execution_history),
            "recovery_metrics": self.metrics.to_dict(),
        }
        
        if not self._execution_history:
            analysis["recommendations"] = ["No executions recorded yet"]
            return analysis
        
        # Calculate execution statistics
        successful_executions = sum(
            1 for e in self._execution_history 
            if e["status"] == PhaseStatus.SUCCESS.value
        )
        avg_execution_time = sum(e["execution_time"] for e in self._execution_history) / len(self._execution_history)
        avg_retries = sum(e["total_retries"] for e in self._execution_history) / len(self._execution_history)
        
        analysis["execution_stats"] = {
            "success_rate": successful_executions / len(self._execution_history),
            "avg_execution_time": avg_execution_time,
            "avg_retries_per_execution": avg_retries,
        }
        
        # Generate recommendations
        recommendations = []
        
        if self.metrics.success_rate() < 0.5:
            recommendations.append(
                "Low recovery success rate - consider increasing max_retries or investigating root causes"
            )
        
        if avg_retries > 1.0:
            recommendations.append(
                f"High average retries ({avg_retries:.1f}) - consider optimizing phases or increasing backoff_factor"
            )
        
        if self.metrics.avg_retry_time() > 10.0:
            recommendations.append(
                f"Long average retry time ({self.metrics.avg_retry_time():.1f}s) - consider reducing backoff_factor"
            )
        
        # Identify problematic phases
        if self.metrics.failure_patterns:
            phase_failures = {}
            for pattern in self.metrics.failure_patterns:
                phase = pattern["phase"]
                phase_failures[phase] = phase_failures.get(phase, 0) + 1
            
            most_failures = max(phase_failures.items(), key=lambda x: x[1])
            if most_failures[1] > len(self._execution_history) * 0.2:
                recommendations.append(
                    f"Phase '{most_failures[0]}' has frequent failures ({most_failures[1]}) - investigate implementation"
                )
        
        analysis["recommendations"] = recommendations or ["Pipeline recovery performing well"]
        
        return analysis
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize pipeline state.
        
        CHIP-N+1 Implication 1.1.1: Composable serialization
        """
        return {
            "name": self.name,
            "strategy": self.strategy.to_dict(),
            "phase_count": len(self.phases),
            "metrics": self.metrics.to_dict(),
            "execution_history": self._execution_history,
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any], 
                  phases: Optional[List[Phase]] = None,
                  logger: Optional[StructuredLogger] = None) -> RecoveryPipeline:
        """Deserialize pipeline state.
        
        Note: Phases must be provided separately as they contain executable code.
        
        Args:
            data: Serialized pipeline data
            phases: List of Phase objects to restore
            logger: StructuredLogger instance (creates default if None)
            
        Returns:
            Reconstructed RecoveryPipeline
        """
        strategy = RecoveryStrategy.from_dict(data.get("strategy", {}))
        pipeline = cls(
            logger=logger,
            strategy=strategy,
            name=data.get("name", "RecoveryPipeline")
        )
        
        if phases:
            for phase in phases:
                pipeline.add_phase(phase)
        
        pipeline.metrics = RecoveryMetrics.from_dict(data.get("metrics", {}))
        pipeline._execution_history = data.get("execution_history", [])
        
        return pipeline

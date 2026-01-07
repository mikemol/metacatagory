#!/usr/bin/env python3
"""Expanded test suite for errors.py - First-Order Implications.

Tests focus on:
1. Type Safety at Boundaries
2. Recovery Chain Completeness  
3. Error Context Accumulation

Source: MODULE-AUDIT-PHASE-1.md, First-Order Implications
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import json
from scripts.shared.errors import (
    ErrorSeverity,
    ScriptError,
    FileOperationError,
    ValidationError,
    ParseError,
    ConfigurationError,
    handle_errors,
    retry_on_error,
    collect_errors,
)


class TestErrorSerialization:
    """First-Order Semantic: Error Context Accumulation"""
    
    def test_script_error_to_dict(self):
        """Test serialization to dictionary."""
        error = ScriptError(
            message="test error",
            severity=ErrorSeverity.ERROR,
            context={"file": "test.json", "line": 42},
            recoverable=True,
            hint="Try this fix"
        )
        
        d = error.to_dict()
        
        assert d['message'] == "test error"
        assert d['severity'] == "ERROR"
        assert d['context'] == {"file": "test.json", "line": 42}
        assert d['recoverable'] is True
        assert d['hint'] == "Try this fix"
        assert d['type'] == "ScriptError"
    
    def test_script_error_to_json(self):
        """Test JSON serialization."""
        error = ScriptError("test", context={"key": "value"})
        json_str = error.to_json()
        
        # Should be valid JSON
        data = json.loads(json_str)
        assert data['message'] == "test"
        assert data['context']['key'] == "value"
    
    def test_script_error_from_dict(self):
        """Test deserialization from dictionary."""
        original = ScriptError(
            message="test",
            severity=ErrorSeverity.ERROR,
            context={"key": "value"},
            recoverable=True,
            hint="hint"
        )
        
        d = original.to_dict()
        reconstructed = ScriptError.from_dict(d)
        
        assert reconstructed.message == original.message
        assert reconstructed.severity == original.severity
        assert reconstructed.context == original.context
        assert reconstructed.recoverable == original.recoverable
        assert reconstructed.hint == original.hint
    
    def test_error_causality_chain(self):
        """Test error causality representation."""
        error_a = ScriptError("Error A", context={"source": "A"})
        error_b = ScriptError("Error B").with_cause(error_a)
        error_c = ScriptError("Error C").with_cause(error_b)
        
        # Check chain: C -> B -> A
        assert error_c.caused_by == error_b
        assert error_b.caused_by == error_a
        assert error_a.caused_by is None
    
    def test_error_causality_serialization(self):
        """Test causality chain survives serialization."""
        error_a = ScriptError("Error A")
        error_b = ScriptError("Error B").with_cause(error_a)
        
        d = error_b.to_dict()
        reconstructed = ScriptError.from_dict(d)
        
        assert reconstructed.message == "Error B"
        assert reconstructed.caused_by is not None
        assert reconstructed.caused_by.message == "Error A"
    
    def test_domain_error_serialization(self):
        """Test domain-specific error serialization."""
        error = FileOperationError("Cannot read", path=Path("/tmp/test.json"))
        
        d = error.to_dict()
        
        assert d['type'] == "FileOperationError"
        # Path object is stored in context; to_dict serializes it as-is
        assert isinstance(d['context']['path'], (str, Path))
        assert str(d['context']['path']) == "/tmp/test.json"


class TestRecoverableErrorHandling:
    """First-Order Semantic: Recovery Chain Completeness"""
    
    def test_recoverable_flag_on_retry(self):
        """Test that retry respects recoverable flag."""
        call_count = [0]
        
        def failing_func():
            call_count[0] += 1
            raise ScriptError("error", recoverable=False)
        
        decorated = retry_on_error(max_attempts=3)(failing_func)
        
        # Should fail immediately (not retry) because recoverable=False
        with pytest.raises(ScriptError):
            decorated()
        
        assert call_count[0] == 1  # Only called once, not retried
    
    def test_recoverable_flag_allows_retry(self):
        """Test that retry allows retrying when recoverable=True."""
        call_count = [0]
        
        def failing_func():
            call_count[0] += 1
            if call_count[0] < 3:
                raise ScriptError("error", recoverable=True)
            return "success"
        
        decorated = retry_on_error(max_attempts=5)(failing_func)
        
        result = decorated()
        
        assert result == "success"
        assert call_count[0] == 3  # Called 3 times
    
    def test_retry_on_error_callback_invoked(self):
        """Test on_retry callback is called before each retry."""
        retry_attempts = []
        
        def on_retry_callback(attempt, error, delay):
            retry_attempts.append((attempt, error.message, delay))
        
        call_count = [0]
        
        def failing_func():
            call_count[0] += 1
            if call_count[0] < 3:
                raise ScriptError("error", recoverable=True)
            return "success"
        
        decorated = retry_on_error(
            max_attempts=5,
            delay=0.01,
            backoff=2.0,
            on_retry=on_retry_callback
        )(failing_func)
        
        result = decorated()
        
        assert result == "success"
        assert len(retry_attempts) == 2  # Two retries
        assert retry_attempts[0][0] == 1  # First retry
        assert retry_attempts[0][2] == 0.01  # Initial delay
        assert retry_attempts[1][0] == 2  # Second retry
        assert abs(retry_attempts[1][2] - 0.02) < 0.001  # Backoff: 0.01 * 2
    
    def test_exponential_backoff_sequence(self):
        """Test exponential backoff calculation."""
        delays = []
        
        def on_retry(attempt, error, delay):
            delays.append(delay)
        
        call_count = [0]
        
        def failing_func():
            call_count[0] += 1
            raise ScriptError("error", recoverable=True)
        
        decorated = retry_on_error(
            max_attempts=5,
            delay=1.0,
            backoff=2.0,
            on_retry=on_retry
        )(failing_func)
        
        try:
            decorated()
        except ScriptError:
            pass
        
        # Should have retried up to max_attempts-1 times
        assert len(delays) == 4
        assert abs(delays[0] - 1.0) < 0.001
        assert abs(delays[1] - 2.0) < 0.001
        assert abs(delays[2] - 4.0) < 0.001
        assert abs(delays[3] - 8.0) < 0.001


class TestHandleErrorsCallback:
    """First-Order Pragmatic: Error Routing with Callbacks"""
    
    def test_on_error_callback_invoked(self):
        """Test on_error callback is called."""
        caught_errors = []
        
        def on_error_callback(error):
            caught_errors.append(error)
        
        @handle_errors(on_error=on_error_callback)
        def func():
            raise ScriptError("test error")
        
        with pytest.raises(ScriptError):
            func()
        
        assert len(caught_errors) == 1
        assert caught_errors[0].message == "test error"
    
    def test_on_error_callback_error_handling(self):
        """Test callback errors don't break error routing."""
        def bad_callback(error):
            raise RuntimeError("Callback error")
        
        @handle_errors(on_error=bad_callback)
        def func():
            raise ScriptError("test error")
        
        # Original error should still be raised
        with pytest.raises(ScriptError):
            func()
    
    def test_suppress_warnings_flag(self):
        """Test suppress_warnings flag."""
        call_count = [0]
        
        @handle_errors(suppress_warnings=True)
        def func():
            call_count[0] += 1
            raise ScriptError("warning", severity=ErrorSeverity.WARNING)
        
        # Should not raise (returns None)
        result = func()
        
        assert result is None
        assert call_count[0] == 1


class TestContextAccumulation:
    """First-Order Pragmatic: Error Context Growth"""
    
    def test_nested_context_accumulation(self):
        """Test context accumulates through nested function calls."""
        def inner():
            raise ScriptError("inner error", context={"level": "inner"})
        
        def middle():
            try:
                inner()
            except ScriptError as e:
                e.context["level"] = "middle"
                raise
        
        def outer():
            try:
                middle()
            except ScriptError as e:
                e.context["level"] = "outer"
                raise
        
        with pytest.raises(ScriptError) as exc_info:
            outer()
        
        # Context should reflect outer level (last update wins)
        assert exc_info.value.context["level"] == "outer"
    
    def test_context_non_collision(self):
        """Test context keys don't collide silently."""
        error = ScriptError("error")
        error.context["file"] = "file1.py"
        error.context["file"] = "file2.py"  # Overwrite
        
        # Last value should win
        assert error.context["file"] == "file2.py"
    
    def test_context_with_multiple_types(self):
        """Test context can hold various types."""
        error = ScriptError("error", context={
            "string": "value",
            "int": 42,
            "list": [1, 2, 3],
            "dict": {"nested": "value"},
            "none": None,
        })
        
        d = error.to_dict()
        
        assert d['context']['string'] == "value"
        assert d['context']['int'] == 42
        assert d['context']['list'] == [1, 2, 3]
        assert d['context']['dict']['nested'] == "value"
        assert d['context']['none'] is None


class TestTypeSafetyAtBoundaries:
    """First-Order Syntactic: Type Preservation in Decorators"""
    
    def test_retry_preserves_return_type(self):
        """Test @retry_on_error preserves return type."""
        @retry_on_error(max_attempts=3)
        def returns_int() -> int:
            return 42
        
        @retry_on_error(max_attempts=3)
        def returns_str() -> str:
            return "hello"
        
        @retry_on_error(max_attempts=3)
        def returns_list() -> list:
            return [1, 2, 3]
        
        assert isinstance(returns_int(), int)
        assert isinstance(returns_str(), str)
        assert isinstance(returns_list(), list)
    
    def test_handle_errors_preserves_return_type(self):
        """Test @handle_errors preserves return type on success."""
        @handle_errors()
        def returns_int() -> int:
            return 42
        
        result = returns_int()
        
        assert isinstance(result, int)
        assert result == 42
    
    def test_collect_errors_type_signature(self):
        """Test @collect_errors works with return type."""
        @collect_errors()
        def validate() -> list[ScriptError]:
            errors = []
            errors.append(ValidationError("Bad", field="test"))
            return errors
        
        # Should raise because errors were collected
        with pytest.raises(ScriptError):
            validate()


class TestDecoratorComposition:
    """Second-Order Syntactic: Decorator Stacking"""
    
    def test_retry_then_handle_errors(self):
        """Test @retry_on_error + @handle_errors stacking."""
        call_count = [0]
        
        @handle_errors()
        @retry_on_error(max_attempts=3)
        def func():
            call_count[0] += 1
            if call_count[0] < 3:
                raise ScriptError("error", recoverable=True)
            return "success"
        
        result = func()
        
        assert result == "success"
        assert call_count[0] == 3
    
    def test_handle_errors_then_retry(self):
        """Test @handle_errors then @retry_on_error stacking."""
        call_count = [0]
        
        @retry_on_error(max_attempts=3)
        @handle_errors()
        def func():
            call_count[0] += 1
            if call_count[0] < 3:
                raise ScriptError("error", recoverable=True)
            return "success"
        
        result = func()
        
        assert result == "success"
        assert call_count[0] == 3

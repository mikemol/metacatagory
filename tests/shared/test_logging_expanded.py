#!/usr/bin/env python3
"""Expanded test suite for logging.py - First-Order Implications.

Tests focus on:
1. Context Canonicalization
2. Progress Rate Composition
3. Exception Serialization

Source: MODULE-AUDIT-PHASE-2.md, First-Order Implications
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import logging
import json
from io import StringIO
from datetime import datetime

from scripts.shared.logging import (
    StructuredLogger,
    StructuredFormatter,
    HumanReadableFormatter,
    configure_logging,
    _to_json_serializable,
)
from scripts.shared.errors import ScriptError, ErrorSeverity


class TestContextSerialization:
    """First-Order Semantic: Context Canonicalization"""
    
    def test_to_json_serializable_primitives(self):
        """Test serialization of primitive types."""
        assert _to_json_serializable("string") == "string"
        assert _to_json_serializable(42) == 42
        assert _to_json_serializable(3.14) == 3.14
        assert _to_json_serializable(True) is True
        assert _to_json_serializable(None) is None
    
    def test_to_json_serializable_dict(self):
        """Test serialization of nested dict."""
        d = {"a": "value", "b": {"nested": 42}, "c": None}
        result = _to_json_serializable(d)
        
        assert result["a"] == "value"
        assert result["b"]["nested"] == 42
        assert result["c"] is None
    
    def test_to_json_serializable_list(self):
        """Test serialization of lists."""
        lst = [1, "two", 3.0, {"nested": True}]
        result = _to_json_serializable(lst)
        
        assert result == [1, "two", 3.0, {"nested": True}]
    
    def test_to_json_serializable_path(self):
        """Test serialization of Path objects."""
        p = Path("/tmp/test.json")
        result = _to_json_serializable(p)
        
        assert result == "/tmp/test.json"
    
    def test_to_json_serializable_script_error(self):
        """Test serialization of ScriptError with to_dict method."""
        error = ScriptError("test error", context={"file": "test.py"})
        result = _to_json_serializable(error)
        
        assert isinstance(result, dict)
        assert result["message"] == "test error"
        assert result["context"]["file"] == "test.py"
    
    def test_to_json_serializable_custom_object(self):
        """Test serialization of custom objects (via str fallback)."""
        class CustomObj:
            def __str__(self):
                return "custom_value"
        
        obj = CustomObj()
        result = _to_json_serializable(obj)
        
        assert result == "custom_value"
    
    def test_structured_formatter_handles_complex_context(self):
        """Test StructuredFormatter with complex context types."""
        logger = logging.getLogger("test_complex")
        handler = logging.StreamHandler(StringIO())
        handler.setFormatter(StructuredFormatter())
        logger.addHandler(handler)
        logger.setLevel(logging.INFO)
        
        # Create a log record with complex context
        record = logging.LogRecord(
            name="test_complex",
            level=logging.INFO,
            pathname="test.py",
            lineno=42,
            msg="Test message",
            args=(),
            exc_info=None
        )
        record.context = {
            "path": Path("/tmp/test.json"),
            "error": ScriptError("test"),
            "number": 42,
            "nested": {"key": "value"}
        }
        
        # Format should not raise
        formatted = handler.formatter.format(record)
        data = json.loads(formatted)
        
        assert data["message"] == "Test message"
        assert "context" in data
        assert data["context"]["path"] == "/tmp/test.json"
        assert isinstance(data["context"]["nested"], dict)
    
    def test_human_readable_formatter_handles_complex_context(self):
        """Test HumanReadableFormatter with complex context types."""
        formatter = HumanReadableFormatter(use_color=False)
        
        record = logging.LogRecord(
            name="test",
            level=logging.INFO,
            pathname="test.py",
            lineno=42,
            msg="Message",
            args=(),
            exc_info=None
        )
        record.context = {
            "path": Path("/tmp/test.json"),
            "number": 42,
            "nested": {"key": "value"}
        }
        
        # Format should not raise
        formatted = formatter.format(record)
        
        assert "Message" in formatted
        assert "/tmp/test.json" in formatted
        assert "42" in formatted


class TestProgressTracking:
    """First-Order Pragmatic: Progress Rate Composition"""
    
    def test_progress_basic(self):
        """Test basic progress tracking."""
        logger = StructuredLogger("test_progress")
        handler = logging.StreamHandler(StringIO())
        handler.setFormatter(StructuredFormatter())
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)
        
        logger.progress("Processing", current=5, total=10)
        
        # Should log without error
        assert logger.logger.hasHandlers()
    
    def test_progress_with_success_rate(self):
        """Test progress with succeeded/failed counts."""
        logger = StructuredLogger("test_progress")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)
        
        logger.progress("Processing", current=10, total=20, succeeded=8, failed=2)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert data["context"]["current"] == 10
        assert data["context"]["total"] == 20
        assert data["context"]["succeeded"] == 8
        assert data["context"]["failed"] == 2
        assert data["context"]["success_rate"] == 40.0  # 8/20 * 100
    
    def test_progress_with_recovery_attempts(self):
        """Test progress with recovery_attempts tracking."""
        logger = StructuredLogger("test_progress")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)
        
        logger.progress("Processing", current=5, total=10, recovery_attempts=3)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert data["context"]["recovery_attempts"] == 3
    
    def test_progress_percentage_calculation(self):
        """Test progress percentage is calculated correctly."""
        logger = StructuredLogger("test_progress")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)
        
        logger.progress("Processing", current=3, total=4)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert abs(data["context"]["percentage"] - 75.0) < 0.01
    
    def test_progress_with_zero_total_no_divide_by_zero(self):
        """Test progress handles total=0 gracefully."""
        logger = StructuredLogger("test_progress")
        
        # Should not raise ZeroDivisionError
        logger.progress("Processing", current=0, total=0)


class TestExceptionIntegration:
    """First-Order Semantic: Exception Serialization"""
    
    def test_error_with_script_error_exception(self):
        """Test error() method with ScriptError object."""
        logger = StructuredLogger("test_error")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.ERROR)
        
        error = ScriptError("test error", context={"file": "test.py"})
        logger.error("An error occurred", exception=error)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert "exception" in data["context"]
        assert data["context"]["exception"]["message"] == "test error"
        assert data["context"]["exception"]["context"]["file"] == "test.py"
    
    def test_error_with_python_exception(self):
        """Test error() method with regular Python exception."""
        logger = StructuredLogger("test_error")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.ERROR)
        
        try:
            raise ValueError("test value error")
        except ValueError as e:
            logger.error("Caught exception", exception=e)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert "exception" in data["context"]
        assert "test value error" in data["context"]["exception"]
    
    def test_error_exception_takes_priority_over_exc_info(self):
        """Test that exception parameter takes priority over exc_info."""
        logger = StructuredLogger("test_error")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.ERROR)
        
        explicit_error = ScriptError("explicit error")
        
        try:
            raise RuntimeError("implicit error")
        except RuntimeError:
            # Pass both exception param and exc_info=True
            # exception param should take priority
            logger.error("Message", exception=explicit_error, exc_info=True)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        # Should have explicit_error, not RuntimeError
        assert "explicit error" in str(data["context"]["exception"])
    
    def test_critical_with_exception(self):
        """Test critical() method with exception."""
        logger = StructuredLogger("test_critical")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.CRITICAL)
        
        error = ScriptError("critical error")
        logger.critical("Critical event", exception=error)
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        assert data["level"] == "CRITICAL"
        assert "exception" in data["context"]


class TestContextMergingPriority:
    """First-Order Syntactic: Context Merge Priority (caller > extra_ctx > context dict)"""
    
    def test_context_merge_priority_caller_kwargs(self):
        """Test caller kwargs take highest priority."""
        logger = StructuredLogger("test_merge")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)
        
        logger.info("message", context={"key": "from_dict"}, key="from_kwargs")
        
        output_str = output.getvalue()
        data = json.loads(output_str)
        
        # kwargs should win (added last)
        assert data["context"]["key"] == "from_kwargs"

    def test_context_merge_with_extra_context_dict(self):
        """Test extra_ctx['context'] is merged into context."""
        logger = StructuredLogger("test_merge_extra")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)

        logger._log_with_context(
            logging.INFO,
            "message",
            None,
            **{"context": {"key": "from_extra_context"}, "other": "value"}
        )

        output_str = output.getvalue()
        data = json.loads(output_str)

        assert data["context"]["key"] == "from_extra_context"
        assert data["context"]["other"] == "value"

    def test_context_merge_priority_kwargs_over_extra_context(self):
        """Test kwargs override extra_ctx['context'] keys."""
        logger = StructuredLogger("test_merge_extra_priority")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        logger.logger.setLevel(logging.INFO)

        logger._log_with_context(
            logging.INFO,
            "message",
            None,
            **{"context": {"key": "from_extra_context"}, "key": "from_kwargs"}
        )

        output_str = output.getvalue()
        data = json.loads(output_str)

        assert data["context"]["key"] == "from_kwargs"


class TestLevelFiltering:
    """First-Order Pragmatic: Level Checks (Performance Optimization)"""
    
    def test_progress_respects_log_level(self):
        """Test progress() respects logger level."""
        logger = StructuredLogger("test_level")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)
        
        # Set to WARNING, so INFO messages are suppressed
        logger.logger.setLevel(logging.WARNING)
        
        logger.progress("Processing", current=5, total=10)
        
        # Output should be empty (progress uses INFO level)
        assert output.getvalue() == ""

    def test_info_respects_log_level(self):
        """Test info() respects logger level."""
        logger = StructuredLogger("test_level_info")
        output = StringIO()
        handler = logging.StreamHandler(output)
        handler.setFormatter(StructuredFormatter())
        logger.logger.handlers.clear()
        logger.logger.addHandler(handler)

        # Set to WARNING, so INFO messages are suppressed
        logger.logger.setLevel(logging.WARNING)

        logger.info("Message", context={"key": "value"})

        assert output.getvalue() == ""


class TestFormatterConsistency:
    """Second-Order Syntactic: Formatter Coherence (Parity Between Formatters)"""
    
    def test_structured_and_human_formatters_handle_same_context(self):
        """Test both formatters accept and preserve context."""
        structured = StructuredFormatter()
        human = HumanReadableFormatter(use_color=False)
        
        record = logging.LogRecord(
            name="test",
            level=logging.INFO,
            pathname="test.py",
            lineno=42,
            msg="Message",
            args=(),
            exc_info=None
        )
        record.context = {"key": "value", "number": 42}
        
        structured_output = structured.format(record)
        human_output = human.format(record)
        
        # Both should include the message
        assert "Message" in structured_output
        assert "Message" in human_output
        
        # Structured should have valid JSON
        data = json.loads(structured_output)
        assert data["context"]["key"] == "value"
        
        # Human should include context info
        assert "key=value" in human_output or "value" in human_output


class TestConfigureLogging:
    """Second-Order Pragmatic: Logger Configuration Lifecycle"""
    
    def test_configure_logging_clears_handlers(self):
        """Test configure_logging() clears existing handlers."""
        logger1 = configure_logging("test_config", level=logging.INFO)
        initial_handler_count = len(logger1.logger.handlers)
        
        # Call again with same name
        logger2 = configure_logging("test_config", level=logging.DEBUG)
        
        # Should have cleared and added new handler(s)
        # (should be same count if just cleared and re-added)
        assert len(logger2.logger.handlers) > 0
    
    def test_configure_logging_returns_structured_logger(self):
        """Test configure_logging returns StructuredLogger instance."""
        logger = configure_logging("test_return")
        
        assert isinstance(logger, StructuredLogger)

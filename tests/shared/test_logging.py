"""Tests for shared.logging module."""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import json
import logging
import io
import sys as _sys
from scripts.shared.logging import (
    StructuredFormatter,
    HumanReadableFormatter,
    StructuredLogger,
    configure_logging,
    log_info,
    log_warning,
    log_error,
)


class TestStructuredFormatter:
    """Test StructuredFormatter for JSON output."""
    
    def test_format_simple_message(self):
        """Test formatting simple log message."""
        formatter = StructuredFormatter()
        record = logging.LogRecord(
            name="test",
            level=logging.INFO,
            pathname="test.py",
            lineno=10,
            msg="Test message",
            args=(),
            exc_info=None
        )
        
        output = formatter.format(record)
        data = json.loads(output)
        
        assert data["message"] == "Test message"
        assert data["level"] == "INFO"
        assert data["name"] == "test"
        assert "timestamp" in data
    
    def test_format_with_context(self):
        """Test formatting message with structured context."""
        formatter = StructuredFormatter()
        record = logging.LogRecord(
            name="test",
            level=logging.WARNING,
            pathname="test.py",
            lineno=20,
            msg="Warning message",
            args=(),
            exc_info=None
        )
        record.context = {"file": "test.json", "line": 42}
        
        output = formatter.format(record)
        data = json.loads(output)
        
        assert data["message"] == "Warning message"
        assert data["context"]["file"] == "test.json"
        assert data["context"]["line"] == 42
    
    def test_format_with_exception(self):
        """Test formatting message with exception info."""
        formatter = StructuredFormatter()
        
        try:
            raise ValueError("Test exception")
        except ValueError:
            exc_info = sys.exc_info()
        
        record = logging.LogRecord(
            name="test",
            level=logging.ERROR,
            pathname="test.py",
            lineno=30,
            msg="Error occurred",
            args=(),
            exc_info=exc_info
        )
        
        output = formatter.format(record)
        data = json.loads(output)
        
        assert data["message"] == "Error occurred"
        assert "exception" in data
        assert "ValueError: Test exception" in data["exception"]


class TestHumanReadableFormatter:
    """Test HumanReadableFormatter for console output."""
    
    def test_format_without_color(self):
        """Test formatting without ANSI colors."""
        formatter = HumanReadableFormatter(use_color=False)
        record = logging.LogRecord(
            name="test",
            level=logging.INFO,
            pathname="test.py",
            lineno=10,
            msg="Test message",
            args=(),
            exc_info=None
        )
        
        output = formatter.format(record)
        assert "INFO" in output
        assert "Test message" in output
        assert "\033[" not in output  # No ANSI codes
    
    def test_format_with_color(self):
        """Test formatting with ANSI colors."""
        formatter = HumanReadableFormatter(use_color=True)
        record = logging.LogRecord(
            name="test",
            level=logging.WARNING,
            pathname="test.py",
            lineno=20,
            msg="Warning message",
            args=(),
            exc_info=None
        )
        
        output = formatter.format(record)
        assert "WARNING" in output
        assert "Warning message" in output
        assert "\033[" in output  # Has ANSI codes
    
    def test_format_with_context(self):
        """Test formatting with structured context."""
        formatter = HumanReadableFormatter(use_color=False)
        record = logging.LogRecord(
            name="test",
            level=logging.ERROR,
            pathname="test.py",
            lineno=30,
            msg="Error message",
            args=(),
            exc_info=None
        )
        record.context = {"file": "data.json", "reason": "invalid"}
        
        output = formatter.format(record)
        assert "Error message" in output
        assert "file=data.json" in output
        assert "reason=invalid" in output
    
    def test_color_by_level(self):
        """Test different colors for different log levels."""
        formatter = HumanReadableFormatter(use_color=True)
        
        levels_and_colors = [
            (logging.DEBUG, "36"),    # Cyan
            (logging.INFO, "32"),     # Green
            (logging.WARNING, "33"),  # Yellow
            (logging.ERROR, "31"),    # Red
            (logging.CRITICAL, "35"), # Magenta
        ]
        
        for level, color_code in levels_and_colors:
            record = logging.LogRecord(
                name="test",
                level=level,
                pathname="test.py",
                lineno=10,
                msg="Message",
                args=(),
                exc_info=None
            )
            output = formatter.format(record)
            assert f"\033[{color_code}m" in output


class TestStructuredLogger:
    """Test StructuredLogger class."""
    
    def setup_method(self):
        """Setup test logger with string stream."""
        self.stream = io.StringIO()
        self.handler = logging.StreamHandler(self.stream)
        self.handler.setFormatter(StructuredFormatter())
        
        self.logger = StructuredLogger("test_logger")
        self.logger.logger.handlers = [self.handler]
        self.logger.logger.setLevel(logging.DEBUG)
    
    def get_last_log(self):
        """Get the last log entry as parsed JSON."""
        self.handler.flush()
        output = self.stream.getvalue()
        lines = [line for line in output.strip().split('\n') if line]
        if not lines:
            return None
        return json.loads(lines[-1])
    
    def test_debug_logging(self):
        """Test debug level logging."""
        self.logger.debug("Debug message", context={"key": "value"})
        log = self.get_last_log()
        
        assert log["level"] == "DEBUG"
        assert log["message"] == "Debug message"
        assert log["context"]["key"] == "value"
    
    def test_info_logging(self):
        """Test info level logging."""
        self.logger.info("Info message")
        log = self.get_last_log()
        
        assert log["level"] == "INFO"
        assert log["message"] == "Info message"
    
    def test_warning_logging(self):
        """Test warning level logging."""
        self.logger.warning("Warning message", context={"issue": "minor"})
        log = self.get_last_log()
        
        assert log["level"] == "WARNING"
        assert log["message"] == "Warning message"
        assert log["context"]["issue"] == "minor"
    
    def test_error_logging(self):
        """Test error level logging."""
        self.logger.error("Error message", context={"error": "major"})
        log = self.get_last_log()
        
        assert log["level"] == "ERROR"
        assert log["message"] == "Error message"
        assert log["context"]["error"] == "major"
    
    def test_critical_logging(self):
        """Test critical level logging."""
        self.logger.critical("Critical message")
        log = self.get_last_log()
        
        assert log["level"] == "CRITICAL"
        assert log["message"] == "Critical message"
    
    def test_progress_logging(self):
        """Test progress logging."""
        self.logger.progress("Processing", current=5, total=10, context={"file": "test.json"})
        log = self.get_last_log()
        
        assert "Processing" in log["message"]
        assert "5/10" in log["message"] or "50%" in log["message"]
        assert log["context"]["file"] == "test.json"
    
    def test_logging_without_context(self):
        """Test logging without context dict."""
        self.logger.info("Simple message")
        log = self.get_last_log()
        
        assert log["message"] == "Simple message"
        assert "context" not in log or log["context"] == {}


class TestConfigureLogging:
    """Test configure_logging function."""
    
    def test_configure_with_default_level(self):
        """Test configuration with default log level."""
        logger = configure_logging("test_default")
        assert logger.logger.level == logging.INFO
    
    def test_configure_with_custom_level(self):
        """Test configuration with custom log level."""
        logger = configure_logging("test_custom", level=logging.DEBUG)
        assert logger.logger.level == logging.DEBUG
    
    def test_configure_structured_output(self):
        """Test configuration for structured JSON output."""
        logger = configure_logging("test_structured", structured=True)
        
        # Check that handler uses StructuredFormatter
        handlers = logger.logger.handlers
        assert len(handlers) > 0
        assert isinstance(handlers[0].formatter, StructuredFormatter)
    
    def test_configure_human_readable_output(self):
        """Test configuration for human-readable output."""
        logger = configure_logging("test_human", structured=False)
        
        # Check that handler uses HumanReadableFormatter
        handlers = logger.logger.handlers
        assert len(handlers) > 0
        assert isinstance(handlers[0].formatter, HumanReadableFormatter)
    
    def test_configure_file_output(self):
        """Test configuration with file output."""
        import tempfile
        import os
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.log') as f:
            log_file = f.name
        
        try:
            logger = configure_logging("test_file", log_file=log_file)
            logger.info("Test message")
            
            # Verify file was created and contains log
            assert os.path.exists(log_file)
            with open(log_file, 'r') as f:
                content = f.read()
                assert "Test message" in content
        finally:
            if os.path.exists(log_file):
                os.unlink(log_file)


class TestConvenienceFunctions:
    """Test convenience logging functions."""
    
    def setup_method(self):
        """Setup test environment."""
        # Reset logging configuration
        logging.root.handlers = []
    
    def test_log_info(self):
        """Test log_info convenience function."""
        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(StructuredFormatter())
        logging.root.addHandler(handler)
        logging.root.setLevel(logging.INFO)
        
        log_info("Info message", context={"key": "value"})
        
        output = stream.getvalue()
        assert "Info message" in output
    
    def test_log_warning(self):
        """Test log_warning convenience function."""
        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(StructuredFormatter())
        logging.root.addHandler(handler)
        logging.root.setLevel(logging.WARNING)
        
        log_warning("Warning message", context={"issue": "test"})
        
        output = stream.getvalue()
        assert "Warning message" in output
    
    def test_log_error(self):
        """Test log_error convenience function."""
        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(StructuredFormatter())
        logging.root.addHandler(handler)
        logging.root.setLevel(logging.ERROR)
        
        log_error("Error message", context={"error": "test"})
        
        output = stream.getvalue()
        assert "Error message" in output


class TestLoggingIntegration:
    """Test logging integration patterns."""
    
    def test_multiple_loggers(self):
        """Test multiple independent loggers."""
        logger1 = configure_logging("logger1")
        logger2 = configure_logging("logger2")
        
        assert logger1.logger.name == "logger1"
        assert logger2.logger.name == "logger2"
        assert logger1 != logger2
    
    def test_context_accumulation(self):
        """Test that context can be built up over multiple log calls."""
        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(StructuredFormatter())
        
        logger = StructuredLogger("test_context")
        logger.logger.handlers = [handler]
        logger.logger.setLevel(logging.INFO)
        
        logger.info("First", context={"step": 1})
        logger.info("Second", context={"step": 2, "detail": "more"})
        
        output = stream.getvalue()
        lines = output.strip().split('\n')
        
        log1 = json.loads(lines[0])
        log2 = json.loads(lines[1])
        
        assert log1["context"]["step"] == 1
        assert log2["context"]["step"] == 2
        assert log2["context"]["detail"] == "more"

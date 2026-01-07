"""Tests for shared.errors module."""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import time
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


class TestErrorSeverity:
    """Test ErrorSeverity enum."""
    
    def test_severity_levels(self):
        """Test severity level ordering."""
        assert ErrorSeverity.WARNING.value < ErrorSeverity.ERROR.value
        assert ErrorSeverity.ERROR.value < ErrorSeverity.FATAL.value
    
    def test_severity_string_representation(self):
        """Test string representation of severity levels."""
        assert str(ErrorSeverity.WARNING) == "ErrorSeverity.WARNING"
        assert ErrorSeverity.WARNING.name == "WARNING"


class TestScriptError:
    """Test ScriptError base class."""
    
    def test_basic_construction(self):
        """Test basic error construction."""
        error = ScriptError("test error")
        assert str(error) == "test error"
        assert error.severity == ErrorSeverity.ERROR
        assert error.context == {}
        assert error.hint is None
    
    def test_construction_with_severity(self):
        """Test construction with custom severity."""
        error = ScriptError("warning", severity=ErrorSeverity.WARNING)
        assert error.severity == ErrorSeverity.WARNING
    
    def test_construction_with_context(self):
        """Test construction with context."""
        context = {"file": "test.json", "line": 42}
        error = ScriptError("error", context=context)
        assert error.context == context
    
    def test_with_hint(self):
        """Test adding hint to error."""
        error = ScriptError("error").with_hint("Try this fix")
        assert error.hint == "Try this fix"
        assert str(error) == "error"
    
    def test_repr(self):
        """Test repr for debugging."""
        error = ScriptError("test", context={"key": "value"})
        repr_str = repr(error)
        assert "ScriptError" in repr_str
        assert "test" in repr_str


class TestDomainSpecificErrors:
    """Test domain-specific error classes."""
    
    def test_file_operation_error(self):
        """Test FileOperationError."""
        error = FileOperationError("Cannot read file", path=Path("/tmp/test.json"))
        assert error.severity == ErrorSeverity.ERROR
        assert error.context["path"] == Path("/tmp/test.json")
    
    def test_validation_error(self):
        """Test ValidationError."""
        error = ValidationError("Invalid schema", field="title", value="")
        assert error.context["field"] == "title"
        assert error.context["value"] == ""
    
    def test_parse_error(self):
        """Test ParseError."""
        error = ParseError("Syntax error", source="test.agda", line=10)
        assert error.context["source"] == "test.agda"
        assert error.context["line"] == 10
    
    def test_configuration_error(self):
        """Test ConfigurationError."""
        error = ConfigurationError("Missing config", setting="api_key")
        assert error.severity == ErrorSeverity.FATAL
        assert error.context["setting"] == "api_key"


class TestHandleErrorsDecorator:
    """Test handle_errors decorator."""
    
    def test_successful_execution(self):
        """Test decorator on successful function."""
        @handle_errors
        def successful_function(x: int) -> int:
            return x * 2
        
        result = successful_function(5)
        assert result == 10
    
    def test_warning_logged_not_raised(self):
        """Test that warnings are logged but not raised."""
        @handle_errors
        def warning_function():
            raise ScriptError("warning", severity=ErrorSeverity.WARNING)
        
        # Should not raise, just log
        result = warning_function()
        assert result is None
    
    def test_error_raised(self):
        """Test that errors are raised."""
        @handle_errors
        def error_function():
            raise ScriptError("error", severity=ErrorSeverity.ERROR)
        
        with pytest.raises(ScriptError) as exc_info:
            error_function()
        
        assert "error" in str(exc_info.value)
    
    def test_fatal_error_raised(self):
        """Test that fatal errors are raised."""
        @handle_errors
        def fatal_function():
            raise ConfigurationError("fatal error", setting="required")
        
        with pytest.raises(ConfigurationError) as exc_info:
            fatal_function()
        
        assert exc_info.value.severity == ErrorSeverity.FATAL
    
    def test_non_script_error_raised(self):
        """Test that non-ScriptError exceptions are re-raised."""
        @handle_errors
        def generic_error_function():
            raise ValueError("standard error")
        
        with pytest.raises(ValueError) as exc_info:
            generic_error_function()
        
        assert "standard error" in str(exc_info.value)
    
    def test_reraise_all_mode(self):
        """Test reraise_all parameter."""
        @handle_errors(reraise_all=True)
        def warning_function():
            raise ScriptError("warning", severity=ErrorSeverity.WARNING)
        
        with pytest.raises(ScriptError):
            warning_function()


class TestRetryOnErrorDecorator:
    """Test retry_on_error decorator."""
    
    def test_successful_first_attempt(self):
        """Test function succeeding on first attempt."""
        call_count = 0
        
        @retry_on_error(max_attempts=3)
        def successful_function():
            nonlocal call_count
            call_count += 1
            return "success"
        
        result = successful_function()
        assert result == "success"
        assert call_count == 1
    
    def test_retry_on_failure(self):
        """Test retry behavior on transient failures."""
        call_count = 0
        
        @retry_on_error(max_attempts=3, delay=0.01)
        def flaky_function():
            nonlocal call_count
            call_count += 1
            if call_count < 3:
                raise FileOperationError("Transient error", path=Path("/tmp/test"))
            return "success"
        
        result = flaky_function()
        assert result == "success"
        assert call_count == 3
    
    def test_retry_exhausted(self):
        """Test behavior when retries are exhausted."""
        @retry_on_error(max_attempts=3, delay=0.01)
        def always_fails():
            raise FileOperationError("Persistent error", path=Path("/tmp/test"))
        
        with pytest.raises(FileOperationError) as exc_info:
            always_fails()
        
        assert "Persistent error" in str(exc_info.value)
    
    def test_exponential_backoff(self):
        """Test exponential backoff timing."""
        call_times = []
        
        @retry_on_error(max_attempts=3, delay=0.1, backoff=2.0)
        def timed_function():
            call_times.append(time.time())
            if len(call_times) < 3:
                raise FileOperationError("Retry", path=Path("/tmp/test"))
            return "success"
        
        result = timed_function()
        assert result == "success"
        
        # Check delays: ~0.1s, ~0.2s (exponential backoff)
        if len(call_times) >= 3:
            delay1 = call_times[1] - call_times[0]
            delay2 = call_times[2] - call_times[1]
            assert delay1 >= 0.09  # Allow small timing variance
            assert delay2 >= 0.18
    
    def test_specific_exception_types(self):
        """Test retry only on specific exception types."""
        call_count = 0
        
        @retry_on_error(max_attempts=3, exceptions=(FileOperationError,), delay=0.01)
        def specific_error_function():
            nonlocal call_count
            call_count += 1
            if call_count == 1:
                raise FileOperationError("Retry this", path=Path("/tmp/test"))
            elif call_count == 2:
                # This should not be retried
                raise ValidationError("Don't retry this", field="test")
            return "success"
        
        with pytest.raises(ValidationError):
            specific_error_function()
        
        # Should have called twice (initial + 1 retry on FileOperationError)
        assert call_count == 2


class TestCollectErrorsDecorator:
    """Test collect_errors decorator."""
    
    def test_collect_multiple_errors(self):
        """Test collecting multiple errors."""
        @collect_errors
        def multi_error_function():
            errors = []
            errors.append(ValidationError("Error 1", field="field1"))
            errors.append(ValidationError("Error 2", field="field2"))
            return errors
        
        with pytest.raises(ScriptError) as exc_info:
            multi_error_function()
        
        error = exc_info.value
        assert "2 errors" in str(error)
        assert "collected_errors" in error.context
        assert len(error.context["collected_errors"]) == 2
    
    def test_collect_single_error(self):
        """Test collecting single error."""
        @collect_errors
        def single_error_function():
            return [ValidationError("Single error", field="test")]
        
        with pytest.raises(ValidationError):
            single_error_function()
    
    def test_no_errors(self):
        """Test function with no errors."""
        @collect_errors
        def no_error_function():
            return []
        
        result = no_error_function()
        assert result == []
    
    def test_custom_threshold(self):
        """Test custom error threshold."""
        @collect_errors(max_errors=5)
        def threshold_function():
            return [ValidationError(f"Error {i}", field=f"field{i}") for i in range(3)]
        
        with pytest.raises(ScriptError) as exc_info:
            threshold_function()
        
        assert "3 errors" in str(exc_info.value)
    
    def test_threshold_exceeded(self):
        """Test behavior when threshold is exceeded."""
        @collect_errors(max_errors=2)
        def too_many_errors():
            return [ValidationError(f"Error {i}", field=f"field{i}") for i in range(5)]
        
        with pytest.raises(ScriptError) as exc_info:
            too_many_errors()
        
        error = exc_info.value
        assert "5 errors" in str(error) or "Too many errors" in str(error)


class TestErrorIntegration:
    """Test error handling integration patterns."""
    
    def test_chained_decorators(self):
        """Test multiple decorators on same function."""
        call_count = 0
        
        @handle_errors
        @retry_on_error(max_attempts=2, delay=0.01)
        def chained_function():
            nonlocal call_count
            call_count += 1
            if call_count < 2:
                raise FileOperationError("Retry", path=Path("/tmp/test"))
            return "success"
        
        result = chained_function()
        assert result == "success"
        assert call_count == 2
    
    def test_error_context_preservation(self):
        """Test that error context is preserved through decorators."""
        @handle_errors
        def context_function():
            raise ValidationError(
                "Invalid data",
                field="title",
                value="",
                context={"extra": "info"}
            )
        
        with pytest.raises(ValidationError) as exc_info:
            context_function()
        
        error = exc_info.value
        assert error.context["field"] == "title"
        assert error.context["extra"] == "info"
    
    def test_hint_in_error_message(self):
        """Test that hints are included in error representation."""
        error = ScriptError("Something went wrong").with_hint(
            "Check your configuration file"
        )
        
        assert error.hint == "Check your configuration file"
        # Hint should be accessible but not in basic str() representation
        assert str(error) == "Something went wrong"

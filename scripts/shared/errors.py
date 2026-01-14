#!/usr/bin/env python3
"""Standardized error handling for metacatagory scripts.

Source: Third-order implications 3.2.1-3.2.3 from behavioral taxonomy
Purpose: Unified error hierarchy with structured context and recovery strategies

Design Principles:
- Errors carry structured context for diagnostics
- Severity levels enable graduated responses
- Recovery hints guide corrective action
- Exception hierarchy enables selective catching
"""

from typing import Any, Callable, TypeVar
from dataclasses import dataclass, field
from enum import Enum
from functools import wraps
import time
import sys

T = TypeVar('T')


class ErrorSeverity(Enum):
    """Error severity levels.
    
    Source: Implication 3.2.1 (error classification)
    """
    WARNING = 1  # Recoverable, log and continue
    ERROR = 2  # Serious but recoverable, retry or skip
    FATAL = 3  # Unrecoverable, must terminate


@dataclass
class ScriptError(Exception):
    """Base class for all script errors.
    
    Source: Implication 3.2.3 (error transformation)
    
    Carries structured context for diagnostics and recovery.
    Each error specifies whether it's recoverable and provides recovery hints.
    """
    message: str
    severity: ErrorSeverity = ErrorSeverity.ERROR
    context: dict[str, Any] = field(default_factory=dict)
    recoverable: bool = False
    hint: str | None = None
    caused_by: 'ScriptError | None' = None
    
    def __str__(self) -> str:
        """Format error message - just return the message."""
        return self.message
    
    def with_hint(self, hint: str) -> 'ScriptError':
        """Add recovery hint to error.
        
        Source: Implication 3.3.3 (error recovery hints)
        """
        self.hint = hint
        return self
    
    def with_context(self, **kwargs) -> 'ScriptError':
        """Add additional context to error."""
        self.context.update(kwargs)
        return self
    
    def with_cause(self, cause: 'ScriptError') -> 'ScriptError':
        """Add causal error (error that caused this one)."""
        self.caused_by = cause
        return self
    
    def to_dict(self) -> dict[str, Any]:
        """Serialize error to dictionary for logging/provenance."""
        result = {
            'message': self.message,
            'severity': self.severity.name,
            'context': self.context,
            'recoverable': self.recoverable,
            'hint': self.hint,
            'type': self.__class__.__name__,
        }
        if self.caused_by:
            result['caused_by'] = self.caused_by.to_dict()
        return result
    
    def to_json(self) -> str:
        """Serialize error to JSON."""
        import json
        return json.dumps(self.to_dict(), default=str)
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> 'ScriptError':
        """Deserialize error from dictionary."""
        severity = ErrorSeverity[data['severity']] if 'severity' in data else ErrorSeverity.ERROR
        error = cls(
            message=data['message'],
            severity=severity,
            context=data.get('context', {}),
            recoverable=data.get('recoverable', False),
            hint=data.get('hint')
        )
        if 'caused_by' in data and data['caused_by']:
            error.caused_by = ScriptError.from_dict(data['caused_by'])
        return error


class FileOperationError(ScriptError):
    """File operation errors (read, write, not found).
    
    Source: FileIO primitive error boundary (implication 1.3)
    
    Note: File operations are generally recoverable (transient I/O issues),
    so recoverable defaults to True.
    """
    def __init__(self, message: str, path, **kwargs):
        # File operations are typically recoverable (transient I/O failures)
        recoverable = kwargs.pop('recoverable', True)
        extra_context = kwargs.pop('context', {})
        context = {"path": path, **kwargs, **extra_context}
        
        super().__init__(
            message=message,
            severity=ErrorSeverity.ERROR,
            context=context,
            recoverable=recoverable
        )


class ValidationError(ScriptError):
    """Data validation errors.
    
    Source: Validation primitive (implications 1.4.2.2.1-1.4.2.2.3)
    """
    def __init__(self, message: str, field: str = "", value: Any = None, **kwargs):
        # Extract context if provided, merge with field/value
        extra_context = kwargs.pop("context", {})
        context = {"field": field, "value": value, **extra_context, **kwargs}
        
        super().__init__(
            message=message,
            severity=ErrorSeverity.ERROR,
            context=context,
            recoverable=True
        )


class ParseError(ScriptError):
    """Parsing errors with location context.
    
    Source: String parsing primitives (implications 1.3.1-1.3.3)
    """
    def __init__(self, message: str, source: str = "", line: int | None = None, 
                 column: int | None = None, **kwargs):
        context = {"source": source}
        if line is not None:
            context["line"] = line
        if column is not None:
            context["column"] = column
        context.update(kwargs)
        
        super().__init__(
            message=message,
            severity=ErrorSeverity.ERROR,
            context=context
        )


class ConfigurationError(ScriptError):
    """Configuration errors.
    
    Source: Configuration management (aspect node 3.4)
    """
    def __init__(self, message: str, setting: str = "", **kwargs):
        super().__init__(
            message=message,
            severity=ErrorSeverity.FATAL,
            context={"setting": setting, **kwargs},
            recoverable=False
        )


def handle_errors(
    func: Callable[..., T] | None = None,
    *,
    reraise_all: bool = False,
    on_error: Callable[[ScriptError], None] | None = None,
    suppress_warnings: bool = False
) -> Callable:
    """Decorator for consistent error handling.
    
    Source: Implications 3.2.1-3.2.3 (error handling strategies)
    
    Args:
        func: The function to wrap (for @ without parameters)
        reraise_all: If True, re-raise all errors including warnings
        on_error: Callback to invoke on error (before raise/suppress decision)
        suppress_warnings: If True, suppress WARNING severity errors
    
    Routing Policy:
    - WARNING: Log to stderr, call on_error callback, suppress or raise based on flags
    - ERROR/FATAL: Log to stderr, call on_error callback, always raise
    
    Usage:
        @handle_errors
        def my_function():
            ...
        
        @handle_errors(on_error=lambda e: log_to_provenance(e))
        def tracked_function():
            ...
    """
    def decorator(f: Callable[..., T]) -> Callable[..., T]:
        @wraps(f)
        def wrapper(*args, **kwargs) -> T:
            try:
                return f(*args, **kwargs)
            except ScriptError as e:
                # Invoke callback if provided
                if on_error:
                    try:
                        on_error(e)
                    except Exception as callback_error:
                        print(f"Error in on_error callback: {callback_error}", file=sys.stderr)
                
                # Route based on severity
                if e.severity == ErrorSeverity.WARNING:
                    print(f"Warning: {e.message}", file=sys.stderr)
                    if suppress_warnings:
                        return None
                    elif reraise_all:
                        raise
                    else:
                        return None
                # ERROR and FATAL: always raise
                else:
                    raise
        return wrapper
    
    # Support both @handle_errors and @handle_errors()
    if func is not None:
        return decorator(func)
    return decorator


def retry_on_error(
    max_attempts: int = 3,
    delay: float = 1.0,
    backoff: float = 2.0,
    exceptions: tuple[type[Exception], ...] | type[Exception] = Exception,
    respect_recoverable: bool = True,
    on_retry: Callable[[int, Exception, float], None] | None = None
) -> Callable:
    """Decorator to retry function on specified exceptions.
    
    Source: Implication 3.2.1 (retry logic with exponential backoff)
    Implements: Implication 1.1 (recovery field operationalization)
    
    Args:
        max_attempts: Maximum number of retry attempts
        delay: Initial delay time in seconds
        backoff: Backoff multiplier (delay *= backoff after each retry)
        exceptions: Exception type(s) to catch and retry
        respect_recoverable: If True (default), only retry ScriptErrors with recoverable=True
        on_retry: Callback invoked before each retry (attempt, error, next_delay)
    
    Backoff Sequence: delay, delay*backoff, delay*backoff^2, ...
    
    Usage:
        @retry_on_error(max_attempts=3, delay=0.1, backoff=2.0)
        def flaky_operation():
            ...
    """
    # Normalize exceptions to tuple
    if not isinstance(exceptions, tuple):
        exceptions = (exceptions,)
    
    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @wraps(func)
        def wrapper(*args, **kwargs) -> T:
            current_delay = delay
            
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except exceptions as e:
                    # Check if error is non-recoverable (should not retry)
                    if respect_recoverable and isinstance(e, ScriptError):
                        if not e.recoverable:
                            raise  # Non-recoverable, re-raise immediately
                    
                    # Decide whether to retry
                    if attempt < max_attempts - 1:
                        # Invoke callback before retry
                        if on_retry:
                            try:
                                on_retry(attempt + 1, e, current_delay)
                            except Exception as cb_error:
                                print(f"Error in on_retry callback: {cb_error}", file=sys.stderr)
                        
                        # Sleep with exponential backoff
                        time.sleep(current_delay)
                        current_delay *= backoff
                        # Continue to next attempt
                        continue
                    
                    # Last attempt failed, re-raise
                    raise
        
        return wrapper
    return decorator


def collect_errors(
    func: Callable[..., list[ScriptError]] | None = None,
    *,
    max_errors: int | None = None
) -> Callable:
    """Decorator to collect errors from a function that returns error list.
    
    Source: Implication 2.2.2 (error accumulation)
    
    The decorated function should return a list of ScriptError objects.
    If the list is non-empty, raises a ScriptError with all collected errors.
    
    Args:
        func: The function to wrap
        max_errors: Maximum errors before raising (None = no limit)
    
    Returns:
        The function result if no errors, otherwise raises ScriptError
    
    Usage:
        @collect_errors
        def validate_items():
            errors = []
            for item in items:
                if not valid(item):
                    errors.append(ValidationError("Invalid", field=item))
            return errors
    """
    def decorator(f: Callable[..., list[ScriptError]]) -> Callable:
        @wraps(f)
        def wrapper(*args, **kwargs):
            errors = f(*args, **kwargs)
            
            if not errors:
                return errors
            
            # Check threshold
            if max_errors and len(errors) > max_errors:
                raise ScriptError(
                    f"Too many errors: {len(errors)} (max: {max_errors})",
                    context={"collected_errors": errors[:max_errors]}
                )
            
            # Single error: raise it directly
            if len(errors) == 1:
                raise errors[0]
            
            # Multiple errors: raise combined error
            raise ScriptError(
                f"{len(errors)} errors occurred",
                context={"collected_errors": errors}
            )
        
        return wrapper
    
    # Support both @collect_errors and @collect_errors()
    if func is not None:
        return decorator(func)
    return decorator

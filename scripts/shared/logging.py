#!/usr/bin/env python3
"""Structured logging for metacatagory scripts.

Source: Third-order implications 3.3 from behavioral taxonomy
Purpose: Replace ad-hoc print statements with structured, machine-readable logging

Design Principles:
- Structured context in all log messages
- Multiple output formats (human-readable, JSON)
- Progress tracking for long-running operations
- Integration with error handling system
"""

import logging
import json
import sys
from typing import Any
from pathlib import Path
from datetime import datetime


def _to_json_serializable(obj: Any) -> Any:
    """Convert object to JSON-serializable form.
    
    Source: Implication 1.1.3 (pragmatic context serialization)
    """
    if isinstance(obj, dict):
        return {k: _to_json_serializable(v) for k, v in obj.items()}
    elif isinstance(obj, (list, tuple)):
        return [_to_json_serializable(item) for item in obj]
    elif hasattr(obj, 'to_dict'):
        # Support ScriptError.to_dict() and similar
        return _to_json_serializable(obj.to_dict())
    elif isinstance(obj, (str, int, float, bool, type(None))):
        return obj
    else:
        # Fallback: convert to string (covers Path, datetime, custom objects)
        return str(obj)


class StructuredFormatter(logging.Formatter):
    """JSON formatter for structured logs.
    
    Source: Implication 3.3.2 (structured logging)
    """
    
    def format(self, record: logging.LogRecord) -> str:
        """Format log record as JSON."""
        log_data = {
            "timestamp": datetime.fromtimestamp(record.created).isoformat(),
            "level": record.levelname,
            "name": record.name,
            "message": record.getMessage(),
        }
        
        # Add any extra fields from the record using canonical serialization
        if hasattr(record, 'context'):
            log_data['context'] = _to_json_serializable(record.context)
        
        if record.exc_info:
            log_data['exception'] = self.formatException(record.exc_info)
        
        return json.dumps(log_data, default=str)


class HumanReadableFormatter(logging.Formatter):
    """Human-readable formatter with colors (if terminal supports it).
    
    Source: Implication 3.3.1 (display formatting)
    """
    
    # ANSI color codes
    COLORS = {
        'DEBUG': '\033[36m',     # Cyan
        'INFO': '\033[32m',      # Green
        'WARNING': '\033[33m',   # Yellow
        'ERROR': '\033[31m',     # Red
        'CRITICAL': '\033[35m',  # Magenta
        'RESET': '\033[0m'
    }
    
    def __init__(self, use_color: bool = True):
        super().__init__()
        # Honor explicit request; tests expect color when True regardless of TTY
        self.use_colors = use_color
    
    def format(self, record: logging.LogRecord) -> str:
        """Format log record for human reading."""
        if self.use_colors:
            color = self.COLORS.get(record.levelname, '')
            reset = self.COLORS['RESET']
            level_str = f"{color}{record.levelname:8}{reset}"
        else:
            level_str = f"{record.levelname:8}"
        
        message = record.getMessage()
        
        # Add context if present, using canonical serialization
        if hasattr(record, 'context') and record.context:
            canonical_ctx = _to_json_serializable(record.context)
            ctx_str = ' '.join(f"{k}={canonical_ctx[k]}" for k in sorted(canonical_ctx.keys()))
            message = f"{message} [{ctx_str}]"
        
        result = f"{level_str} {message}"
        
        if record.exc_info:
            result += '\n' + self.formatException(record.exc_info)
        
        return result


class StructuredLogger:
    """Logger that emits structured log events.
    
    Source: Aspect node 3.3 (logging cross-cutting concern)
    
    Wraps Python's logging module with convenient methods for
    structured context and progress tracking.
    """
    
    def __init__(self, name: str, level: int = logging.INFO):
        """Initialize structured logger.
        
        Args:
            name: Logger name (typically __name__ of calling module)
            level: Minimum log level to emit
        """
        self.logger = logging.getLogger(name)
        self.logger.setLevel(level)
    
    def _log_with_context(self, level: int, message: str, context: dict[str, Any] | None = None, **extra_ctx):
        """Internal method to log with structured context."""
        if self.logger.isEnabledFor(level):
            # Merge provided context dict and any extra kwargs
            merged_ctx: dict[str, Any] = {}
            if context and isinstance(context, dict):
                merged_ctx.update(context)
            # If a 'context' key was passed via extra kwargs, merge it too
            if 'context' in extra_ctx and isinstance(extra_ctx['context'], dict):
                merged_ctx.update(extra_ctx.pop('context'))
            # Remaining extra kwargs become part of context
            if extra_ctx:
                merged_ctx.update(extra_ctx)
            # Create a LogRecord with extra context
            extra = {'context': merged_ctx} if merged_ctx else {}
            self.logger.log(level, message, extra=extra)
    
    def debug(self, message: str, context: dict[str, Any] | None = None, **extra_ctx):
        """Log debug message with structured context."""
        self._log_with_context(logging.DEBUG, message, context, **extra_ctx)
    
    def info(self, message: str, context: dict[str, Any] | None = None, **extra_ctx):
        """Log info message with structured context."""
        self._log_with_context(logging.INFO, message, context, **extra_ctx)
    
    def warning(self, message: str, context: dict[str, Any] | None = None, **extra_ctx):
        """Log warning with context."""
        self._log_with_context(logging.WARNING, message, context, **extra_ctx)
    
    def error(self, message: str, exc_info: bool = False, exception: Exception | None = None,
              context: dict[str, Any] | None = None, **extra_ctx):
        """Log error with context and optional exception.
        
        Source: Implication 1.3 (exception serialization)
                Implication 2.2 (logger-error feedback loop)
        
        Args:
            message: Error message
            exc_info: Include Python exception traceback
            exception: Exception object (ScriptError or any Exception) - takes priority over exc_info
            **context: Additional structured context
        """
        merged_ctx = context.copy() if isinstance(context, dict) else {}
        
        # Handle exception parameter (takes priority)
        if exception is not None:
            merged_ctx['exception'] = _to_json_serializable(
                exception.to_dict() if hasattr(exception, 'to_dict') else str(exception)
            )
            exc_info = False  # exception param takes priority
        
        # Merge other context sources
        if 'context' in extra_ctx and isinstance(extra_ctx['context'], dict):
            merged_ctx.update(extra_ctx.pop('context'))
        if extra_ctx:
            merged_ctx.update(extra_ctx)
        
        if exc_info:
            self.logger.error(message, exc_info=True, extra={'context': merged_ctx} if merged_ctx else {})
        else:
            self._log_with_context(logging.ERROR, message, merged_ctx if merged_ctx else None)
    
    def critical(self, message: str, exc_info: bool = False, exception: Exception | None = None,
                 context: dict[str, Any] | None = None, **extra_ctx):
        """Log critical error."""
        merged_ctx = context.copy() if isinstance(context, dict) else {}
        
        # Handle exception parameter (takes priority)
        if exception is not None:
            merged_ctx['exception'] = _to_json_serializable(
                exception.to_dict() if hasattr(exception, 'to_dict') else str(exception)
            )
            exc_info = False  # exception param takes priority
        
        # Merge other context sources
        if 'context' in extra_ctx and isinstance(extra_ctx['context'], dict):
            merged_ctx.update(extra_ctx.pop('context'))
        if extra_ctx:
            merged_ctx.update(extra_ctx)
        
        if exc_info:
            self.logger.critical(message, exc_info=True, extra={'context': merged_ctx} if merged_ctx else {})
        else:
            self._log_with_context(logging.CRITICAL, message, merged_ctx if merged_ctx else None)
    
    def progress(self, message: str = "Processing", *, current: int, total: int, 
                 succeeded: int = 0, failed: int = 0, recovery_attempts: int = 0,
                 context: dict[str, Any] | None = None, **extra_ctx):
        """Log progress for long-running operations.
        
        Source: Implication 2.1 (progress-to-provenance binding)
                Implication 1.2 (progress rate composition)
        
        Args:
            message: Operation description
            current: Current item number
            total: Total number of items
            succeeded: Number of successfully processed items
            failed: Number of failed items  
            recovery_attempts: Number of retry attempts made
            **context: Additional context
            
        Example:
            logger.progress("Processing", current=5, total=10, 
                           succeeded=4, failed=1, recovery_attempts=2)
            → Logs: "Processing [5/10] 50.0% (4 ok, 1 failed, 2 retries)"
        """
        if total > 0:
            percentage = (current / total) * 100
            success_rate = (succeeded / total * 100) if total > 0 else 0.0
            
            # Build status message
            status_parts = [f"{current}/{total}", f"{percentage:.1f}%"]
            if succeeded > 0 or failed > 0:
                status_parts.append(f"{succeeded} ok")
                if failed > 0:
                    status_parts.append(f"{failed} failed")
            if recovery_attempts > 0:
                status_parts.append(f"{recovery_attempts} retries")
            
            status_str = ", ".join(status_parts)
            
            # Build context
            merged = {
                "current": current,
                "total": total,
                "percentage": percentage,
                "succeeded": succeeded,
                "failed": failed,
                "success_rate": success_rate,
                "recovery_attempts": recovery_attempts,
            }
            if isinstance(context, dict):
                merged.update(context)
            if 'context' in extra_ctx and isinstance(extra_ctx['context'], dict):
                merged.update(extra_ctx.pop('context'))
            if extra_ctx:
                merged.update(extra_ctx)
            
            self.info(f"{message} [{status_str}]", context=merged)


def configure_logging(
    name: str,
    level: int = logging.INFO,
    log_file: Path | str | None = None,
    structured: bool = False,
    use_color: bool = True
) -> StructuredLogger:
    """Configure logging for script.
    
    Source: Configuration management for logging aspect
    
    Args:
        level: Minimum log level (logging.DEBUG, INFO, WARNING, ERROR, CRITICAL)
        log_file: Optional file to write logs to
        structured: Use JSON format (for machine parsing)
        use_colors: Use ANSI colors in console output
    
    Usage:
        logger = configure_logging(__name__, level=logging.DEBUG, structured=True)
        logger.info("Starting process", input_file="data.json")
    """
    logger = StructuredLogger(name, level=level)
    # Clear existing handlers on this named logger
    logger.logger.handlers.clear()

    # Console handler
    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setLevel(level)
    console_handler.setFormatter(StructuredFormatter() if structured else HumanReadableFormatter(use_color=use_color))
    logger.logger.addHandler(console_handler)

    # File handler (if specified)
    if log_file:
        lf_path = Path(log_file)
        lf_path.parent.mkdir(parents=True, exist_ok=True)
        file_handler = logging.FileHandler(lf_path)
        file_handler.setLevel(level)
        file_handler.setFormatter(StructuredFormatter())
        logger.logger.addHandler(file_handler)

    return logger


# Convenience functions for quick logging without creating logger instance
_default_logger = StructuredLogger('default')


def log_debug(message: str, context: dict[str, Any] | None = None, **extra_ctx):
    """Quick debug log."""
    _default_logger.debug(message, context=context, **extra_ctx)


def log_info(message: str, context: dict[str, Any] | None = None, **extra_ctx):
    """Quick info log."""
    _default_logger.info(message, context=context, **extra_ctx)


def log_warning(message: str, context: dict[str, Any] | None = None, **extra_ctx):
    """Quick warning log."""
    _default_logger.warning(message, context=context, **extra_ctx)


def log_error(message: str, context: dict[str, Any] | None = None, **extra_ctx):
    """Quick error log."""
    _default_logger.error(message, context=context, **extra_ctx)


def log_progress(current: int, total: int, message: str = "Processing", 
                  succeeded: int = 0, failed: int = 0, recovery_attempts: int = 0,
                  context: dict[str, Any] | None = None, **extra_ctx):
    """Quick progress log.
    
    Source: Implication 1.5.2 (lazy evaluation for performance)
    """
    if _default_logger.logger.isEnabledFor(logging.INFO):
        _default_logger.progress(message, current=current, total=total,
                                succeeded=succeeded, failed=failed,
                                recovery_attempts=recovery_attempts,
                                context=context, **extra_ctx)

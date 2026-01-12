#!/usr/bin/env python3
"""Configuration management for metacatagory scripts.

Source: Third-order implications 3.4 from behavioral taxonomy
Purpose: Centralize configuration scattered across scripts

Design Principles:
- Single source of truth for configuration
- Environment variable overrides
- Type-safe configuration access
- Extensible for script-specific settings
"""

import os
from pathlib import Path
from typing import Any
from dataclasses import dataclass, field, fields
import json
from inspect import signature
from contextvars import ContextVar
import warnings
from .errors import FileOperationError, ValidationError


@dataclass
class Config:
    """Global configuration for metacatagory scripts.
    
    Source: Aspect node 3.4 (configuration management)
    
    Centralizes configuration that was previously scattered across scripts.
    Supports environment variable overrides and file-based configuration.
    """
    
    # Paths (from shared/paths.py - avoiding circular import)
    repo_root: Path = field(default_factory=lambda: Path(__file__).parent.parent.parent)
    
    @property
    def build_dir(self) -> Path:
        """Build output directory."""
        return self.repo_root / "build"
    
    @property
    def src_dir(self) -> Path:
        """Source directory."""
        return self.repo_root / "src"
    
    @property
    def agda_dir(self) -> Path:
        """Agda source directory."""
        return self.src_dir / "agda"
    
    @property
    def scripts_dir(self) -> Path:
        """Scripts directory."""
        return self.repo_root / "scripts"
    
    @property
    def tests_dir(self) -> Path:
        """Tests directory."""
        return self.repo_root / "tests"
    
    @property
    def docs_dir(self) -> Path:
        """Documentation directory."""
        return self.repo_root / "docs"
    
    # Behavior flags
    verbose: bool = False
    dry_run: bool = False
    parallel: bool = False
    workers: int = 4
    
    # Validation settings
    strict_validation: bool = True
    fail_on_warning: bool = False
    report_mode: str = "stdout"  # stdout | write
    
    # I/O settings
    json_indent: int = 2
    create_parents: bool = True
    backup_on_overwrite: bool = False
    
    # Logging settings
    log_level: str = "INFO"  # DEBUG, INFO, WARNING, ERROR, CRITICAL
    log_structured: bool = False
    log_file: Path | None = None
    
    # Custom settings (extensible)
    custom: dict[str, Any] = field(default_factory=dict)
    
    @classmethod
    def from_env(cls) -> 'Config':
        """Load configuration from environment variables.
        
        Source: Implication 3.1.3 (configuration override via env vars)
        
        Environment variables:
            METACATAGORY_REPO_ROOT: Override repository root
            METACATAGORY_VERBOSE: Enable verbose output (1/true/yes)
            METACATAGORY_DRY_RUN: Dry run mode (1/true/yes)
            METACATAGORY_PARALLEL: Enable parallel processing
            METACATAGORY_WORKERS: Number of parallel workers
            METACATAGORY_STRICT: Strict validation mode
            METACATAGORY_LOG_LEVEL: Logging level
        """
        config = cls()
        
        # Override repo root if specified
        if repo_root := os.getenv('METACATAGORY_REPO_ROOT'):
            config.repo_root = Path(repo_root)
        
        # Boolean flags
        config.verbose = _parse_bool(os.getenv('METACATAGORY_VERBOSE', 'false'))
        config.dry_run = _parse_bool(os.getenv('METACATAGORY_DRY_RUN', 'false'))
        config.parallel = _parse_bool(os.getenv('METACATAGORY_PARALLEL', 'false'))
        config.strict_validation = _parse_bool(os.getenv('METACATAGORY_STRICT', 'true'))
        config.fail_on_warning = _parse_bool(os.getenv('METACATAGORY_FAIL_ON_WARNING', 'false'))
        if report_mode := os.getenv('METACATAGORY_REPORT_MODE'):
            config.report_mode = report_mode.lower()
        config.log_structured = _parse_bool(os.getenv('METACATAGORY_LOG_STRUCTURED', 'false'))
        config.create_parents = _parse_bool(os.getenv('METACATAGORY_CREATE_PARENTS', 'true'))
        config.backup_on_overwrite = _parse_bool(os.getenv('METACATAGORY_BACKUP_ON_OVERWRITE', 'false'))
        
        # Integer settings
        if workers := os.getenv('METACATAGORY_WORKERS'):
            try:
                config.workers = int(workers)
            except ValueError:
                pass
        
        # Logging settings
        if log_level := os.getenv('METACATAGORY_LOG_LEVEL'):
            config.log_level = log_level.upper()
        
        if log_file := os.getenv('METACATAGORY_LOG_FILE'):
            config.log_file = Path(log_file)

        if json_indent := os.getenv('METACATAGORY_JSON_INDENT'):
            try:
                config.json_indent = int(json_indent)
            except ValueError:
                pass
        
        return config
    
    @classmethod
    def from_file(cls, path: Path | str) -> 'Config':
        """Load configuration from JSON file.
        
        Source: Implication 3.1.2 (file-based configuration)
        
        Args:
            path: Path to JSON configuration file
        
        Returns:
            Config instance with values from file
        """
        config = cls()
        
        path_obj = Path(path)
        if not path_obj.exists():
            raise FileOperationError("Configuration file not found", path=path_obj)
        
        with open(path_obj) as f:
            data = json.load(f)
        
        # Update config from file data
        for key, value in data.items():
            if hasattr(config, key):
                # Convert path strings to Path objects
                if key.endswith('_dir') or key.endswith('_file') or key == 'repo_root':
                    value = Path(value)
                setattr(config, key, value)
            else:
                # Unknown keys go into custom dict
                config.custom[key] = value
                warnings.warn(
                    f"Unknown config key '{key}' added to custom dict.",
                    stacklevel=2
                )
        
        return config
    
    def override(self, warn_unknown: bool = True, **kwargs) -> 'Config':
        """Create new config with overridden values.
        
        Source: Implication 2.3 (override composition)
                Implication 2.5 (override validation)
        
        Args:
            warn_unknown: Warn if unknown keys are provided
            **kwargs: Configuration values to override
        
        Returns:
            New Config instance with overridden values
        
        Raises:
            TypeError: If override value type doesn't match field type
        
        Usage:
            dev_config = config.override(verbose=True, strict_validation=False)
        """
        import copy
        new_config = copy.deepcopy(self)
        
        # Get field names and types from dataclass
        field_info = {f.name: f.type for f in fields(self.__class__)}
        
        for key, value in kwargs.items():
            if key in field_info:
                # Validate type if possible (basic check)
                expected_type = field_info[key]
                # Skip complex type checking for generics like dict[str, Any]
                if not (hasattr(expected_type, '__origin__') or key == 'custom'):
                    # Simple type check for primitives
                    if not isinstance(value, expected_type):
                        # Allow None for optional types
                        if value is not None:
                            raise TypeError(
                                f"Override value for '{key}' has wrong type: "
                                f"expected {expected_type.__name__}, got {type(value).__name__}"
                            )
                setattr(new_config, key, value)
            elif hasattr(new_config, key):
                # Property or method - can't override but exists
                if warn_unknown:
                    warnings.warn(f"Cannot override property/method '{key}'")
            else:
                # Unknown key
                if warn_unknown:
                    warnings.warn(
                        f"Unknown config key '{key}' added to custom dict. "
                        f"Did you mean one of: {', '.join(field_info.keys())}?"
                    )
                new_config.custom[key] = value
        
        return new_config
    
    def to_dict(self) -> dict[str, Any]:
        """Export configuration as dictionary.
        
        Source: Implication 1.3.2 (serialization symmetry)
        Useful for serialization or passing to functions.
        """
        return {
            'repo_root': str(self.repo_root),
            'verbose': self.verbose,
            'dry_run': self.dry_run,
            'parallel': self.parallel,
            'workers': self.workers,
            'strict_validation': self.strict_validation,
            'fail_on_warning': self.fail_on_warning,
            'json_indent': self.json_indent,
            'create_parents': self.create_parents,
            'backup_on_overwrite': self.backup_on_overwrite,
            'log_level': self.log_level,
            'log_structured': self.log_structured,
            'log_file': str(self.log_file) if self.log_file else None,
            'custom': self.custom
        }
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> 'Config':
        """Load configuration from dictionary.
        
        Source: Implication 1.3.1 (serialization round-trip)
        
        Args:
            data: Dictionary with config values
        
        Returns:
            Config instance
        """
        config = cls()
        
        for key, value in data.items():
            if key == 'custom':
                config.custom = value
            elif hasattr(config, key):
                # Convert path strings to Path objects
                if key.endswith('_dir') or key.endswith('_file') or key == 'repo_root':
                    if value is not None:
                        value = Path(value)
                setattr(config, key, value)
            else:
                # Unknown keys go into custom dict
                config.custom[key] = value
        
        return config
    
    def validate(self, strict: bool = False) -> 'Config':
        """Validate configuration paths and settings.
        
        Source: Implication 1.2 (path validation contract)
        
        Args:
            strict: If True, raise on any validation failure.
                   If False, only raise on critical failures.
        
        Returns:
            Self (for chaining)
        
        Raises:
            FileOperationError: If critical paths don't exist
            ValidationError: If settings are invalid
        """
        # Verify repo_root exists and looks like metacatagory repo
        if not self.repo_root.exists():
            raise FileOperationError(
                "Repository root does not exist",
                path=self.repo_root,
                hint="Check METACATAGORY_REPO_ROOT environment variable"
            )
        
        # Check for .git directory (indicates repository root)
        git_dir = self.repo_root / ".git"
        if not git_dir.exists() and strict:
            raise FileOperationError(
                "Repository root does not contain .git directory",
                path=self.repo_root,
                hint="Ensure repo_root points to metacatagory repository"
            )
        
        # Verify critical directories exist or are creatable
        critical_dirs = [self.src_dir, self.scripts_dir]
        for dir_path in critical_dirs:
            if not dir_path.exists():
                if strict:
                    raise FileOperationError(
                        f"Critical directory does not exist: {dir_path.name}",
                        path=dir_path,
                        hint="Check repository structure"
                    )
                else:
                    warnings.warn(f"Directory not found: {dir_path}")
        
        # Validate settings
        if self.workers < 1:
            raise ValidationError(
                "workers must be >= 1",
                field="workers",
                hint=f"Current value: {self.workers}"
            )
        
        if self.json_indent < 0:
            raise ValidationError(
                "json_indent must be >= 0",
                field="json_indent",
                hint=f"Current value: {self.json_indent}"
            )
        
        valid_log_levels = {'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'}
        if self.log_level not in valid_log_levels:
            raise ValidationError(
                f"Invalid log_level: {self.log_level}",
                field="log_level",
                hint=f"Must be one of: {', '.join(valid_log_levels)}"
            )

        valid_report_modes = {'stdout', 'write'}
        if self.report_mode not in valid_report_modes:
            raise ValidationError(
                f"Invalid report_mode: {self.report_mode}",
                field="report_mode",
                hint="Must be one of: stdout, write"
            )
        
        return self


def _parse_bool(value: str) -> bool:
    """Parse boolean from string.
    
    Accepts: 1/0, true/false, yes/no (case-insensitive)
    """
    return value.lower() in ('1', 'true', 'yes', 'on')


# Thread-safe config using contextvars (Implication 1.1: context-local configuration)
_config_var: ContextVar[Config | None] = ContextVar('config', default=None)


def get_config() -> Config:
    """Get context-local configuration.
    
    Source: Implication 1.1.2 (thread-isolated config)
    
    Lazily initializes config from environment on first call.
    Thread-safe using contextvars.
    
    Returns:
        Context-local Config instance
    
    Usage:
        from shared.config import get_config
        
        config = get_config()
        if config.verbose:
            print("Verbose mode enabled")
    """
    config = _config_var.get()
    if config is None:
        config = Config.from_env()
        _config_var.set(config)
    return config


def set_config(config: Config):
    """Set context-local configuration.
    
    Source: Implication 1.1.1 (context-local config)
    
    Useful for testing or programmatic configuration.
    Thread-safe using contextvars.
    
    Args:
        config: Config instance to use in current context
    """
    _config_var.set(config)


def reset_config():
    """Reset context-local configuration.
    
    Source: Implication 1.1.3 (context isolation)
    
    Forces re-initialization from environment on next get_config() call.
    Thread-safe using contextvars.
    Useful for testing.
    """
    _config_var.set(None)


def with_config(func=None, **overrides):
    """Decorator to temporarily override config for a function.
    
    Source: Implication 3.1.1 (scoped configuration)
    
    Args:
        **overrides: Configuration values to override
    
    Returns:
        Decorator function
    
    Usage:
        @with_config(verbose=True, strict_validation=False)
        def my_function():
            config = get_config()
            # config.verbose will be True here
            ...
    """
    def decorator(f):
        from functools import wraps
        
        @wraps(f)
        def wrapper(*args, **kwargs):
            # Get current config from context
            original_config = _config_var.get()
            try:
                # Apply overrides to context-local config for the duration of the call
                effective = (original_config or Config.from_env())
                if overrides:
                    effective = effective.override(**overrides)
                _config_var.set(effective)

                # Inject 'config' kwarg if function accepts it and it's not provided
                try:
                    params = signature(f).parameters
                    if 'config' in params and 'config' not in kwargs:
                        kwargs['config'] = _config_var.get()
                except (ValueError, TypeError):
                    # Signature inspection can fail for built-ins or special functions
                    pass

                return f(*args, **kwargs)
            finally:
                # Restore original config
                _config_var.set(original_config)
        
        return wrapper
    
    # Support both @with_config and @with_config(**overrides)
    if callable(func):
        return decorator(func)
    return decorator

#!/usr/bin/env python3
"""Expanded test suite for config.py - First-Order Implications.

Tests focus on:
1. Context-Local Configuration (thread safety)
2. Path Validation Contract
3. Serialization Symmetry

Source: MODULE-AUDIT-PHASE-3.md, First-Order Implications
"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import threading
import warnings
from scripts.shared.config import (
    Config,
    get_config,
    set_config,
    reset_config,
    with_config,
)
from scripts.shared.errors import FileOperationError, ValidationError


class TestSerializationSymmetry:
    """First-Order Implication 1.3: Serialization Round-Trip"""
    
    def test_to_dict_includes_all_fields(self):
        """Test to_dict() exports all config fields."""
        config = Config(verbose=True, workers=8, log_level="DEBUG")
        d = config.to_dict()
        
        assert 'verbose' in d
        assert d['verbose'] is True
        assert 'workers' in d
        assert d['workers'] == 8
        assert 'log_level' in d
        assert d['log_level'] == "DEBUG"
    
    def test_from_dict_reconstructs_config(self):
        """Test from_dict() reconstructs config from dictionary."""
        original = Config(verbose=True, workers=8, dry_run=True)
        d = original.to_dict()
        reconstructed = Config.from_dict(d)
        
        assert reconstructed.verbose == original.verbose
        assert reconstructed.workers == original.workers
        assert reconstructed.dry_run == original.dry_run
    
    def test_round_trip_identity(self):
        """Test config -> dict -> config round-trip preserves values."""
        original = Config(
            verbose=True,
            workers=8,
            strict_validation=False,
            json_indent=4,
            log_level="DEBUG"
        )
        
        # Round-trip
        d = original.to_dict()
        reconstructed = Config.from_dict(d)
        
        # Verify all fields match
        assert reconstructed.to_dict() == d
    
    def test_from_dict_handles_paths(self):
        """Test from_dict() converts path strings to Path objects."""
        data = {
            'repo_root': '/tmp/test',
            'log_file': '/tmp/test.log',
            'verbose': True
        }
        
        config = Config.from_dict(data)
        
        assert isinstance(config.repo_root, Path)
        assert config.repo_root == Path('/tmp/test')
        assert isinstance(config.log_file, Path)
        assert config.log_file == Path('/tmp/test.log')
    
    def test_from_dict_handles_custom_fields(self):
        """Test from_dict() puts unknown keys in custom dict."""
        data = {
            'verbose': True,
            'custom_key': 'custom_value',
            'another_custom': 123
        }
        
        config = Config.from_dict(data)
        
        assert config.verbose is True
        assert config.custom['custom_key'] == 'custom_value'
        assert config.custom['another_custom'] == 123


class TestPathValidation:
    """First-Order Implication 1.2: Path Validation Contract"""
    
    def test_validate_succeeds_for_valid_repo(self):
        """Test validate() succeeds for valid repository."""
        config = Config()  # Uses default repo_root
        
        # Should not raise
        result = config.validate(strict=False)
        
        # Should return self for chaining
        assert result is config
    
    def test_validate_raises_for_nonexistent_repo_root(self):
        """Test validate() raises for nonexistent repo_root."""
        config = Config()
        config.repo_root = Path("/nonexistent/path/to/repo")
        
        with pytest.raises(FileOperationError) as exc_info:
            config.validate()
        
        assert "does not exist" in str(exc_info.value)
    
    def test_validate_checks_workers_positive(self):
        """Test validate() ensures workers >= 1."""
        config = Config(workers=0)
        
        with pytest.raises(ValidationError) as exc_info:
            config.validate()
        
        assert "workers must be >= 1" in str(exc_info.value)
    
    def test_validate_checks_json_indent_non_negative(self):
        """Test validate() ensures json_indent >= 0."""
        config = Config(json_indent=-1)
        
        with pytest.raises(ValidationError) as exc_info:
            config.validate()
        
        assert "json_indent must be >= 0" in str(exc_info.value)
    
    def test_validate_checks_log_level_valid(self):
        """Test validate() ensures log_level is valid."""
        config = Config(log_level="INVALID")
        
        with pytest.raises(ValidationError) as exc_info:
            config.validate()
        
        assert "Invalid log_level" in str(exc_info.value)
    
    def test_validate_warns_for_missing_dirs_in_non_strict(self):
        """Test validate() warns (doesn't raise) for missing dirs in non-strict mode."""
        config = Config()
        config.repo_root = Path("/tmp/fake_repo_for_test")
        config.repo_root.mkdir(parents=True, exist_ok=True)
        (config.repo_root / ".git").mkdir(exist_ok=True)
        
        try:
            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")
                config.validate(strict=False)
                
                # Should have warnings about missing src/, scripts/ dirs
                assert len(w) > 0
        finally:
            # Cleanup
            import shutil
            shutil.rmtree(config.repo_root, ignore_errors=True)


class TestOverrideValidation:
    """First-Order Implication 2.5: Override Validation"""
    
    def test_override_validates_known_fields(self):
        """Test override() accepts known fields."""
        config = Config()
        new_config = config.override(verbose=True, workers=8)
        
        assert new_config.verbose is True
        assert new_config.workers == 8
        assert config.verbose is False  # Original unchanged
    
    def test_override_warns_on_unknown_fields(self):
        """Test override() warns about unknown fields."""
        config = Config()
        
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            config.override(unknown_key="value")
            
            assert len(w) == 1
            assert "Unknown config key" in str(w[0].message)
    
    def test_override_detects_type_mismatch(self):
        """Test override() detects type mismatches."""
        config = Config()
        
        with pytest.raises(TypeError) as exc_info:
            config.override(workers="not_a_number")
        
        assert "wrong type" in str(exc_info.value)
    
    def test_override_chains(self):
        """Test override() supports chaining."""
        config = Config()
        
        new_config = config.override(verbose=True).override(workers=8)
        
        assert new_config.verbose is True
        assert new_config.workers == 8


class TestContextLocalConfiguration:
    """First-Order Implication 1.1: Context-Local Config (Thread Safety)"""
    
    def test_get_config_initializes_lazily(self):
        """Test get_config() initializes on first call."""
        reset_config()
        
        config = get_config()
        
        assert config is not None
        assert isinstance(config, Config)
    
    def test_set_config_updates_context(self):
        """Test set_config() updates context-local config."""
        custom_config = Config(verbose=True, workers=16)
        set_config(custom_config)
        
        config = get_config()
        
        assert config.verbose is True
        assert config.workers == 16
    
    def test_reset_config_clears_context(self):
        """Test reset_config() clears context."""
        set_config(Config(verbose=True))
        reset_config()
        
        config = get_config()
        
        # Should reinitialize from env (default verbose=False)
        assert config.verbose is False
    
    def test_with_config_decorator_overrides_temporarily(self):
        """Test @with_config decorator overrides for function scope."""
        reset_config()
        
        @with_config(verbose=True, workers=16)
        def get_verbose_and_workers(config):
            return config.verbose, config.workers
        
        verbose, workers = get_verbose_and_workers()
        
        assert verbose is True
        assert workers == 16
        
        # After function returns, config should be restored
        config = get_config()
        assert config.verbose is False  # Default
    
    def test_with_config_restores_original_config(self):
        """Test @with_config restores original config after execution."""
        set_config(Config(verbose=False, workers=4))
        
        @with_config(verbose=True)
        def inner():
            return get_config().verbose
        
        result = inner()
        assert result is True
        
        # Original config should be restored
        config = get_config()
        assert config.verbose is False
    
    def test_thread_isolation(self):
        """Test configs are isolated between threads."""
        reset_config()
        results = {}
        
        def thread1_func():
            set_config(Config(verbose=True, workers=8))
            import time
            time.sleep(0.01)  # Let thread2 run
            results['thread1'] = get_config().workers
        
        def thread2_func():
            set_config(Config(verbose=False, workers=16))
            import time
            time.sleep(0.01)  # Let thread1 run
            results['thread2'] = get_config().workers
        
        t1 = threading.Thread(target=thread1_func)
        t2 = threading.Thread(target=thread2_func)
        
        t1.start()
        t2.start()
        t1.join()
        t2.join()
        
        # Each thread should have seen its own config
        assert results['thread1'] == 8
        assert results['thread2'] == 16


class TestConfigProperties:
    """Test computed path properties."""
    
    def test_build_dir_property(self):
        """Test build_dir property returns correct path."""
        config = Config()
        
        expected = config.repo_root / "build"
        assert config.build_dir == expected
    
    def test_src_dir_property(self):
        """Test src_dir property returns correct path."""
        config = Config()
        
        expected = config.repo_root / "src"
        assert config.src_dir == expected
    
    def test_agda_dir_property(self):
        """Test agda_dir property returns correct path."""
        config = Config()
        
        expected = config.repo_root / "src" / "agda"
        assert config.agda_dir == expected


class TestDecoratorComposition:
    """Second-Order Implication 2.3: Nested with_config behavior"""
    
    def test_nested_with_config_decorators(self):
        """Test nested @with_config decorators merge overrides."""
        reset_config()
        
        @with_config(verbose=True)
        def outer():
            @with_config(workers=16)
            def inner():
                config = get_config()
                return config.verbose, config.workers
            
            return inner()
        
        verbose, workers = outer()
        
        # Inner decorator should see its override
        assert workers == 16
        # Outer decorator's override should be visible
        # (Note: current implementation last-set-wins due to context nesting)
        assert verbose is True


class TestConfigIntegration:
    """Test integration with errors and logging modules."""
    
    def test_validate_raises_file_operation_error(self):
        """Test validate() uses FileOperationError from errors module."""
        config = Config()
        config.repo_root = Path("/nonexistent")
        
        with pytest.raises(FileOperationError):
            config.validate()
    
    def test_validate_raises_validation_error(self):
        """Test validate() uses ValidationError from errors module."""
        config = Config(workers=0)
        
        with pytest.raises(ValidationError):
            config.validate()

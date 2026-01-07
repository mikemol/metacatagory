"""Tests for shared.config module."""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import pytest
import os
import json
import tempfile
from scripts.shared.config import Config, get_config, reset_config, with_config


class TestConfigDefaults:
    """Test Config default values."""
    
    def setup_method(self):
        """Reset config before each test."""
        reset_config()
    
    def test_default_paths(self):
        """Test default path configurations."""
        config = Config()
        
        assert config.repo_root.is_absolute()
        assert config.build_dir.name == "build"
        assert config.src_dir.name == "src"
        assert config.agda_dir.name == "agda"
        assert config.scripts_dir.name == "scripts"
        assert config.tests_dir.name == "tests"
        assert config.docs_dir.name == "docs"
    
    def test_default_behavior(self):
        """Test default behavior flags."""
        config = Config()
        
        assert config.verbose is False
        assert config.dry_run is False
        assert config.parallel is False
        assert config.workers == 4
    
    def test_default_validation(self):
        """Test default validation settings."""
        config = Config()
        
        assert config.strict_validation is True
        assert config.fail_on_warning is False
    
    def test_default_io_settings(self):
        """Test default I/O settings."""
        config = Config()
        
        assert config.json_indent == 2
        assert config.create_parents is True
        assert config.backup_on_overwrite is False
    
    def test_default_logging_settings(self):
        """Test default logging settings."""
        config = Config()
        
        assert config.log_level == "INFO"
        assert config.log_structured is False
        assert config.log_file is None


class TestConfigFromEnv:
    """Test Config.from_env() environment variable parsing."""
    
    def setup_method(self):
        """Clean environment before each test."""
        reset_config()
        for key in list(os.environ.keys()):
            if key.startswith("METACATAGORY_"):
                del os.environ[key]
    
    def teardown_method(self):
        """Clean environment after each test."""
        for key in list(os.environ.keys()):
            if key.startswith("METACATAGORY_"):
                del os.environ[key]
    
    def test_repo_root_from_env(self):
        """Test reading repo_root from environment."""
        os.environ["METACATAGORY_REPO_ROOT"] = "/custom/path"
        config = Config.from_env()
        
        assert config.repo_root == Path("/custom/path")
    
    def test_verbose_from_env(self):
        """Test reading boolean flags from environment."""
        os.environ["METACATAGORY_VERBOSE"] = "true"
        config = Config.from_env()
        assert config.verbose is True
        
        os.environ["METACATAGORY_VERBOSE"] = "false"
        config = Config.from_env()
        assert config.verbose is False
        
        os.environ["METACATAGORY_VERBOSE"] = "1"
        config = Config.from_env()
        assert config.verbose is True
    
    def test_workers_from_env(self):
        """Test reading integer values from environment."""
        os.environ["METACATAGORY_WORKERS"] = "8"
        config = Config.from_env()
        
        assert config.workers == 8
    
    def test_log_level_from_env(self):
        """Test reading string values from environment."""
        os.environ["METACATAGORY_LOG_LEVEL"] = "DEBUG"
        config = Config.from_env()
        
        assert config.log_level == "DEBUG"
    
    def test_multiple_env_vars(self):
        """Test reading multiple environment variables."""
        os.environ["METACATAGORY_VERBOSE"] = "true"
        os.environ["METACATAGORY_WORKERS"] = "12"
        os.environ["METACATAGORY_LOG_LEVEL"] = "WARNING"
        
        config = Config.from_env()
        
        assert config.verbose is True
        assert config.workers == 12
        assert config.log_level == "WARNING"


class TestConfigFromFile:
    """Test Config.from_file() JSON configuration loading."""
    
    def setup_method(self):
        """Reset config before each test."""
        reset_config()
    
    def test_load_from_json_file(self):
        """Test loading config from JSON file."""
        config_data = {
            "verbose": True,
            "workers": 16,
            "log_level": "DEBUG",
            "strict_validation": False
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            json.dump(config_data, f)
            config_file = f.name
        
        try:
            config = Config.from_file(config_file)
            
            assert config.verbose is True
            assert config.workers == 16
            assert config.log_level == "DEBUG"
            assert config.strict_validation is False
        finally:
            os.unlink(config_file)
    
    def test_load_custom_settings(self):
        """Test loading custom settings from JSON."""
        config_data = {
            "custom": {
                "api_key": "test-key",
                "endpoint": "https://example.com"
            }
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            json.dump(config_data, f)
            config_file = f.name
        
        try:
            config = Config.from_file(config_file)
            
            assert config.custom["api_key"] == "test-key"
            assert config.custom["endpoint"] == "https://example.com"
        finally:
            os.unlink(config_file)
    
    def test_file_not_found(self):
        """Test behavior when config file doesn't exist."""
        from scripts.shared.errors import FileOperationError
        
        with pytest.raises(FileOperationError):
            Config.from_file("/nonexistent/config.json")


class TestConfigOverride:
    """Test Config.override() composition."""
    
    def test_override_single_field(self):
        """Test overriding a single field."""
        base_config = Config(verbose=False, workers=4)
        new_config = base_config.override(verbose=True)
        
        assert new_config.verbose is True
        assert new_config.workers == 4
        assert base_config.verbose is False  # Original unchanged
    
    def test_override_multiple_fields(self):
        """Test overriding multiple fields."""
        base_config = Config(verbose=False, workers=4, log_level="INFO")
        new_config = base_config.override(verbose=True, workers=8, log_level="DEBUG")
        
        assert new_config.verbose is True
        assert new_config.workers == 8
        assert new_config.log_level == "DEBUG"
    
    def test_override_custom_settings(self):
        """Test overriding custom settings."""
        base_config = Config(custom={"key1": "value1"})
        new_config = base_config.override(custom={"key2": "value2"})
        
        # Custom settings should be merged
        assert "key2" in new_config.custom
        assert new_config.custom["key2"] == "value2"
    
    def test_override_immutability(self):
        """Test that override creates new instance."""
        base_config = Config(verbose=False)
        new_config = base_config.override(verbose=True)
        
        assert base_config is not new_config
        assert base_config.verbose is False
        assert new_config.verbose is True


class TestGetConfigSingleton:
    """Test get_config() singleton pattern."""
    
    def setup_method(self):
        """Reset config singleton before each test."""
        reset_config()
    
    def test_get_config_returns_singleton(self):
        """Test that get_config returns same instance."""
        config1 = get_config()
        config2 = get_config()
        
        assert config1 is config2
    
    def test_get_config_with_env_vars(self):
        """Test get_config reads environment variables."""
        os.environ["METACATAGORY_VERBOSE"] = "true"
        
        try:
            reset_config()  # Force reload
            config = get_config()
            assert config.verbose is True
        finally:
            if "METACATAGORY_VERBOSE" in os.environ:
                del os.environ["METACATAGORY_VERBOSE"]
    
    def test_reset_config(self):
        """Test reset_config() clears singleton."""
        config1 = get_config()
        reset_config()
        config2 = get_config()
        
        # Should be different instances after reset
        assert config1 is not config2


class TestWithConfigDecorator:
    """Test with_config() decorator."""
    
    def setup_method(self):
        """Reset config before each test."""
        reset_config()
    
    def test_function_with_config_kwarg(self):
        """Test decorator with explicit config kwarg."""
        @with_config
        def test_function(x: int, config: Config = None) -> int:
            return x * config.workers
        
        custom_config = Config(workers=10)
        result = test_function(5, config=custom_config)
        
        assert result == 50
    
    def test_function_without_config_kwarg(self):
        """Test decorator uses default config when not provided."""
        @with_config
        def test_function(x: int, config: Config = None) -> int:
            return x * config.workers
        
        result = test_function(5)
        
        # Should use default config (workers=4)
        assert result == 20
    
    def test_config_parameter_injected(self):
        """Test that config parameter is properly injected."""
        received_config = None
        
        @with_config
        def test_function(config: Config = None):
            nonlocal received_config
            received_config = config
        
        test_function()
        
        assert received_config is not None
        assert isinstance(received_config, Config)
    
    def test_scoped_override(self):
        """Test scoped config override."""
        @with_config
        def outer_function(config: Config = None) -> int:
            @with_config
            def inner_function(config: Config = None) -> int:
                return config.workers
            
            # Inner uses custom config
            return inner_function(config=Config(workers=20))
        
        # Outer uses default config
        result = outer_function()
        assert result == 20


class TestConfigIntegration:
    """Test Config integration patterns."""
    
    def setup_method(self):
        """Reset config before each test."""
        reset_config()
    
    def test_env_overrides_defaults(self):
        """Test environment variables override defaults."""
        os.environ["METACATAGORY_WORKERS"] = "16"
        
        try:
            reset_config()
            config = get_config()
            assert config.workers == 16
        finally:
            if "METACATAGORY_WORKERS" in os.environ:
                del os.environ["METACATAGORY_WORKERS"]
    
    def test_file_overrides_env(self):
        """Test file config overrides environment variables."""
        os.environ["METACATAGORY_WORKERS"] = "8"
        
        config_data = {"workers": 16}
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            json.dump(config_data, f)
            config_file = f.name
        
        try:
            config = Config.from_file(config_file)
            assert config.workers == 16
        finally:
            os.unlink(config_file)
            if "METACATAGORY_WORKERS" in os.environ:
                del os.environ["METACATAGORY_WORKERS"]
    
    def test_explicit_overrides_all(self):
        """Test explicit override() takes precedence."""
        os.environ["METACATAGORY_WORKERS"] = "8"
        
        try:
            reset_config()
            base_config = get_config()
            custom_config = base_config.override(workers=32)
            
            assert base_config.workers == 8
            assert custom_config.workers == 32
        finally:
            if "METACATAGORY_WORKERS" in os.environ:
                del os.environ["METACATAGORY_WORKERS"]
    
    def test_path_consistency(self):
        """Test that derived paths are consistent with repo_root."""
        config = Config(repo_root=Path("/custom/root"))
        
        assert config.build_dir == Path("/custom/root/build")
        assert config.src_dir == Path("/custom/root/src")
        assert config.agda_dir == Path("/custom/root/src/agda")

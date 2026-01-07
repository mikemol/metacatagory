#!/usr/bin/env python3
"""Tests for scripts/shared/paths.py module."""

import pytest
import sys
from pathlib import Path

# Import the module under test
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from scripts.shared.paths import (
    REPO_ROOT,
    BUILD_DIR,
    PLANNING_INDEX_JSON,
    get_module_path_from_name,
    get_module_name_from_path,
    ensure_build_dirs
)


class TestPathConstants:
    """Test path constant definitions."""
    
    def test_repo_root_exists(self):
        """REPO_ROOT should point to repository root."""
        assert REPO_ROOT.exists()
        assert (REPO_ROOT / "Makefile").exists()
    
    def test_build_dir_path(self):
        """BUILD_DIR should be under REPO_ROOT."""
        assert BUILD_DIR.parent == REPO_ROOT
        assert BUILD_DIR.name == "build"
    
    def test_planning_index_json_path(self):
        """PLANNING_INDEX_JSON should be in build directory."""
        assert PLANNING_INDEX_JSON.parent == BUILD_DIR
        assert PLANNING_INDEX_JSON.name == "planning_index.json"


class TestGetModulePathFromName:
    """Test get_module_path_from_name function."""
    
    def test_simple_module_name(self):
        """Should convert simple module name to path."""
        result = get_module_path_from_name("Test.Module")
        
        assert result.name == "Module.agda"
        assert result.parts[-2] == "Test"
    
    def test_nested_module_name(self):
        """Should convert nested module name to path."""
        result = get_module_path_from_name("Plan.CIM.Utility")
        
        assert result.name == "Utility.agda"
        assert result.parts[-2] == "CIM"
        assert result.parts[-3] == "Plan"
    
    def test_custom_base_dir(self, tmp_path):
        """Should use custom base directory."""
        result = get_module_path_from_name("Test.Module", base_dir=tmp_path)
        
        expected = tmp_path / "Test" / "Module.agda"
        assert result == expected


class TestGetModuleNameFromPath:
    """Test get_module_name_from_path function."""
    
    def test_path_with_agda_dir(self):
        """Should extract module name from path containing 'agda'."""
        path = Path("src/agda/Plan/CIM/Utility.agda")
        
        result = get_module_name_from_path(path)
        
        assert result == "Plan.CIM.Utility"
    
    def test_path_without_agda_dir(self, tmp_path):
        """Should handle path without 'agda' directory."""
        agda_dir = tmp_path / "agda"
        agda_dir.mkdir()
        path = agda_dir / "Test" / "Module.agda"
        
        result = get_module_name_from_path(path, base_dir=agda_dir)
        
        assert result == "Test.Module"
    
    def test_single_file(self):
        """Should handle single file path."""
        path = Path("Utility.agda")
        
        result = get_module_name_from_path(path)
        
        assert result == "Utility"


class TestEnsureBuildDirs:
    """Test ensure_build_dirs function."""
    
    def test_creates_build_directories(self, tmp_path, monkeypatch):
        """Should create all build directories."""
        # Temporarily override paths to use tmp_path
        from scripts.shared import paths as paths_module
        monkeypatch.setattr(paths_module, 'BUILD_DIR', tmp_path / 'build')
        monkeypatch.setattr(paths_module, 'REPORTS_DIR', tmp_path / 'build' / 'reports')
        monkeypatch.setattr(paths_module, 'DEPS_DIR', tmp_path / 'data' / 'deps')
        monkeypatch.setattr(paths_module, 'ENRICHED_DIR', tmp_path / 'data' / 'enriched')
        monkeypatch.setattr(paths_module, 'PLANNING_DIR', tmp_path / 'data' / 'planning')
        
        ensure_build_dirs()
        
        assert (tmp_path / 'build').exists()
        assert (tmp_path / 'build' / 'reports').exists()
        assert (tmp_path / 'data' / 'deps').exists()
        assert (tmp_path / 'data' / 'enriched').exists()
        assert (tmp_path / 'data' / 'planning').exists()

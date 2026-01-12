#!/usr/bin/env python3
"""Shared path constants and utilities.

Centralizes repository structure knowledge and standard build paths.
"""

import os
from pathlib import Path

def _resolve_repo_path(value: str, base: Path) -> Path:
    path = Path(value)
    return path if path.is_absolute() else base / path

# Repository root - calculated once
REPO_ROOT = Path(__file__).parent.parent.parent

# Standard build output directories
BUILD_DIR = REPO_ROOT / "build"
REPORTS_DIR = _resolve_repo_path(
    os.getenv("CI_REPORT_DIR", str(BUILD_DIR / "reports")),
    REPO_ROOT
)
DEPS_DIR = REPO_ROOT / "data" / "deps"
ENRICHED_DIR = REPO_ROOT / "data" / "enriched"
PLANNING_DIR = REPO_ROOT / "data" / "planning"

# Standard build artifacts
PLANNING_INDEX_JSON = BUILD_DIR / "planning_index.json"
CANONICAL_ROADMAP_JSON = BUILD_DIR / "canonical_roadmap.json"
CANONICAL_ENRICHED_JSON = BUILD_DIR / "canonical_enriched.json"
DEPENDENCY_GRAPH_JSON = BUILD_DIR / "dependency_graph.json"
DEPENDENCY_GRAPH_RECOMPOSED_JSON = BUILD_DIR / "dependency_graph_recomposed.json"
MODULE_MAPPINGS_JSON = BUILD_DIR / "module_mappings.json"
DOCLINT_ROADMAP_JSON = BUILD_DIR / "doclint_roadmap.json"
PRIORITY_PROFILE_JSON = BUILD_DIR / "priority_profile.json"
INGESTED_METADATA_JSON = BUILD_DIR / "ingested_metadata.json"
GP_ROADMAP_SPPF_JSON = BUILD_DIR / "gp_roadmap_sppf.json"

# Source directories
SRC_DIR = REPO_ROOT / "src"
AGDA_DIR = SRC_DIR / "agda"
SCRIPTS_DIR = REPO_ROOT / "scripts"
TESTS_DIR = REPO_ROOT / "tests"
DOCS_DIR = REPO_ROOT / "docs"
INTAKE_DIR = REPO_ROOT / "intake"

# Standard documentation files
ROADMAP_MD = REPO_ROOT / "ROADMAP.md"
README_MD = REPO_ROOT / "README.md"
ARCHITECTURE_MD = REPO_ROOT / "ARCHITECTURE.md"


def get_module_path_from_name(module_name: str, base_dir: Path = None) -> Path:
    """Convert Agda module name to file path.
    
    Args:
        module_name: Dot-separated module name (e.g., "Plan.CIM.Utility")
        base_dir: Base directory for modules (defaults to AGDA_DIR)
        
    Returns:
        Path to .agda file
        
    Example:
        >>> get_module_path_from_name("Plan.CIM.Utility")
        Path('/path/to/src/agda/Plan/CIM/Utility.agda')
    """
    if base_dir is None:
        base_dir = AGDA_DIR
    
    parts = module_name.split('.')
    return base_dir / Path(*parts).with_suffix('.agda')


def get_module_name_from_path(file_path: Path, base_dir: Path = None) -> str:
    """Convert file path to Agda module name.
    
    Args:
        file_path: Path to .agda file
        base_dir: Base directory for modules (defaults to AGDA_DIR)
        
    Returns:
        Dot-separated module name
        
    Example:
        >>> get_module_name_from_path(Path("src/agda/Plan/CIM/Utility.agda"))
        'Plan.CIM.Utility'
    """
    if base_dir is None:
        base_dir = AGDA_DIR
    
    # Try to find 'agda' in path
    parts = file_path.parts
    if 'agda' in parts:
        idx = parts.index('agda')
        module_parts = parts[idx + 1:]
    else:
        # Try relative to base_dir
        try:
            rel_path = file_path.relative_to(base_dir)
            module_parts = rel_path.parts
        except ValueError:
            # Fall back to just filename
            module_parts = [file_path.stem]
    
    # Remove .agda extension from last part
    if module_parts and module_parts[-1].endswith('.agda'):
        module_parts = list(module_parts)
        module_parts[-1] = module_parts[-1][:-5]  # Remove .agda
    
    return '.'.join(module_parts)


def ensure_build_dirs() -> None:
    """Ensure all standard build directories exist."""
    for dir_path in [BUILD_DIR, REPORTS_DIR, DEPS_DIR, ENRICHED_DIR, PLANNING_DIR]:
        dir_path.mkdir(parents=True, exist_ok=True)

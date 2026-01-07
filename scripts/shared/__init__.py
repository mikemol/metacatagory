"""Shared utility modules for scripts.

This package provides common functionality used across multiple scripts.
Following SPPF model with 8 foundational and extended modules.

Core Modules (Foundational):
- errors: Structured error handling with recovery strategies
- logging: Structured logging (JSON/human-readable) replacing print statements
- config: Centralized configuration management with dependency injection
- validation: Schema validation with composable validators and error accumulation

Extended Modules (Specialized):
- pipelines: Composable phase abstraction for multi-step transformations
- agda: Agda module introspection, dependency analysis, coverage tracking
- markdown: Markdown parsing, building, and validation with frontmatter
- provenance: Traceability and lineage tracking for artifacts

Architecture:
- Type hints on all public APIs
- Dependency injection for testability
- Error handling via shared.errors
- Comprehensive docstrings and documentation
"""

__all__ = [
    'errors',
    'logging',
    'config',
    'validation',
    'pipelines',
    'agda',
    'markdown',
    'provenance',
]

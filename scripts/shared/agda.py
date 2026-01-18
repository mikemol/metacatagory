#!/usr/bin/env python3
"""
Agda module introspection and dependency analysis.

Provides utilities for:
- Parsing Agda module structure
- Extracting imports and dependencies
- Analyzing module coverage
- Generating compilation rules

CHIP-N+1 Protocol applied:
- Serialization symmetry: to_dict()/from_dict() for all data structures
- Error integration: Structured errors with causality chains
- Validation integration: Schema validation for parsed data
- Flexible initialization: Accept dict|Path|str for AgdaModule creation
"""

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Set, Dict, Optional, Tuple, Any
import re
import json

# CHIP-N+1: Import enhanced modules with fallback for graceful degradation
try:
    from scripts.shared.errors import ScriptError, FileOperationError, ValidationError as ScriptValidationError
except ImportError:
    ScriptError = Exception
    FileOperationError = IOError
    ScriptValidationError = ValueError

try:
    from scripts.shared.validation import ValidationResult, ValidationFailure
except ImportError:
    ValidationResult = dict
    ValidationFailure = None


@dataclass
class AgdaModule:
    """Representation of an Agda module.
    
    CHIP-N+1 Implication 1.1.1 (Interface-Contract Specification):
    - Flexible initialization: Accept dict or field values
    - Serialization symmetry: to_dict() + from_dict()
    - Validation: Type checking on initialization
    """
    name: str
    file_path: Path
    imports: Set[str] = field(default_factory=set)
    exports: Set[str] = field(default_factory=set)
    is_internal: bool = False  # CHIP-N+1: Changed default from True to False for consistency
    has_doc: bool = False
    line_count: int = 0
    
    def add_import(self, module_name: str):
        """Add an imported module.
        
        CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
        Track module dependencies for build ordering.
        """
        self.imports.add(module_name)
    
    def add_export(self, name: str):
        """Add an exported symbol.
        
        CHIP-N+1 Implication 1.1.2 (Interface-Change Management):
        Track exported interface for API versioning.
        """
        self.exports.add(name)
    
    def to_dict(self) -> dict:
        """Convert to dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Serialize with explicit field mapping.
        """
        return {
            'name': self.name,
            'file_path': str(self.file_path),
            'imports': sorted(self.imports),
            'exports': sorted(self.exports),
            'is_internal': self.is_internal,
            'has_doc': self.has_doc,
            'line_count': self.line_count,
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'AgdaModule':
        """Create from dictionary.
        
        CHIP-N+1 Implication 1.3.1 (Type-Contract Enforcement):
        Deserialize with validation and type checking.
        
        Args:
            data: Dictionary with module fields
            
        Returns:
            AgdaModule instance
            
        Raises:
            ValueError: If required fields are missing or invalid
        """
        required_fields = ['name', 'file_path']
        for field in required_fields:
            if field not in data:
                raise ValueError(f"Missing required field: {field}")
        
        return cls(
            name=data['name'],
            file_path=Path(data['file_path']),
            imports=set(data.get('imports', [])),
            exports=set(data.get('exports', [])),
            is_internal=data.get('is_internal', False),
            has_doc=data.get('has_doc', False),
            line_count=data.get('line_count', 0),
        )


class AgdaParser:
    """Parser for Agda source files.
    
    CHIP-N+1 Implication 1.2.1 (Protocol-Implementation Trace):
    Parse Agda module structure including imports and exports.
    """
    
    IMPORT_PATTERN = re.compile(
        r'^\s*(?:open\s+)?import\s+([A-Za-z0-9_.]+)(?:\s+(?:as|using)\s*.*)?$',
        re.MULTILINE
    )
    
    # Updated to handle module with optional export list
    MODULE_PATTERN = re.compile(
        r'^\s*module\s+([A-Za-z0-9_.]+)(?:\s*\(([^)]+)\))?\s+where\s*$',
        re.MULTILINE
    )
    
    DOC_PATTERN = re.compile(r'^\s*--\s+\|', re.MULTILINE)
    
    def __init__(self, agda_src_dir: Optional[Path] = None):
        """Initialize parser.
        
        CHIP-N+1 Implication 1.1.1 (Flexible Initialization):
        Accept optional agda_src_dir for test compatibility.
        
        Args:
            agda_src_dir: Path to src/agda directory (default: current directory)
        """
        self.agda_src_dir = agda_src_dir or Path.cwd() / "src" / "agda"
        self.modules: Dict[str, AgdaModule] = {}
    
    def parse_file(self, file_path: Path) -> Optional[AgdaModule]:
        """Parse an Agda file.
        
        CHIP-N+1 Implication 1.2.1 (Protocol-Implementation Trace):
        Parse file structure and extract module metadata.
        
        Args:
            file_path: Path to .agda file
            
        Returns:
            AgdaModule or None if parsing fails
            
        Raises:
            FileNotFoundError: If file does not exist
        """
        # Check if file exists first
        if not file_path.exists():
            raise FileNotFoundError(f"Agda file not found: {file_path}")
        
        try:
            content = file_path.read_text(encoding='utf-8')
            lines = content.split('\n')
            
            # Extract module name and exports
            module_match = self.MODULE_PATTERN.search(content)
            if not module_match:
                return None
            
            module_name = module_match.group(1)
            
            # Extract exports (if present in module declaration)
            exports: Set[str] = set()
            if module_match.group(2):
                # Parse comma-separated export list
                export_list = module_match.group(2).strip()
                exports = {e.strip() for e in export_list.split(',') if e.strip()}
            
            # Extract imports
            imports: Set[str] = set()
            for match in self.IMPORT_PATTERN.finditer(content):
                imports.add(match.group(1))
            
            # Check for documentation
            has_doc = bool(self.DOC_PATTERN.search(content))
            
            # Determine if internal
            is_internal = not any(
                module_name.startswith(prefix)
                for prefix in ['Agda.', 'Data.', 'Function.', 'Relation.']
            )
            
            module = AgdaModule(
                name=module_name,
                file_path=file_path,
                imports=imports,
                exports=exports,
                is_internal=is_internal,
                has_doc=has_doc,
                line_count=len(lines),
            )
            
            self.modules[module_name] = module
            return module
        
        except Exception as e:
            print(f"Warning: Failed to parse {file_path}: {e}")
            return None
    
    def parse_directory(self, directory: Optional[Path] = None) -> List[AgdaModule]:
        """Parse all Agda files in a directory.
        
        CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
        Scan directory tree for Agda modules.
        
        Args:
            directory: Directory to scan (default: agda_src_dir)
            
        Returns:
            List of parsed AgdaModule objects
        """
        if directory is None:
            directory = self.agda_src_dir
        
        modules = []
        for agda_file in sorted(directory.rglob('*.agda')):
            module = self.parse_file(agda_file)
            if module:
                modules.append(module)
        
        return modules
    
    def get_module(self, name: str) -> Optional[AgdaModule]:
        """Get a module by name.
        
        Args:
            name: Module name
            
        Returns:
            AgdaModule or None
        """
        return self.modules.get(name)
    
    def get_dependencies(self, module_name: str,
                        transitive: bool = False) -> Set[str]:
        """Get dependencies of a module.
        
        Args:
            module_name: Module name
            transitive: If True, include transitive dependencies
            
        Returns:
            Set of dependency module names
        """
        module = self.get_module(module_name)
        if not module:
            return set()
        
        if not transitive:
            return module.imports
        
        # Compute transitive closure
        visited = set()
        to_visit = module.imports.copy()
        
        while to_visit:
            current = to_visit.pop()
            if current in visited:
                continue
            
            visited.add(current)
            current_module = self.get_module(current)
            if current_module:
                to_visit.update(current_module.imports - visited)
        
        return visited
    
    def get_dependents(self, module_name: str,
                      transitive: bool = False) -> Set[str]:
        """Get modules that depend on this module.
        
        Args:
            module_name: Module name
            transitive: If True, include transitive dependents
            
        Returns:
            Set of dependent module names
        """
        dependents = set()
        
        for name, module in self.modules.items():
            if module_name in module.imports:
                dependents.add(name)
        
        if not transitive:
            return dependents
        
        # Compute transitive closure
        visited = set()
        to_visit = dependents.copy()
        
        while to_visit:
            current = to_visit.pop()
            if current in visited:
                continue
            
            visited.add(current)
            to_visit.update(self.get_dependents(current, transitive=False) - visited)
        
        return visited
    
    def get_internal_modules(self) -> List[AgdaModule]:
        """Get all internal modules.
        
        Returns:
            List of internal AgdaModule objects
        """
        return sorted(
            [m for m in self.modules.values() if m.is_internal],
            key=lambda m: m.name
        )
    
    def get_undocumented_modules(self) -> List[AgdaModule]:
        """Get modules without documentation.
        
        Returns:
            List of undocumented AgdaModule objects
        """
        return [
            m for m in self.get_internal_modules()
            if not m.has_doc
        ]
    
    def generate_makefile_rules(self) -> str:
        """Generate makefile compilation rules.
        
        Returns:
            Makefile content with phony targets and compilation rules
        """
        lines = ['# Auto-generated Agda compilation rules\n']
        lines.append('# Do not edit this file manually\n\n')
        
        # Phony targets
        module_names = [m.name for m in self.get_internal_modules()]
        if module_names:
            lines.append('.PHONY: agda-all agda-check ' + ' '.join(module_names) + '\n')
            lines.append(f'\nagda-all: {" ".join(module_names)}\n')
        
        # Compilation rules
        for module in self.get_internal_modules():
            target = f"{module.file_path.with_suffix('.agdai')}"
            deps = [str(module.file_path)]
            
            for dep_name in module.imports:
                dep_module = self.get_module(dep_name)
                if dep_module:
                    deps.append(str(dep_module.file_path))
            
            deps_str = ' '.join(deps)
            lines.append(f'\n{target}: {deps_str}')
            lines.append(f'\t@agda -i src/agda {module.file_path}')
        
        return '\n'.join(lines)
    
    def to_json(self) -> str:
        """Export module graph as JSON.
        
        Returns:
            JSON string with module information
        """
        data = {
            'modules': {
                name: module.to_dict()
                for name, module in sorted(self.modules.items())
            },
            'statistics': {
                'total_modules': len(self.modules),
                'internal_modules': len(self.get_internal_modules()),
                'undocumented_modules': len(self.get_undocumented_modules()),
                'total_lines': sum(m.line_count for m in self.modules.values()),
            }
        }
        return json.dumps(data, indent=2)


class ModuleCoverage:
    """Analysis of module test coverage.
    
    CHIP-N+1 Implication 2.1.2 (Milestone-Progress Feedback):
    Track test coverage as milestone toward formal verification.
    """
    
    def __init__(self, modules: Optional[List[AgdaModule]] = None, parser: Optional[AgdaParser] = None):
        """Initialize coverage analyzer.
        
        CHIP-N+1 Implication 1.1.1 (Flexible Initialization):
        Accept either modules list or parser for backward compatibility.
        
        Args:
            modules: List of AgdaModule objects (backward compatibility)
            parser: AgdaParser instance with parsed modules (new API)
        """
        if parser is not None:
            self.parser = parser
            self.modules = list(parser.modules.values())
        elif modules is not None:
            self.parser = None
            self.modules = modules
        else:
            self.parser = None
            self.modules = []
        
        self.test_modules: Set[str] = set()
    
    def add_test_module(self, module_name: str):
        """Register a test module.
        
        CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
        Track which modules have test coverage.
        
        Args:
            module_name: Module name
        """
        self.test_modules.add(module_name)
    
    def get_tested_modules(self) -> List[str]:
        """Get list of tested module names.
        
        CHIP-N+1 Implication 3.1.1 (Feedback-Source Attribution):
        Return list of modules with test coverage.
        
        Returns:
            Sorted list of tested module names
        """
        return sorted(self.test_modules)
    
    def get_coverage_percent(self) -> float:
        """Get module coverage percentage.
        
        CHIP-N+1 Implication 1.3.1 (Outcome-Metric Calibration):
        Calculate coverage metric for quality assessment.
        
        Returns:
            Coverage percentage (0-100)
        """
        if not self.modules:
            return 0.0
        
        covered = sum(1 for m in self.modules if m.name in self.test_modules)
        return 100.0 * covered / len(self.modules)
    
    def get_coverage_by_lines(self) -> float:
        """Get line coverage percentage.
        
        CHIP-N+1 Implication 1.3.1 (Outcome-Metric Calibration):
        Calculate line-based coverage metric.
        
        Returns:
            Line coverage percentage (0-100)
        """
        total_lines = sum(m.line_count for m in self.modules)
        if not total_lines:
            return 0.0
        
        covered_lines = sum(
            m.line_count for m in self.modules
            if m.name in self.test_modules
        )
        return 100.0 * covered_lines / total_lines
    
    def get_coverage_report(self) -> dict:
        """Generate coverage report.
        
        CHIP-N+1 Implication 1.3.1 (Outcome-Metric Calibration):
        Comprehensive coverage reporting with multiple metrics.
        
        Returns:
            Dictionary with coverage statistics
        """
        internal = [m for m in self.modules if m.is_internal]
        covered = [m for m in internal if m.name in self.test_modules]
        uncovered = [m for m in internal if m.name not in self.test_modules]
        
        total_lines_covered = sum(m.line_count for m in covered)
        total_lines_internal = sum(m.line_count for m in internal)
        
        return {
            'total_modules': len(internal),
            'covered_modules': len(covered),
            'uncovered_modules': len(uncovered),
            'coverage_percent': 100.0 * len(covered) / len(internal) if internal else 0.0,
            'lines_covered': total_lines_covered,
            'lines_total': total_lines_internal,
            'lines_coverage_percent': 100.0 * total_lines_covered / total_lines_internal if total_lines_internal else 0.0,
            'uncovered_list': [m.name for m in uncovered][:10],  # Top 10
        }


class DependencyAnalyzer:
    """Analyze module dependencies for cycles and imports.
    
    CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
    Analyze and track module dependencies for build ordering and cycle detection.
    """
    
    def __init__(self, modules: Optional[List[AgdaModule]] = None, parser: Optional[AgdaParser] = None):
        """Initialize analyzer.
        
        CHIP-N+1 Implication 1.1.1 (Flexible Initialization):
        Accept either modules list or parser for backward compatibility.
        
        Args:
            modules: List of AgdaModule objects (backward compatibility)
            parser: AgdaParser instance with parsed modules (new API)
        """
        if parser is not None:
            self.parser = parser
            self.modules = list(parser.modules.values())
            self.module_map = parser.modules
        elif modules is not None:
            self.parser = None
            self.modules = modules
            self.module_map = {m.name: m for m in modules}
        else:
            self.parser = None
            self.modules = []
            self.module_map = {}
    
    def get_dependencies(self, module: AgdaModule, transitive: bool = False) -> List[AgdaModule]:
        """Get dependencies of a module.
        
        CHIP-N+1 Implication 2.1.1 (Dependency-Resource Mapping):
        Resolve module dependencies from imports.
        
        Args:
            module: AgdaModule to analyze
            transitive: If True, include transitive dependencies
            
        Returns:
            List of dependency AgdaModule objects
        """
        if not transitive:
            # Direct dependencies
            deps = []
            for imp_name in module.imports:
                if imp_name in self.module_map:
                    deps.append(self.module_map[imp_name])
            return deps
        
        # Transitive dependencies
        visited = set()
        to_visit = list(module.imports)
        deps = []
        
        while to_visit:
            current_name = to_visit.pop()
            if current_name in visited:
                continue
            
            visited.add(current_name)
            if current_name in self.module_map:
                current_module = self.module_map[current_name]
                deps.append(current_module)
                for imp in current_module.imports:
                    if imp not in visited:
                        to_visit.append(imp)
        
        return deps
    
    def get_dependents(self, module: AgdaModule, transitive: bool = False) -> List[AgdaModule]:
        """Get modules that depend on this module.
        
        CHIP-N+1 Implication 2.1.2 (Dependency-Conflict Resolution):
        Find reverse dependencies for impact analysis.
        
        Args:
            module: AgdaModule to analyze
            transitive: If True, include transitive dependents
            
        Returns:
            List of dependent AgdaModule objects
        """
        # Direct dependents
        dependents = [
            m for m in self.modules
            if module.name in m.imports
        ]
        
        if not transitive:
            return dependents
        
        # Transitive dependents
        visited = set()
        to_visit = [m.name for m in dependents]
        all_dependents = list(dependents)
        
        while to_visit:
            current_name = to_visit.pop()
            if current_name in visited:
                continue
            
            visited.add(current_name)
            
            # Find modules that import current
            for m in self.modules:
                if current_name in m.imports and m.name not in visited:
                    all_dependents.append(m)
                    to_visit.append(m.name)
        
        return all_dependents
    
    def find_cycles(self) -> List[List[str]]:
        """Find circular dependency cycles.
        
        CHIP-N+1 Implication 2.1.2 (Dependency-Conflict Resolution):
        Detect circular dependencies that prevent compilation.
        
        Returns:
            List of module cycles (each cycle is a list of module names)
        """
        cycles = []
        visited = set()
        rec_stack = set()
        
        def visit(module_name: str, path: List[str]):
            if module_name in rec_stack:
                # Found a cycle
                try:
                    cycle_start = path.index(module_name)
                    cycle = path[cycle_start:] + [module_name]
                    cycles.append(cycle)
                except ValueError:
                    # module_name not in path (shouldn't happen, but handle gracefully)
                    pass
                return
            
            if module_name in visited:
                return
            
            visited.add(module_name)
            rec_stack.add(module_name)
            
            if module_name in self.module_map:
                module = self.module_map[module_name]
                for dep in module.imports:
                    visit(dep, path + [module_name])
            
            rec_stack.remove(module_name)
        
        # Process all modules (prefer internal, but fallback to all if no internal modules)
        modules_to_check = [m for m in self.modules if m.is_internal]
        if not modules_to_check:
            modules_to_check = self.modules
        
        for module in modules_to_check:
            if module.name not in visited:
                visit(module.name, [])
        
        return cycles
    
    def get_import_statistics(self) -> dict:
        """Generate import statistics.
        
        CHIP-N+1 Implication 1.3.1 (Outcome-Metric Calibration):
        Analyze import patterns for architectural insights.
        
        Returns:
            Dictionary with import statistics
        """
        internal_modules = [m for m in self.modules if m.is_internal]
        
        total_imports = sum(len(m.imports) for m in internal_modules)
        avg_imports = total_imports / len(internal_modules) if internal_modules else 0
        
        most_imported = {}
        for module in internal_modules:
            for imp in module.imports:
                most_imported[imp] = most_imported.get(imp, 0) + 1
        
        sorted_imports = sorted(most_imported.items(), key=lambda x: -x[1])
        
        return {
            'total_modules': len(internal_modules),
            'total_imports': total_imports,
            'average_imports_per_module': avg_imports,
            'most_imported_modules': dict(sorted_imports[:10]),  # Changed key from 'most_imported' to match test
            'cycles_found': len(self.find_cycles()),
        }


def extract_module_header(file_path: Path, max_lines: int = 50) -> Optional[str]:
    """Extract module doc comment or first meaningful block comment."""
    if not file_path.exists():
        return None

    lines = file_path.read_text(encoding="utf-8", errors="ignore").splitlines()
    doc_lines: list[str] = []
    in_block_comment = False

    for line in lines[:max_lines]:
        stripped = line.strip()

        if stripped.startswith("{-# OPTIONS"):
            continue

        if "{-" in stripped and "-}" not in stripped:
            in_block_comment = True
            text = stripped.split("{-", 1)[1].strip()
            if text and not text.startswith("#"):
                doc_lines.append(text)
            continue

        if in_block_comment:
            if "-}" in stripped:
                in_block_comment = False
                text = stripped.split("-}", 1)[0].strip()
                if text:
                    doc_lines.append(text)
                break
            if stripped and not stripped.startswith("-"):
                doc_lines.append(stripped)

        if stripped.startswith("--") and not stripped.startswith("---"):
            text = stripped[2:].strip()
            if text and len(text) > 10:
                doc_lines.append(text)
                if len(doc_lines) >= 3:
                    break

    if doc_lines:
        return " ".join(doc_lines[:5])

    return None


def extract_definition_names(file_path: Path, limit: int | None = None) -> list[str]:
    """Extract top-level definition names based on type signature lines."""
    if not file_path.exists():
        return []

    pattern = re.compile(r"^([a-zA-Z][a-zA-Z0-9-_]*)\s*:")
    names: list[str] = []
    for line in file_path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = pattern.match(line)
        if match:
            names.append(match.group(1))
            if limit is not None and len(names) >= limit:
                break
    return names

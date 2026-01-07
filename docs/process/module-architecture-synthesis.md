# Module Architecture Synthesis from Implication Tree

## Context

**Source**: [docs/process/python-behavioral-taxonomy.md](python-behavioral-taxonomy.md)  
**Phase**: CHIP-N+1 Reification - Synthesizing concrete module structure from 108 third-order implications  
**Test Baseline**: 248 test cases, 13 errors (to be analyzed)  
**Quality Mandate**: Ensure completeness, correctness, concreteness, depth, compliance, coherence, comprehensiveness, structure, meticulousness

---

## I. Module Architecture Overview

### Derived from SPPF Node Hierarchy + Implication Analysis

```
scripts/shared/
├── __init__.py                    # Package initialization
├── io.py                          # ✓ EXISTS - File I/O primitives
├── paths.py                       # ✓ EXISTS - Path manipulation
├── normalization.py               # ✓ EXISTS - String normalization
├── validation.py                  # NEW - Schema validation (from 1.4 implications)
├── pipelines.py                   # NEW - Composition patterns (from 2.1, 2.2 implications)
├── agda.py                        # NEW - Agda parsing utilities (from 2.2 implications)
├── markdown.py                    # NEW - Markdown generation/parsing (from 2.4 implications)
├── provenance.py                  # NEW - Provenance tracking (from 3.1 implications)
├── errors.py                      # NEW - Error handling strategies (from 3.2 implications)
├── logging.py                     # NEW - Structured logging (from 3.3 implications)
└── config.py                      # NEW - Configuration management (from 3.4 implications)
```

---

## II. Detailed Module Specifications

### 2.1 `shared/validation.py` - Schema Validation Module

**Source**: Third-order implications 1.4.1.1.1 through 1.4.3.3.3 (27 implications total)

**Purpose**: Centralize data validation with schema-driven approach

**Key Design Decisions** (from implication analysis):
- **Static + Runtime Typing**: Combine type hints (mypy) with runtime validation (1.1.1)
- **Error Accumulation**: Collect all validation errors before failing (2.2.1, 2.2.2)
- **Schema Language**: Use TypedDict + dataclasses for schema definition (3.1.1)
- **Pluggable Validators**: Support custom validation functions (1.2.2)

**Interface**:
```python
from typing import TypedDict, Protocol, Callable, List, Union, Any
from dataclasses import dataclass

@dataclass
class ValidationError:
    """Represents a single validation failure.
    
    Third-order sources: 3.2.3 (error context), 3.3.3 (recovery hints)
    """
    path: str  # JSON path to invalid field (e.g., "items[2].status")
    constraint: str  # Which constraint was violated
    value: Any  # The invalid value
    hint: str  # Suggested correction

class Schema(Protocol):
    """Protocol for validation schemas.
    
    Source: 3.1.1 (schema language), 3.1.2 (doc generation)
    """
    def validate(self, data: Any) -> List[ValidationError]:
        """Validate data against this schema."""
        ...
    
    def generate_example(self) -> Any:
        """Generate valid example data.
        
        Source: 3.2.1 (example generation for tests)
        """
        ...

def field_validator(
    field_type: type,
    required: bool = True,
    constraints: List[Callable[[Any], bool]] = None,
    default: Any = None
) -> Callable[[Any], List[ValidationError]]:
    """Create a field validator.
    
    Third-order sources:
    - 1.3.1: Required field checking
    - 1.2.1, 1.2.2, 1.2.3: Value constraint checking
    - 2.3.1: Selective validation (field-level)
    """
    ...

def struct_validator(
    fields: dict[str, Callable],
    allow_extra: bool = False
) -> Callable[[dict], List[ValidationError]]:
    """Create a struct validator for dictionaries.
    
    Third-order sources:
    - 1.3.2: Nested validation
    - 1.3.3: Cross-field constraints
    - 2.2.3: Validation strictness
    """
    ...

class ValidatedData:
    """Base class for validated data structures.
    
    Source: 2.1.2 (output validation), 2.1.3 (invariant checking)
    
    Usage:
        class RoadmapItem(ValidatedData):
            id: str
            title: str
            status: str
            
    The class automatically validates on construction and mutation.
    """
    ...

# Validation profiles for different use cases
class ValidationProfile:
    """Configurable validation behavior.
    
    Source: 2.3.1, 2.3.2, 2.3.3 (partial/incremental/selective validation)
    """
    strict: bool = True  # Reject invalid vs. coerce
    fail_fast: bool = True  # Stop on first error vs. collect all
    validate_on_write: bool = True  # Validate at save time
    validate_on_read: bool = False  # Validate at load time
```

**Test Contract** (from implication 3.2):
- Property-based tests generate random valid/invalid inputs
- Boundary tests check edge cases (empty strings, max lengths, etc.)
- Integration tests with real data from existing JSON files

---

### 2.2 `shared/pipelines.py` - Compositional Pipeline Module

**Source**: Second-order implications 2.1.1 through 2.1.3 (9 implications)  
**Third-order expansion**: 2.1.1.1 through 2.1.3.3.3 (27 implications total)

**Purpose**: Eliminate duplication of load→transform→save patterns across 25+ scripts

**Key Design Decisions** (from implication analysis):
- **Monad Pattern**: Use Result type for error handling (1.1.1)
- **Lazy Evaluation**: Generators for memory efficiency (1.2.1, 1.2.2)
- **Parallel Execution**: Support concurrent transformations (1.3.1, 1.3.2)
- **Composability**: Transformations are pure functions that compose (1.1.1, 2.2.3)

**Interface**:
```python
from typing import Callable, TypeVar, Generic, Union, Iterator
from dataclasses import dataclass
from pathlib import Path
from enum import Enum

T = TypeVar('T')
U = TypeVar('U')

@dataclass
class Success(Generic[T]):
    """Successful result.
    
    Source: 1.1.1 (Monad pattern for error handling)
    """
    value: T

@dataclass
class Failure(Generic[T]):
    """Failed result with error information.
    
    Source: 3.1.3 (error reporting with context)
    """
    error: Exception
    partial_result: T | None = None  # For partial success (3.3.1, 3.3.2)
    
Result = Union[Success[T], Failure[T]]

class TransformStrategy(Enum):
    """How to handle errors in transformations.
    
    Source: 3.2.1, 3.2.2, 3.2.3 (error recovery strategies)
    """
    FAIL_FAST = "fail_fast"  # Stop on first error
    COLLECT_ERRORS = "collect_errors"  # Continue, collect all errors
    SKIP_INVALID = "skip_invalid"  # Skip invalid records, process rest

class Pipeline(Generic[T, U]):
    """Composable data processing pipeline.
    
    Third-order sources:
    - 1.1.1: Functional composition
    - 1.2.1: Lazy evaluation via generators
    - 1.3.1, 1.3.2, 1.3.3: Parallel execution
    - 2.1.1, 2.1.2, 2.1.3: Unit testing of transformations
    - 3.1.1: Short-circuit evaluation
    - 3.2.1, 3.2.2, 3.2.3: Error recovery
    """
    
    def __init__(self, source: Callable[[], Iterator[T]]):
        """Initialize pipeline with data source."""
        self.source = source
        self.transforms: List[Callable[[T], Result[U]]] = []
        self.strategy = TransformStrategy.FAIL_FAST
    
    def map(self, func: Callable[[T], U]) -> 'Pipeline[T, U]':
        """Apply transformation to each element.
        
        Source: 2.2.2 (transformation reuse)
        """
        ...
    
    def filter(self, predicate: Callable[[T], bool]) -> 'Pipeline[T, T]':
        """Keep only elements matching predicate."""
        ...
    
    def validate(self, schema: Schema) -> 'Pipeline[T, T]':
        """Validate elements against schema.
        
        Source: Integration with validation.py module
        """
        ...
    
    def parallel(self, workers: int = None) -> 'Pipeline[T, U]':
        """Process elements in parallel.
        
        Source: 1.3.1 (data parallelism), 1.3.2 (task parallelism)
        """
        ...
    
    def with_provenance(self, source_name: str) -> 'Pipeline[T, T]':
        """Annotate elements with provenance.
        
        Source: Integration with provenance.py module
        """
        ...
    
    def collect(self) -> Result[List[U]]:
        """Execute pipeline and collect results.
        
        Source: Terminal operation that triggers lazy evaluation (1.2.1)
        """
        ...
    
    def save_json(self, path: Path, indent: int = 2) -> Result[None]:
        """Execute pipeline and save to JSON.
        
        Source: Integration with io.py module
        """
        ...

def json_pipeline(path: Path) -> Pipeline[dict, dict]:
    """Create pipeline starting from JSON file.
    
    Source: 25+ scripts use this pattern (from SPPF sharing analysis)
    
    Usage:
        result = (json_pipeline("input.json")
                  .map(normalize_item)
                  .filter(lambda x: x['status'] != 'completed')
                  .validate(roadmap_schema)
                  .with_provenance("my_script")
                  .save_json("output.json"))
    """
    ...

def agda_pipeline(path: Path, parser: Callable) -> Pipeline[str, dict]:
    """Create pipeline starting from Agda file.
    
    Source: Integration with agda.py module
    """
    ...
```

**Test Contract**:
- Unit tests for each pipeline operation (map, filter, validate, etc.)
- Integration tests reproducing existing script behavior
- Performance tests for parallel execution
- Property-based tests for transformation composition

---

### 2.3 `shared/agda.py` - Agda Parsing Utilities

**Source**: Second-order implications 2.2.1 through 2.2.3 (Agda parsing patterns)

**Purpose**: Unify Agda parsing logic across extract_roadmaps, ingest_gp_files, agda_makefile_deps

**Key Design Decisions**:
- **Regex + Structure**: Use regex for initial extraction, structured parsing for complex cases
- **Error Tolerance**: Gracefully handle malformed Agda (partial parsing)
- **Extensibility**: Support pluggable parsers for different Agda constructs

**Interface**:
```python
from typing import Dict, List, Callable, Iterator
from pathlib import Path
from dataclasses import dataclass
import re

@dataclass
class AgdaModule:
    """Represents a parsed Agda module."""
    name: str
    imports: List[str]
    records: List['AgdaRecord']
    functions: List['AgdaFunction']
    source_file: Path

@dataclass
class AgdaRecord:
    """Represents an Agda record definition."""
    name: str
    fields: Dict[str, str]  # field name -> type
    doc_comment: str | None

@dataclass
class AgdaFunction:
    """Represents an Agda function signature."""
    name: str
    type_signature: str
    doc_comment: str | None

class AgdaParser:
    """Configurable Agda parser.
    
    Uses regex patterns for common constructs, extensible via plugins.
    """
    
    def __init__(self):
        self.patterns = {
            'module': re.compile(r'module\s+(\S+)\s+where'),
            'import': re.compile(r'import\s+(\S+)'),
            'record': re.compile(r'record\s+(\w+)\s*:\s*Set\s+where'),
            # More patterns...
        }
        self.parsers: Dict[str, Callable] = {}
    
    def register_parser(self, construct: str, parser: Callable):
        """Register custom parser for Agda construct.
        
        Enables scripts to add domain-specific parsing logic.
        """
        self.parsers[construct] = parser
    
    def parse_file(self, path: Path) -> AgdaModule:
        """Parse Agda file into structured representation."""
        ...
    
    def extract_records(self, content: str) -> List[AgdaRecord]:
        """Extract record definitions from Agda source."""
        ...
    
    def extract_functions(self, content: str) -> List[AgdaFunction]:
        """Extract function signatures from Agda source."""
        ...
    
    def extract_roadmap_steps(self, content: str) -> List[Dict]:
        """Extract RoadmapStep records.
        
        Domain-specific parser for Plan.CIM modules.
        """
        ...

def parse_roadmap_step_record(match: re.Match) -> Dict:
    """Parse a single RoadmapStep record.
    
    Used by extract_roadmaps.py and merge_roadmaps.py
    """
    ...

def parse_module_dependencies(content: str) -> List[str]:
    """Extract module dependencies from import statements.
    
    Used by agda_makefile_deps.py and analyze_dependencies.py
    """
    ...
```

**Test Contract**:
- Unit tests with hand-crafted Agda snippets
- Integration tests with real Agda files from src/agda/
- Regression tests ensuring compatibility with existing parsers
- Error handling tests for malformed Agda

---

### 2.4 `shared/markdown.py` - Markdown Generation/Parsing

**Source**: Analysis of export_canonical_md, export_enriched_md, export_roadmap, normalize_generated_markdown

**Purpose**: Unify Markdown generation and parsing logic

**Interface**:
```python
from typing import List, Dict, Callable
from dataclasses import dataclass

@dataclass
class MarkdownSection:
    """Represents a section in a Markdown document."""
    level: int  # Heading level (1-6)
    title: str
    content: str
    subsections: List['MarkdownSection']

class MarkdownBuilder:
    """Fluent interface for building Markdown documents."""
    
    def heading(self, text: str, level: int = 1) -> 'MarkdownBuilder':
        """Add heading."""
        ...
    
    def paragraph(self, text: str) -> 'MarkdownBuilder':
        """Add paragraph."""
        ...
    
    def list_items(self, items: List[str], ordered: bool = False) -> 'MarkdownBuilder':
        """Add list."""
        ...
    
    def table(self, headers: List[str], rows: List[List[str]]) -> 'MarkdownBuilder':
        """Add table."""
        ...
    
    def code_block(self, code: str, language: str = "") -> 'MarkdownBuilder':
        """Add fenced code block."""
        ...
    
    def link(self, text: str, url: str) -> str:
        """Generate markdown link."""
        return f"[{text}]({url})"
    
    def build(self) -> str:
        """Generate final Markdown string."""
        ...

class MarkdownParser:
    """Parse Markdown into structured representation."""
    
    def parse(self, content: str) -> List[MarkdownSection]:
        """Parse Markdown into section hierarchy."""
        ...
    
    def extract_tables(self, content: str) -> List[List[List[str]]]:
        """Extract all tables from Markdown."""
        ...
    
    def extract_code_blocks(self, content: str) -> List[tuple[str, str]]:
        """Extract code blocks with their languages."""
        ...

def normalize_markdown(content: str) -> str:
    """Normalize Markdown for consistent formatting.
    
    Used by normalize_generated_markdown.py
    """
    ...
```

---

### 2.5 `shared/provenance.py` - Provenance Tracking

**Source**: Third-order implications from 3.1 (cross-cutting concern analysis)

**Purpose**: Centralize provenance tracking across data transformations

**Interface**:
```python
from typing import Any, Dict, List
from dataclasses import dataclass, field
from datetime import datetime

@dataclass
class ProvenanceRecord:
    """Records the origin and transformation history of data."""
    source: str  # Original source (file, module, user input)
    timestamp: datetime = field(default_factory=datetime.now)
    transformations: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def add_transformation(self, operation: str, params: Dict = None):
        """Record a transformation applied to this data."""
        entry = f"{operation}"
        if params:
            entry += f"({params})"
        self.transformations.append(entry)
    
    def format_provenance(self) -> str:
        """Format as ID|source string for storage.
        
        Compatible with existing provenance field format.
        """
        ...

class ProvenanceTracker:
    """Context manager for tracking provenance in a pipeline."""
    
    def __enter__(self):
        """Start tracking."""
        ...
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Finalize provenance records."""
        ...
    
    def track(self, item: Dict, source: str) -> Dict:
        """Add provenance to item."""
        ...

def ensure_provenance(item: Dict) -> None:
    """Ensure item has provenance field.
    
    DEPRECATED: Use ProvenanceTracker instead.
    Maintained for backward compatibility with merge_roadmaps.py
    """
    ...
```

---

### 2.6 `shared/errors.py` - Error Handling Strategies

**Source**: Third-order implications from 3.2 (error handling cross-cutting concern)

**Purpose**: Standardize error handling across all scripts

**Interface**:
```python
from typing import TypeVar, Callable, Union
from dataclasses import dataclass
from enum import Enum

T = TypeVar('T')

class ErrorSeverity(Enum):
    """Error severity levels."""
    WARNING = "warning"  # Recoverable, log and continue
    ERROR = "error"  # Serious but recoverable, retry or skip
    FATAL = "fatal"  # Unrecoverable, must terminate

@dataclass
class ScriptError(Exception):
    """Base class for script errors."""
    message: str
    severity: ErrorSeverity
    context: dict
    recoverable: bool = False
    
    def with_hint(self, hint: str) -> 'ScriptError':
        """Add recovery hint to error."""
        self.context['hint'] = hint
        return self

class FileNotFoundError(ScriptError):
    """File not found error with path context."""
    ...

class ValidationError(ScriptError):
    """Data validation error."""
    ...

class ParseError(ScriptError):
    """Parsing error with line/column context."""
    ...

def handle_errors(
    *,
    on_warning: Callable[[ScriptError], None] = None,
    on_error: Callable[[ScriptError], None] = None,
    on_fatal: Callable[[ScriptError], None] = None
) -> Callable:
    """Decorator for consistent error handling.
    
    Usage:
        @handle_errors(
            on_warning=log_warning,
            on_error=retry_operation,
            on_fatal=sys.exit
        )
        def my_function():
            ...
    """
    ...

def retry_on_error(
    max_attempts: int = 3,
    backoff: float = 1.0,
    exceptions: tuple = (Exception,)
) -> Callable:
    """Decorator to retry function on specified exceptions.
    
    Source: Third-order implication 3.2.1 (retry logic)
    """
    ...
```

---

### 2.7 `shared/logging.py` - Structured Logging

**Source**: Third-order implications from 3.3 (logging cross-cutting concern)

**Purpose**: Replace ad-hoc print statements with structured logging

**Interface**:
```python
import logging
from typing import Any, Dict
from pathlib import Path

class StructuredLogger:
    """Logger that emits structured (JSON) log events."""
    
    def __init__(self, name: str, level: int = logging.INFO):
        self.logger = logging.getLogger(name)
        self.logger.setLevel(level)
    
    def info(self, message: str, **context):
        """Log info message with structured context."""
        ...
    
    def warning(self, message: str, **context):
        """Log warning with context."""
        ...
    
    def error(self, message: str, exc_info: bool = False, **context):
        """Log error with context and optional exception."""
        ...
    
    def progress(self, current: int, total: int, message: str):
        """Log progress for long-running operations."""
        ...

def configure_logging(
    level: int = logging.INFO,
    log_file: Path = None,
    structured: bool = True
):
    """Configure logging for script."""
    ...

# Convenience functions for scripts that don't need full StructuredLogger
def log_info(message: str, **context):
    """Quick info log."""
    ...

def log_warning(message: str, **context):
    """Quick warning log."""
    ...

def log_error(message: str, **context):
    """Quick error log."""
    ...
```

---

### 2.8 `shared/config.py` - Configuration Management

**Source**: Third-order implications from 3.4 (configuration cross-cutting concern)

**Purpose**: Centralize configuration that's currently scattered across scripts

**Interface**:
```python
from pathlib import Path
from typing import Any, Dict
from dataclasses import dataclass, field

@dataclass
class Config:
    """Global configuration."""
    
    # Paths (from shared/paths.py)
    repo_root: Path
    build_dir: Path
    src_dir: Path
    
    # Behavior flags
    verbose: bool = False
    dry_run: bool = False
    parallel: bool = False
    workers: int = 4
    
    # Validation settings
    strict_validation: bool = True
    fail_on_warning: bool = False
    
    # I/O settings
    json_indent: int = 2
    create_parents: bool = True
    backup_on_overwrite: bool = False
    
    # Custom settings (extensible)
    custom: Dict[str, Any] = field(default_factory=dict)
    
    @classmethod
    def from_env(cls) -> 'Config':
        """Load configuration from environment variables."""
        ...
    
    @classmethod
    def from_file(cls, path: Path) -> 'Config':
        """Load configuration from YAML/JSON file."""
        ...
    
    def override(self, **kwargs) -> 'Config':
        """Create new config with overridden values."""
        ...

# Global config instance
config = Config.from_env()

def get_config() -> Config:
    """Get global configuration."""
    return config

def with_config(**overrides):
    """Decorator to override config for a function.
    
    Usage:
        @with_config(verbose=True, strict_validation=False)
        def my_function():
            ...
    """
    ...
```

---

## III. Refactoring Strategy

### Phase 1: Implement New Modules (Current Phase)
1. Create validation.py with comprehensive schema support
2. Create pipelines.py with Pipeline class and json_pipeline helper
3. Create agda.py with unified parsing utilities
4. Create markdown.py for generation/parsing
5. Create provenance.py for tracking
6. Create errors.py for standardized error handling
7. Create logging.py for structured logging
8. Create config.py for centralized configuration

### Phase 2: Refactor High-Impact Scripts
1. merge_roadmaps.py → Use Pipeline, validation, provenance
2. enrich_canonical.py → Use Pipeline, validation
3. export_roadmap.py → Use markdown, Pipeline
4. extract_roadmaps.py → Use agda, Pipeline
5. (Continue with remaining scripts...)

### Phase 3: Update Tests
1. Ensure all existing tests still pass
2. Add tests for new modules
3. Achieve >90% code coverage

### Phase 4: Documentation
1. Update ARCHITECTURE.md with new module structure
2. Create module-specific documentation
3. Add usage examples for each module

---

## IV. Success Metrics

**Quantitative**:
- Reduce total lines of code by 20-30%
- Achieve >90% test coverage
- Eliminate all code duplication (DRY violations)
- Zero regression in existing functionality

**Qualitative** (from quality mandates):
- **Completeness**: All behavioral patterns captured in shared modules
- **Correctness**: All tests pass, no regressions
- **Concreteness**: No "conceptual" implementations, all code is executable
- **Depth**: Full recursive decomposition following SPPF model
- **Compliance**: Adheres to architectural principles in ARCHITECTURE.md
- **Coherence**: Modules compose cleanly, interfaces are consistent
- **Comprehensiveness**: All scripts benefit from refactoring
- **Structure**: Clear SPPF hierarchy, well-documented dependencies
- **Meticulousness**: Every design decision traced to implication analysis

---

**Status**: Architecture specification complete, ready for implementation  
**Next**: Implement modules one at a time, starting with validation.py

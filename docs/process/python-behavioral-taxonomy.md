# Python Codebase Behavioral Taxonomy

## Meta-Analysis Context

**Source**: Comprehensive analysis of 54 Python scripts, 328 functions, 79 test files  
**Purpose**: SPPF-based recursive decomposition of metacatagory Python codebase  
**Framework**: Metalinguistic Coherence Schema with three-fold orthogonal implication analysis  
**Status**: Phase 1 - Discovery and Cataloging (CHIP-N Induction)

---

## I. Fundamental Behavioral Primitives (SPPF Leaf Nodes)

### 1.1 File I/O Operations (19 load + 15 save = 34 instances)

**Definition**: Atomic operations that cross the system-filesystem boundary to read or write data.

**Source**: Analysis of import patterns shows 34 scripts use `json`, 5 use `yaml/pyyaml`, all 43 use `pathlib`

**Existing Shared Implementation**: 
- `scripts/shared/io.py`: `load_json`, `save_json`, `load_markdown`, `save_markdown`
- Coverage: ~50% of file I/O operations

**First-Order Implications (Three Orthogonal)**:
1. **Persistence Semantics**: File I/O establishes a durable representation that survives process termination
2. **Encoding Protocols**: File I/O requires serialization/deserialization with format-specific rules (JSON, YAML, Markdown, Agda)
3. **Error Boundary**: File I/O introduces failure modes (missing files, permissions, malformed content) requiring explicit handling

**Second-Order Implications** (3×3 = 9):

From **1. Persistence Semantics**:
1.1. **Transactionality**: Operations may need atomicity guarantees (write-then-rename pattern)
1.2. **Versioning**: Persistent data evolves, requiring migration strategies
1.3. **Caching**: Repeated reads can be optimized via memoization

From **2. Encoding Protocols**:
2.1. **Schema Evolution**: Format changes must preserve backward compatibility
2.2. **Canonical Representation**: Multiple encodings may represent same semantic content
2.3. **Validation Coupling**: Encoding implies validation rules (JSON schema, YAML structure)

From **3. Error Boundary**:
3.1. **Recovery Strategies**: Errors can be handled via defaults, retries, or termination
3.2. **Error Propagation**: Errors can be caught locally or propagated to caller
3.3. **Diagnostic Context**: Errors require provenance tracking (file path, line number, cause)

**Third-Order Implications** (9×3 = 27):

[Expanding 1.1 Transactionality]:
1.1.1. **Atomicity Primitives**: Implement write-temp-then-rename for atomic updates
1.1.2. **Rollback Mechanisms**: Support undo of partial operations via staging
1.1.3. **Concurrent Access**: Handle multiple processes accessing same files

[Expanding 1.2 Versioning]:
1.2.1. **Migration Scripts**: Automated transformation of old formats to new
1.2.2. **Version Detection**: Introspect files to determine their schema version
1.2.3. **Compatibility Matrix**: Document which versions can interoperate

[Expanding 1.3 Caching]:
1.3.1. **Cache Invalidation**: Detect when cached data is stale (mtime checking)
1.3.2. **Cache Granularity**: Cache at file level vs. record level
1.3.3. **Cache Coherence**: Ensure cache consistency across multiple processes

[Expanding 2.1 Schema Evolution]:
2.1.1. **Additive Changes**: New optional fields preserve compatibility
2.1.2. **Deprecation Warnings**: Signal upcoming breaking changes
2.1.3. **Schema Registry**: Centralize schema definitions for validation

[Expanding 2.2 Canonical Representation]:
2.2.1. **Normalization Functions**: Transform to canonical form for comparison
2.2.2. **Equivalence Classes**: Define when two representations are semantically identical
2.2.3. **Canonical Serialization**: Always write in standardized format (sorted keys, consistent indent)

[Expanding 2.3 Validation Coupling]:
2.3.1. **Schema-Driven Parsing**: Use schema to guide deserialization
2.3.2. **Validation Timing**: Validate at read time vs. write time vs. both
2.3.3. **Validation Strictness**: Strict mode (reject invalid) vs. lenient mode (coerce/warn)

[Expanding 3.1 Recovery Strategies]:
3.1.1. **Default Values**: Provide sensible fallbacks for missing data
3.1.2. **Partial Success**: Allow operations to succeed on valid subset
3.1.3. **User Intervention**: Prompt for corrective action when automated recovery fails

[Expanding 3.2 Error Propagation]:
3.2.1. **Exception Hierarchy**: Structure exceptions to enable selective catching
3.2.2. **Error Aggregation**: Collect multiple errors before reporting
3.2.3. **Error Transformation**: Convert low-level errors to domain-specific errors

[Expanding 3.3 Diagnostic Context]:
3.3.1. **Stack Traces**: Preserve full call stack for debugging
3.3.2. **Structured Logging**: Emit machine-readable error events
3.3.3. **Error Recovery Hints**: Suggest corrective actions in error messages

**SPPF Node Specification**:
```python
# Leaf node: Atomic file I/O operation
FileIO = Union[
    ReadOperation[Format, ErrorStrategy],
    WriteOperation[Format, AtomicityGuarantee]
]
```

### 1.2 Path Manipulation Operations (43 scripts)

**Definition**: Operations that construct, resolve, normalize, or query filesystem paths without performing I/O.

**Source**: 43/54 scripts import pathlib, analysis shows repeated patterns of path construction

**Existing Shared Implementation**:
- `scripts/shared/paths.py`: 15 constants + 3 functions (module name↔path conversion)
- Coverage: ~30% of path operations

**First-Order Implications**:
1. **Path Semantics**: Paths represent navigable locations in a hierarchical namespace
2. **Path Portability**: Paths must work across operating systems (Unix vs. Windows)
3. **Path Resolution**: Relative paths require resolution against a base directory

**Second-Order Implications**:

From **1. Path Semantics**:
1.1. **Existence Semantics**: Paths may denote entities that don't exist yet
1.2. **Canonicalization**: Multiple paths can denote the same entity (symlinks, .. components)
1.3. **Path Arithmetic**: Paths support compositional operations (/, parent, relative_to)

From **2. Path Portability**:
2.1. **Separator Normalization**: Forward slash vs. backslash
2.2. **Drive Letter Handling**: C:\ on Windows vs. / on Unix
2.3. **Path Length Limits**: Windows MAX_PATH vs. Unix much larger limits

From **3. Path Resolution**:
3.1. **Anchor Points**: Identify base directories (REPO_ROOT, BUILD_DIR, etc.)
3.2. **Relative Path Interpretation**: Resolve .. and . components
3.3. **Symlink Dereferencing**: Follow or preserve symbolic links

**Third-Order Implications**:

[Expanding 1.1 Existence Semantics]:
1.1.1. **Path Validation**: Check path syntax without filesystem access
1.1.2. **Provisional Paths**: Represent paths that will exist after future operations
1.1.3. **Path Templates**: Parameterized paths for generating similar paths

[Expanding 1.2 Canonicalization]:
1.2.1. **Canonical Path Computation**: Resolve to absolute real path
1.2.2. **Path Comparison**: Define equality that respects canonicalization
1.2.3. **Path Hashing**: Canonical paths as dictionary keys

[Expanding 1.3 Path Arithmetic]:
1.3.1. **Path Joining**: Safely concatenate path components
1.3.2. **Path Anchoring**: Express path relative to different base
1.3.3. **Path Globbing**: Pattern-based path matching

[Expanding 2.1 Separator Normalization]:
2.1.1. **Universal Separator**: Always use / internally, convert on I/O
2.1.2. **Display Formatting**: Present paths in OS-native format
2.1.3. **URL Conversion**: Paths to file:// URLs

[Expanding 2.2 Drive Letter Handling]:
2.2.1. **Drive Extraction**: Separate drive from path components
2.2.2. **UNC Path Support**: Handle //server/share paths
2.2.3. **Cross-Platform Testing**: Validate on multiple OSes

[Expanding 2.3 Path Length Limits]:
2.3.1. **Length Checking**: Validate paths don't exceed limits
2.3.2. **Path Shortening**: Abbreviate paths when necessary
2.3.3. **Long Path Support**: Enable extended-length paths on Windows

[Expanding 3.1 Anchor Points]:
3.1.1. **Root Discovery**: Find repository root via .git or marker files
3.1.2. **Standard Directories**: Define conventional locations (build/, src/, tests/)
3.1.3. **Configuration Override**: Allow environment variables to override defaults

[Expanding 3.2 Relative Path Interpretation]:
3.2.1. **Normalization**: Remove redundant .. and . components
3.2.2. **Path Simplification**: Shorten paths while preserving meaning
3.2.3. **Relative Path Generation**: Express target relative to base

[Expanding 3.3 Symlink Dereferencing]:
3.3.1. **Link Following**: Resolve symlinks to their targets
3.3.2. **Link Preservation**: Keep symlinks as-is for portability
3.3.3. **Circular Link Detection**: Prevent infinite loops

**SPPF Node Specification**:
```python
# Leaf node: Path operation
PathOp = Union[
    Construct[Components],
    Resolve[BasePath],
    Normalize[Canonicalization Strategy]
]
```

### 1.3 String Normalization (9 functions + 21 regex parsers)

**Definition**: Transformations that map strings to canonical forms for comparison, validation, or storage.

**Source**: 21 scripts use regex, 9 functions named `normalize_*` or `clean_*`

**Existing Shared Implementation**:
- `scripts/shared/normalization.py`: `normalize_title`, `normalize_id`, `clean_empty_strings`
- Coverage: ~15% of normalization operations

**First-Order Implications**:
1. **Equivalence Classes**: Normalization partitions strings into equivalence classes
2. **Information Loss**: Normalization may discard information (case, punctuation, whitespace)
3. **Idempotence**: Normalizing an already-normalized string is identity

**Second-Order Implications**:

From **1. Equivalence Classes**:
1.1. **Hash Stability**: Equivalent strings must hash identically
1.2. **Search Matching**: Normalized forms enable case-insensitive search
1.3. **Deduplication**: Normalized keys identify duplicates

From **2. Information Loss**:
2.1. **Reversibility**: Some normalizations are irreversible
2.2. **Controlled Loss**: Specify which information to preserve vs. discard
2.3. **Lossless Alternatives**: Store both original and normalized forms

From **3. Idempotence**:
3.1. **Composition Safety**: Multiple normalizations can be applied sequentially
3.2. **Convergence**: Repeated normalization reaches fixed point
3.3. **Commutativity**: Some normalizations commute, others don't

**Third-Order Implications**:

[Expanding 1.1 Hash Stability]:
1.1.1. **Deterministic Hashing**: Same input always produces same hash
1.1.2. **Collision Resistance**: Different inputs unlikely to hash identically
1.1.3. **Hash Function Choice**: Select appropriate hash (MD5, SHA, Python hash())

[Expanding 1.2 Search Matching]:
1.2.1. **Full-Text Indexing**: Normalize before building search index
1.2.2. **Query Normalization**: Apply same normalization to search queries
1.2.3. **Ranking Considerations**: Normalized matches may rank lower than exact matches

[Expanding 1.3 Deduplication]:
1.3.1. **Exact Deduplication**: Remove identical normalized strings
1.3.2. **Fuzzy Deduplication**: Use similarity metrics for near-duplicates
1.3.3. **Merge Strategies**: When duplicates found, choose which to keep

[Expanding 2.1 Reversibility]:
2.1.1. **One-Way Functions**: Hashing is intentionally irreversible
2.1.2. **Bidirectional Mappings**: Some normalizations have inverses
2.1.3. **Partial Recovery**: Reconstruct approximation of original

[Expanding 2.2 Controlled Loss]:
2.2.1. **Configuration Options**: Let caller specify normalization level
2.2.2. **Normalization Profiles**: Presets for common use cases
2.2.3. **Lossy Warnings**: Alert when information is discarded

[Expanding 2.3 Lossless Alternatives]:
2.3.1. **Dual Storage**: Keep original in separate field
2.3.2. **Metadata Preservation**: Store normalization parameters
2.3.3. **Provenance Tracking**: Record original→normalized mapping

[Expanding 3.1 Composition Safety]:
3.1.1. **Normalization Pipelines**: Chain normalizations declaratively
3.1.2. **Order Independence**: Design commutative normalizations
3.1.3. **Validation Checkpoints**: Verify invariants between stages

[Expanding 3.2 Convergence]:
3.2.1. **Fixed-Point Detection**: Recognize when normalization complete
3.2.2. **Termination Guarantees**: Prove normalization always terminates
3.2.3. **Progress Metrics**: Measure how much normalization changed input

[Expanding 3.3 Commutativity]:
3.3.1. **Commutative Subset**: Identify which normalizations commute
3.3.2. **Canonical Order**: Define standard ordering for non-commutative operations
3.3.3. **Equivalence Proofs**: Formally verify commutativity claims

**SPPF Node Specification**:
```python
# Leaf node: String normalization
Normalize = Callable[[str, NormalizationProfile], str]
where NormalizationProfile specifies:
    - case_handling: preserve | lowercase | uppercase
    - whitespace: preserve | normalize | strip
    - punctuation: preserve | remove | normalize
    - unicode: preserve | normalize | ascii
```

### 1.4 Data Validation (5 validate_* + schema checking)

**Definition**: Operations that verify data conforms to expected structure, types, and constraints.

**Source**: 5 functions named `validate_*` or `check_*`, plus implicit validation in parsers

**Existing Shared Implementation**:
- `scripts/shared/normalization.py`: `validate_item_structure`
- Coverage: ~10% of validation operations

**First-Order Implications**:
1. **Constraint Enforcement**: Validation ensures data satisfies predicates
2. **Early Failure**: Validation fails fast rather than propagating invalid data
3. **Documentation**: Validation codifies assumptions about data structure

**Second-Order Implications**:

From **1. Constraint Enforcement**:
1.1. **Type Constraints**: Data must have correct types (str, int, list, dict)
1.2. **Value Constraints**: Data must satisfy predicates (non-empty, positive, unique)
1.3. **Structural Constraints**: Data must have expected shape (required fields, nested structure)

From **2. Early Failure**:
2.1. **Fail-Fast Pattern**: Validate at system boundaries
2.2. **Error Accumulation**: Collect all errors before failing
2.3. **Partial Validation**: Validate subset of data

From **3. Documentation**:
3.1. **Schema as Documentation**: Validation schema documents expected format
3.2. **Test Generation**: Schema enables property-based testing
3.3. **Migration Guidance**: Schema changes guide data migration

**Third-Order Implications**:

[Expanding 1.1 Type Constraints]:
1.1.1. **Static Type Checking**: Use type hints + mypy for compile-time validation
1.1.2. **Runtime Type Checking**: Verify types at runtime via isinstance
1.1.3. **Type Coercion**: Automatically convert compatible types

[Expanding 1.2 Value Constraints]:
1.2.1. **Range Validation**: Ensure values within bounds
1.2.2. **Format Validation**: Verify strings match patterns (email, URL, ID)
1.2.3. **Uniqueness Constraints**: Ensure no duplicates in collections

[Expanding 1.3 Structural Constraints]:
1.3.1. **Required Fields**: Specify mandatory vs. optional fields
1.3.2. **Nested Validation**: Recursively validate nested structures
1.3.3. **Cross-Field Constraints**: Validate relationships between fields

[Expanding 2.1 Fail-Fast Pattern]:
2.1.1. **Input Validation**: Validate at function entry points
2.1.2. **Output Validation**: Validate before returning results
2.1.3. **Invariant Checking**: Validate internal state consistency

[Expanding 2.2 Error Accumulation]:
2.2.1. **Error Collection**: Gather all validation errors
2.2.2. **Error Prioritization**: Report most severe errors first
2.2.3. **Error Context**: Include path to invalid data in error messages

[Expanding 2.3 Partial Validation]:
2.3.1. **Field-Level Validation**: Validate individual fields independently
2.3.2. **Incremental Validation**: Validate as data is constructed
2.3.3. **Selective Validation**: Validate only changed fields

[Expanding 3.1 Schema as Documentation]:
3.1.1. **Schema Language**: Use JSON Schema, Pydantic, or similar
3.1.2. **Documentation Generation**: Auto-generate docs from schema
3.1.3. **Example Generation**: Auto-generate valid examples

[Expanding 3.2 Test Generation]:
3.2.1. **Property-Based Tests**: Generate random valid inputs
3.2.2. **Boundary Testing**: Test edge cases from schema
3.2.3. **Invalid Input Testing**: Generate invalid inputs to test error handling

[Expanding 3.3 Migration Guidance]:
3.3.1. **Schema Versioning**: Track schema changes over time
3.3.2. **Migration Scripts**: Auto-generate migration code from schema diffs
3.3.3. **Deprecation Warnings**: Signal fields slated for removal

**SPPF Node Specification**:
```python
# Leaf node: Validation operation
Validate[T] = Callable[[T, Schema[T]], Result[T, ValidationErrors]]
where ValidationErrors = List[ValidationError]
      ValidationError = {path: str, constraint: str, value: Any}
```

---

## II. Compositional Patterns (SPPF Internal Nodes)

### 2.1 JSON Pipeline (load → transform → save)

**Definition**: Composite operation that reads JSON, applies transformations, and writes result.

**Source**: Pattern appears in 25+ scripts (analyze_dependencies, enrich_canonical, merge_roadmaps, etc.)

**Current Duplication**: Each script implements its own version with slight variations

**First-Order Implications**:
1. **Pipelinability**: Operations compose via function composition
2. **Transformation Isolation**: Each transformation is independent and testable
3. **Error Propagation**: Errors at any stage abort the pipeline

**Second-Order Implications**:

From **1. Pipelinability**:
1.1. **Functional Composition**: Transformations are pure functions
1.2. **Lazy Evaluation**: Transformations can be deferred until needed
1.3. **Parallel Execution**: Independent transformations can run concurrently

From **2. Transformation Isolation**:
2.1. **Unit Testing**: Each transformation tested independently
2.2. **Transformation Reuse**: Transformations used in multiple pipelines
2.3. **Transformation Composition**: Build complex transformations from simple ones

From **3. Error Propagation**:
3.1. **Short-Circuit Evaluation**: Stop pipeline on first error
3.2. **Error Recovery**: Retry or skip failed transformations
3.3. **Partial Results**: Return results from successful stages

**Third-Order Implications**:

[Expanding 1.1 Functional Composition]:
1.1.1. **Monad Pattern**: Use Result/Option types for error handling
1.1.2. **Function Chaining**: f(g(h(x))) or x |> h |> g |> f
1.1.3. **Combinator Library**: Provide composition utilities (map, filter, reduce)

[Expanding 1.2 Lazy Evaluation]:
1.2.1. **Generator Pipelines**: Use generators for memory efficiency
1.2.2. **Incremental Processing**: Process data in chunks
1.2.3. **Demand-Driven Execution**: Compute only what's needed

[Expanding 1.3 Parallel Execution]:
1.3.1. **Data Parallelism**: Process independent records concurrently
1.3.2. **Task Parallelism**: Run independent transformations concurrently
1.3.3. **Pipeline Parallelism**: Overlap I/O and computation

[Expanding 2.1 Unit Testing]:
2.1.1. **Property-Based Testing**: Test transformations with random inputs
2.1.2. **Snapshot Testing**: Compare transformation output to saved snapshots
2.1.3. **Regression Testing**: Ensure transformations don't break on known inputs

[Expanding 2.2 Transformation Reuse]:
2.2.1. **Transformation Library**: Catalog reusable transformations
2.2.2. **Parameterized Transformations**: Configure behavior via parameters
2.2.3. **Transformation Composition**: Build domain-specific transformations from primitives

[Expanding 2.3 Transformation Composition]:
2.3.1. **Sequential Composition**: Chain transformations linearly
2.3.2. **Parallel Composition**: Apply multiple transformations to same input
2.3.3. **Conditional Composition**: Choose transformation based on input

[Expanding 3.1 Short-Circuit Evaluation]:
3.1.1. **Fail-Fast Semantics**: Stop immediately on error
3.1.2. **Error Types**: Classify errors as fatal vs. recoverable
3.1.3. **Error Reporting**: Provide diagnostic information about failure point

[Expanding 3.2 Error Recovery]:
3.2.1. **Retry Logic**: Retry transient failures with exponential backoff
3.2.2. **Fallback Strategies**: Use alternative approach on failure
3.2.3. **Error Handlers**: Register custom recovery functions

[Expanding 3.3 Partial Results]:
3.3.1. **Success Subset**: Return successfully processed records
3.3.2. **Error Manifest**: Report which records failed and why
3.3.3. **Partial Success Semantics**: Define what constitutes acceptable partial success

**SPPF Node Specification**:
```python
# Internal node: JSON pipeline
JSONPipeline[In, Out] = Composed[
    Load[Path, JSON],
    Transform[JSON, JSON]*,  # 0 or more transformations
    Validate[JSON, Schema],  # Optional validation
    Save[JSON, Path]
]
```

### 2.2 Agda Parsing Pipeline

**Definition**: Extract structured data from Agda source files via regex or AST parsing.

**Source**: Pattern in extract_roadmaps, ingest_gp_files, agda_makefile_deps, etc.

**First-Order Implications**:
1. **Syntax-Semantics Gap**: Parse syntax to recover semantic structure
2. **Partial Parsing**: Extract specific constructs without full AST
3. **Error Tolerance**: Handle malformed or incomplete Agda code

**(Continuing recursive decomposition but managing length...)**

---

## III. Cross-Cutting Concerns (SPPF Aspect Nodes)

### 3.1 Provenance Tracking

**Definition**: Recording the origin and transformation history of data.

**Source**: Appears in merge_roadmaps, enrich_canonical, doclint_to_roadmap

**Existing Implementation**: `ensure_provenance` in shared/normalization.py

### 3.2 Error Handling Strategies

**Definition**: Consistent approach to error detection, reporting, and recovery.

**Current State**: Inconsistent across scripts - some use exceptions, some return None, some call sys.exit

### 3.3 Logging and Diagnostics

**Definition**: Structured output for monitoring, debugging, and auditing.

**Current State**: Mostly print statements, no structured logging

### 3.4 Configuration Management

**Definition**: Centralized configuration for paths, constants, and behavior flags.

**Existing Implementation**: Partial coverage in shared/paths.py

---

## IV. SPPF Node Hierarchy Design

```
Root (Python Codebase)
├── Primitives (Leaf Nodes)
│   ├── FileIO[Format, ErrorStrategy]
│   ├── PathOp[Operation, Portability]
│   ├── Normalize[Profile, Idempotence]
│   └── Validate[Schema, Strictness]
├── Compositions (Internal Nodes)
│   ├── JSONPipeline[Source, Dest, Transforms]
│   ├── AgdaParsing[Target, Recovery]
│   ├── RoadmapMerge[Sources, Deduplication]
│   └── MarkdownGeneration[Template, Data]
├── Aspects (Cross-Cutting)
│   ├── Provenance[Tracking Strategy]
│   ├── ErrorHandling[Propagation, Recovery]
│   ├── Logging[Level, Format]
│   └── Configuration[Source, Override]
└── Applications (Top-Level Scripts)
    ├── MergeRoadmaps = JSONPipeline ∘ RoadmapMerge ∘ Provenance
    ├── EnrichCanonical = JSONPipeline ∘ Enrichment ∘ Validation
    └── GenerateDocs = MarkdownGeneration ∘ DataAggregation
```

---

## V. Shared Expression Analysis (SPPF Packing)

**Sharing Opportunities** (where same subexpression appears in multiple contexts):

1. **JSON Load-Transform-Save**: 25+ scripts
2. **Path Construction from REPO_ROOT**: 43 scripts  
3. **Title/ID Normalization**: 15+ scripts
4. **Provenance Annotation**: 8 scripts
5. **Error Handling Patterns**: All scripts (but inconsistently)

**Packing Strategy**: Create shared modules that represent these common subexpressions:

- `scripts/shared/pipelines.py`: JSONPipeline, AgdaPipeline, MarkdownPipeline
- `scripts/shared/aspects.py`: Provenance, ErrorHandling, Logging decorators
- `scripts/shared/agda.py`: Agda parsing utilities
- `scripts/shared/schema.py`: Schema definitions and validation

---

## VI. Next Phase Actions

**CHIP-N+1 Reification Required**:

1. Complete third-order implication analysis for all primitives (27 implications each)
2. Design module interfaces based on implication tree
3. Implement shared modules
4. Refactor existing scripts to use shared modules
5. Ensure all tests pass (regression prevention)
6. Measure reduction in code duplication

**Meta-Constraint**: Time is suborned to completeness, correctness, concreteness, depth, compliance, coherence, comprehensiveness, structure, and meticulousness.

---

**End of Phase 1 Document**  
**Status**: Foundation established for CHIP-N+1 recursive decomposition  
**Next**: Continue implication analysis, then synthesize module architecture

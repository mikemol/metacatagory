# Metacatagory

A composable, formal system for semantic parsing and protocol management using categorical proof-driven architecture and topological semantics.

## Overview

Metacatagory unifies parsing, ambiguity resolution, and protocol composition through:
- **SPPF-based semantics**: Shared Packed Parse Forests as topological lattices
- **Categorical abstractions**: Functors, natural transformations, and universal properties
- **Proof-driven composition**: Witness objects and coherence proofs for protocol assembly
- **Roadmap-driven development**: Systematic integration via implication-driven roadmaps

## Key Architecture

- **src/agda/Core/** - Core categorical infrastructure (phases, functors, witnesses)
- **src/agda/Plan/CIM/** - Protocol records, utility functions, and roadmap structures
- **src/agda/Algebra/** - Algebraic field hierarchies and categorical foundations
- **src/agda/Examples/** - Runnable examples and test infrastructure

## Integrated Roadmaps

The following roadmap steps guide development and protocol composition:

### Formalize JSON transformation schema in Agda

**ID:** `BUILD-JSON-SCHEMA`

**Description:** Define record types for Monolithic and Hierarchical JSON representations, along with natural transformation laws. Prove isomorphism: backward ∘ forward ≡ id and structure preservation. Extended with adequacy framework: synthesize transformations from compositional primitives (json-get, json-set, json-merge) instead of monolithic Python FFI.

**Category:** Build Infrastructure

**Status:** `in-progress`

**Files:** src/agda/Plan/CIM/JSONTransformation.agda, src/agda/Plan/CIM/JSONTransformationAdequacy.agda, src/agda/Plan/CIM/JSONTransformationProofs.agda

**Tags:** JSON, FormalizationProof, CategoryTheory, Adequacy

### Decompose large JSON artifacts via natural transformations

**ID:** `BUILD-JSON-DECOMPOSITION`

**Description:** Implement hierarchical decomposition of monolithic build JSONs (dependency_graph.json, canonical_enriched.json, etc.) using category-theoretic natural transformations. Preserve losslessness and structure while improving granularity, diffability, and parallel build efficiency.

**Category:** Build Infrastructure

**Status:** `in-progress`

**Files:** src/agda/Plan/CIM/JSONTransformation.agda, scripts/json_decompose.py, scripts/json_recompose.py

**Tags:** JSON, NaturalTransformation, SPPF, Build

### Complete generic functor interface laws and helpers

**ID:** `LOCAL-GENERIC-FUNCTOR-INTERFACE-COMPLETENESS`

**Description:** Complete the generic functor interface laws and helpers to provide the coherence proofs downstream components expect.

**Category:** Infrastructure

**Status:** `in-progress`

**Files:** src/agda/Infrastructure/Functor/Interface.agda

**Tags:** Functor, Interface

### Introduce generic functor interface for protocol bundles

**ID:** `LOCAL-GENERIC-FUNCTOR`

**Description:** Establish the generic functor interface for protocol bundles so adapters share a single abstraction.

**Category:** Infrastructure

**Status:** `in-progress`

**Files:** src/agda/Infrastructure/Functor/Interface.agda, src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda, src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda, src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda, src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda, src/agda/Infrastructure/Functor/Instances/Ambiguity.agda, src/agda/Infrastructure/Functor/Compose.agda, src/agda/Core/PhaseCategoryWrapper.agda

**Tags:** Functor, Interface

### Add lint to enforce module-level docstrings

**ID:** `DOCS-AGDA-LINT`

**Description:** Add lint that enforces module-level docstrings to maintain documentation coverage.

**Category:** Infrastructure

**Status:** `in-progress`

**Files:** scripts, docs/automation

**Tags:** Documentation, Lint, Agda

### Design hierarchical JSON directory structure

**ID:** `BUILD-JSON-HIERARCHY`

**Description:** Document and implement the target hierarchy: dependency_graph → deps/, canonical_enriched → enriched/, planning_index → planning/ with _index.json manifests, metadata files, and aggregation rules.

**Category:** Build Infrastructure

**Status:** `done`

**Files:** docs/process/JSON-DECOMPOSITION.md, build/schemas/hierarchy.md

**Tags:** JSON, Documentation, Schema

### GP400: The "Elasticity" of Meaning

**ID:** `GP-Gp400`

**Description:** GP400: The "Elasticity" of Meaning. Source: Plan/CIM/IngestedRoadmaps/Corrections.agda. Category: IngestedGP. Tags: GP. Affects: Implementation.agda

**Category:** IngestedGP

**Status:** `done`

**Files:** src/agda/Plan/CIM/Elasticity.agda, src/agda/Plan/CIM/Implementation.agda

**Tags:** GP

### Normalize FunctionCategory functor instance

**ID:** `LOCAL-GENERIC-FUNCTOR-NORMALIZE-FUNCTION`

**Description:** Align the FunctionCategory instance with the generic functor interface so generic algorithms see a consistent surface.

**Category:** Infrastructure

**Status:** `done`

**Files:** src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda

**Tags:** Functor, Refactor

### Add regression examples exercising functor identity/compose

**ID:** `LOCAL-GENERIC-FUNCTOR-EXAMPLES`

**Description:** Add regression examples to demonstrate how the generic functor interface simplifies identity and compose reasoning.

**Category:** Infrastructure

**Status:** `done`

**Files:** src/agda/Examples/FunctorComposition.agda

**Tags:** Functor, Example

### Implement Definition Dictionary adequacy module

**ID:** `LOCAL-DEF-DICT`

**Description:** Package the Definition Dictionary adequacy module so downstream proofs rely on a canonical definition space.

**Category:** Infrastructure

**Status:** `done`

**Files:** src/agda/Infrastructure/Definitions/Dictionary.agda

**Tags:** Definition, Adequacy

## Development Workflow

1. **Review architecture** in [ARCHITECTURE.md](ARCHITECTURE.md) for design principles
2. **Check roadmap** in [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) for current integration points
3. **Follow contribution guidelines** in [CONTRIBUTING.md](CONTRIBUTING.md)
4. **Build and test** with `make agda-all` (requires Agda 2.6.4.3+)

## Building

```bash
# Full Agda compilation
make agda-all

# Generate documentation
make docs-all

# Lint markdown
make md-lint
```

## Build Topology

`make` is treated as the lazy evaluator for the roadmap automation. Use one
of the entry-point gravity wells (`check`, `priority-refresh`, `docs-all`,
`validate-constructive`) and let Make rebuild only the intermediates whose
inputs changed. The intermediate nodes (`roadmap-*`, lint helpers, priority
pipelines, etc.) are documented in [docs/process/BUILD-TOPOLOGY.md](docs/process/BUILD-TOPOLOGY.md)
so tooling and contributors alike know which high-level commands drive the
internal planning machinery.

## Navigation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Detailed architectural documentation and design patterns
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines and coding standards
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap and phase timeline
- **[COPILOT_SYNERGY.md](COPILOT_SYNERGY.md)** - LLM integration guidance

## License

See [LICENSE](LICENSE) for licensing information.

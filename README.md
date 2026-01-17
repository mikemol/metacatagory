# The Metacategory: A Homotopical Algebra System

## Formalizing the Axiom of Well-Founded Indexed Composition

## 1. Overview

This repository contains a formal verification and computational framework implemented in Agda. It unifies Abstract Algebra, Category Theory, and Constructive Algorithmics into a single, self-referential Directed Acyclic Graph (DAG).

The system is not merely a library of mathematical proofs; it is a mathematical operating system. It operates under the Curry-Howard-Lambek (CHL) correspondence, unifying lexical scope (syntax), semantic composition (logic), and categorical morphisms (structure).

### The Core Axioms

The architecture is strictly bound by the following formally internalized principles:

1. The Axiom of Well-Founded Indexed Composition: Every node N is assigned a static coordinate (x, y). A composite node N_n may only depend on constituents N_i where (x_i, y_i) < (x_n, y_n). This enforces a global DAG structure, preventing circular definitions.

2. The Axiom of Universal Reification: Every concept—whether a data structure, a proof, or an ambiguity—is reified as an indexed Identifier.

3. The Axiom of Gödelian Boundedness: The system explicitly models its own incompleteness. Limit objects (e.g., unprovable statements, infinite regress) are reified as nodes within the solution space.

## 2. The Ontological Stack

The system is stratified into phases, representing the evolution from static definitions to dynamic execution.

### Layer I: The Substrate (Core)

This layer defines the physics of the system.

- Metamodel.agda: Defines Identifier, Coordinate, and the ordering relation <ᶜ. This is the syntactic bedrock.

- Core.Phase: Reifies transformation as a first-class citizen. A Phase A B is a morphism in the Category of Phases, allowing for sequential and parallel composition of algorithmic steps.

- Core.GodelBoundary: A formal acknowledgment of the system's limits. It constructs witnesses for self-referential paradoxes and reifies the gap between the system and its self-model.

- Core.PathAggregator: Implements Homotopy Type Theory (HoTT) principles. It aggregates individual serialization roundtrips into a GlobalClosureWitness, proving that the system's coordinate geometry is invariant under transformation.

### Layer II: The Algebraic Pillars (Algebra)

This hierarchy builds the objects of the universe.

- Hierarchy: Magma → Semigroup → Monoid → Group → Ring → Field.

- Modules & Algebras: Extends rings to LeftModule, VectorSpace, and RAlgebra.

- Feature: These are not just typeclasses; they are deeply nested records containing Constructive Witnesses. A FieldExtension carries proofs of its degree, basis, and separability.

### Layer III: The Categorical Pillars (Chapter1, Chapter2, Chapter3)

This hierarchy builds the laws of the universe.

- Chapter1 (Fundamentals): Limits, Colimits, Adjunctions, Kan Extensions.

- Chapter2 (Structure): Abelian Categories, Regular Categories, Monads, Fibrations.

- Chapter3 (Topos Theory): Locales, Sheaves, Ω-sets.

- Deep Integration: These modules define Universal Properties. For example, KernelAsEqualizer defines the algebraic kernel strictly as a categorical limit.

## 3. The Unified Bridge: Taking the Product

The system's power lies in the intersection of the Algebraic and Categorical pillars. This is achieved via the Adapter Pattern.

### Core.CategoricalAdapter

This module provides a universal interface CategoricalAdapter T that wraps any algebraic structure T. It creates a morphism from the Unit type to T, effectively treating specific algebraic instances as objects in a generalized category.

### Tests.ObligationAdapters

This is the proving ground. It systematically maps algebraic constructs to categorical requirements.

- Example: It proves that an Algebra.Modules.Basic.KernelOfModuleHomomorphism satisfies the Chapter2.KernelAsEqualizerDefinition.

- Mechanism: It uses Indexed Adapters to carry the proof that status ≡ true, ensuring that every algebraic feature is categorically sound.

## 4. Development Roadmaps

## Development Roadmaps

- **Provide algebraic structures...** — Enables formal treatment... [status: not-started]
Target: `src/agda/Plan/CIM/Ambiguity.agda`

- **Establish metric structures...** — Enables quantitative... [status: not-started]
Target: `src/agda/Plan/CIM/Metricization.agda`

- **Define compositional...** — Enables systematic... [status: not-started]
Target: `src/agda/Plan/CIM/TransformationSystem.agda`

- **Implement functorial mappings...** — Enables formal... [status: not-started]
Target: `src/agda/Plan/CIM/FunctorialConstructs.agda`

- **Integrate 2D gating logic...** — Enables composable phase space... [status: not-started]
Target: `src/agda/Plan/CIM/Elasticity.agda`

- **Implement topological inflation...** — Enables composable category expansion... [status: not-started]
Target: `src/agda/Plan/CIM/PolytopeExpansion.agda`

- **Implement Mitosis Engine...** — Enables dynamic... [status: not-started]
Target: (deferred; historical `nedge_topology/mitosis.py` reference no longer present)

- **Integrate Earley parsing...** — Enables composable geometric... [status: not-started]
Target: (deferred; historical `nedge_topology/parser.py` reference no longer present)

## Intake Sources

Raw intake artifacts live under `intake/`. The canonical source set is
`intake/GP/`; everything else in `intake/` is archival context and snapshots.
See `intake/README.md` for guidance.

## Building

```text
make agda-all  # Compile all Agda modules
make docs-all  # Generate roadmap AST + module docs
make ROADMAP.md  # Regenerate roadmap markdown
make regen-all  # Regenerate all tracked artifacts
make check-all  # Full validation suite (alias: make check)
```

## Quickstart

```bash
mise install
MUTATE_LEVEL=repo make regen-makefile
# Full suite (strict roundtrip validation by default)
MUTATE_LEVEL=repo make check
# Or narrower targets:
# MUTATE_LEVEL=repo make json-roundtrip-validate
# MUTATE_LEVEL=report make check-docs
# For pytest/md targets, install dev deps (`mise run dev-setup`) and note network is required (or use the CI container).
```

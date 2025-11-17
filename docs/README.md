# MetaCategory Documentation

This directory contains auto-generated Markdown documentation from the Agda source modules. The docs are rebuilt and committed automatically by CI on each push.

## Quick Navigation

### Core Modules

* [Core](Core.md) – Foundational definitions, axioms, and the unified proof layer
* [Metamodel](Metamodel.md) – Meta-level constructs and identifiers
* [PropertyRegistry](PropertyRegistry.md) – Canonical property identifiers for compact queries
* [Core.PhaseCategory](Core.PhaseCategory.md) – Category of Phases (morphisms are `Phase A B`), with raw functors, raw natural transformations, and a monoidal (parallel) structure

### Guides & Overviews

* [Phase Category Guide](PhaseCategory.md) – Short overview and usage examples for `Core/PhaseCategory.agda`

### Chapter 1: The Language of Categories

* [Chapter 1 Index](Chapter1.Level1Index.md) – Entry point for Chapter 1
  * Categories, Functors, Natural Transformations
  * Limits and Colimits
  * Properties of Morphisms (Monos, Epis, Isomorphisms)
  * Duality Principle
  * Completeness and Universal Properties

### Chapter 2: Advanced Topics

* [Chapter 2 Index](Chapter2.Level2Index.md) – Entry point for Chapter 2
  * [Level 2.1](Chapter2.Level2sub1.md) – Abelian Categories
  * [Level 2.2](Chapter2.Level2sub2.md) – Exact Categories
  * [Level 2.3](Chapter2.Level2sub3.md) – Lawvere Theories and Algebraic Categories
  * [Level 2.4](Chapter2.Level2sub4.md) – Monads and Comonads
  * [Level 2.5](Chapter2.Level2sub5.md) – Accessible and Locally Presentable Categories
  * [Level 2.6](Chapter2.Level2sub6.md) – Enriched Category Theory
  * [Level 2.7](Chapter2.Level2sub7.md) – Topological Categories
  * [Level 2.8](Chapter2.Level2sub8.md) – Fibred Categories

### Chapter 3: Locales and Sheaves

* [Chapter 3 Index](Chapter3.Level3Index.md) – Entry point for Chapter 3
  * [Level 3.1](Chapter3.Level3sub1.md) – Locales (Intuitionistic Logic, Heyting Algebras, Frames)
  * [Level 3.2](Chapter3.Level3sub2.md) – Sheaves on Locales (Presheaves, Gluing Axioms, Grothendieck Toposes)

### Algebra: Hungerford's Graduate Algebra

* [Algebra Index](Algebra.Index.md) – Entry point for algebraic structures
  * [Foundation](Algebra.Foundation.md) – Magma, Semigroup, Monoid, Group, Abelian Group hierarchies
  * Groups, Rings, Modules, Fields (coming soon)

## Build Info

* **Source**: `src/agda/`
* **Generated via**: Agda HTML output → Pandoc → GitHub-Flavored Markdown
* **Build command**: `make docs-md`
* **CI**: Automatically updated on push to any branch

## Repository Structure

```text
src/agda/
├── Core.agda                 # Unified proof layer
├── Metamodel.agda            # Meta-level constructs
├── PropertyRegistry.agda     # Property identifiers
├── Algebra/
│   ├── Index.agda            # Algebra aggregator
│   ├── Foundation.agda       # Basic algebraic structures
│   ├── Groups/               # Group theory (Hungerford Ch I-II)
│   ├── Rings/                # Ring theory (Hungerford Ch III)
│   ├── Modules/              # Module theory (Hungerford Ch IV)
│   └── Fields/               # Field theory & Galois (Hungerford Ch V-VI)
├── Chapter1/
│   ├── Level1Index.agda      # Chapter 1 aggregator
│   └── Level1*.agda          # Individual modules
├── Chapter2/
│   ├── Level2Index.agda      # Chapter 2 aggregator
│   └── Level2sub*.agda       # Individual sections
└── Chapter3/
    ├── Level3Index.agda      # Chapter 3 aggregator
    └── Level3sub*.agda       # Individual sections
```

For typechecking and building locally, see the main [README](../README.md).

## Examples

Source examples for the Category of Phases can be found at:

* `src/agda/Examples/PhaseCategoryExamples.agda` – identity/compose/parallel on phases, a simple raw functor, and identity natural transformations.

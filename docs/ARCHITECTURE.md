# Architecture Overview

This document maps the repository’s architecture as of 2025‑11‑18. It connects the core layers, algebraic hierarchy, algorithm interfaces, universal properties, constructive witnesses, and the behavioral test suite with the project’s phased roadmap.

## Directory Layout

- `src/agda/Metamodel.agda`: Minimal self-contained metamodel
  - `Identifier`, `Coordinate`, `mkId`/`mkIdAt`, ordering `_ <ⁱ _` (lexicographic on coordinates)
- `src/agda/Core/`
  - `Phase.agda`, `PhaseCategory.agda`: Phase abstraction (pipelines), monoidal/category-of-phases
  - `UniversalProperties.agda`: UMP records (products, coproducts, equalizers, etc.)
  - `AlgebraicAlgorithms.agda`: Algorithm interfaces (minimal polynomial, splitting field, galois group, etc.)
  - `AlgorithmUniversality.agda`: Bridges algorithms ↔ universal properties (constructs UMPs from algs)
  - `AlgorithmCorrectness.agda`: Correctness obligations/witness framework
  - `ConstructiveWitnesses.agda`: Computable witnesses and verification helpers
  - `Algorithms/` (Bundle, Registry, FiniteFields, NumberFields, FunctionFields, External): Algorithm families and registries
  - `Adapter*`, `CategoricalAdapter.agda`: Adapter infrastructure for obligations
  - `AlgorithmComplexity.agda`: Complexity classes, annotations, and wrappers (Phase III.1)
  - `Limitations.agda`: Error-as-Specification carrier `LimitationEvidence` + `Outcome` (Phase III.3)
  - `Witnesses.agda`, `PolynomialsF2.agda`: Supporting witnesses and sample polynomial arithmetic
- `src/agda/Algebra/`
  - `Foundation.agda`: Magma→Semigroup→Monoid→Group→Abelian indices (`AlgebraIndex`) and declarations
  - `Rings/Basic.agda`: Rings through Fields hierarchy (records for ring-like structures)
  - `Fields/Basic.agda`, `Advanced.agda`: Field-theoretic constructions: `SplittingField`, `GaloisGroup`, etc.
  - `Groups/`, `Modules/`, `Enrichment.agda`, and chapter indexes
- `src/agda/Tests/`
  - Checklist suites for structures (Groups, Rings, Modules, Fields, etc.)
  - Behavioral suites: Algorithm composition, serialization, performance, error handling
  - Index aggregators: `Index.agda`, `Index_PhaseII.agda`
- `src/agda/Examples/`: Small programmatic examples and compositions

## Core Concepts

- Identifiers and Ordering
  - `Metamodel.Identifier` carries a static `Coordinate`; `_ <ⁱ _` enforces well‑founded DAG composition.
  - `mkIdAt` enables explicit coordinates; used in concrete normalization tests.

- Phase Abstraction
  - `Core.Phase`: total/partial transformations with composition; `PhaseCategory` provides categorical structure.

- Algorithms and Universal Properties
  - `Core.AlgebraicAlgorithms`: algorithm interfaces for algebraic constructions.
  - `Core.UniversalProperties`: categorical specs (UMPs) for products/coproducts/equalizers/etc.
  - `Core.AlgorithmUniversality`: witnesses algorithms implement the UMPs (bridge layer).

- Constructive Witnesses and Correctness
  - `Core.ConstructiveWitnesses`: constructive realizations (e.g., splitting fields with explicit roots), plus verification helpers.
  - `Core.AlgorithmCorrectness`: records for correctness obligations and evidence threading.

- Complexity and Limitations (Phase III)
  - `Core.AlgorithmComplexity`: `ComplexityClass` (Constant…Factorial, Unknown), `ComplexityAnnotation`, `AnnotatedAlgorithm`, and ordering `_ ≤ᶜ _`.
  - `Core.Limitations`: `LimitationEvidence` and `Outcome (ok | limit)`, with `acknowledge` helper.

## Algebraic Layer

- `Algebra.Foundation` provides declaration records with `index : AlgebraIndex` and re‑exports (`MonoidDeclaration`, `GroupDeclaration`, etc.).
- `Algebra.Rings.Basic` and `Algebra.Fields.Basic` define declarative structures used by algorithms and witnesses.
  - `SplittingField (F f)` includes `baseField`, `polynomial`, and `splittingField` (a `FieldDeclaration`).
  - `GaloisGroup (F E)` provides `automorphisms : Identifier` and `group : GroupDeclaration`.

## Behavioral Test Suite (Selected)

- Composition and Pipelines
  - `Tests/AlgorithmCompositionTests.agda`: multi‑step pipelines across phases.
    - Phase 10 (III.2): DAG compositional validation — `MinimalPolynomial → SplittingField → GaloisGroup`.
      - Ordering checks use `M.<ⁱ`; `GaloisGroup.automorphisms` supplies a final node identifier.
      - Splitting‑field node id is currently postulated; a real thread (adapter or record augmentation) is a known follow‑up.
      - A concrete coordinate‑based instance normalizes ordering to `true` via `refl`.

- Complexity and Performance
  - `Tests/PerformanceBoundaryTests.agda`: concrete complexity annotations and ordering proofs using `_ ≤ᶜ _` (Phase III.1).

- Error‑as‑Specification
  - `Tests/ErrorAsSpecificationTests.agda`: demonstrates capturing and acknowledging limitations into `Outcome` (Phase III.3 scaffold).

- Universal Properties and Witnesses
  - `Tests/UniversalPropertyTests.agda`: bridge from algorithms to UMPs, plus indexed composition checks.
  - `Tests/ConstructiveWitnessTests.agda`: build and validate constructive witnesses (e.g., splitting field with explicit roots).

## Phase Roadmap Status (Concise)

- Phase I (Foundational Indexing & Spec Validation): COMPLETE — indices threaded, spec validation re‑enabled (except Topos deferral).
- Phase II (Universal Property & Coherence): COMPLETE for focused index; `Tests/Index_PhaseII.agda` remains green.
- Phase III
  - III.1 Complexity Classification: COMPLETE — `Core/AlgorithmComplexity.agda`, tests updated.
  - III.2 DAG Validation: IN PROGRESS — Phase 10 added; Galois id fixed; splitting‑field id still postulated; concrete `refl` proofs added; pending branching/diamond DAG example and postulate removal.
  - III.3 Error‑as‑Spec: IN PROGRESS — core data type + focused test added; integration pending.
  - III.4 Serialization Roundtrip: PLANNED — identifier isomorphism tests.

## Known Follow‑ups

- Replace Phase 10 splitting‑field id `postulate` with a concrete identifier thread (adapter, registry, or record wrapper).
- Add branching (diamond) DAG composition example to Phase 10.
- Resolve `Tests/Index.agda` presheaf adapter issue (unrelated to Phase III work).
- Integrate `Limitations` into adapters and add rejection tests.

## How to Navigate

- Start with `Metamodel.agda` (identifiers and ordering), then `Core/Phase*.agda` (phase abstraction).
- Skim `Core/AlgebraicAlgorithms.agda` and `Core/UniversalProperties.agda` to understand the algorithm/spec interface.
- Review `Core/AlgorithmUniversality.agda` to see how algorithms realize UMPs.
- Explore `Core/ConstructiveWitnesses.agda` for concrete computational content.
- Read the targeted tests in `src/agda/Tests/` to see the system exercised by phase.

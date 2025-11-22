# Metacatagory

**Copyright 2025 Michael Mol**  
Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

---

## Overview

**Metacatagory** is a formal implementation of the **Unified Axiomatic Framework**, constructed in Agda. It is a constructive, self-reflexive system that reifies algebraic and categorical structures into a unified, coordinate-indexed Directed Acyclic Graph (DAG).

Unlike traditional mathematics libraries which aim merely to catalog theorems, Metacatagory is designed to model the *process* of mathematical knowledge construction itself. It rigorously enforces principles of universal reification, well-founded composition, and explicit boundary management (handling incompleteness and technical debt as first-class objects).

## The Axiomatic Foundation

The architecture of this repository is strictly governed by the following axiomatic principles. Every module and data structure is an exegetical expression of these mandates.

### Part I: Structure and Composition

**1. Universal Reification (`Metamodel.agda`)**
Every concept, proof, and phenomenon is reified as a unique, indexed node.
*   **Implementation:** The `Identifier` type wraps a static `Coordinate`, assigning a unique identity to every declaration.
*   **Ambiguity:** Ambiguity is not elided; it is modeled explicitly via constructive witnesses and disjoint union types, representing the "Packed Node" concept of an SPPF (Shared Packed Parse Forest).

**2. Well-Founded Indexed Composition (`Metamodel.agda`)**
To ensure the knowledge structure remains a Directed Acyclic Graph (DAG) free of circular definitions, every node is assigned an absolute coordinate `(x, y) ∈ ℕ × ℕ`.
*   **The Axiom:** A composite node $N_n$ may only be composed of constituents $N_i$ where index$(N_i) <$ index$(N_n)$.
*   **Ordering:** The partial order is defined non-lexicographically in `Metamodel.agda` to enforce strict causal dependency.

**3. Unified Syntax (CHL Unification)**
The system unifies lexical scope and semantic composition under the Curry-Howard-Lambek correspondence. The `Core.Phase` abstraction unifies syntactic transformations, logical proofs, and categorical morphisms into a single `Phase A B` type, representing a structure-preserving map between spaces.

### Part II: Metatheory and Growth

**4. Gödelian Boundedness (`Core/GodelBoundary.agda`)**
The system explicitly models its own limits. It acknowledges that its solution space $S_0$ is bounded by incompleteness.
*   **Reification of Limits:** Rather than leaving undecidable propositions as comments or errors, they are reified as `LimitObject`s (e.g., `totalSelfReflectionLimit`) within the system.
*   **The Gap:** Missing proofs are not voids; they are reified as `Gap` objects in `Core/Gap.agda`, containing formal `DebtAnnotation`s.

**5. Solution Space Expansion (`Core/GrowthMetrics.agda`)**
The objective goal of the system is the expansion of the solution space.
*   **Instrumentation:** The system tracks its own growth via `CoordinateAllocation` and `GrowthSnapshot`, providing metrics on phase density and expansion patterns.

### Part III: Process and Interaction

**6. Universal Construction (`Core/UniversalProperties.agda`)**
All relationships are defined by their universal properties (Limits, Colimits, Adjunctions). Algorithms are verified not just by output, but by producing witnesses that satisfy these categorical universals.

**7. Constructive Witnesses (`Core/ConstructiveWitnesses.agda`)**
Concepts are not merely asserted; they are constructed. A `ConstructiveMinimalPolynomial`, for example, carries not just the polynomial, but the computational path (coefficients, degree computation, root verification) required to produce it.

---

## Repository Structure

### 1. The Metamodel (`Metamodel.agda`)
The substrate of the system. Defines `Identifier`, `Coordinate`, and the fundamental ordering relations that enforce the DAG structure.

### 2. Core Infrastructure (`Core/`)
The engine allowing the system to reason about itself.
*   **`Phase.agda` & `PhaseCategory.agda`:** The fundamental unit of computation and composition. Phases are morphisms in a category, supporting profiling, invariants, and monadic composition.
*   **`Gap.agda` & `TechnicalDebt.agda`:** First-class support for tracking missing proofs (`Gap`) and prioritizing work (`DebtAnnotation`).
*   **`AlgorithmRegistry.agda`:** A type-safe dispatch mechanism using dependent pairs and lazy instance resolution to route requests to the most specific algorithm bundle (e.g., Finite Field vs. Number Field algorithms).

### 3. Algebraic Hierarchy (`Algebra/`)
A constructive formalization of abstract algebra, grounded in category theory.
*   **`Foundation.agda`:** Magmas through Groups.
*   **`Rings/`:** Rings, Ideals, UFDs, PIDs.
*   **`Fields/`:** Galois Theory, Splitting Fields, Algebraic Closure.
*   **`Modules/`:** Vector Spaces, Tensor Products, Homological Algebra basics.

### 4. Category Theory (`ChapterN/`)
A structural encoding of standard categorical texts (e.g., Borceux, Mac Lane), formalized to bridge the gap between "textbook math" and computable Agda types.
*   **`Chapter1`:** Categories, Functors, Natural Transformations.
*   **`Chapter2`:** Abelian Categories, Regular Categories, Monads, Fibrations.
*   **`Chapter3`:** Toposes, Locales, Sheaves.

### 5. Verification (`Tests/`)
The system verifies itself through a dual approach:
*   **Checklists:** Formal adapters (`Tests/ObligationAdapters.agda`) that map abstract requirements to concrete implementations, ensuring total coverage.
*   **Behavioral Tests:** Validates that algorithms (e.g., `MinimalPolynomialAlgorithm`) actually produce witnesses satisfying their universal properties.

---

## Key Mechanisms

### The Phase Abstraction
The `Phase` is the atom of the system's operation. It represents a transformation `A → B` that preserves invariants.
```agda
record Phase {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field transform : A → B
```
Phases can be composed (`_⟫_`), profiled (`ProfiledPhase`), and logically constrained (`PhaseWithInvariant`).

### Explicit Gaps
We do not use `postulate` to hide ignorance. We use `Core.Gap` to explicitly explicitly admit a gap in the proof graph, annotated with rationale and priority.
```agda
proofGap : {A : Set} → Gap A
proofGap = makeGap "proof-pending" "Formal proof construction pending"
```
This satisfies Axiom I.1 by reifying the *absence* of a proof as a distinct object.

### Automatic Evidence Dispatch
The system utilizes a "Lazy Hybrid" approach to algorithm dispatch (`Core/Algorithms/Registry.agda`). It allows the compiler to automatically find evidence (e.g., that a field is finite) via instance search, while using explicit data structures (`Classifiable`) to prevent circular dependency cycles during type checking.

---

## Usage

This system is a library of constructive mathematics. To verify the entire structure and ensuring all axioms hold:

```bash
agda --no-main -i . Tests/Index.agda
```

To export the Technical Debt registry to JSON for external analysis:

```bash
agda --compile --ghc-flag="-Wno-missing-methods" Examples/TechnicalDebtRegistry.agda
./Examples/TechnicalDebtRegistry
```
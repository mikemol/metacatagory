# The Metacategory: A Homotopical Algebra System

### *Formalizing the Axiom of Well-Founded Indexed Composition*

## 1. Overview

This repository contains a formal verification and computational framework implemented in **Agda**. It unifies Abstract Algebra, Category Theory, and Constructive Algorithmics into a single, self-referential Directed Acyclic Graph (DAG).

The system is not merely a library of mathematical proofs; it is a **mathematical operating system**. It operates under the **Curry-Howard-Lambek (CHL) correspondence**, unifying lexical scope (syntax), semantic composition (logic), and categorical morphisms (structure).

### The Core Axioms

The architecture is strictly bound by the following formally internalized principles:

1.  **The Axiom of Well-Founded Indexed Composition**: Every node $N$ is assigned a static coordinate $(x, y)$. A composite node $N\_n$ may only depend on constituents $N\_i$ where $(x\_i, y\_i) < (x\_n, y\_n)$. This enforces a global DAG structure, preventing circular definitions.
2.  **The Axiom of Universal Reification**: Every concept—whether a data structure, a proof, or an ambiguity—is reified as an indexed `Identifier`.
3.  **The Axiom of Gödelian Boundedness**: The system explicitly models its own incompleteness. Limit objects (e.g., unprovable statements, infinite regress) are reified as nodes within the solution space.

***

## 2. The Ontological Stack

The system is stratified into phases, representing the evolution from static definitions to dynamic execution.

### Layer I: The Substrate (`Core`)

This layer defines the physics of the system.

*   **`Metamodel.agda`**: Defines `Identifier`, `Coordinate`, and the ordering relation `<ᶜ`. This is the syntactic bedrock.
*   **`Core.Phase`**: Reifies "transformation" as a first-class citizen. A `Phase A B` is a morphism in the **Category of Phases**, allowing for sequential (`_⟫_`) and parallel (`_⊗_`) composition of algorithmic steps.
*   **`Core.GodelBoundary`**: A formal acknowledgment of the system's limits. It constructs witnesses for self-referential paradoxes and reifies the gap between the system and its self-model.
*   **`Core.PathAggregator`**: Implements **Homotopy Type Theory (HoTT)** principles. It aggregates individual serialization roundtrips into a `GlobalClosureWitness`, proving that the system's coordinate geometry is invariant under transformation (an isomorphism of paths).

### Layer II: The Algebraic Pillars (`Algebra`)

This hierarchy builds the "objects" of the universe.

*   **Hierarchy**: `Magma` $\to$ `Semigroup` $\to$ `Monoid` $\to$ `Group` $\to$ `Ring` $\to$ `Field`.
*   **Modules & Algebras**: Extends rings to `LeftModule`, `VectorSpace`, and `RAlgebra`.
*   **Feature**: These are not just typeclasses; they are deeply nested records containing **Constructive Witnesses**. A `FieldExtension` carries proofs of its degree, basis, and separability.

### Layer III: The Categorical Pillars (`Chapter1`, `Chapter2`, `Chapter3`)

This hierarchy builds the "laws" of the universe.

*   **`Chapter1` (Fundamentals)**: Limits, Colimits, Adjunctions, Kan Extensions.
*   **`Chapter2` (Structure)**: Abelian Categories, Regular Categories, Monads, Fibrations.
*   **`Chapter3` (Topos Theory)**: Locales, Sheaves, $\Omega$-sets.
*   **Deep Integration**: These modules define **Universal Properties**. For example, `KernelAsEqualizer` defines the algebraic kernel strictly as a categorical limit.

***

## 3. The Unified Bridge: Taking the Product

The system's power lies in the intersection of the Algebraic and Categorical pillars. This is achieved via the **Adapter Pattern**.

### `Core.CategoricalAdapter`

This module provides a universal interface `CategoricalAdapter T` that wraps any algebraic structure $T$. It creates a morphism from the Unit type to $T$, effectively treating specific algebraic instances as objects in a generalized category.

### `Tests.ObligationAdapters`

This is the proving ground. It systematically maps algebraic constructs to categorical requirements.

*   *Example*: It proves that an `Algebra.Modules.Basic.KernelOfModuleHomomorphism` satisfies the `Chapter2.Level2sub1.KernelAsEqualizerDefinition`.
*   *Mechanism*: It uses **Indexed Adapters** to carry the proof that `status ≡ true`, ensuring that every algebraic feature is categorically sound.

***

## 4. The Algorithmic Core

The system is constructive. It does not just assert existence; it computes.

### The Registry (`Core.Algorithms.Registry`)

A centralized dispatch mechanism that selects the most specific algorithm for a given problem based on field classification.

*   **The Problem**: Algorithms for Finite Fields, Number Fields, and Function Fields differ drastically.
*   **The Solution**: A **Lazy Hybrid Dispatch**.
    *   **Evidence**: `IsFiniteField F`, `IsNumberField F`.
    *   **Classification**: `FieldClassification F` (a dependent pair of Tag + Evidence).
    *   **Bundle**: `AlgorithmBundle F E` containing all relevant algorithms (MinPoly, GaloisGroup, etc.).
*   **Cycle Breaking**: Explicit instance construction prevents the infinite loops common in type-class resolution for mutually recursive algebraic structures.

### Constructive Witnesses (`Core.ConstructiveWitnesses`)

Algorithms return rich data structures called **Witnesses**.

*   **Non-Constructive**: "There exists a minimal polynomial."
*   **Constructive**: "Here is the polynomial $P$, a proof that $P(\alpha)=0$, a proof that $P$ is monic, and a proof that $P$ divides any other polynomial vanishing at $\alpha$."
*   **`Core.AlgorithmCorrectness`**: Defines the `CorrectnessCertificate`, creating a verified link between the computational output and the `Core.UniversalProperties` specification.

***

## 5. Directory Structure & Exegesis

```text
.
├── Algebra/                # The Object Hierarchy
│   ├── Foundation.agda     # Magma -> Group
│   ├── Rings/              # Ring Theory
│   ├── Fields/             # Galois Theory
│   ├── Groups/             # Structure & Free Groups
│   └── Modules/            # Linear Algebra & Homology
├── Chapter1/               # Category Theory: Foundations
│   └── ... (Limits, Adjunctions, Yoneda)
├── Chapter2/               # Category Theory: Structure
│   └── ... (Abelian, Monoidal, Fibrations, Monads)
├── Chapter3/               # Category Theory: Topos
│   └── ... (Locales, Sheaves)
├── Core/                   # The System Kernel
│   ├── Phase.agda          # Transformation Vector
│   ├── Metamodel.agda      # Coordinate System
│   ├── UniversalProperties # Categorical Interfaces
│   ├── ConstructiveWitness # Proofs-as-Data
│   ├── GodelBoundary.agda  # Metatheoretic Limits
│   ├── PathAggregator.agda # HoTT Closure
│   ├── GrowthMetrics.agda  # Telemetry
│   └── Algorithms/         # Dispatch & Registry
├── Examples/               # Concrete Instantiations
│   ├── FiniteField/        # GF(8) example
│   └── NumberField/        # Q(√2) example
└── Tests/                  # Validation & Verification
    ├── *Checklist.agda     # Phase Compliance Tests
    └── ObligationAdapters  # The Bridge Logic
```

***

## 6. Theoretical Metrics

### The Coherence Hierarchy Induction Principle

The system adheres to **Axiom III.3**. When ambiguity arises (e.g., multiple valid algorithms for `GaloisGroup`), the system reifies this ambiguity as a `(n+1)-cell`—a higher-order object (the `AlgorithmRegistry`) that mediates the choice based on specific evidence (`IsFiniteField`), encoding the structural preference as a geometric distance in the solution space.

### Error-as-Specification

Errors are not failures; they are **Boundaries**.

*   `Core.Limitations` defines `LimitationEvidence`.
*   An algorithm that cannot process a specific input does not crash; it returns a `limitedResult` containing a formal description of the boundary it encountered. This allows the system to map the "edges" of the computable solution space.

***

## 7. Building and Verifying

To verify the entire specificational tower, including the integrity of the phase boundaries and the categorical-algebraic bridge:

```bash
agda --no-main -i . Tests/Index.agda
```

This process performs the following:

1.  **Reification**: Instantiates all algebraic structures.
2.  **Adaptation**: Wraps them in categorical adapters.
3.  **Dispatch**: Verifies the algorithm registry resolves correctly.
4.  **Closure**: Aggregates all HoTT paths to prove global system consistency.

***

## 8. Interpreting Reports & Metrics

The repository generates several reports and metrics to help track technical debt, deferred items, and overall code health:

- **[top-offenders.md](.github/badges/top-offenders.md):** Lists files with the highest weighted technical debt. Use this to prioritize refactoring and documentation efforts.
- **Deferred Items & Technical Debt:** See [DEFERRED-TRACKING.md](DEFERRED-TRACKING.md) for a summary and guidance on addressing deferred work.
- **Automation & Metrics:** For details on how reports are generated and how to run them locally, see [.github/scripts/README.md](.github/scripts/README.md) and Makefile targets (`make badges`, `make deferred-items`, etc.).

Regularly review these reports to guide maintenance, refactoring, and documentation priorities.

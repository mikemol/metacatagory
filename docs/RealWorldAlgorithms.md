# Real-World Algorithm Implementations

## Overview

This guide shows how to encode small, practical algorithms as Phases and exercise them via a minimal test harness. The goal is to demonstrate how computation slots into the same categorical layer as your existing phases, witnesses, and correctness scaffolding.

We avoid external dependencies and use the minimal primitives already present (e.g., `ℕ` in `Core/Phase` and lists from `Agda.Builtin.List`).

## Implemented Examples

Module: `src/agda/Examples/RealWorldAlgorithms.agda`

* Natural-number arithmetic: `_+_`, `_*_`, `one`
* Factorial: `fact : ℕ → ℕ`
* Exponentiation: `pow : ℕ → ℕ → ℕ`
* List operations: append `_++_`, reverse `rev`
* Phases:
  * `factPhase : Phase ℕ ℕ` wrapping `fact`
  * `powPhase : Phase (ℕ × ℕ) ℕ` computing `pow (fst p) (snd p)`
  * `reversePhase : ∀ {A} → Phase (List A) (List A)` wrapping `rev`

These are deliberately small and total, relying on structural recursion so definitional equalities hold and examples reduce to `refl`.

## Usage Examples

See `src/agda/Tests/RealWorldAlgorithmsTests.agda` for simple checks:

* Factorial on small inputs reduces definally:
  * `factPhase $ₚ 0 ≡ 1`
  * `factPhase $ₚ 3 ≡ 3 * (2 * 1)`
* Exponentiation via a pair input:
  * `powPhase $ₚ (2 , 3) ≡ 8`
* List reverse as a Phase:
  * `reversePhase $ₚ [1,2,3] ≡ [3,2,1]`

All tests are computational equalities verified by `refl`.

## How to run

* Typecheck just the examples module:

```bash
agda --no-main -i src/agda src/agda/Examples/RealWorldAlgorithms.agda
```

* Run the unified test entry (includes these tests):

```bash
agda --no-main -i src/agda src/agda/Tests/Index.agda
```

## Extending further

* Euclidean GCD: Add a subtraction- or modulo-based version with structural recursion measures.
* Sorting: Insertion sort on lists of `ℕ` with a simple comparator; start with computational checks, later add correctness properties (sortedness, permutation).
* Graph traversals: Outline BFS/DFS over a simple adjacency-list type if/when a graph representation is added.

These can be integrated with the existing `ConstructiveWitnesses` and `AlgorithmCorrectness` frameworks by adding specifications and witnesses once the basic computation surface is in place.

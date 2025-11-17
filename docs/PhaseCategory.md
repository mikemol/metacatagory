# Category of Phases

## Overview

`Core/PhaseCategory.agda` shows that phases form a category where:

- Objects are types `A : Set ℓ`.
- Morphisms are phases `Phase A B`.
- Identity is `idPhase` and composition is sequencing `_⟫_`.
- Categorical laws (left/right identity, associativity) hold pointwise.

It also provides lightweight scaffolding for categorical structure around phases:

- Raw endofunctors on phases: a pair `(F₀, F₁)` mapping objects and morphisms.
- Functor laws as a separate record (preserve identity and composition).
- Raw natural transformations with separate naturality laws.
- A monoidal structure given by parallel composition: tensors on objects and morphisms, with unit, associator, and unitors.

This design keeps proofs definally simple (many equalities reduce to `refl`) while avoiding brittle definitional equality in composed functors or natural transformations by splitting structure from laws. Where necessary, small postulates express the law composition behavior to keep the framework lightweight and compilable without external libraries.

## What’s in the module

- Phase category (specialized): `phaseCategory : (ℓ : Level) → PhaseCategory ℓ`
- Raw functors: `RawPhaseFunctor` with `F₀` and `F₁`
- Functor laws: `PhaseFunctorLaws` with `F-id` and `F-comp`
- Identity and composition for raw functors: `idRawFunctor`, `composeRawFunctors`
- Raw natural transformations: `RawNaturalTransformation` with components `η`
- Natural transformation laws: `NaturalTransformationLaws`
- Identity, vertical, and horizontal composition for raw nats: `idRawNat`, `_∘ᵥʳ_`, `_∘ₕʳ_`
- Monoidal structure:
  - Objects tensor `_⊗₀_` and unit `Unit`
  - Morphisms tensor `_⊗₁_`
  - Associator `α`, inverse `α⁻¹`, left/right unitors `λᵤ`, `ρᵤ` and inverses

## Quick examples

See `src/agda/Examples/PhaseCategoryExamples.agda` for minimal demonstrations. Highlights:

- Identity and composition are pointwise equalities via `refl`.
- Parallel composition distributes over pairs; associator/unitors rearrange tuples.
- A tiny raw endofunctor `Dup` defined by `F₀ A = A ⊗₀ Unit` and `F₁ f = f ⊗₁ idPhase`.
- Identity raw natural transformation laws (`idRawNatLaws`) as a canonical naturality example.

```agda
open import Core.Phase
open import Core.PhaseCategory

identityExample : ∀ {A : Set ℓ} (a : A) → idPhase $ₚ a ≡ a
identityExample a = refl

compositionExample : ∀ {A B C}
  (f : Phase B C) (g : Phase A B) (a : A) →
  (g ⟫ f) $ₚ a ≡ f $ₚ (g $ₚ a)
compositionExample f g a = refl

parallelExample : ∀ {A B C D}
  (f : Phase A B) (g : Phase C D) (ac : A × C) →
  (f ⊗ g) $ₚ ac ≡ (f $ₚ fst ac , g $ₚ snd ac)
parallelExample f g ac = refl
```

## How to run

- Typecheck only the examples module:

```bash
agda --no-main -i src/agda src/agda/Examples/PhaseCategoryExamples.agda
```

- Run the unified test entry (includes a minimal runner that imports the examples):

```bash
agda --no-main -i src/agda src/agda/Tests/Index.agda
```

## Notes

- The module avoids a dependency on the Agda standard library. Products, unit, and tensors are provided minimally in `Core/Phase`.
- Composition laws for functors and natural transformations are postulated where definitional equalities would be fragile; proofs that matter for usage remain definally equal.

-- Examples.PhaseCategoryExamples: Demonstrations for the category of phases

module Examples.PhaseCategoryExamples where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Core.Phase using
  ( Phase
  ; mkPhase
  ; idPhase
  ; _⟫_
  ; _$ₚ_
  ; _×_
  ; _,_
  ; fst
  ; snd
  ; _⊗_
  )

open import Core.PhaseCategory using
  ( phaseCategory
  ; RawPhaseFunctor
  ; PhaseFunctorLaws
  ; idRawFunctor
  ; idFunctorLaws
  ; composeRawFunctors
  ; RawNaturalTransformation
  ; NaturalTransformationLaws
  ; idRawNat
  ; idRawNatLaws
  ; _∘ᵥʳ_
  ; _∘ₕʳ_
  ; _⊗₀_
  ; _⊗₁_
  ; Unit
  ; unit
  ; α
  ; α⁻¹
  ; λᵤ
  ; λᵤ⁻¹
  ; ρᵤ
  ; ρᵤ⁻¹
  )

-- Universe parameter (examples are generic over a level)
private
  variable ℓ : Level

-- ---------------------------------------------------------------------------
-- Identity and composition examples
-- ---------------------------------------------------------------------------

-- Identity behaves as expected
identityExample : ∀ {A : Set ℓ} (a : A) → idPhase $ₚ a ≡ a
identityExample a = refl

-- Composition uses categorical order: (f ∘ g) = g ⟫ f
compositionExample : ∀ {A B C : Set ℓ}
                   (f : Phase B C) (g : Phase A B) (a : A)
                 → (g ⟫ f) $ₚ a ≡ f $ₚ (g $ₚ a)
compositionExample f g a = refl

-- Associativity holds pointwise
associativityExample : ∀ {A B C D : Set ℓ}
                      (f : Phase A B) (g : Phase B C) (h : Phase C D)
                      (a : A)
                    → ((f ⟫ g) ⟫ h) $ₚ a ≡ (f ⟫ (g ⟫ h)) $ₚ a
associativityExample f g h a = refl

-- ---------------------------------------------------------------------------
-- Parallel composition (monoidal) examples
-- ---------------------------------------------------------------------------

-- Parallel composition on pairs
parallelExample : ∀ {A B C D : Set ℓ}
                 (f : Phase A B) (g : Phase C D) (ac : A × C)
               → (f ⊗ g) $ₚ ac ≡ (f $ₚ fst ac , g $ₚ snd ac)
parallelExample f g ac = refl

-- Associator witness rearranges tuples
associatorExample : ∀ {A B C : Set ℓ} (abc : (A ⊗₀ B) ⊗₀ C)
                  → (α $ₚ abc) ≡ (fst (fst abc) , (snd (fst abc) , snd abc))
associatorExample abc = refl

-- Left and right unitors
leftUnitorExample : ∀ {A : Set ℓ} (ua : Unit ⊗₀ A) → (λᵤ $ₚ ua) ≡ snd ua
leftUnitorExample ua = refl

rightUnitorExample : ∀ {A : Set ℓ} (au : A ⊗₀ Unit) → (ρᵤ $ₚ au) ≡ fst au
rightUnitorExample au = refl

-- ---------------------------------------------------------------------------
-- Raw functors and natural transformations
-- ---------------------------------------------------------------------------

-- A simple duplicating raw functor: F₀ A = A × Unit; F₁ f = f ⊗ id
Dup : ∀ {ℓ : Level} → RawPhaseFunctor ℓ ℓ
Dup {ℓ} = record
  { F₀ = λ A → A ⊗₀ Unit
  ; F₁ = λ {A} {B} f → f ⊗₁ idPhase
  }

-- Laws for Dup are not provided (they are separate and optional for examples)

-- Identity raw functor and its laws are available
idFunctorIsIdentity : ∀ {A : Set ℓ} (a : A) →
  RawPhaseFunctor.F₁ (idRawFunctor {ℓ}) {A} {A} idPhase $ₚ a ≡ idPhase $ₚ a
idFunctorIsIdentity a = refl

-- Composition of raw functors (Dup ∘ Id) on objects
composeRawExample-objects : ∀ {A : Set ℓ} →
  RawPhaseFunctor.F₀ (composeRawFunctors Dup (idRawFunctor {ℓ})) A ≡ (A ⊗₀ Unit)
composeRawExample-objects {A} = refl

-- Identity raw natural transformation satisfies naturality by idRawNatLaws
idRawNatNaturality : ∀ {A B : Set ℓ} (f : Phase A B) (a : A) →
  NaturalTransformationLaws.naturality (idRawNatLaws {F = idRawFunctor {ℓ}}) f a ≡ refl
idRawNatNaturality f a = refl

-- Vertical and horizontal composition of raw natural transformations (structural)
rawNatComposeVertical : ∀ {A : Set ℓ}
  → RawNaturalTransformation.η (idRawNat {F = idRawFunctor {ℓ}} ∘ᵥʳ idRawNat) A ≡ idPhase
rawNatComposeVertical {A} = refl

rawNatComposeHorizontal-objects : ∀ {A : Set ℓ}
  → RawPhaseFunctor.F₀ (composeRawFunctors (idRawFunctor {ℓ}) (idRawFunctor {ℓ})) A ≡ A
rawNatComposeHorizontal-objects {A} = refl

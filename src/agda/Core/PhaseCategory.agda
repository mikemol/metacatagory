{-# OPTIONS --without-K #-}
module Core.PhaseCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)

record Lift {α β : Level} (A : Set α) : Set (α ⊔ β) where
  constructor lift
  field lower : A
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Functor.Interface using (CategoryLike; FunctorInstance)
open import Core.Phase using (Phase; _⟫_; idPhase; _$ₚ_; mkPhase; _×_; _,_; fst; snd; _⊗_)

------------------------------------------------------------------------
-- Category of phases re-expressed with CategoryLike
------------------------------------------------------------------------

phaseCategory : ∀ {ℓ} → CategoryLike {lsuc ℓ} (Setℓ ℓ)
phaseCategory {ℓ} = record
  { Hom      = λ A B → Lift {α = ℓ} {β = lsuc ℓ} (Phase A B)
  ; id       = lift idPhase
  ; _∘_      = λ g f → lift (Lift.lower f ⟫ Lift.lower g)
  ; id-left  = λ f → refl
  ; id-right = λ f → refl
  ; assoc    = λ h g f → refl
  }

phaseIdentity : ∀ {ℓ} → FunctorInstance (phaseCategory {ℓ}) (phaseCategory {ℓ})
phaseIdentity = record
  { objMap      = λ A → A
  ; map         = λ f → f
  ; map-id      = refl
  ; map-compose = λ _ _ → refl
  }

-- ============================================================================
-- Functors between Phase Categories (raw structure + laws)
-- ============================================================================

-- Raw functor: object mapping and morphism mapping only
record RawPhaseFunctor (ℓ₁ ℓ₂ : Level) : Set (lsuc ℓ₁ ⊔ lsuc ℓ₂) where
  field
    F₀ : Set ℓ₁ → Set ℓ₂
    F₁ : ∀ {A B : Set ℓ₁} → Phase A B → Phase (F₀ A) (F₀ B)

-- Laws for a raw functor to be a functor
record PhaseFunctorLaws {ℓ₁ ℓ₂ : Level} (F : RawPhaseFunctor ℓ₁ ℓ₂)
       : Set (lsuc ℓ₁ ⊔ lsuc ℓ₂) where
  field
    -- Identity preservation (pointwise)
    F-id   : ∀ {A : Set ℓ₁} (a : RawPhaseFunctor.F₀ F A) →
             RawPhaseFunctor.F₁ F (idPhase {A = A}) $ₚ a ≡ idPhase $ₚ a
    -- Composition preservation (pointwise)
    F-comp : ∀ {A B C : Set ℓ₁} (f : Phase A B) (g : Phase B C)
             (a : RawPhaseFunctor.F₀ F A) →
             RawPhaseFunctor.F₁ F (f ⟫ g) $ₚ a ≡
             (RawPhaseFunctor.F₁ F f ⟫ RawPhaseFunctor.F₁ F g) $ₚ a

-- Identity raw functor
idRawFunctor : ∀ {ℓ : Level} → RawPhaseFunctor ℓ ℓ
idRawFunctor = record
  { F₀ = λ A → A
  ; F₁ = λ f → f
  }

-- Laws for identity functor
idFunctorLaws : ∀ {ℓ : Level} → PhaseFunctorLaws (idRawFunctor {ℓ})
idFunctorLaws = record
  { F-id   = λ a → refl
  ; F-comp = λ f g a → refl
  }

-- Composition of raw functors
composeRawFunctors : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level} →
                     RawPhaseFunctor ℓ₂ ℓ₃ → RawPhaseFunctor ℓ₁ ℓ₂ →
                     RawPhaseFunctor ℓ₁ ℓ₃
composeRawFunctors G F = record
  { F₀ = λ A → RawPhaseFunctor.F₀ G (RawPhaseFunctor.F₀ F A)
  ; F₁ = λ f → RawPhaseFunctor.F₁ G (RawPhaseFunctor.F₁ F f)
  }

-- Laws for composition (require laws for parts; postulated composition laws)
postulate
  composeFunctorLaws : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level}
                     → (G : RawPhaseFunctor ℓ₂ ℓ₃) (F : RawPhaseFunctor ℓ₁ ℓ₂)
                     → PhaseFunctorLaws G → PhaseFunctorLaws F
                     → PhaseFunctorLaws (composeRawFunctors G F)

-- Raw natural transformation (components only)
record RawNaturalTransformation {ℓ₁ ℓ₂ : Level}
                                (F G : RawPhaseFunctor ℓ₁ ℓ₂)
       : Set (lsuc ℓ₁ ⊔ lsuc ℓ₂) where
  field
    η : ∀ (A : Set ℓ₁) → Phase (RawPhaseFunctor.F₀ F A) (RawPhaseFunctor.F₀ G A)

-- Naturality laws for a raw natural transformation
record NaturalTransformationLaws {ℓ₁ ℓ₂ : Level}
                                 {F G : RawPhaseFunctor ℓ₁ ℓ₂}
                                 (η : RawNaturalTransformation F G)
       : Set (lsuc ℓ₁ ⊔ lsuc ℓ₂) where
  field
    naturality : ∀ {A B : Set ℓ₁} (f : Phase A B)
                 (a : RawPhaseFunctor.F₀ F A) →
                 (RawNaturalTransformation.η η A ⟫ RawPhaseFunctor.F₁ G f) $ₚ a ≡
                 (RawPhaseFunctor.F₁ F f ⟫ RawNaturalTransformation.η η B) $ₚ a

-- Identity raw natural transformation
idRawNat : ∀ {ℓ₁ ℓ₂ : Level} {F : RawPhaseFunctor ℓ₁ ℓ₂} →
           RawNaturalTransformation F F
idRawNat = record { η = λ A → idPhase }

-- Laws for identity natural transformation
idRawNatLaws : ∀ {ℓ₁ ℓ₂ : Level} {F : RawPhaseFunctor ℓ₁ ℓ₂}
             → NaturalTransformationLaws (idRawNat {F = F})
idRawNatLaws = record { naturality = λ f a → refl }

-- Vertical composition of raw natural transformations
_∘ᵥʳ_ : ∀ {ℓ₁ ℓ₂ : Level}
       {F G H : RawPhaseFunctor ℓ₁ ℓ₂}
       → RawNaturalTransformation G H
       → RawNaturalTransformation F G
       → RawNaturalTransformation F H
_∘ᵥʳ_ μ η = record
  { η = λ A → RawNaturalTransformation.η η A ⟫ RawNaturalTransformation.η μ A }

-- Horizontal composition of raw natural transformations
_∘ₕʳ_ : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level}
       {F G : RawPhaseFunctor ℓ₁ ℓ₂}
       {F' G' : RawPhaseFunctor ℓ₂ ℓ₃}
       → RawNaturalTransformation F' G'
       → RawNaturalTransformation F G
       → RawNaturalTransformation (composeRawFunctors F' F)
                                  (composeRawFunctors G' G)
_∘ₕʳ_ {_} {_} {_} {F} {G} {F'} {G'} μ η = record
  { η = λ A →
      let ηA = RawNaturalTransformation.η η A
          μGA = RawNaturalTransformation.η μ (RawPhaseFunctor.F₀ G A)
      in RawPhaseFunctor.F₁ F' ηA ⟫ μGA
  }

-- If inputs satisfy naturality, outputs do too (postulated laws)
postulate
  vertRawNatLaws : ∀ {ℓ₁ ℓ₂ : Level}
                 {F G H : RawPhaseFunctor ℓ₁ ℓ₂}
                 (μ : RawNaturalTransformation G H)
                 (η : RawNaturalTransformation F G)
                 → NaturalTransformationLaws μ
                 → NaturalTransformationLaws η
                 → NaturalTransformationLaws (μ ∘ᵥʳ η)

  horizRawNatLaws : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level}
                 {F G : RawPhaseFunctor ℓ₁ ℓ₂}
                 {F' G' : RawPhaseFunctor ℓ₂ ℓ₃}
                 (μ : RawNaturalTransformation F' G')
                 (η : RawNaturalTransformation F G)
                 → NaturalTransformationLaws μ
                 → NaturalTransformationLaws η
                 → NaturalTransformationLaws (μ ∘ₕʳ η)

-- ============================================================================
-- Monoidal structure (parallel composition)
-- ============================================================================

-- Tensor on objects uses the product defined in Core.Phase
_⊗₀_ : ∀ {ℓ : Level} → Set ℓ → Set ℓ → Set ℓ
A ⊗₀ B = A × B

infixr 7 _⊗₀_

-- Tensor on morphisms is parallel composition from Core.Phase
_⊗₁_ : ∀ {ℓ : Level} {A B C D : Set ℓ}
      → Phase A B → Phase C D → Phase (A ⊗₀ C) (B ⊗₀ D)
_⊗₁_ f g = f ⊗ g

infixr 7 _⊗₁_

-- Unit object for monoidal structure
data Unit {ℓ : Level} : Set ℓ where
  unit : Unit

-- Associator and its inverse
α : ∀ {ℓ : Level} {A B C : Set ℓ} → Phase ((A ⊗₀ B) ⊗₀ C) (A ⊗₀ (B ⊗₀ C))
α = mkPhase (λ { ((a , b) , c) → (a , (b , c)) })

α⁻¹ : ∀ {ℓ : Level} {A B C : Set ℓ} → Phase (A ⊗₀ (B ⊗₀ C)) ((A ⊗₀ B) ⊗₀ C)
α⁻¹ = mkPhase (λ { (a , (b , c)) → ((a , b) , c) })

-- Left and right unitors
λᵤ : ∀ {ℓ : Level} {A : Set ℓ} → Phase (Unit ⊗₀ A) A
λᵤ = mkPhase snd

λᵤ⁻¹ : ∀ {ℓ : Level} {A : Set ℓ} → Phase A (Unit ⊗₀ A)
λᵤ⁻¹ = mkPhase (λ a → (unit , a))

ρᵤ : ∀ {ℓ : Level} {A : Set ℓ} → Phase (A ⊗₀ Unit) A
ρᵤ = mkPhase fst

ρᵤ⁻¹ : ∀ {ℓ : Level} {A : Set ℓ} → Phase A (A ⊗₀ Unit)
ρᵤ⁻¹ = mkPhase (λ a → (a , unit))

-- | FunctionPathCategory: functions as morphisms with pointwise equality.
-- Provides a CategoryLike instance and pointwise identity/compose proofs.

module Infrastructure.Functor.Instances.FunctionPathCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Axiom.Adequacy using (PathAlgebra)
open import Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial

-- Simple lift to adjust universe for Hom
record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

-- Category of functions with pointwise equality
FunctionPathCategory : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (FunctionPathCategory {ℓ}) A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
CategoryLike.id       (FunctionPathCategory {ℓ}) = lift (λ x → x)
CategoryLike._∘_      (FunctionPathCategory {ℓ}) f g = lift (comp (Lift.lower f) (Lift.lower g))
  where
    comp : ∀ {A B C : Set ℓ} → (B → C) → (A → B) → A → C
    comp h k x = h (k x)
CategoryLike.id-left  (FunctionPathCategory {ℓ}) f = refl
CategoryLike.id-right (FunctionPathCategory {ℓ}) f = refl
CategoryLike.assoc    (FunctionPathCategory {ℓ}) h g f = refl

-- Identity functor via the generic IdentityFunctor
module FunctionPathIdentity {ℓ} where
  open IdentityFunctor (FunctionPathCategory {ℓ}) public
    using () renaming (identity to functionPathIdentityFunctor)

------------------------------------------------------------------------
-- Optional adequacy kit for FunctionPathCategory
------------------------------------------------------------------------

module FunctionPathAdequacy {ℓ} where
  open Infrastructure.Functor.Interface
  open import Infrastructure.Axiom.Adequacy
  open import Infrastructure.Axiom.Instance

  functionPathAlgebra : PathAlgebra {ℓV = lsuc ℓ} {ℓP = lsuc ℓ} (Set ℓ)
  PathAlgebra.Path functionPathAlgebra A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
  PathAlgebra._++_ functionPathAlgebra {A} {B} {C} f g =
    lift (λ x → Lift.lower g (Lift.lower f x))
  PathAlgebra.++-assoc functionPathAlgebra f g h = refl
  PathAlgebra.id functionPathAlgebra = CategoryLike.id (FunctionPathCategory {ℓ})
  PathAlgebra.id-left functionPathAlgebra p = refl
  PathAlgebra.id-right functionPathAlgebra p = refl

  record FunctionPathKit : Set (lsuc ℓ) where
    field
      A B : Set ℓ
      f   : Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)

  functionPathFace : FunctionPathKit → FramedFace functionPathAlgebra
  FramedFace.a    (functionPathFace k) = FunctionPathKit.A k
  FramedFace.b    (functionPathFace k) = FunctionPathKit.B k
  FramedFace.face (functionPathFace k) = record { lhs = FunctionPathKit.f k ; rhs = FunctionPathKit.f k }

  functionPathAxiomInstance : AxiomInstance functionPathAlgebra
  AxiomInstance.Kit   functionPathAxiomInstance = FunctionPathKit
  AxiomInstance.face  functionPathAxiomInstance = functionPathFace
  AxiomInstance.solve functionPathAxiomInstance _ = refl

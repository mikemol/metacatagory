{-# OPTIONS --without-K #-}

-- | FunctionCategory: CategoryLike instance for sets with functions as morphisms,
-- plus the identity functor via the generic interface.

module Infrastructure.Functor.Instances.FunctionCategory where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial

record Lift {ℓ ℓ'} (A : Set ℓ) : Set (ℓ ⊔ ℓ') where
  constructor lift
  field lower : A

FunctionCategory : ∀ {ℓ} → CategoryLike (Set ℓ)
CategoryLike.Hom      (FunctionCategory {ℓ}) A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
CategoryLike.id       (FunctionCategory {ℓ}) = lift (λ x → x)
CategoryLike._∘_      (FunctionCategory {ℓ}) f g = lift (comp (Lift.lower f) (Lift.lower g))
  where
    comp : ∀ {A B C : Set ℓ} → (B → C) → (A → B) → A → C
    comp h k x = h (k x)
CategoryLike.id-left  (FunctionCategory {ℓ}) _ = refl
CategoryLike.id-right (FunctionCategory {ℓ}) _ = refl
CategoryLike.assoc    (FunctionCategory {ℓ}) _ _ _ = refl

module FunctionIdentity {ℓ} where
  open IdentityFunctor (FunctionCategory {ℓ}) public
    using () renaming (identity to functionIdentityFunctor)

------------------------------------------------------------------------
-- Optional adequacy kit (parameterized to avoid forcing requirements)
------------------------------------------------------------------------

module FunctionCategoryAdequacy {ℓ} where
  open Infrastructure.Functor.Interface
  open import Infrastructure.Axiom.Adequacy
  open import Infrastructure.Axiom.Instance

  -- Path algebra mirroring the function category (paths are lifted functions).
  functionPathAlgebra : PathAlgebra {ℓV = lsuc ℓ} {ℓP = lsuc ℓ} (Set ℓ)
  PathAlgebra.Path functionPathAlgebra A B = Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)
  PathAlgebra._++_ functionPathAlgebra {A} {B} {C} f g =
    lift (λ x → Lift.lower g (Lift.lower f x))
  PathAlgebra.++-assoc functionPathAlgebra f g h = refl
  PathAlgebra.id functionPathAlgebra = CategoryLike.id (FunctionCategory {ℓ})
  PathAlgebra.id-left functionPathAlgebra p = refl
  PathAlgebra.id-right functionPathAlgebra p = refl

  -- Framed faces and trivial solver: each lifted function solves its own face.
  record FunctionKit : Set (lsuc ℓ) where
    field
      A B : Set ℓ
      f   : Lift {ℓ = ℓ} {ℓ' = lsuc ℓ} (A → B)

  functionFace : FunctionKit → FramedFace functionPathAlgebra
  FramedFace.a    (functionFace k) = FunctionKit.A k
  FramedFace.b    (functionFace k) = FunctionKit.B k
  FramedFace.face (functionFace k) = record { lhs = FunctionKit.f k ; rhs = FunctionKit.f k }

  functionAxiomInstance : AxiomInstance functionPathAlgebra
  AxiomInstance.Kit   functionAxiomInstance = FunctionKit
  AxiomInstance.face  functionAxiomInstance = functionFace
  AxiomInstance.solve functionAxiomInstance _ = refl

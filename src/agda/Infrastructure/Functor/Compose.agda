{-# OPTIONS --without-K #-}

-- | Functor composition helper: compose two FunctorInstance values
-- sharing a middle category, with proofs for identity and composition.

module Infrastructure.Functor.Compose where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Interface
open import Infrastructure.Equality using (cong; trans)

composeFunctor : ∀ {ℓ₁ ℓ₂ ℓ₃}
  {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂} {Obj₃ : Set ℓ₃} →
  (C : CategoryLike Obj₁) (D : CategoryLike Obj₂) (E : CategoryLike Obj₃) →
  FunctorInstance C D → FunctorInstance D E → FunctorInstance C E
composeFunctor C D E F G = record
  { objMap = λ A → FunctorInstance.objMap G (FunctorInstance.objMap F A)
  ; map    = λ f → FunctorInstance.map G (FunctorInstance.map F f)
  ; map-id = λ {A} →
      trans (cong (FunctorInstance.map G) (FunctorInstance.map-id F {A = A}))
            (FunctorInstance.map-id G {A = FunctorInstance.objMap F A})
  ; map-compose = λ g f →
      trans (cong (FunctorInstance.map G) (FunctorInstance.map-compose F g f))
            (FunctorInstance.map-compose G (FunctorInstance.map F g) (FunctorInstance.map F f))
  }

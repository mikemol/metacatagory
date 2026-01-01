{-# OPTIONS --without-K #-}

-- | Functor composition helper: compose two FunctorInstance values
-- sharing a middle category, with proofs for identity and composition.

module Infrastructure.Functor.Compose where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Infrastructure.Functor.Interface

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
  where
    cong : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} {x y : A} (f : A → B) → x ≡ y → f x ≡ f y
    cong f refl = refl
    trans : ∀ {ℓ} {A : Set ℓ} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
    trans refl refl = refl

{-# OPTIONS --without-K #-}

-- | FunctorInterface: generic functor specification for category-like bundles.
-- This is intentionally minimal: it works with any record that supplies
-- objects, morphisms, identity, and composition, and it encodes the usual
-- identity and composition preservation laws.

module Infrastructure.Functor.Interface where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_)
open import Infrastructure.Equality using (cong)

------------------------------------------------------------------------
-- Category-like structure (lightweight to avoid pulling a full library)
------------------------------------------------------------------------

-- | Minimal category signature: objects, morphisms, identities, and composition laws.
record CategoryLike {ℓ : Level} (Obj : Set ℓ) : Set (lsuc ℓ) where
  field
    Hom      : Obj → Obj → Set ℓ
    id       : ∀ {A} → Hom A A
    _∘_      : ∀ {A B C} → Hom B C → Hom A B → Hom A C
    id-left  : ∀ {A B} (f : Hom A B) → _∘_ id f ≡ f
    id-right : ∀ {A B} (f : Hom A B) → _∘_ f id ≡ f
    assoc    : ∀ {A B C D} (h : Hom C D) (g : Hom B C) (f : Hom A B) →
                 _∘_ h (_∘_ g f) ≡ _∘_ (_∘_ h g) f

open CategoryLike public

------------------------------------------------------------------------
-- Functor specification
------------------------------------------------------------------------

-- | Structure-preserving mapping between two CategoryLike bundles.
record FunctorInstance
  {ℓ₁ ℓ₂ : Level}
  {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
  (C : CategoryLike Obj₁) (D : CategoryLike Obj₂)
  : Set (lsuc (ℓ₁ ⊔ ℓ₂)) where
  private
    module C = CategoryLike C renaming (Hom to Hom₁; id to id₁; _∘_ to _∘₁_)
    module D = CategoryLike D renaming (Hom to Hom₂; id to id₂; _∘_ to _∘₂_)

  field
    objMap      : Obj₁ → Obj₂
    map         : ∀ {A B} → C.Hom₁ A B → D.Hom₂ (objMap A) (objMap B)
    map-id      : ∀ {A} → map C.id₁ ≡ D.id₂ {objMap A}
    map-compose : ∀ {A B C} (g : C.Hom₁ B C) (f : C.Hom₁ A B) →
                    map (C._∘₁_ g f) ≡ D._∘₂_ (map g) (map f)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

map-cong :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D)
    {A B : Obj₁} {f g : CategoryLike.Hom C A B} →
    f ≡ g → FunctorInstance.map F f ≡ FunctorInstance.map F g
map-cong F {A} {B} {f} {g} p =
  cong (FunctorInstance.map F {A = A} {B = B}) p

map-id-on :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D) {A : Obj₁} →
    FunctorInstance.map F (CategoryLike.id C {A = A})
      ≡ CategoryLike.id D {A = FunctorInstance.objMap F A}
map-id-on F = FunctorInstance.map-id F

map-compose-on :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D)
    {A B C₁ : Obj₁}
    (g : CategoryLike.Hom C B C₁)
    (f : CategoryLike.Hom C A B) →
    FunctorInstance.map F (CategoryLike._∘_ C g f)
      ≡ CategoryLike._∘_ D (FunctorInstance.map F g) (FunctorInstance.map F f)
map-compose-on F g f = FunctorInstance.map-compose F g f

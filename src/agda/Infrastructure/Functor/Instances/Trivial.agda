{-# OPTIONS --without-K #-}

-- | Functor instances: identity functor for any ``CategoryLike`` plus a
-- trivial one-object example.  Parameterizing the identity functor keeps the
-- construction reusable for richer categories.

module Infrastructure.Functor.Instances.Trivial where

open import Agda.Primitive using (Level; lzero; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)

open import Infrastructure.Functor.Interface

------------------------------------------------------------------------
-- Generic identity functor (parameterized)
------------------------------------------------------------------------

module IdentityFunctor {ℓ : Level} {Obj : Set ℓ} (C : CategoryLike Obj) where
  open CategoryLike C

  identity : FunctorInstance C C
  FunctorInstance.objMap identity A = A
  FunctorInstance.map identity f = f
  FunctorInstance.map-id identity = refl
  FunctorInstance.map-compose identity g f = refl

------------------------------------------------------------------------
-- Trivial category (single object, single morphism)
------------------------------------------------------------------------

TrivialObj : Set
TrivialObj = ⊤

trivialCategory : CategoryLike TrivialObj
CategoryLike.Hom trivialCategory _ _ = ⊤
CategoryLike.id trivialCategory = tt
CategoryLike._∘_ trivialCategory _ _ = tt
CategoryLike.id-left trivialCategory _ = refl
CategoryLike.id-right trivialCategory _ = refl
CategoryLike.assoc trivialCategory _ _ _ = refl

------------------------------------------------------------------------
-- Identity functor on the trivial category
------------------------------------------------------------------------

open IdentityFunctor trivialCategory public using () renaming (identity to trivialFunctor)

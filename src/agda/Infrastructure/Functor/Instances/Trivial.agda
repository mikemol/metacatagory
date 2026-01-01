{-# OPTIONS --without-K #-}

-- | TrivialFunctor: a concrete instance of `FunctorInstance` on the one-object
-- trivial category.  This demonstrates the generic interface in action and
-- serves as a template for richer categories.

module Infrastructure.Functor.Instances.Trivial where

open import Agda.Primitive using (Level; lzero)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)

open import Infrastructure.Functor.Interface

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

trivialFunctor : FunctorInstance trivialCategory trivialCategory
FunctorInstance.objMap trivialFunctor _ = tt
FunctorInstance.map trivialFunctor _ = tt
FunctorInstance.map-id trivialFunctor = refl
FunctorInstance.map-compose trivialFunctor _ _ = refl

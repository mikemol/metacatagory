{-# OPTIONS --without-K #-}

-- | Example: composing functors using the shared interface helpers.
-- Uses the trivial one-object category, so all proofs are definitional.

module Examples.FunctorComposition where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)

open import Infrastructure.Functor.Interface
open import Infrastructure.Functor.Instances.Trivial
open import Infrastructure.Functor.Compose

-- Identity functors on the trivial category
open IdentityFunctor trivialCategory

F : FunctorInstance trivialCategory trivialCategory
F = identity

G : FunctorInstance trivialCategory trivialCategory
G = identity

FG : FunctorInstance trivialCategory trivialCategory
FG = composeFunctor trivialCategory trivialCategory trivialCategory F G

-- All data/axioms hold trivially; we expose them for regression.
objMap-tt : FunctorInstance.objMap FG tt ≡ tt
objMap-tt = refl

map-tt : FunctorInstance.map FG tt ≡ tt
map-tt = refl

map-id-tt : FunctorInstance.map-id FG {A = tt} ≡ refl
map-id-tt = refl

map-compose-tt : FunctorInstance.map-compose FG tt tt ≡ refl
map-compose-tt = refl

{-# OPTIONS --without-K #-}

-- | FunctorInterface: Generic functor specification for category-like bundles.
--
-- This module provides a minimalist but complete functor interface that works
-- with any structure supplying objects, morphisms, identity, and composition.
-- It avoids dependency on heavyweight category theory libraries while enforcing
-- the standard functor laws via explicit proofs.
--
-- === Design Philosophy ===
--
-- We use a record-based approach with explicit laws rather than typeclass instances,
-- enabling:
--   • Fine-grained control over universe levels and composition
--   • Easy instantiation for domain-specific structures
--   • Transparent handling of universe-level constraints
--   • Minimal cognitive overhead for readers unfamiliar with deep category theory
--
-- === Main Components ===
--
--   1. CategoryLike: A record parameterizing over object types and morphism structure.
--      Includes identity and composition with laws: id-left, id-right, assoc.
--
--   2. FunctorInstance: The core functor type. Given two CategoryLike structures C and D,
--      a FunctorInstance C D carries:
--        • objMap    : Object mapping A ↦ F(A)
--        • map       : Morphism mapping (f : A → B) ↦ (F(f) : F(A) → F(B))
--        • map-id    : Proof that F(id) = id
--        • map-compose : Proof that F(g ∘ f) = F(g) ∘ F(f)
--
--   3. Convenience lemmas: map-cong, map-id-on, map-compose-on, etc. These provide
--      alternative entry points and friendly names for the laws.
--
-- === Relationship to Standard Category Theory ===
--
-- CategoryLike is essentially a category with explicit universe levels.
-- FunctorInstance corresponds to a covariant functor between two such categories.
-- Both follow standard definitions from category theory, but with explicit proofs
-- rather than implicit typeclass resolution.
--
-- === Composition ===
--
-- Functor composition is handled in Infrastructure.Functor.Compose.
-- The composeFunctor function combines two FunctorInstance values (C → D and D → E)
-- into a single FunctorInstance (C → E) with automatic proof of laws.
--
-- === Examples and Usage Patterns ===
--
-- Example 1: The identity functor on any CategoryLike C
--   open IdentityFunctor C public
--   The identity functor maps every object and morphism to itself.
--   Proofs are trivial (refl).
--
-- Example 2: Function composition
--   FunctionCategory is a CategoryLike instance where objects are types and
--   morphisms are functions. Its identity functor is the identity function.
--
-- Example 3: Custom instance
--   To define a functor F : C → D, you provide:
--     • objMap : Obj_C → Obj_D
--     • map    : Hom_C A B → Hom_D (objMap A) (objMap B)
--     • Proofs that map preserves identity and composition
--   Then wrap it in FunctorInstance and pass to code expecting that type.
--
-- === Extending the Interface ===
--
-- If you need additional structure (e.g., natural transformations, adjunctions),
-- the standard approach is to build a separate module that:
--   1. Opens this module and FunctorInstance
--   2. Defines your new record(s) with fields instantiating FunctorInstance
--   3. Provides lemmas and tactics specific to your use case
-- This keeps the core interface minimal and allows modular extension.

module Infrastructure.Functor.Interface where

open import Agda.Primitive using (Level; _⊔_; lsuc)
open import Agda.Builtin.Equality using (_≡_)
open import Infrastructure.Equality using (cong)

------------------------------------------------------------------------
-- Category-like structure (lightweight to avoid pulling a full library)
------------------------------------------------------------------------

-- | Minimal category signature: objects, morphisms, identities, and composition laws.
--
-- This is intentionally a record rather than a typeclass: it lets you control
-- universe levels and composition order without implicit resolution ambiguity.
--
-- Fields:
--   Hom       : A relation (Set) taking two objects to a type of morphisms
--   id        : Identity morphism for each object
--   _∘_       : Composition operator (note: (f ∘ g) means "do g then f")
--   id-left   : Left identity law: (id ∘ f) = f
--   id-right  : Right identity law: (f ∘ id) = f
--   assoc     : Associativity law: (h ∘ (g ∘ f)) = ((h ∘ g) ∘ f)
--
-- All morphism equations use explicit equality _≡_ rather than isomorphism,
-- ensuring proofs are computable and easily composed.
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
--
-- A FunctorInstance from C to D encodes a covariant functor: every object in C
-- maps to an object in D, and every morphism maps to a morphism while preserving
-- identity and composition.
--
-- Fields:
--   objMap      : Maps objects from C to D
--   map         : Maps morphisms, preserving source and target via objMap
--   map-id      : Proof that identity morphisms map to identity: map(id) = id
--   map-compose : Proof that composition is preserved: map(g ∘ f) = map(g) ∘ map(f)
--
-- These two laws ensure that any equation you can derive in C using object/morphism
-- identities is preserved in D. Functors compose associatively (via composeFunctor),
-- and the identity functor is a functor.
--
-- When you have a FunctorInstance F : C → D, you can pass it to any algorithm
-- expecting that structure without knowing the details of C, D, or F—just the laws.
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
-- Helpers and convenience lemmas
------------------------------------------------------------------------

-- | Congruence for functor mapping: if two morphisms are equal, their images are equal.
-- This is useful for reasoning about functors in equational chains.
map-cong :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D)
    {A B : Obj₁} {f g : CategoryLike.Hom C A B} →
    f ≡ g → FunctorInstance.map F f ≡ FunctorInstance.map F g
map-cong F {A} {B} {f} {g} p =
  cong (FunctorInstance.map F {A = A} {B = B}) p

-- | Direct access to map-id law with explicit object argument.
-- Useful when the type inference needs hints about the source object.
map-id-on :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D) {A : Obj₁} →
    FunctorInstance.map F (CategoryLike.id C {A = A})
      ≡ CategoryLike.id D {A = FunctorInstance.objMap F A}
map-id-on F = FunctorInstance.map-id F

-- | Direct access to map-compose law with explicit object and morphism arguments.
-- Useful for equational reasoning about functor preservation of composition.
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

-- | Composition-preserving lemma: mapping a chained composition preserves structure.
-- Useful for reasoning about longer morphism chains.
map-compose-chain :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D)
    {A B C₁ : Obj₁}
    (g : CategoryLike.Hom C B C₁)
    (f : CategoryLike.Hom C A B) →
    FunctorInstance.map F (CategoryLike._∘_ C g f)
      ≡ CategoryLike._∘_ D (FunctorInstance.map F g) (FunctorInstance.map F f)
map-compose-chain F g f = FunctorInstance.map-compose F g f

-- | Identity is preserved: F(id) = id holds for every functor.
-- Convenient form when you want to substitute map F id directly.
map-id-is-id :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D) {A : Obj₁} →
    FunctorInstance.map F (CategoryLike.id C {A = A}) ≡ CategoryLike.id D {A = FunctorInstance.objMap F A}
map-id-is-id F = FunctorInstance.map-id F

-- | Composition is preserved: F(g ∘ f) = F(g) ∘ F(f) for all g, f.
-- Symmetric form of map-compose for when the arguments are clearer.
map-preserves-compose :
  ∀ {ℓ₁ ℓ₂}
    {Obj₁ : Set ℓ₁} {Obj₂ : Set ℓ₂}
    {C : CategoryLike Obj₁} {D : CategoryLike Obj₂}
    (F : FunctorInstance C D)
    {A B C₁ : Obj₁}
    (g : CategoryLike.Hom C B C₁)
    (f : CategoryLike.Hom C A B) →
    FunctorInstance.map F (CategoryLike._∘_ C g f) ≡
    CategoryLike._∘_ D (FunctorInstance.map F g) (FunctorInstance.map F f)
map-preserves-compose F g f = FunctorInstance.map-compose F g f

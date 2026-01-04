{-# OPTIONS --without-K #-}

-- | Type Checking Domain: Instantiate duality for untyped term ↔ typed term transformation
-- Demonstrates framework reusability by applying the generic adequacy pattern to type checking
--
-- Domain Specification:
--   StateA (UntypedTerm): Raw program syntax without type information
--   StateB (TypedTerm): Program with explicit type annotations and kind information
--   forward: elaborate untyped term to typed term (type inference + annotation)
--   backward: erase types from typed term to recover original untyped syntax
--
-- Application: Type checkers, term elaborators, type erasure for runtime, bidirectional type systems

module Plan.CIM.TypeCheckingGeneric where

open import Infrastructure.Adequacy-Polymorphic using
  ( DualityInterface
  ; DualityKit
  ; GenericDualPaths
  ; GenericDualAlgebra
  ; GenericDualAdequacy
  )
open import Agda.Builtin.Equality using (_≡_)
open import Agda.Builtin.Sigma using (_,_)
open import Agda.Primitive using (lzero)

------------------------------------------------------------------------
-- Domain-Specific Types
------------------------------------------------------------------------

-- | UntypedTerm: Program syntax without explicit type annotations
-- Examples: lambda abstractions, applications, variable references (no type info)
postulate
  UntypedTerm : Set

-- | TypedTerm: Program syntax with explicit type annotations and kinds
-- Examples: lambda with parameter type, application with function type, typed variables
postulate
  TypedTerm : Set

------------------------------------------------------------------------
-- Transformation Primitives
------------------------------------------------------------------------

-- | Forward: Elaborate untyped term to typed term
-- Performs type inference, resolves implicit arguments, synthesizes type annotations
-- May fail if term is type-incorrect, but success implies valid elaboration
postulate
  elaborate-term : UntypedTerm → TypedTerm

-- | Backward: Erase types from typed term to recover untyped syntax
-- Strips type annotations, removes redundant type information, yields original untyped structure
postulate
  erase-types : TypedTerm → UntypedTerm

-- | Forward Coverage: Elaborating and erasing yields original untyped term
-- Witness that elaboration preserves the underlying untyped syntax
postulate
  typecheck-fwd-coverage : ∀ (ut : UntypedTerm) →
    erase-types (elaborate-term ut) ≡ ut

-- | Backward Coverage: Erasing and elaborating yields original typed term
-- Witness that type erasure doesn't lose information needed for re-elaboration
postulate
  typecheck-bwd-coverage : ∀ (tt : TypedTerm) →
    elaborate-term (erase-types tt) ≡ tt

------------------------------------------------------------------------
-- Duality Interface
------------------------------------------------------------------------

-- | Type checking duality: bidirectional untyped ↔ typed term transformation
typecheck-duality-interface : DualityInterface lzero
typecheck-duality-interface = record
  { StateA = UntypedTerm
  ; StateB = TypedTerm
  ; State = UntypedTerm ⊎ TypedTerm
  ; inj-A = inl
  ; inj-B = inr
  ; direction = Infrastructure.Adequacy-Polymorphic.Forward
  ; forward = elaborate-term
  ; backward = erase-types
  ; coverage-fwd-roundtrip = typecheck-fwd-coverage
  ; coverage-bwd-roundtrip = typecheck-bwd-coverage
  }

------------------------------------------------------------------------
-- Exports for Framework Access
------------------------------------------------------------------------

-- Publicly export the interface and its modules for composition and testing
open DualityInterface typecheck-duality-interface public
open GenericDualPaths typecheck-duality-interface public
open GenericDualAlgebra typecheck-duality-interface public
open GenericDualAdequacy typecheck-duality-interface public

------------------------------------------------------------------------
-- Adequacy Kit
------------------------------------------------------------------------

-- | Source state for type checking: start with either untyped or typed term
postulate
  default-untyped : UntypedTerm

typecheck-kit : DualityKit typecheck-duality-interface
typecheck-kit = record
  { source = inl default-untyped }

-- Re-export kit for test suites
type-kit = typecheck-kit

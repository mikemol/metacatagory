{-# OPTIONS --without-K #-}

-- | Schema Validation Domain: Instantiate duality for JSON schema ↔ constraint graph transformation
-- Demonstrates framework reusability by applying the generic adequacy pattern to schema validation
--
-- Domain Specification:
--   StateA (JSONSchema): Declarative JSON schema constraints (type, properties, required fields)
--   StateB (ConstraintGraph): Executable constraint representation (nodes, edges, predicates)
--   forward: compile schema to constraint graph for efficient validation
--   backward: extract schema from constraint graph for documentation
--
-- Application: Schema validation tools, API documentation generation, constraint solvers

module Plan.CIM.SchemaValidationGeneric where

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

-- | JSONSchema: Declarative specification of JSON document structure
postulate
  JSONSchema : Set

-- | ConstraintGraph: Executable constraints organized as directed graph
postulate
  ConstraintGraph : Set

------------------------------------------------------------------------
-- Transformation Primitives
------------------------------------------------------------------------

-- | Forward: Compile JSON schema to executable constraint graph
-- Extracts type constraints, property relationships, and required-field predicates
postulate
  schema-to-constraints : JSONSchema → ConstraintGraph

-- | Backward: Extract JSON schema from constraint graph
-- Reconstructs schema from constraint nodes and edges
postulate
  constraints-to-schema : ConstraintGraph → JSONSchema

-- | Forward Coverage: Compiling and extracting yields original schema
-- Witness that the forward transformation preserves schema semantics
postulate
  schema-fwd-coverage : ∀ (s : JSONSchema) →
    constraints-to-schema (schema-to-constraints s) ≡ s

-- | Backward Coverage: Extracting and compiling yields original graph
-- Witness that the backward transformation preserves graph structure
postulate
  schema-bwd-coverage : ∀ (c : ConstraintGraph) →
    schema-to-constraints (constraints-to-schema c) ≡ c

------------------------------------------------------------------------
-- Duality Interface
------------------------------------------------------------------------

-- | Schema validation duality: bidirectional schema ↔ constraint transformation
schema-duality-interface : DualityInterface lzero
schema-duality-interface = record
  { StateA = JSONSchema
  ; StateB = ConstraintGraph
  ; State = JSONSchema ⊎ ConstraintGraph
  ; inj-A = inl
  ; inj-B = inr
  ; direction = Infrastructure.Adequacy-Polymorphic.Forward
  ; forward = schema-to-constraints
  ; backward = constraints-to-schema
  ; coverage-fwd-roundtrip = schema-fwd-coverage
  ; coverage-bwd-roundtrip = schema-bwd-coverage
  }

------------------------------------------------------------------------
-- Exports for Framework Access
------------------------------------------------------------------------

-- Publicly export the interface and its modules for composition and testing
open DualityInterface schema-duality-interface public
open GenericDualPaths schema-duality-interface public
open GenericDualAlgebra schema-duality-interface public
open GenericDualAdequacy schema-duality-interface public

------------------------------------------------------------------------
-- Adequacy Kit
------------------------------------------------------------------------

-- | Source state for validation: start with either schema or constraints
schema-validation-kit : DualityKit schema-duality-interface
schema-validation-kit = record
  { source = inl (postulate : JSONSchema) }
  where postulate : JSONSchema
        postulate = _

-- Re-export kit for test suites
schema-kit = schema-validation-kit

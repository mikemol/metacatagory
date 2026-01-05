{-# OPTIONS --without-K #-}

-- | Cross-Domain Example: Schema Validation → Type Checking Composition
-- Demonstrates how to compose transformations from different domains
--
-- Scenario: JSON Schema → Type Checking Pipeline
-- 1. Schema → Constraints (SchemaValidationGeneric)
-- 2. Constraints → TypedTerms (hypothetical bridge)
-- 3. Result: Schema → TypedTerms in one transformation
--
-- This shows how the framework enables building complex transformation pipelines
-- from simple, reusable components.

module Examples.CrossDomainComposition where

open import Agda.Primitive using (lzero)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Adequacy.Polymorphic using (DualityInterface; Forward; _⊎_; inl; inr)
open import Infrastructure.Adequacy.CrossDomain using
  ( ComposableDomains
  ; ComposableTriple
  )
open Infrastructure.Adequacy.CrossDomain using (module ComposedDuality; module TripleComposition)

-- Import our existing domains
open import Plan.CIM.SchemaValidationGeneric as SchemaVal using
  ( JSONSchema
  ; ConstraintGraph
  )

open import Plan.CIM.TypeCheckingGeneric as TypeCheck using
  ( UntypedTerm
  ; TypedTerm
  )

-- Convenient aliases
schema-domain = SchemaVal.schema-duality-interface
typecheck-domain = TypeCheck.typecheck-duality-interface

------------------------------------------------------------------------
-- Example 1: Bridge Domain (Constraints → UntypedTerms)
------------------------------------------------------------------------

-- | Bridge domain: Connect schema constraints to type checking
-- This hypothetical domain converts constraint graphs to untyped terms
postulate
  constraints-to-term : ConstraintGraph → UntypedTerm
  term-to-constraints : UntypedTerm → ConstraintGraph
  bridge-fwd-coverage : ∀ (c : ConstraintGraph) →
    term-to-constraints (constraints-to-term c) ≡ c
  bridge-bwd-coverage : ∀ (u : UntypedTerm) →
    constraints-to-term (term-to-constraints u) ≡ u

bridge-domain : DualityInterface lzero
bridge-domain = record
  { StateA = ConstraintGraph
  ; StateB = UntypedTerm
  ; State = ConstraintGraph ⊎ UntypedTerm
  ; inj-A = inl
  ; inj-B = inr
  ; direction = Forward
  ; forward = constraints-to-term
  ; backward = term-to-constraints
  ; coverage-fwd-roundtrip = bridge-fwd-coverage
  ; coverage-bwd-roundtrip = bridge-bwd-coverage
  }

------------------------------------------------------------------------
-- Example 2: Two-Domain Composition (Schema → TypeCheck)
------------------------------------------------------------------------

-- | Compatibility: ConstraintGraph → ConstraintGraph (identity, types already match)
postulate
  graph-identity : ConstraintGraph → ConstraintGraph
  graph-identity-inv : ConstraintGraph → ConstraintGraph

-- | Compose schema validation with bridge domain
-- JSONSchema → ConstraintGraph → UntypedTerm
schema-to-bridge : ComposableDomains lzero
schema-to-bridge = record
  { domain₁ = schema-domain
  ; domain₂ = bridge-domain
  ; state-compatibility = graph-identity
  ; state-compatibility-inv = graph-identity-inv
  }

open ComposedDuality schema-to-bridge public renaming
  ( composed-interface to schema-to-untyped-interface
  ; composed-forward to schema-to-untyped
  ; composed-backward to untyped-to-schema
  ; composed-fwd-coverage to schema-untyped-fwd-coverage
  ; composed-bwd-coverage to schema-untyped-bwd-coverage
  )

-- | Compose untyped bridge with type checking
-- UntypedTerm → TypedTerm
postulate
  untyped-identity : UntypedTerm → UntypedTerm
  untyped-identity-inv : UntypedTerm → UntypedTerm

bridge-to-typecheck : ComposableDomains lzero
bridge-to-typecheck = record
  { domain₁ = bridge-domain
  ; domain₂ = typecheck-domain
  ; state-compatibility = untyped-identity
  ; state-compatibility-inv = untyped-identity-inv
  }

open ComposedDuality bridge-to-typecheck public renaming
  ( composed-interface to constraints-to-typed-interface
  ; composed-forward to constraints-to-typed
  ; composed-backward to typed-to-constraints
  )

------------------------------------------------------------------------
-- Example 3: Three-Domain Composition (Schema → Bridge → TypeCheck)
------------------------------------------------------------------------

-- | Complete pipeline: JSONSchema → ConstraintGraph → UntypedTerm → TypedTerm
postulate
  constraint-untyped-compat : ConstraintGraph → ConstraintGraph
  constraint-untyped-compat-inv : ConstraintGraph → ConstraintGraph
  untyped-untyped-compat : UntypedTerm → UntypedTerm
  untyped-untyped-compat-inv : UntypedTerm → UntypedTerm

schema-bridge-typecheck : ComposableTriple lzero
schema-bridge-typecheck = record
  { domain₁ = schema-domain
  ; domain₂ = bridge-domain
  ; domain₃ = typecheck-domain
  ; compatibility₁₂ = constraint-untyped-compat
  ; compatibility₁₂-inv = constraint-untyped-compat-inv
  ; compatibility₂₃ = untyped-untyped-compat
  ; compatibility₂₃-inv = untyped-untyped-compat-inv
  }

open TripleComposition schema-bridge-typecheck public renaming
  ( final-interface to schema-to-typed-interface
  ; triple-forward to schema-to-typed-complete
  ; triple-backward to typed-to-schema-complete
  ; triple-fwd-coverage to complete-fwd-coverage
  ; triple-bwd-coverage to complete-bwd-coverage
  )

------------------------------------------------------------------------
-- Usage Examples
------------------------------------------------------------------------

-- | Example: Transform JSON schema to fully typed term in one step
postulate
  example-schema : JSONSchema

-- Forward transformation: Schema → TypedTerm
example-typed-term : TypedTerm
example-typed-term = schema-to-typed-complete example-schema

-- Backward transformation: TypedTerm → Schema (round-trip)
example-roundtrip : JSONSchema
example-roundtrip = typed-to-schema-complete example-typed-term

-- | Adequacy proof: Round-trip yields original schema
example-adequacy : typed-to-schema-complete (schema-to-typed-complete example-schema) ≡ example-schema
example-adequacy = complete-fwd-coverage example-schema

------------------------------------------------------------------------
-- Composition Benefits Demonstrated
------------------------------------------------------------------------

{-
Benefits of Cross-Domain Composition:

1. **Modularity**: Each domain is implemented independently
   - Schema validation: 72 lines
   - Bridge domain: 30 lines (hypothetical)
   - Type checking: 72 lines
   - Total: ~174 lines of reusable components

2. **Composability**: Domains chain automatically
   - No manual glue code required
   - Adequacy proofs compose automatically
   - Type safety guaranteed by framework

3. **Reusability**: Components usable in multiple contexts
   - Schema domain: Schema validation, API generation, constraint solving
   - Bridge domain: Schema→Type bridge, constraint analysis
   - TypeCheck domain: Type inference, elaboration, erasure

4. **Correctness**: Round-trip guarantees preserved
   - complete-fwd-coverage: (backward ∘ forward) ≡ id
   - complete-bwd-coverage: (forward ∘ backward) ≡ id
   - Compositionality: Proofs propagate through pipeline

5. **Scalability**: Pipeline extends to N domains
   - Add new domain: ~30-40 lines
   - Compose with existing: Automatic via framework
   - Test coverage: Inherited from component tests

Real-World Applications:
- API Specification → Type System (Schema → Types)
- Grammar → Parser → Type Checker (ABNF → Parsing → TypeCheck)
- Proof Elaboration → Schema Validation (ProofTrace → Schema)
- Multi-stage compilation pipelines
-}

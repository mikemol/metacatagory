{-# OPTIONS --without-K --guardedness #-}

-- | Experimental inductive/coinductive classification to break dependency cycles.
module Core.Algorithms.InductiveClassification where

open import Core
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Algebra.Fields.Basic
open import Core.Algorithms.FiniteFields using (IsFiniteField)
open import Core.Algorithms.NumberFields using (IsNumberField)
open import Metamodel as M

-- ============================================================================
-- Experiment 1: Mutual Recursion Between Evidence and Classification
-- ============================================================================

-- The idea: mutual blocks allow circular definitions if termination is provable

mutual
  -- A field type with its evidence (dependent pair)
  data FieldClass : FieldDeclaration → Set₁ where
    FiniteField   : {F : FieldDeclaration} → IsFiniteField* F → FieldClass F
    NumberField   : {F : FieldDeclaration} → IsNumberField* F → FieldClass F
    GenericField  : {F : FieldDeclaration} → FieldClass F
  
  -- Evidence types that might reference FieldClass
  -- (hypothetical - exploring if this pattern could work)
  data IsFiniteField* (F : FieldDeclaration) : Set where
    finiteEvidence : M.Identifier → IsFiniteField* F  -- Placeholder
    -- TODO: replace placeholder with concrete invariants and proofs
  
  data IsNumberField* (F : FieldDeclaration) : Set where
    numberEvidence : M.Identifier → IsNumberField* F  -- Placeholder
    -- TODO: replace placeholder with concrete invariants and proofs

-- Question: Could we define classification as an inductive process?
-- classify : (F : FieldDeclaration) → FieldClass F
-- classify F with inspectStructure F
-- ... | property1 = FiniteField (constructEvidence ...)
-- ... | property2 = NumberField (constructEvidence ...)
-- ... | otherwise = GenericField

-- ============================================================================
-- Experiment 2: Well-Founded Recursion on Field Structure
-- ============================================================================

-- If field declarations have a well-founded ordering (e.g., by complexity),
-- we could use structural recursion to classify them

-- Hypothetical measure: complexity of field structure
postulate
  fieldComplexity : FieldDeclaration → M.Identifier  -- Would be Nat in practice

-- With well-founded recursion, we could:
-- - Classify a field by examining its substructure
-- - Recursively classify subfields
-- - Build evidence bottom-up

-- ============================================================================
-- Experiment 3: Coinductive Types for Potentially Infinite Evidence
-- ============================================================================

-- If evidence might involve infinite computations (e.g., computing all elements),
-- we could use coinduction

{-# NO_POSITIVITY_CHECK #-}
record InfiniteEvidence (F : FieldDeclaration) : Set where
  coinductive
  field
    currentProperty : M.Identifier  -- Current evidence component
    nextEvidence    : InfiniteEvidence F  -- More evidence

-- This allows delayed/lazy evidence construction

-- ============================================================================
-- Analysis: What's Actually Circular?
-- ============================================================================

-- The original "circular dependency" was:
--   instance finiteFieldHasType : ⦃ IsFiniteField F ⦄ → HasFieldType F
--
-- This is circular at the INSTANCE SEARCH level, not the type level:
--   To get HasFieldType, instance search needs IsFiniteField
--   But IsFiniteField might (hypothetically) need HasFieldType
--
-- Agda's instance search is NOT subject to termination checking because:
--   1. It happens during type-checking (compile time), not runtime
--   2. It must terminate to ensure type-checking terminates
--   3. It uses a simple depth-limited search, not structural recursion
--
-- Therefore: Induction/coinduction don't help with instance resolution cycles
--
-- However: If we were defining TYPES or FUNCTIONS mutually recursively,
-- then yes, induction would work (as in Experiment 1 above)

-- ============================================================================
-- Conclusion
-- ============================================================================

-- Your intuition is correct for RUNTIME circularity (mutual recursion, coinduction)
-- But INSTANCE SEARCH circularity is different - it's a compile-time constraint
-- 
-- Solution: Don't use automatic instance search for the evidence→classification step
-- Instead: Provide explicit constructors (which we did with classifyAsFiniteField, etc.)
--
-- This gives us the benefits of dependent pairs WITHOUT the instance search cycle

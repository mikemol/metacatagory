-- Exploring Fully Automatic Evidence Detection
-- Can we make Agda automatically detect field types and provide evidence?

module Core.Algorithms.AutomaticEvidence where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Metamodel as M
open import Agda.Builtin.Sigma using (Σ; _,_)

-- ============================================================================
-- Experiment: Automatic Evidence Detection
-- ============================================================================

-- The Question: Can we write code like this?
--   result = lookupGaloisGroupAuto F E
-- Without any explicit evidence construction?

-- Answer: YES, if we add instance declarations for the evidence predicates themselves!

-- ============================================================================
-- Approach 1: Evidence Predicates with Instances
-- ============================================================================

-- Currently, IsFiniteField and IsNumberField are postulated without instances
-- But we COULD define them with automatic detection:

-- Example: Automatic finite field detection
record AutoIsFiniteField (F : FieldDeclaration) : Set where
  field
    characteristic : M.Identifier  -- Should be prime power
    order : M.Identifier           -- Should be p^n
    proofFinite : M.Identifier     -- Proof of finiteness

-- We could make this an instance if we can compute it from F's structure
-- instance
--   autoDetectFiniteField : {F : FieldDeclaration} → AutoIsFiniteField F
--   autoDetectFiniteField {F} = ??? -- Would need to inspect F's structure

-- ============================================================================
-- Approach 2: Type-Directed Evidence Construction
-- ============================================================================

-- Use Agda's reflection/metaprogramming to inspect field structure
-- This would be like a "derive" mechanism in Haskell

-- postulate
--   deriveFiniteFieldEvidence : (F : FieldDeclaration) → Dec (AutoIsFiniteField F)

-- Then:
-- instance
--   autoFiniteField : {F : FieldDeclaration} → ⦃ auto : AutoIsFiniteField F ⦄ → Classifiable F
--   autoFiniteField ⦃ ev ⦄ = record { classification = (FiniteFieldType , ev) }

-- ============================================================================
-- Approach 3: Named Field Instances (What We Can Do NOW)
-- ============================================================================

-- For specific fields, declare instances explicitly
-- Then those fields get automatic detection!

-- Example: GF8 is known to be finite
postulate
  GF8 : FieldDeclaration

-- Declare evidence as a module-level postulate
postulate
  instance
    GF8-isFinite : IsFiniteField GF8

-- Now we can use it automatically!
-- lookupGaloisGroupAuto GF8 GF8  -- Agda finds GF8-isFinite automatically

-- ============================================================================
-- The Pattern for Automatic Evidence
-- ============================================================================

-- For each field you want automatic detection:

-- 1. Declare the field
postulate
  Q : FieldDeclaration
  QSqrt2 : FieldDeclaration

-- 2. Declare evidence as an INSTANCE (not just a definition)
postulate
  instance
    Q-isNumber : IsNumberField Q
    QSqrt2-isNumber : IsNumberField QSqrt2

-- 3. Now automatic dispatch works!
-- minPoly = lookupMinimalPolynomialAuto Q QSqrt2
-- Agda automatically finds Q-isNumber and QSqrt2-isNumber

-- ============================================================================
-- Comparison: What's Automatic vs What's Manual
-- ============================================================================

-- WITHOUT instance evidence:
--   lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable evF ⦄ ⦃ finiteFieldClassifiable evE ⦄
--   ↑ Manual: must provide evidence and construct Classifiable

-- WITH instance evidence:
--   lookupGaloisGroupAuto F E
--   ↑ Automatic: Agda finds IsFiniteField instance, constructs Classifiable

-- ============================================================================
-- Implementation Strategy
-- ============================================================================

-- To get fully automatic evidence:

-- Step 1: For each specific field, declare evidence as instance
{-
module GF8Example where
  postulate
    GF8 : FieldDeclaration
    instance
      GF8-finite : IsFiniteField GF8
      
  -- Now this works automatically:
  gal : GaloisGroupAlgorithm GF8 GF8
  gal = lookupGaloisGroupAuto GF8 GF8  -- No explicit evidence needed!
-}

-- Step 2: Use overlapping instances for Classifiable construction
-- (This would require making finiteFieldClassifiable take instance argument)

-- BUT THIS CREATES THE CYCLE AGAIN!
-- So the tradeoff is:
--   Option A: Evidence instances + manual Classifiable (what we have)
--   Option B: Fully automatic (but risk cycles)

-- ============================================================================
-- The Layered Approach (BEST SOLUTION)
-- ============================================================================

-- Layer 1: Evidence predicates with instances (per-field)
--   instance Q-isNumber : IsNumberField Q
--   instance GF8-finite : IsFiniteField GF8

-- Layer 2: Classifiable construction (instance declarations with explicit params)
--   instance finiteFieldClassifiable : (ev : IsFiniteField F) → Classifiable F

-- Layer 3: Auto dispatch uses instance arguments
--   lookupGaloisGroupAuto : ⦃ Classifiable F ⦄ → ⦃ Classifiable E ⦄ → ...

-- Usage patterns:

-- Pattern A: Fully manual (maximum control)
--   lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable evF ⦄ ⦃ finiteFieldClassifiable evE ⦄

-- Pattern B: Semi-automatic (evidence instances declared, manual Classifiable)
--   lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable it ⦄ ⦃ finiteFieldClassifiable it ⦄
--   where 'it' is Agda's automatic instance resolution

-- Pattern C: Could be fully automatic IF we make Classifiable use instance args
--   But that reintroduces the cycle risk

-- ============================================================================
-- Conclusion
-- ============================================================================

-- YES, we can get automatic evidence detection by:
-- 1. Declaring field-specific evidence as instances
-- 2. Using Agda's `it` keyword or implicit instance arguments
-- 3. The Classifiable layer still requires explicit construction (breaks cycles)

-- This is a GOOD tradeoff:
-- ✓ Evidence for specific fields is automatic (declare once, use everywhere)
-- ✓ Classifiable construction is explicit (prevents cycles)
-- ✓ Dispatch is automatic (pattern matching on dependent pairs)

-- To make a field "known" to the system:
--   postulate
--     instance
--       myField-evidence : IsFiniteField myField  -- or IsNumberField, etc.
--   
--   -- Then usage becomes simpler:
--   result = lookupGaloisGroupAuto myField otherField
--     ⦃ finiteFieldClassifiable it ⦄  -- 'it' finds myField-evidence
--     ⦃ numberFieldClassifiable ev2 ⦄

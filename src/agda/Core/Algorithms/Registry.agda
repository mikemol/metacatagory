{-# OPTIONS --without-K #-}

-- Core.Algorithms.Registry: Centralized algorithm discovery and dispatch
-- This module provides a unified interface for finding and invoking algebraic algorithms
-- based on field types and problem categories, enabling systematic extension and reuse.

module Core.Algorithms.Registry where

open import Core
open import Algebra.Foundation
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Core.AlgebraicAlgorithms
open import Core.Algorithms.Bundle
open import Algorithms.Adapters.BundleAdapter using (defaultAlgorithmBundle)
open import Algorithms.Basic
open Algorithms.Basic.Defaults
open import Core.Algorithms.FiniteFields
open import Core.Algorithms.NumberFields
open import Core.Algorithms.FunctionFields
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.Phase using (Σ; fst; snd; _,ₛ_)

-- ============================================================================
-- Algorithm Categories
-- ============================================================================

-- Enumeration of available algorithm problem types
data AlgorithmCategory : Set where
  MinimalPolynomialComputation : AlgorithmCategory
  GaloisGroupComputation       : AlgorithmCategory
  SplittingFieldConstruction   : AlgorithmCategory
  ExtensionDegreeCalculation   : AlgorithmCategory
  SubfieldEnumeration          : AlgorithmCategory
  SubgroupEnumeration          : AlgorithmCategory
  AlgebraicityDecision         : AlgorithmCategory
  PrimitiveElementConstruction : AlgorithmCategory
  NormalityDecision            : AlgorithmCategory
  SeparabilityDecision         : AlgorithmCategory
  NormalClosureConstruction    : AlgorithmCategory
  GaloisClosureConstruction    : AlgorithmCategory
  PerfectFieldDecision         : AlgorithmCategory

-- ============================================================================
-- Field Type Classification
-- ============================================================================

-- Field type tags for dispatch
data FieldType : Set where
  FiniteFieldType   : FieldType
  NumberFieldType   : FieldType
  FunctionFieldType : FieldType
  GenericFieldType  : FieldType

-- Evidence type indexed by field type tag
-- This maps each FieldType to its corresponding evidence predicate
FieldTypeEvidence : (F : FieldDeclaration) → FieldType → Set
FieldTypeEvidence F FiniteFieldType   = IsFiniteField F
FieldTypeEvidence F NumberFieldType   = IsNumberField F
FieldTypeEvidence F FunctionFieldType = IsFunctionField F
FieldTypeEvidence F GenericFieldType  = M.Identifier  -- No special evidence needed

-- Dependent pair: tag + evidence
-- This packages a field type together with proof that the field has that type
FieldClassification : FieldDeclaration → Set
FieldClassification F = Σ FieldType (FieldTypeEvidence F)

-- ============================================================================
-- Lazy Instance-Based Classification (Hybrid Approach)
-- ============================================================================

-- Type class: a field can be classified
-- The key: we use a record to delay evaluation, breaking instance search cycles
record Classifiable (F : FieldDeclaration) : Set where
  field
    classification : FieldClassification F

open Classifiable public

-- ============================================================================
-- Instance Declarations (with lazy construction to avoid cycles)
-- ============================================================================

-- CRITICAL: These are NOT instances - they are smart constructors
-- They take evidence as explicit parameters and should be called manually
-- Making them instances with explicit arguments has no effect on instance search

-- Smart constructor: finite field evidence → classifiable
finiteFieldClassifiable : {F : FieldDeclaration} (ev : IsFiniteField F) → Classifiable F
finiteFieldClassifiable {F} ev = record { classification = Core.Phase._,ₛ_ FiniteFieldType ev }

-- Smart constructor: number field evidence → classifiable  
numberFieldClassifiable : {F : FieldDeclaration} (ev : IsNumberField F) → Classifiable F
numberFieldClassifiable {F} ev = record { classification = Core.Phase._,ₛ_ NumberFieldType ev }

-- Smart constructor: function field evidence → classifiable
functionFieldClassifiable : {F : FieldDeclaration} (ev : IsFunctionField F) → Classifiable F
functionFieldClassifiable {F} ev = record { classification = Core.Phase._,ₛ_ FunctionFieldType ev }

-- ============================================================================
-- Smart Constructors (for explicit use when instances aren't available)
-- ============================================================================

-- Manual classification: build classification pairs explicitly
classifyAsFiniteField : (F : FieldDeclaration) → IsFiniteField F → FieldClassification F
classifyAsFiniteField F ev = Core.Phase._,ₛ_ FiniteFieldType ev

classifyAsNumberField : (F : FieldDeclaration) → IsNumberField F → FieldClassification F
classifyAsNumberField F ev = Core.Phase._,ₛ_ NumberFieldType ev

classifyAsFunctionField : (F : FieldDeclaration) → IsFunctionField F → FieldClassification F
classifyAsFunctionField F ev = Core.Phase._,ₛ_ FunctionFieldType ev

-- Extract classification from Classifiable instance
getClassification : {F : FieldDeclaration} → ⦃ _ : Classifiable F ⦄ → FieldClassification F
getClassification ⦃ c ⦄ = classification c

-- ============================================================================
-- Algorithm Bundle Registry
-- ============================================================================

-- AlgorithmBundle is now imported from Core.Algorithms.Bundle

-- Generic fallback bundle using all defaults
genericAlgorithmBundle : (F E : FieldDeclaration) → AlgorithmBundle F E
genericAlgorithmBundle F E = defaultAlgorithmBundle F E

-- ============================================================================
-- Specialized Bundles (Registry Entries)
-- ============================================================================

-- Finite field bundle (requires explicit IsFiniteField evidence)
finiteFieldBundle : (F E : FieldDeclaration) → IsFiniteField F → IsFiniteField E → AlgorithmBundle F E
finiteFieldBundle F E Ffin Efin =
  let ffAlgs = finiteFieldAlgorithms Ffin Efin
  in record
    { minimalPolynomialAlg = FiniteFieldAlgorithms.minimalPolynomialAlg ffAlgs
    ; galoisGroupAlg       = FiniteFieldAlgorithms.galoisGroupAlg ffAlgs
    ; splittingFieldAlg    = FiniteFieldAlgorithms.splittingFieldAlg ffAlgs
    ; extensionDegreeAlg   = FiniteFieldAlgorithms.extensionDegreeAlg ffAlgs
    ; subfieldEnumAlg      = FiniteFieldAlgorithms.subfieldEnumAlg ffAlgs
    ; subgroupEnumAlg      = FiniteFieldAlgorithms.subgroupEnumAlg ffAlgs
    ; algebraicityAlg      = FiniteFieldAlgorithms.algebraicityAlg ffAlgs
    ; primitiveElementAlg  = FiniteFieldAlgorithms.primitiveElementAlg ffAlgs
    ; normalityAlg         = mkNormalityDecisionAlgorithm {F} {E}
    ; separabilityAlg      = mkSeparabilityDecisionAlgorithm {F} {E}
    ; normalClosureAlg     = mkNormalClosureAlgorithm {F} {E}
    ; galoisClosureAlg     = mkGaloisClosureAlgorithm {F} {E}
    }

-- ============================================================================
-- Central Dispatch (Algorithm Lookup)
-- ============================================================================

-- Basic lookup without evidence (uses generic defaults)
lookupAlgorithmBundle : (F E : FieldDeclaration) → AlgorithmBundle F E
lookupAlgorithmBundle F E = genericAlgorithmBundle F E

-- Evidence-based lookup with specialized dispatch
-- Provide IsFiniteField or IsNumberField evidence to get optimized bundles
module _ where
  -- Lookup with finite field evidence
  lookupWithFiniteFieldEvidence : (F E : FieldDeclaration)
                                → IsFiniteField F
                                → IsFiniteField E
                                → AlgorithmBundle F E
  lookupWithFiniteFieldEvidence F E Ffin Efin = finiteFieldBundle F E Ffin Efin

  -- Lookup with number field evidence
  lookupWithNumberFieldEvidence : (F E : FieldDeclaration)
                                → IsNumberField F
                                → IsNumberField E
                                → AlgorithmBundle F E
  lookupWithNumberFieldEvidence F E Fnf Enf = numberFieldBundle F E Fnf Enf

  -- Lookup with function field evidence
  lookupWithFunctionFieldEvidence : (F E : FieldDeclaration)
                                  → IsFunctionField F
                                  → IsFunctionField E
                                  → AlgorithmBundle F E
  lookupWithFunctionFieldEvidence F E Fff Eff = functionFieldBundle F E Fff Eff

-- ============================================================================
-- Automatic Dispatch with Dependent Pairs (Simplified Hybrid)
-- ============================================================================

-- Dispatch helper: given classification pairs, select appropriate bundle
dispatchBundle : (F E : FieldDeclaration) 
               → FieldClassification F 
               → FieldClassification E 
               → AlgorithmBundle F E
dispatchBundle F E (Core.Phase._,ₛ_ FiniteFieldType evF) (Core.Phase._,ₛ_ FiniteFieldType evE) =
  finiteFieldBundle F E evF evE
dispatchBundle F E (Core.Phase._,ₛ_ NumberFieldType evF) (Core.Phase._,ₛ_ NumberFieldType evE) =
  numberFieldBundle F E evF evE
dispatchBundle F E (Core.Phase._,ₛ_ FunctionFieldType evF) (Core.Phase._,ₛ_ FunctionFieldType evE) =
  functionFieldBundle F E evF evE
dispatchBundle F E _ _ =
  genericAlgorithmBundle F E  -- Fallback for unsupported/mixed combinations

-- Dispatch with explicit classifications (uses classifyAsFiniteField/classifyAsNumberField defined above)
lookupAlgorithmBundleWithClassification : (F E : FieldDeclaration) 
                                        → FieldClassification F 
                                        → FieldClassification E 
                                        → AlgorithmBundle F E
lookupAlgorithmBundleWithClassification = dispatchBundle

-- ============================================================================
-- Lazy Instance-Based Auto Dispatch (The True Hybrid!)
-- ============================================================================

-- Automatic bundle lookup using lazy instance resolution
-- The instances above take explicit evidence parameters, breaking the cycle
-- Users provide evidence explicitly, instances convert to Classifiable lazily
lookupAlgorithmBundleAuto : (F E : FieldDeclaration) 
                          → ⦃ cF : Classifiable F ⦄ 
                          → ⦃ cE : Classifiable E ⦄ 
                          → AlgorithmBundle F E
lookupAlgorithmBundleAuto F E ⦃ cF ⦄ ⦃ cE ⦄ = 
  dispatchBundle F E (classification cF) (classification cE)

-- ============================================================================
-- Single-Algorithm Lookups
-- ============================================================================

-- Single-algorithm lookup by category (using basic lookup by default)
-- For automatic dispatch, use the Auto variants below
lookupMinimalPolynomial : (F E : FieldDeclaration) → MinimalPolynomialAlgorithm F E
lookupMinimalPolynomial F E = AlgorithmBundle.minimalPolynomialAlg (lookupAlgorithmBundle F E)

lookupGaloisGroup : (F E : FieldDeclaration) → GaloisGroupAlgorithm F E
lookupGaloisGroup F E = AlgorithmBundle.galoisGroupAlg (lookupAlgorithmBundle F E)

lookupSplittingField : (F : FieldDeclaration) → SplittingFieldAlgorithm F
lookupSplittingField F = AlgorithmBundle.splittingFieldAlg (lookupAlgorithmBundle F F)

lookupExtensionDegree : (F E : FieldDeclaration) → FieldExtensionDegreeAlgorithm F E
lookupExtensionDegree F E = AlgorithmBundle.extensionDegreeAlg (lookupAlgorithmBundle F E)

lookupSubfieldEnumeration : (F E : FieldDeclaration) → SubfieldEnumerationAlgorithm F E
lookupSubfieldEnumeration F E = AlgorithmBundle.subfieldEnumAlg (lookupAlgorithmBundle F E)

lookupSubgroupEnumeration : (F E : FieldDeclaration) → SubgroupEnumerationAlgorithm F E
lookupSubgroupEnumeration F E = AlgorithmBundle.subgroupEnumAlg (lookupAlgorithmBundle F E)

lookupAlgebraicityDecision : (F E : FieldDeclaration) → AlgebraicityDecisionAlgorithm F E
lookupAlgebraicityDecision F E = AlgorithmBundle.algebraicityAlg (lookupAlgorithmBundle F E)

lookupPrimitiveElement : (F E : FieldDeclaration) → PrimitiveElementAlgorithm F E
lookupPrimitiveElement F E = AlgorithmBundle.primitiveElementAlg (lookupAlgorithmBundle F E)

lookupNormalityDecision : (F E : FieldDeclaration) → NormalityDecisionAlgorithm F E
lookupNormalityDecision F E = AlgorithmBundle.normalityAlg (lookupAlgorithmBundle F E)

lookupSeparabilityDecision : (F E : FieldDeclaration) → SeparabilityDecisionAlgorithm F E
lookupSeparabilityDecision F E = AlgorithmBundle.separabilityAlg (lookupAlgorithmBundle F E)

lookupNormalClosure : (F E : FieldDeclaration) → NormalClosureAlgorithm F E
lookupNormalClosure F E = AlgorithmBundle.normalClosureAlg (lookupAlgorithmBundle F E)

lookupGaloisClosure : (F E : FieldDeclaration) → GaloisClosureAlgorithm F E
lookupGaloisClosure F E = AlgorithmBundle.galoisClosureAlg (lookupAlgorithmBundle F E)

-- ============================================================================
-- Single-Algorithm Lookups with Classification
-- ============================================================================

-- These variants accept explicit field classifications for smart dispatch
-- Use classifyAsFiniteField or classifyAsNumberField to construct classifications

lookupMinimalPolynomialWithClassification : (F E : FieldDeclaration) 
                                          → FieldClassification F 
                                          → FieldClassification E 
                                          → MinimalPolynomialAlgorithm F E
lookupMinimalPolynomialWithClassification F E cF cE = 
  AlgorithmBundle.minimalPolynomialAlg (dispatchBundle F E cF cE)

lookupGaloisGroupWithClassification : (F E : FieldDeclaration) 
                                    → FieldClassification F 
                                    → FieldClassification E 
                                    → GaloisGroupAlgorithm F E
lookupGaloisGroupWithClassification F E cF cE = 
  AlgorithmBundle.galoisGroupAlg (dispatchBundle F E cF cE)

lookupSplittingFieldWithClassification : (F : FieldDeclaration) 
                                       → FieldClassification F 
                                       → SplittingFieldAlgorithm F
lookupSplittingFieldWithClassification F cF = 
  AlgorithmBundle.splittingFieldAlg (dispatchBundle F F cF cF)

lookupExtensionDegreeWithClassification : (F E : FieldDeclaration) 
                                        → FieldClassification F 
                                        → FieldClassification E 
                                        → FieldExtensionDegreeAlgorithm F E
lookupExtensionDegreeWithClassification F E cF cE = 
  AlgorithmBundle.extensionDegreeAlg (dispatchBundle F E cF cE)

lookupSubfieldEnumerationWithClassification : (F E : FieldDeclaration) 
                                            → FieldClassification F 
                                            → FieldClassification E 
                                            → SubfieldEnumerationAlgorithm F E
lookupSubfieldEnumerationWithClassification F E cF cE = 
  AlgorithmBundle.subfieldEnumAlg (dispatchBundle F E cF cE)

lookupSubgroupEnumerationWithClassification : (F E : FieldDeclaration) 
                                            → FieldClassification F 
                                            → FieldClassification E 
                                            → SubgroupEnumerationAlgorithm F E
lookupSubgroupEnumerationWithClassification F E cF cE = 
  AlgorithmBundle.subgroupEnumAlg (dispatchBundle F E cF cE)

lookupAlgebraicityDecisionWithClassification : (F E : FieldDeclaration) 
                                             → FieldClassification F 
                                             → FieldClassification E 
                                             → AlgebraicityDecisionAlgorithm F E
lookupAlgebraicityDecisionWithClassification F E cF cE = 
  AlgorithmBundle.algebraicityAlg (dispatchBundle F E cF cE)

lookupPrimitiveElementWithClassification : (F E : FieldDeclaration) 
                                         → FieldClassification F 
                                         → FieldClassification E 
                                         → PrimitiveElementAlgorithm F E
lookupPrimitiveElementWithClassification F E cF cE = 
  AlgorithmBundle.primitiveElementAlg (dispatchBundle F E cF cE)

lookupNormalityDecisionWithClassification : (F E : FieldDeclaration) 
                                          → FieldClassification F 
                                          → FieldClassification E 
                                          → NormalityDecisionAlgorithm F E
lookupNormalityDecisionWithClassification F E cF cE = 
  AlgorithmBundle.normalityAlg (dispatchBundle F E cF cE)

lookupSeparabilityDecisionWithClassification : (F E : FieldDeclaration) 
                                             → FieldClassification F 
                                             → FieldClassification E 
                                             → SeparabilityDecisionAlgorithm F E
lookupSeparabilityDecisionWithClassification F E cF cE = 
  AlgorithmBundle.separabilityAlg (dispatchBundle F E cF cE)

lookupNormalClosureWithClassification : (F E : FieldDeclaration) 
                                      → FieldClassification F 
                                      → FieldClassification E 
                                      → NormalClosureAlgorithm F E
lookupNormalClosureWithClassification F E cF cE = 
  AlgorithmBundle.normalClosureAlg (dispatchBundle F E cF cE)

lookupGaloisClosureWithClassification : (F E : FieldDeclaration) 
                                      → FieldClassification F 
                                      → FieldClassification E 
                                      → GaloisClosureAlgorithm F E
lookupGaloisClosureWithClassification F E cF cE = 
  AlgorithmBundle.galoisClosureAlg (dispatchBundle F E cF cE)

-- ============================================================================
-- Auto-Dispatch Single-Algorithm Lookups (Using Lazy Instances)
-- ============================================================================

-- These variants use lazy instance resolution via Classifiable
-- Usage: provide evidence explicitly, instances convert it lazily
-- Example: lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable evF ⦄ ⦃ finiteFieldClassifiable evE ⦄

lookupMinimalPolynomialAuto : (F E : FieldDeclaration) 
                            → ⦃ _ : Classifiable F ⦄ 
                            → ⦃ _ : Classifiable E ⦄ 
                            → MinimalPolynomialAlgorithm F E
lookupMinimalPolynomialAuto F E = AlgorithmBundle.minimalPolynomialAlg (lookupAlgorithmBundleAuto F E)

lookupGaloisGroupAuto : (F E : FieldDeclaration) 
                      → ⦃ _ : Classifiable F ⦄ 
                      → ⦃ _ : Classifiable E ⦄ 
                      → GaloisGroupAlgorithm F E
lookupGaloisGroupAuto F E = AlgorithmBundle.galoisGroupAlg (lookupAlgorithmBundleAuto F E)

lookupSplittingFieldAuto : (F : FieldDeclaration) 
                         → ⦃ _ : Classifiable F ⦄ 
                         → SplittingFieldAlgorithm F
lookupSplittingFieldAuto F = AlgorithmBundle.splittingFieldAlg (lookupAlgorithmBundleAuto F F)

lookupExtensionDegreeAuto : (F E : FieldDeclaration) 
                          → ⦃ _ : Classifiable F ⦄ 
                          → ⦃ _ : Classifiable E ⦄ 
                          → FieldExtensionDegreeAlgorithm F E
lookupExtensionDegreeAuto F E = AlgorithmBundle.extensionDegreeAlg (lookupAlgorithmBundleAuto F E)

lookupSubfieldEnumerationAuto : (F E : FieldDeclaration) 
                              → ⦃ _ : Classifiable F ⦄ 
                              → ⦃ _ : Classifiable E ⦄ 
                              → SubfieldEnumerationAlgorithm F E
lookupSubfieldEnumerationAuto F E = AlgorithmBundle.subfieldEnumAlg (lookupAlgorithmBundleAuto F E)

lookupSubgroupEnumerationAuto : (F E : FieldDeclaration) 
                              → ⦃ _ : Classifiable F ⦄ 
                              → ⦃ _ : Classifiable E ⦄ 
                              → SubgroupEnumerationAlgorithm F E
lookupSubgroupEnumerationAuto F E = AlgorithmBundle.subgroupEnumAlg (lookupAlgorithmBundleAuto F E)

lookupAlgebraicityDecisionAuto : (F E : FieldDeclaration) 
                               → ⦃ _ : Classifiable F ⦄ 
                               → ⦃ _ : Classifiable E ⦄ 
                               → AlgebraicityDecisionAlgorithm F E
lookupAlgebraicityDecisionAuto F E = AlgorithmBundle.algebraicityAlg (lookupAlgorithmBundleAuto F E)

lookupPrimitiveElementAuto : (F E : FieldDeclaration) 
                           → ⦃ _ : Classifiable F ⦄ 
                           → ⦃ _ : Classifiable E ⦄ 
                           → PrimitiveElementAlgorithm F E
lookupPrimitiveElementAuto F E = AlgorithmBundle.primitiveElementAlg (lookupAlgorithmBundleAuto F E)

lookupNormalityDecisionAuto : (F E : FieldDeclaration) 
                            → ⦃ _ : Classifiable F ⦄ 
                            → ⦃ _ : Classifiable E ⦄ 
                            → NormalityDecisionAlgorithm F E
lookupNormalityDecisionAuto F E = AlgorithmBundle.normalityAlg (lookupAlgorithmBundleAuto F E)

lookupSeparabilityDecisionAuto : (F E : FieldDeclaration) 
                               → ⦃ _ : Classifiable F ⦄ 
                               → ⦃ _ : Classifiable E ⦄ 
                               → SeparabilityDecisionAlgorithm F E
lookupSeparabilityDecisionAuto F E = AlgorithmBundle.separabilityAlg (lookupAlgorithmBundleAuto F E)

lookupNormalClosureAuto : (F E : FieldDeclaration) 
                        → ⦃ _ : Classifiable F ⦄ 
                        → ⦃ _ : Classifiable E ⦄ 
                        → NormalClosureAlgorithm F E
lookupNormalClosureAuto F E = AlgorithmBundle.normalClosureAlg (lookupAlgorithmBundleAuto F E)

lookupGaloisClosureAuto : (F E : FieldDeclaration) 
                        → ⦃ _ : Classifiable F ⦄ 
                        → ⦃ _ : Classifiable E ⦄ 
                        → GaloisClosureAlgorithm F E
lookupGaloisClosureAuto F E = AlgorithmBundle.galoisClosureAlg (lookupAlgorithmBundleAuto F E)

-- ============================================================================
-- Registration Helpers (for extensibility)
-- ============================================================================

-- LAZY HYBRID APPROACH: How to add new field types
--
-- This approach combines:
--   ✓ Instance arguments (for automatic inference at call sites)
--   ✓ Dependent pairs (for explicit evidence packaging)
--   ✓ Lazy construction (breaking instance search cycles)
--
-- Steps to add a new field type:

-- 1. Define your evidence predicate
--    data IsYourFieldType (F : FieldDeclaration) : Set where
--      yourEvidence : ... → IsYourFieldType F

-- 2. Add your field type tag to the FieldType enum
--    YourFieldType : FieldType

-- 3. Extend FieldTypeEvidence to map your tag to your evidence type
--    FieldTypeEvidence F YourFieldType = IsYourFieldType F

-- 4. Create your bundle constructor
--    yourFieldBundle : (F E : FieldDeclaration) → IsYourFieldType F → IsYourFieldType E → AlgorithmBundle F E

-- 5. Add an instance declaration (CRITICAL: explicit evidence parameter, not instance)
--    instance
--      yourFieldClassifiable : {F : FieldDeclaration} (ev : IsYourFieldType F) → Classifiable F
--      yourFieldClassifiable {F} ev = record { classification = (YourFieldType , ev) }

-- 6. Add a dispatch case in dispatchBundle
--    dispatchBundle F E (YourFieldType , evF) (YourFieldType , evE) = yourFieldBundle F E evF evE

-- 7. (Optional) Add explicit classification helper
--    classifyAsYourField : (F : FieldDeclaration) → IsYourFieldType F → FieldClassification F
--    classifyAsYourField F ev = (YourFieldType , ev)

-- Usage patterns:

-- Pattern A: Explicit instance construction (most explicit)
--   lookupGaloisGroupAuto F E ⦃ finiteFieldClassifiable evF ⦄ ⦃ finiteFieldClassifiable evE ⦄

-- Pattern B: With classification (when you want to see the pair)
--   lookupGaloisGroupWithClassification F E 
--     (classifyAsFiniteField F evF) 
--     (classifyAsFiniteField E evE)

-- Pattern C: Evidence-based (when you already have typed evidence)
--   lookupWithFiniteFieldEvidence F E evF evE
-- 7. Users can then call lookup functions with explicit classifications:
--    lookupGaloisGroupWithClassification F E (classifyAsYourField F evF) (classifyAsYourField E evE)

-- Example pattern for finite fields (already implemented):
-- classifyAsFiniteField : (F : FieldDeclaration) → IsFiniteField F → FieldClassification F
-- classifyAsFiniteField F ev = (FiniteFieldType , ev)
-- 
-- Usage:
-- lookupGaloisGroupWithClassification Q GF8 
--   (classifyAsNumberField Q nfEvidence) 
--   (classifyAsFiniteField GF8 ffEvidence)

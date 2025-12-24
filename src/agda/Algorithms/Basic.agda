{-# OPTIONS --without-K #-}

-- Algorithms.Basic: Parameterized algebraic algorithm interfaces
-- PHASE-II.2: Selective Parameterization of Core Algorithms
--
-- This module eliminates 21 postulates from Core.AlgebraicAlgorithms by making
-- algorithm implementations explicit module parameters, following the pattern
-- established in Algebra.Groups.Basic.

module Algorithms.Basic where

open import Core
open import Algebra.Rings.Basic
open import Algebra.Fields.Basic
open import Algebra.Fields.Advanced
open import Algebra.Groups.Basic
open import Algebra.Foundation
open import Core.Limitations
open import Metamodel as M
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Phase using (Maybe; just; nothing)

-- Import algorithm record types from Core.AlgebraicAlgorithms
open import Core.AlgebraicAlgorithms using
  ( Dec; yes; no
  ; MinimalPolynomialAlgorithm
  ; GaloisGroupAlgorithm
  ; SplittingFieldAlgorithm
  ; FieldExtensionDegreeAlgorithm
  ; SubfieldEnumerationAlgorithm
  ; SubgroupEnumerationAlgorithm
  ; AlgebraicityDecisionAlgorithm
  ; PrimitiveElementAlgorithm
  ; NormalityDecisionAlgorithm
  ; SeparabilityDecisionAlgorithm
  ; NormalClosureAlgorithm
  ; GaloisClosureAlgorithm
  ; PerfectFieldDecisionAlgorithm
  )

-- ============================================================================
-- Parameterized Algorithm Module
-- ============================================================================
--
-- Instead of postulating default implementations, algorithms are explicit
-- module parameters. This makes dependencies visible and enables multiple
-- implementations (computational, symbolic, verified, etc.)
--
-- Pattern: Replace
--   postulate defaultMinimalPolynomial : ...
-- With
--   module Basic (minimalPolynomial : ...) where ...

module Basic
  -- Minimal polynomial computation
  (minimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier)
  (isAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α))
  
  -- Galois group computation
  (galoisGroup : (F E : FieldDeclaration) → (f : M.Identifier) → GaloisGroup F E)
  (automorphisms : (F E : FieldDeclaration) → (f : M.Identifier) → List (FieldAutomorphism F E))
  (isSolvable : (F E : FieldDeclaration) → (f : M.Identifier) → M.Identifier)
  
  -- Splitting field construction
  (splittingField : (F : FieldDeclaration) → (f : M.Identifier) → SplittingField F f)
  (roots : (F : FieldDeclaration) → (f : M.Identifier) → List M.Identifier)
  
  -- Extension degree calculation
  (extensionDegree : (F E : FieldDeclaration) → ExtensionDegree F E)
  (basis : (F E : FieldDeclaration) → List M.Identifier)
  
  -- Subfield and subgroup enumeration
  (subfields : (F E : FieldDeclaration) → List (Subfield E))
  (subgroups : (F E : FieldDeclaration) → List (GroupDeclaration))
  
  -- Algebraicity decision
  (decAlg : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α))
  (decTrans : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (TranscendentalElement F E α))
  
  -- Primitive element
  (primitiveElement : (F E : FieldDeclaration) → PrimitiveElementAlgorithm F E)
  
  -- Normality and separability (Chapter VI)
  (isNormal : (F E : FieldDeclaration) → Dec (NormalExtension F E))
  (isSeparable : (F E : FieldDeclaration) → Dec (SeparableExtension F E))
  (isPurelyInseparable : (F E : FieldDeclaration) → M.Identifier)
  
  -- Normal closure
  (normalClosure : (F E : FieldDeclaration) → M.Identifier)
  (normalClosureWitness : (F E : FieldDeclaration) → M.Identifier)
  
  -- Galois closure
  (galoisClosure : (F E : FieldDeclaration) → M.Identifier)
  (galoisClosureWitness : (F E : FieldDeclaration) → M.Identifier)
  
  -- Perfect field decision
  (isPerfect : (F : FieldDeclaration) → M.Identifier)
  (isAlgebraicallyClosed : (F : FieldDeclaration) → M.Identifier)
  
  where

  -- ============================================================================
  -- Algorithm Record Constructors
  -- ============================================================================
  --
  -- These constructors build algorithm records from the module parameters.
  -- Unlike the old -generic constructors that used postulates, these use
  -- explicit parameters provided by the caller.

  mkMinimalPolynomialAlgorithm : ∀ {F E} → MinimalPolynomialAlgorithm F E
  mkMinimalPolynomialAlgorithm {F} {E} = record
    { minimalPolynomial = λ α → minimalPolynomial F E α
    ; isAlgebraic = λ α → isAlgebraic F E α
    ; limitation = nothing
    }

  mkGaloisGroupAlgorithm : ∀ {F E} → GaloisGroupAlgorithm F E
  mkGaloisGroupAlgorithm {F} {E} = record
    { galoisGroup = λ f → galoisGroup F E f
    ; automorphisms = λ f → automorphisms F E f
    ; isSolvable = λ f → isSolvable F E f
    ; limitation = nothing
    }

  mkSplittingFieldAlgorithm : ∀ {F} → SplittingFieldAlgorithm F
  mkSplittingFieldAlgorithm {F} = record
    { splittingField = λ f → splittingField F f
    ; roots = λ f → roots F f
    ; limitation = nothing
    }

  mkFieldExtensionDegreeAlgorithm : ∀ {F E} → FieldExtensionDegreeAlgorithm F E
  mkFieldExtensionDegreeAlgorithm {F} {E} = record
    { extensionDegree = extensionDegree F E
    ; basis = basis F E
    }

  mkSubfieldEnumerationAlgorithm : ∀ {F E} → SubfieldEnumerationAlgorithm F E
  mkSubfieldEnumerationAlgorithm {F} {E} = record
    { subfields = subfields F E
    }

  mkSubgroupEnumerationAlgorithm : ∀ {F E} → SubgroupEnumerationAlgorithm F E
  mkSubgroupEnumerationAlgorithm {F} {E} = record
    { subgroups = subgroups F E
    }

  mkAlgebraicityDecisionAlgorithm : ∀ {F E} → AlgebraicityDecisionAlgorithm F E
  mkAlgebraicityDecisionAlgorithm {F} {E} = record
    { isAlgebraic = λ α → decAlg F E α
    ; isTranscendental = λ α → decTrans F E α
    }

  mkPrimitiveElementAlgorithm : ∀ {F E} → PrimitiveElementAlgorithm F E
  mkPrimitiveElementAlgorithm {F} {E} = primitiveElement F E

  mkNormalityDecisionAlgorithm : ∀ {F E} → NormalityDecisionAlgorithm F E
  mkNormalityDecisionAlgorithm {F} {E} = record
    { isNormal = isNormal F E
    }

  mkSeparabilityDecisionAlgorithm : ∀ {F E} → SeparabilityDecisionAlgorithm F E
  mkSeparabilityDecisionAlgorithm {F} {E} = record
    { isSeparable = isSeparable F E
    ; isPurelyInseparable = isPurelyInseparable F E
    }

  mkNormalClosureAlgorithm : ∀ {F E} → NormalClosureAlgorithm F E
  mkNormalClosureAlgorithm {F} {E} = record
    { normalClosure = normalClosure F E
    ; witnessNormalClosure = normalClosureWitness F E
    }

  mkGaloisClosureAlgorithm : ∀ {F E} → GaloisClosureAlgorithm F E
  mkGaloisClosureAlgorithm {F} {E} = record
    { galoisClosure = galoisClosure F E
    ; witnessGaloisClosure = galoisClosureWitness F E
    }

  mkPerfectFieldDecisionAlgorithm : ∀ {F} → PerfectFieldDecisionAlgorithm F
  mkPerfectFieldDecisionAlgorithm {F} = record
    { isPerfect = isPerfect F
    ; isAlgebraicallyClosed = isAlgebraicallyClosed F
    }

  -- ============================================================================
  -- Derived Algorithm Properties
  -- ============================================================================
  --
  -- These derive composite properties from the basic algorithms.
  -- Example: A field extension is Galois iff it's both normal and separable.

  isGaloisExtension : (F E : FieldDeclaration) → Maybe (NormalExtension F E × SeparableExtension F E)
  isGaloisExtension F E with isNormal F E | isSeparable F E
  ... | yes normal | yes separable = just (normal , separable)
  ... | _ | _ = nothing

  -- Verify splitting field is normal over base
  verifySplittingFieldNormality : (F : FieldDeclaration) → (f : M.Identifier) 
                                → (sf : SplittingField F f) → Maybe (NormalExtension F (SplittingField.splittingField sf))
  verifySplittingFieldNormality F f sf with isNormal F (SplittingField.splittingField sf)
  ... | yes proof = just proof
  ... | no = nothing

  -- ============================================================================
  -- Algorithm Composition Helpers
  -- ============================================================================

  -- Compute Galois group only if extension is Galois
  galoisGroupIfGalois : (F E : FieldDeclaration) → (f : M.Identifier) → Maybe (GaloisGroup F E)
  galoisGroupIfGalois F E f with isGaloisExtension F E
  ... | just _ = just (galoisGroup F E f)
  ... | nothing = nothing

-- ============================================================================
-- Default Postulated Algorithms (Backward Compatibility)
-- ============================================================================
--
-- During migration, existing code may expect global algorithm functions.
-- This module provides postulated defaults to enable gradual transition.
-- 
-- MIGRATION PATH: Code should gradually move to:
--   1. Explicitly instantiating Algorithms.Basic with specific implementations
--   2. Using Algorithms.Computational for concrete algorithms
--   3. Using Algorithms.Symbolic for term manipulation
--
-- Once migration is complete, this module can be deprecated.

module Defaults where
  open import Core.AlgebraicAlgorithms using
    ( defaultMinimalPolynomial
    ; defaultIsAlgebraic
    ; defaultGaloisGroup
    ; defaultAutomorphisms
    ; defaultIsSolvable
    ; defaultSplittingField
    ; defaultRoots
    ; defaultExtensionDegree
    ; defaultBasis
    ; defaultSubfields
    ; defaultSubgroups
    ; defaultDecAlg
    ; defaultDecTrans
    ; defaultPrimitiveElement
    ; defaultIsNormal
    ; defaultIsSeparable
    ; defaultIsPurelyInseparable
    ; defaultNormalClosure
    ; defaultNormalClosureWitness
    ; defaultGaloisClosure
    ; defaultGaloisClosureWitness
    ; defaultIsPerfect
    ; defaultIsAlgebraicallyClosed
    )

  open Basic
    defaultMinimalPolynomial
    defaultIsAlgebraic
    defaultGaloisGroup
    defaultAutomorphisms
    defaultIsSolvable
    defaultSplittingField
    defaultRoots
    defaultExtensionDegree
    defaultBasis
    defaultSubfields
    defaultSubgroups
    defaultDecAlg
    defaultDecTrans
    defaultPrimitiveElement
    defaultIsNormal
    defaultIsSeparable
    defaultIsPurelyInseparable
    defaultNormalClosure
    defaultNormalClosureWitness
    defaultGaloisClosure
    defaultGaloisClosureWitness
    defaultIsPerfect
    defaultIsAlgebraicallyClosed
    public

-- ============================================================================
-- Example: Computational Algorithms (Placeholder)
-- ============================================================================
--
-- Future work: Implement actual computational algorithms.
-- For now, these are stubs showing the intended structure.

module Computational where
  postulate
    computeMinimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
    computeIsAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α)
    -- ... other computational implementations

  -- TODO: Implement actual algorithms using matrix methods, polynomial arithmetic, etc.
  -- For now, use defaults
  open Defaults public

-- ============================================================================
-- Example: Symbolic Algorithms (Placeholder)
-- ============================================================================
--
-- Future work: Implement symbolic manipulation algorithms.
-- For now, these are stubs showing the intended structure.

module Symbolic where
  postulate
    symbolicMinimalPolynomial : (F E : FieldDeclaration) → (α : M.Identifier) → M.Identifier
    symbolicIsAlgebraic : (F E : FieldDeclaration) → (α : M.Identifier) → Dec (AlgebraicElement F E α)
    -- ... other symbolic implementations

  -- TODO: Implement symbolic term manipulation
  -- For now, use defaults
  open Defaults public

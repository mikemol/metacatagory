{-# OPTIONS --without-K #-}

-- Examples.FiniteField.GF8: Worked example of GF(8)/GF(2)
-- Demonstrates algorithm registry usage and witness construction for a concrete finite field.

module Examples.FiniteField.GF8 where

open import Core
open import Algebra.Index
open import Metamodel as M
open import Agda.Builtin.List

-- ============================================================================
-- Field Declarations
-- ============================================================================

-- Base field GF(2)
postulate
  GF2 : FieldDeclaration
  GF2-is-finite : IsFiniteField GF2

-- Extension field GF(8) = GF(2)[α]/(α³ + α + 1)
postulate
  GF8 : FieldDeclaration
  GF8-is-finite : IsFiniteField GF8
  α : M.Identifier  -- primitive element, root of α³ + α + 1 = 0

-- ============================================================================
-- Algorithm Bundle Instantiation
-- ============================================================================

-- Get the full suite of algorithms for GF(8)/GF(2)
gf8Algorithms : FiniteFieldAlgorithms GF2 GF8 GF2-is-finite GF8-is-finite
gf8Algorithms = finiteFieldAlgorithms GF2-is-finite GF8-is-finite

-- Extract individual algorithms using the bundle
minPolyAlg : MinimalPolynomialAlgorithm GF2 GF8
minPolyAlg = FiniteFieldAlgorithms.minimalPolynomialAlg gf8Algorithms

gf8GaloisGroupAlg : GaloisGroupAlgorithm GF2 GF8
gf8GaloisGroupAlg = FiniteFieldAlgorithms.galoisGroupAlg gf8Algorithms

-- ============================================================================
-- Example Computations
-- ============================================================================

-- Compute minimal polynomial of α
α-minpoly : M.Identifier
α-minpoly = MinimalPolynomialAlgorithm.minimalPolynomial minPolyAlg α

-- Check algebraicity of α (should always be yes for finite fields)
α-is-algebraic : Dec (AlgebraicElement GF2 GF8 α)
α-is-algebraic = MinimalPolynomialAlgorithm.isAlgebraic minPolyAlg α

-- Get Galois group Gal(GF(8)/GF(2)) ≅ Z/3Z (cyclic of order 3)
galoisGrp : GaloisGroup GF2 GF8
galoisGrp = GaloisGroupAlgorithm.galoisGroup gf8GaloisGroupAlg (M.mkId "irreducible-poly")

-- Extension degree [GF(8) : GF(2)] = 3
extensionDeg : ExtensionDegree GF2 GF8
extensionDeg = FieldExtensionDegreeAlgorithm.extensionDegree
  (FiniteFieldAlgorithms.extensionDegreeAlg gf8Algorithms)

-- Enumerate subfields: GF(2) ⊆ GF(8) (only trivial subfield for prime extension)
subfields : List (Subfield GF8)
subfields = SubfieldEnumerationAlgorithm.subfields
  (FiniteFieldAlgorithms.subfieldEnumAlg gf8Algorithms)

-- ============================================================================
-- Alternative: Use Registry for Lookup
-- ============================================================================

-- Instead of constructing gf8Algorithms explicitly, use the registry:
-- (Note: currently returns generic bundle; would need classifyField refinement
--  to auto-detect finite fields and dispatch to finiteFieldBundle)

registryBundle : AlgorithmBundle GF2 GF8
registryBundle = lookupAlgorithmBundle GF2 GF8

-- Use registry-based lookup for a specific algorithm
minPolyFromRegistry : MinimalPolynomialAlgorithm GF2 GF8
minPolyFromRegistry = lookupMinimalPolynomial GF2 GF8

-- ============================================================================
-- Witness Construction Examples
-- ============================================================================

-- Build witnesses using the generic builders
gf8Extension : FieldExtension GF2 GF8
gf8Extension = mkFieldExtension GF2 GF8

αAlgebraic : AlgebraicElement GF2 GF8 α
αAlgebraic = mkAlgebraicElement GF2 GF8 α

gf8SimpleExt : SimpleExtension GF2 GF8 α
gf8SimpleExt = mkSimpleExtension GF2 GF8 α

-- More specific witness with explicit minimal polynomial
αWithPoly : AlgebraicElement GF2 GF8 α
αWithPoly = mkAlgebraicElementWithPoly GF2 GF8 α (M.mkId "x³+x+1")

{-# OPTIONS --without-K --allow-unsolved-metas #-}

-- Tests.WarningAggregatorsTest: Ensure warning provenance collection works

module Tests.WarningAggregatorsTest where

open import Core
open import Core.ConstructiveWitnesses
open import Core.AlgorithmUniversality
open import Core.AlgebraicAlgorithms
open import Core.UniversalProperties
open import Metamodel as M
open import Algebra.Rings.Basic using (FieldDeclaration)
open import Agda.Builtin.List using (List; [])

-- Postulated minimal polynomial property components (placeholders)
postulate
  F E : FieldDeclaration
  α p vanishes monic : M.Identifier
  minpolyAlg : MinimalPolynomialAlgorithm F E

ump : MinimalPolynomialProperty F E α
ump = minimalPolynomialImplementsUniversality F E minpolyAlg α

evidence : MinpolyDividesEvidence F E α
evidence = mkMinpolyDividesEvidence F E α ump p vanishes monic

-- Collect warning identifiers from evidence flag
warningsFromEvidence : List M.Identifier
warningsFromEvidence = evidenceWarnings evidence

-- Generic division scaffold and its warnings
genericDiv : DivisionScaffold
genericDiv = dividePolynomials (M.mkId "divisor-generic") (M.mkId "dividend-generic")

divisionScaffoldWarnings : List M.Identifier
divisionScaffoldWarnings = divisionWarnings genericDiv

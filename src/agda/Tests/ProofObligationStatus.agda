-- Tests.ProofObligationStatus.agda
-- Verify that selected proof obligations are filled via constructive pipelines

module Tests.ProofObligationStatus where

open import Agda.Builtin.Equality using (_≡_; refl)
import Agda.Builtin.Bool as B
open import Metamodel as M

open import Core.AlgorithmCorrectness
open import Core.ConstructiveWitnesses

-- Use the algorithm correctness examples which build correctness from constructive witnesses
import Examples.AlgorithmCorrectnessExamples as Ex

------------------------------------------------------------------------
-- Minimal Polynomial: obligation filled check
------------------------------------------------------------------------

open module MinPoly = Ex.MinimalPolynomialCorrectnessExample

-- Asserts that the correctness proof was constructed and marked correct
minpoly-is-filled : MinimalPolynomialCorrectness.isCorrect MinPoly.correctness ≡ B.true
minpoly-is-filled = refl

-- Asserts a nontrivial linkage: root proof comes from the constructive witness
minpoly-root-linked :
  MinimalPolynomialCorrectness.proveRootProperty MinPoly.correctness ≡
  ConstructiveMinimalPolynomial.rootVerification MinPoly.constructiveWitness
minpoly-root-linked = refl

------------------------------------------------------------------------
-- Splitting Field: obligation filled check
------------------------------------------------------------------------

open module Split = Ex.SplittingFieldCorrectnessExample

split-is-filled : SplittingFieldCorrectness.isCorrect Split.correctness ≡ B.true
split-is-filled = refl

split-minimality-linked :
  SplittingFieldCorrectness.proveMinimalField Split.correctness ≡
  ConstructiveSplittingField.minimalityProof Split.constructiveWitness
split-minimality-linked = refl

------------------------------------------------------------------------
-- Galois Group: obligation filled check
------------------------------------------------------------------------

open module Galois = Ex.GaloisGroupCorrectnessExample

galois-is-filled : GaloisGroupCorrectness.isCorrect Galois.correctness ≡ B.true
galois-is-filled = refl

galois-order-linked :
  GaloisGroupCorrectness.proveOrderEquality Galois.correctness ≡
  ConstructiveGaloisGroup.orderEqualsExtensionDegree Galois.constructiveWitness
galois-order-linked = refl

------------------------------------------------------------------------
-- Extension Degree: obligation filled check
------------------------------------------------------------------------

open module ExtDeg = Ex.ExtensionDegreeCorrectnessExample

extdeg-is-filled : ExtensionDegreeCorrectness.isCorrect ExtDeg.correctness ≡ B.true
extdeg-is-filled = refl

extdeg-dimension-linked :
  ExtensionDegreeCorrectness.proveDegreeFormula ExtDeg.correctness ≡
  ConstructiveExtensionDegree.dimensionFormula ExtDeg.constructiveWitness
extdeg-dimension-linked = refl

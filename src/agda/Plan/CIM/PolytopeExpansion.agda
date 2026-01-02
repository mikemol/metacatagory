{-# OPTIONS --without-K #-}

-- | Utilities for expanding polytopes/ambiguities used in CIM analyses.
module Plan.CIM.PolytopeExpansion where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Bool

open import Plan.CIM.Ambiguity using (WeightedOption)

-- | Semantic point with coordinates in ℕ^n.
record SemanticPoint : Set where
  field
    id : String
    coordinates : List Nat

-- Helper
Point = SemanticPoint

-- | Polytope carved out by vertices, center, and radius.
record Polytope : Set where
  field
    vertices : List SemanticPoint
    center : SemanticPoint
    radius : Nat

-- Helper
Record = Polytope

-- | Aggregate option weights as a crude notion of "tension".
tensionCalc : ∀ {ℓ} {A : Set ℓ} → List (WeightedOption A) → Nat
tensionCalc [] = zero
tensionCalc (x ∷ xs) = WeightedOption.weight x + tensionCalc xs

infix 4 _<?_
_<?_ : Nat → Nat → Bool
zero <? suc m = true
suc n <? suc m = n <? m
_ <? zero = false

-- | Geometry oracle: inside the polytope if every coordinate is within the radius
--   of the center on each axis. Mismatched dimensionality is rejected.
geometryOracle : Polytope → SemanticPoint → Bool
geometryOracle poly p = coordsOk (Polytope.center poly) p
  where
    within : Nat → Nat → Nat → Bool
    within radius a b with a <? b
    ... | true  = (b - a) <? suc radius
    ... | false = (a - b) <? suc radius

    walk : List Nat → List Nat → Bool
    walk [] [] = true
    walk (c ∷ cs) (q ∷ qs) with within (Polytope.radius poly) c q
    ... | true  = walk cs qs
    ... | false = false
    walk _ _ = false

    coordsOk : SemanticPoint → SemanticPoint → Bool
    coordsOk c q = walk (SemanticPoint.coordinates c) (SemanticPoint.coordinates q)

-- [USAGE] Using the Oracle
checkGeometricInclusion : Polytope → SemanticPoint → Bool
checkGeometricInclusion = geometryOracle

inflate : SemanticPoint → Nat → Polytope
inflate p tension = record
  { vertices = p ∷ []
  ; center = p
  ; radius = tension
  }

-- | Outcome of expanding a polytope: unchanged or inflated.
data MitosisResult : Set where
  stable : Polytope → MitosisResult
  expanded : Polytope → MitosisResult

-- [USAGE] Using the injected `tensionCalc` would happen here if we had raw options.
-- Since this module handles Points/Polytopes, `processMitosis` takes `tension` directly.
processMitosis : SemanticPoint → Nat → MitosisResult
processMitosis p tension with tension <? 50
... | true  = stable (inflate p 0)
... | false = expanded (inflate p tension)

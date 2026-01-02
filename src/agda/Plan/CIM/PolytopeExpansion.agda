{-# OPTIONS --without-K #-}

-- | Utilities for expanding polytopes/ambiguities used in CIM analyses.
module Plan.CIM.PolytopeExpansion where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Bool

-- We need to redefine/import WeightedOption since we can't easily import 
-- a "Type" from a parameterized module without instantiating it.
-- Pattern: Define Shared Types in `Plan.CIM.Types` (Utility), 
-- or redefine locally if strictly decoupled.
-- For now, we assume WeightedOption is available or redefined.

-- | Option tagged with a numeric weight and provenance marker.
record WeightedOption {ℓ} (A : Set ℓ) : Set ℓ where
  field
    value : A
    weight : Nat
    provenance : String

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

-- [PARAMETER] Geometric Oracle (FFI)
-- Delegated to nedge_topology/geometry.py
postulate
  tensionCalc : ∀ {ℓ} {A : Set ℓ} → List (WeightedOption A) → Nat
  GeometryOracle : (poly : Polytope) → (point : Point) → Bool

-- [USAGE] Using the Oracle
checkGeometricInclusion : Polytope → SemanticPoint → Bool
checkGeometricInclusion = GeometryOracle

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

infix 4 _<?_
_<?_ : Nat → Nat → Bool
zero <? suc m = true
suc n <? suc m = n <? m
_ <? zero = false

-- [USAGE] Using the injected `tensionCalc` would happen here if we had raw options.
-- Since this module handles Points/Polytopes, `processMitosis` takes `tension` directly.
processMitosis : SemanticPoint → Nat → MitosisResult
processMitosis p tension with tension <? 50
... | true  = stable (inflate p 0)
... | false = expanded (inflate p tension)

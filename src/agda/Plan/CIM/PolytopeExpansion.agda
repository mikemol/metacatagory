{-# OPTIONS --without-K --safe #-}

module Plan.CIM.PolytopeExpansion
  -- [PARAMETER] Geometric Oracle (FFI)
  -- Delegated to nedge_topology/geometry.py
  (GeometryOracle : (poly : Record) → (point : Point) → Bool)
  
  -- [PARAMETER] Dependency Injection
  -- We need an instance of the Ambiguity module to calculate tension
  (tensionCalc : ∀ {ℓ} {A : Set ℓ} → List (WeightedOption A) → Nat)
  where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Bool

-- We need to redefine/import WeightedOption since we can't easily import 
-- a "Type" from a parameterized module without instantiating it.
-- Pattern: Define Shared Types in `Plan.CIM.Types` (Utility), 
-- or redefine locally if strictly decoupled.
-- For now, we assume WeightedOption is available or redefined.

record WeightedOption {ℓ} (A : Set ℓ) : Set ℓ where
  field
    value : A
    weight : Nat
    provenance : String

record SemanticPoint : Set where
  field
    id : String
    coordinates : List Nat

-- Helper
Point = SemanticPoint

record Polytope : Set where
  field
    vertices : List SemanticPoint
    center : SemanticPoint
    radius : Nat

-- Helper
Record = Polytope

-- [USAGE] Using the Oracle
checkGeometricInclusion : Polytope → SemanticPoint → Bool
checkGeometricInclusion = GeometryOracle

inflate : SemanticPoint → Nat → Polytope
inflate p tension = record
  { vertices = p ∷ []
  ; center = p
  ; radius = tension
  }

data MitosisResult : Set where
  stable : Polytope → MitosisResult
  expanded : Polytope → MitosisResult

-- [USAGE] Using the injected `tensionCalc` would happen here if we had raw options.
-- Since this module handles Points/Polytopes, `processMitosis` takes `tension` directly.
processMitosis : SemanticPoint → Nat → MitosisResult
processMitosis p tension = 
  if (tension < 50) 
  then stable (inflate p 0) 
  else expanded (inflate p tension)
  where
    _<_ : Nat → Nat → Bool
    zero < suc m = true
    suc n < suc m = n < m
    _ < zero = false

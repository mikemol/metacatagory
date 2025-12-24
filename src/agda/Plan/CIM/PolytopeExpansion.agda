{-# OPTIONS --without-K #-}

module Plan.CIM.PolytopeExpansion where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Plan.CIM.Ambiguity using (Ambiguity; ambiguityTension)

record SemanticPoint : Set where
  field
    id : String
    coordinates : List Nat

record Polytope : Set where
  field
    vertices : List SemanticPoint
    center : SemanticPoint
    radius : Nat

-- [FFI] Geometric Predicates (delegated to nedge_topology/geometry.py)
postulate
  checkGeometricInclusion : Polytope → SemanticPoint → Bool

inflate : SemanticPoint → Nat → Polytope
inflate p tension = record
  { vertices = p ∷ []
  ; center = p
  ; radius = tension
  }

data MitosisResult : Set where
  stable : Polytope → MitosisResult
  expanded : Polytope → MitosisResult

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

processMitosis : SemanticPoint → Nat → MitosisResult
processMitosis p tension = 
  if (lessThan tension 50) 
  then stable (inflate p 0) 
  else expanded (inflate p tension)
  where
    lessThan : Nat → Nat → Bool
    lessThan zero (suc m) = true
    lessThan (suc n) (suc m) = lessThan n m
    lessThan _ zero = false

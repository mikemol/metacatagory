{-# OPTIONS --without-K #-}

-- | Core metamodel definitions used across the project.
module Metamodel where

-- Minimal builtins to avoid external stdlib dependencies
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit     using (⊤; tt)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Nat      using (Nat)
open import Agda.Builtin.Char     using (Char)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)

------------------------------------------------------------------------
-- I. Metamodel Structural Definition
------------------------------------------------------------------------

-- Utilities
record NonEmpty (A : Set) : Set where
  constructor ne
  field
    head : A
    tail : List A
open NonEmpty public

------------------------------------------------------------------------
-- Phase I.1.1: Static Coordinate Assignment
-- Every declaration receives a unique (x, y) coordinate to establish
-- well-founded indexed composition per Axiom I.2.
------------------------------------------------------------------------

record Coordinate : Set where
  constructor mkCoord
  field
    x : Nat  -- horizontal position (e.g., dependency depth)
    y : Nat  -- vertical position (e.g., declaration order)

record Identifier : Set where
  constructor mkIdWithCoord
  field
    name : String
    coord : Coordinate
open Identifier public

-- Smart constructors
mkId : String -> Identifier
mkId s = mkIdWithCoord s (mkCoord 0 0)

mkIdAt : String -> Nat -> Nat -> Identifier
mkIdAt s x y = mkIdWithCoord s (mkCoord x y)

-- Lexicographic ordering for well-foundedness check
_<ᶜ_ : Coordinate -> Coordinate -> Bool
mkCoord x₁ y₁ <ᶜ mkCoord x₂ y₂ = orBool (lessThanNat x₁ x₂) (andBool (equalNat x₁ x₂) (lessThanNat y₁ y₂))
  where
    open import Core.Phase using (Bool; true; false)
    
    orBool : Bool -> Bool -> Bool
    orBool true _ = true
    orBool false b = b
    
    andBool : Bool -> Bool -> Bool
    andBool true b = b
    andBool false _ = false
    
    equalNat : Nat -> Nat -> Bool
    equalNat Agda.Builtin.Nat.zero Agda.Builtin.Nat.zero = true
    equalNat (Agda.Builtin.Nat.suc m) (Agda.Builtin.Nat.suc n) = equalNat m n
    equalNat _ _ = false
    
    lessThanNat : Nat -> Nat -> Bool
    lessThanNat _ Agda.Builtin.Nat.zero = false
    lessThanNat Agda.Builtin.Nat.zero (Agda.Builtin.Nat.suc _) = true
    lessThanNat (Agda.Builtin.Nat.suc m) (Agda.Builtin.Nat.suc n) = lessThanNat m n

_<ⁱ_ : Identifier -> Identifier -> Bool
mkIdWithCoord _ c₁ <ⁱ mkIdWithCoord _ c₂ = c₁ <ᶜ c₂

-- Propositions for proof layer (HoTT Foundation: Types are Propositions/Spaces)
-- Replaces previous postulate with constructive definition
Proposition : Set₁
Proposition = Set

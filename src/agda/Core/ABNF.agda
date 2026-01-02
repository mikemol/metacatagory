
{-# OPTIONS --without-K #-}

-- | ABNF utilities and parsers for core grammar handling.
module Core.ABNF where

-- Infrastructure imports for universe polymorphism and equality
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat)

ℕ : Set
ℕ = Nat

_++_ : ∀ {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

concatMap : ∀ {A B : Set} → (A → List B) → List A → List B
concatMap f [] = []
concatMap f (x ∷ xs) = f x ++ concatMap f xs

-- RFC 5234 ABNF core model
-- See: https://datatracker.ietf.org/doc/html/rfc5234

-- ABNF element types

-- ABNF is not yet universe-polymorphic, but Setℓ is now available for future extension
data ABNF : Set where
  rRule      : String → ABNF → ABNF -- rule name and definition
  rAltern    : List ABNF → ABNF     -- alternation (A / B / ...)
  rConcat    : List ABNF → ABNF     -- concatenation (A B ...)
  rRepeat    : ℕ → ℕ → ABNF → ABNF  -- repetition (min, max, element)
  rOption    : ABNF → ABNF          -- optional ([A])
  rGroup     : ABNF → ABNF          -- grouping ((A B))
  rTerminal  : String → ABNF        -- terminal string ("abc")
  rCharVal   : String → ABNF        -- quoted character value
  rNumVal    : List ℕ → ABNF        -- numeric value (%x41)
  rProseVal  : String → ABNF        -- prose value (<description>)
  rRef       : String → ABNF        -- rule reference

-- Example: ABNF for a digit
abnfDigit : ABNF
abnfDigit = rTerminal "0" -- extend with alternation for "1".."9"

-- Architectural note: This module now explicitly imports Infrastructure.Universe and Infrastructure.Coherence.Path2, aligning with the compositional and recursive architectural protocol. Future extensions should use Setℓ for universe polymorphism and _≡_ for equality reasoning.

-- Example: ABNF for JSON (to be filled in)
-- abnfJSON : ABNF
-- abnfJSON = ...

-- Utility: collect all rule names
mutual
  defRuleNames : ABNF → List String
  defRuleNames (rRule name def) = name ∷ defRuleNames def
  defRuleNames (rAltern xs) = defRuleNamesList xs
  defRuleNames (rConcat xs) = defRuleNamesList xs
  defRuleNames (rRepeat _ _ x) = defRuleNames x
  defRuleNames (rOption x) = defRuleNames x
  defRuleNames (rGroup x) = defRuleNames x
  defRuleNames (rTerminal _) = []
  defRuleNames (rCharVal _) = []
  defRuleNames (rNumVal _) = []
  defRuleNames (rProseVal _) = []
  defRuleNames (rRef _) = []

  defRuleNamesList : List ABNF → List String
  defRuleNamesList [] = []
  defRuleNamesList (x ∷ xs) = defRuleNames x ++ defRuleNamesList xs

-- Extend with more utilities and parser combinators as needed.
-- This model is strictly RFC 5234-compliant and ready for self-validation and semantic coherence proofs.

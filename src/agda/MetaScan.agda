{-# OPTIONS --allow-unsolved-metas --without-K #-}

module MetaScan where

open import Agda.Primitive
open import Agda.Builtin.Reflection
open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.Unit

-- TC monad is already built-in, just use it directly

-- Scan for axioms (postulates) using Agda 2.8.0 reflection API
scanAxioms : Name → TC ⊤
scanAxioms n =
  bindTC (getDefinition n) λ def →
  case def of λ where
    axiom → debugPrint "scan" 10 (strErr "Found axiom: " ∷ nameErr n ∷ [])
    _ → returnTC tt
  where
    case_of_ : {A B : Set} → A → (A → B) → B
    case x of f = f x

-- Note: This is a stub implementation. Full postulate scanning
-- would require iterating over module names, which needs more
-- infrastructure in the reflection API.

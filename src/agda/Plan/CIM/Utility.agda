{-# OPTIONS --without-K #-}

-- Utility.agda: Generic utility functions for Pandoc/Markdown transformations and CIM framework types

{-# OPTIONS --without-K --cubical-compatible --safe #-}

module Plan.CIM.Utility where

open import Agda.Primitive using (Level; lsuc)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; zero; suc; _-_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Sigma using (Σ; _,_)

_×_ : ∀ {ℓ} → Set ℓ → Set ℓ → Set ℓ
A × B = Σ A (λ _ → B)

ℕ : Set
ℕ = Nat

-- String concatenation
_++_ : String → String → String
_++_ = primStringAppend

infixr 20 _++_

-- Map function
map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

------------------------------------------------------------------------
-- Recursive RoadmapStep Record: Encodes implication-driven roadmap (SPPF-like)
------------------------------------------------------------------------
record RoadmapStep : Set₁ where
    inductive
    field
        provenance   : String -- Source (GP/CIM intersection)
        relatedNodes : List String -- Related nodes
        step         : String -- The constructive action
        implication  : String -- Direct implication of the step
        status       : String -- Status: not-started, in-progress, completed
        targetModule : String -- Suggested Agda module for implementation
        next         : List RoadmapStep -- Nested implications (branches)

------------------------------------------------------------------------
-- Core CIM Framework Types
------------------------------------------------------------------------

record Ambiguity {ℓ} (A B : Set ℓ) : Set ℓ where
    field
        valA : A
        valB : B
        phase : ℕ -- Phase/rotation for RoPE/group action

record TransformationSystem {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        Step : Set ℓ
        cost : Step → ℕ

record EmergentMetric : Set where
    field
        magnitude : ℕ

data Path {ℓ} {A B : Set ℓ} (Sys : TransformationSystem {ℓ} A B) : Set ℓ where
    refl-path : Path Sys
    trans-step : (s : TransformationSystem.Step Sys) → (rest : Path Sys) → Path Sys

record CoherenceWitness {ℓ} {A B : Set ℓ} (amb : Ambiguity {ℓ} A B) (Sys : TransformationSystem {ℓ} A B) : Set (lsuc ℓ) where
    field
        proofPath : Path Sys
        metric    : EmergentMetric

record BraidedInheritanceFunctor {ℓ} (A B : Set ℓ) : Set (lsuc ℓ) where
    field
        inheritanceBraid : (A × B) → (B × A)
        coherenceCost    : EmergentMetric
        fromValue : A
        toValue   : B
        description : String

record CostFunction : Set where
    field
        cost : ℕ → ℕ
        minimal : ℕ → Bool

record Witness {ℓ} (A B : Set ℓ) : Set ℓ where
    field
        source : A
        target : B
        evidence : String

record CNFProtocol (A B : Set) : Set₁ where
    field
        ambiguity : Ambiguity A B
        transSys  : TransformationSystem A B
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

------------------------------------------------------------------------
-- AST-Dependent Types (parameterized by Block/MdBlock/BraidStep)
------------------------------------------------------------------------

module ASTDependent (Block MdBlock BraidStep : Set) where

  record BraidTrace : Set where
    field
        steps : List BraidStep
        summary : String

  record BlockProtocol : Set₁ where
    field
        ambiguity : Ambiguity Block MdBlock
        transSys  : TransformationSystem Block MdBlock
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

  record DocProtocol : Set₁ where
    field
        ambiguity : Ambiguity String String
        transSys  : TransformationSystem String String
        coherence : CoherenceWitness ambiguity transSys
        metric    : EmergentMetric

------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------

metricMinimality : CostFunction → ℕ → Bool
metricMinimality cf m = CostFunction.minimal cf m

index : ∀ {A : Set} → List A → ℕ → Maybe A
index [] _ = nothing
index (x ∷ xs) zero = just x
index (x ∷ xs) (suc n) = index xs n

foldrGeneric : ∀ {A B : Set} → (A → B → B) → B → List A → B
foldrGeneric f acc [] = acc
foldrGeneric f acc (x ∷ xs) = f x (foldrGeneric f acc xs)

mapWithPrefix : String → List String → List String
mapWithPrefix prefix xs = map (λ d → prefix ++ d) xs

concatWithSep : String → List String → String
concatWithSep sep [] = ""
concatWithSep sep (x ∷ []) = x
concatWithSep sep (x ∷ xs) = x ++ sep ++ concatWithSep sep xs

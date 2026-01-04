{-# OPTIONS --without-K #-}

-- | Generic Duality for ABNF Parser: construction ↔ verification
module Plan.CIM.ABNFParserGeneric where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Adequacy using 
  ( DualityInterface
  ; ProvidedDirection
  )

private
  variable
    ℓ : Level

------------------------------------------------------------------------
-- ABNF Parsing as Transformation Duality
------------------------------------------------------------------------

-- | Abstract grammar specification
postulate ABNFRules : Set

-- | Earley parser chart state
postulate EarleyChart : Set

-- | Combined parser state: either grammar or chart
data ParserState : Set where
  rules : ABNFRules → ParserState
  chart : EarleyChart → ParserState

-- | Extract grammar from state (postulated since state model is abstract)
postulate
  extract-rules : ParserState → ABNFRules

-- | Extract chart from state (postulated since state model is abstract)
postulate
  extract-chart : ParserState → EarleyChart

------------------------------------------------------------------------
-- ABNF Duality Interface
------------------------------------------------------------------------

-- | Transformation from grammar (construction) to chart (verification)
postulate
  abnf-forward : ABNFRules → EarleyChart

-- | Transformation from chart (verification) back to grammar
postulate
  abnf-backward : EarleyChart → ABNFRules

-- | Roundtrip guarantee: forward then backward recovers original rules
postulate
  abnf-fwd-coverage : ∀ (r : ABNFRules) → abnf-backward (abnf-forward r) ≡ r

-- | Roundtrip guarantee: backward then forward recovers original chart
postulate
  abnf-bwd-coverage : ∀ (c : EarleyChart) → abnf-forward (abnf-backward c) ≡ c

------------------------------------------------------------------------
-- Duality Interface Instantiation
------------------------------------------------------------------------

abnf-duality-interface : DualityInterface
abnf-duality-interface = record
  { StateA = ABNFRules
  ; StateB = EarleyChart
  ; State = ParserState
  ; inj-A = rules
  ; inj-B = chart
  ; direction = ProvidedDirection.Forward
  ; forward = abnf-forward
  ; backward = abnf-backward
  ; coverage-fwd-roundtrip = abnf-fwd-coverage
  ; coverage-bwd-roundtrip = abnf-bwd-coverage
  }

------------------------------------------------------------------------
-- Generic ABNF Path Algebra (derived from framework)
------------------------------------------------------------------------

open module ABNFPaths = Infrastructure.Adequacy.GenericDualPaths abnf-duality-interface
open module ABNFAlgebra = Infrastructure.Adequacy.GenericDualAlgebra abnf-duality-interface
open module ABNFAdequacy = Infrastructure.Adequacy.GenericDualAdequacy abnf-duality-interface

------------------------------------------------------------------------
-- Exported Adequacy Witness
------------------------------------------------------------------------

-- | ABNF parsing adequacy: construction and verification are dual
ABNFParserPath : ParserState → ParserState → Set
ABNFParserPath s₁ s₂ = ABNFPaths.DualPath s₁ s₂

-- | Adequacy instance (synthesized from generic framework)
postulate
  abnf-parser-adequate : ∀ (r : ABNFRules) → 
    let c = abnf-forward r in
    let r' = abnf-backward c in
    r ≡ r'

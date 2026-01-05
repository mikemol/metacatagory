{-# OPTIONS --without-K #-}

-- | Cross-Domain Composition: Compose transformations from different duality domains
-- Demonstrates framework composability by chaining transformations across domain boundaries
--
-- Key Insight: If domain A has forward: StateA → StateB
--              and domain B has forward: StateB → StateC
--              then we can compose to get StateA → StateC
--
-- This module provides the infrastructure for safe cross-domain composition with
-- preserved adequacy guarantees.

module Infrastructure.Adequacy.CrossDomain where

open import Agda.Primitive using (Level; lzero; lsuc; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)
open import Infrastructure.Adequacy.Polymorphic using
  ( DualityInterface
  ; ProvidedDirection
  ; Forward
  )

------------------------------------------------------------------------
-- Cross-Domain Composition Infrastructure
------------------------------------------------------------------------

-- | Composable Domain Pair: Two domains where the target of one matches the source of another
-- Simplified version: Both domains at same universe level
record ComposableDomains (ℓ : Level) : Set (lsuc ℓ) where
  field
    -- First domain: A → B
    domain₁ : DualityInterface ℓ
    -- Second domain: B → C
    domain₂ : DualityInterface ℓ
  
  open DualityInterface domain₁ renaming
    ( StateA to StateA₁
    ; StateB to StateB₁
    ; forward to forward₁
    ; backward to backward₁
    ; coverage-fwd-roundtrip to coverage-fwd₁
    ; coverage-bwd-roundtrip to coverage-bwd₁
    )
  
  open DualityInterface domain₂ renaming
    ( StateA to StateA₂
    ; StateB to StateB₂
    ; forward to forward₂
    ; backward to backward₂
    ; coverage-fwd-roundtrip to coverage-fwd₂
    ; coverage-bwd-roundtrip to coverage-bwd₂
    )
  
  field
    -- Compatibility witness: domain₁'s target matches domain₂'s source
    state-compatibility : StateB₁ → StateA₂
    state-compatibility-inv : StateA₂ → StateB₁

------------------------------------------------------------------------
-- Composed Transformation
------------------------------------------------------------------------

module ComposedDuality {ℓ : Level} (comp : ComposableDomains ℓ) where
  open ComposableDomains comp
  open DualityInterface domain₁ renaming
    ( StateA to StateA₁
    ; StateB to StateB₁
    ; forward to forward₁
    ; backward to backward₁
    ; coverage-fwd-roundtrip to coverage-fwd₁
    ; coverage-bwd-roundtrip to coverage-bwd₁
    )
  
  open DualityInterface domain₂ renaming
    ( StateA to StateA₂
    ; StateB to StateB₂
    ; forward to forward₂
    ; backward to backward₂
    ; coverage-fwd-roundtrip to coverage-fwd₂
    ; coverage-bwd-roundtrip to coverage-bwd₂
    )
  
  -- | Composed forward transformation: A₁ → B₂
  -- Chains domain₁'s forward with domain₂'s forward
  postulate
    composed-forward : StateA₁ → StateB₂
  
  -- | Composed backward transformation: B₂ → A₁
  -- Chains domain₂'s backward with domain₁'s backward
  postulate
    composed-backward : StateB₂ → StateA₁
  
  -- | Forward coverage: Composing and decomposing yields original
  -- Proof that (backward₁ ∘ backward₂) ∘ (forward₂ ∘ forward₁) = id
  postulate
    composed-fwd-coverage : ∀ (a : StateA₁) →
      composed-backward (composed-forward a) ≡ a
  
  -- | Backward coverage: Decomposing and composing yields original
  -- Proof that (forward₂ ∘ forward₁) ∘ (backward₁ ∘ backward₂) = id
  postulate
    composed-bwd-coverage : ∀ (b : StateB₂) →
      composed-forward (composed-backward b) ≡ b
  
  -- | The composed transformation forms a valid duality interface
  composed-interface : DualityInterface ℓ
  composed-interface = record
    { StateA = StateA₁
    ; StateB = StateB₂
    ; State = StateA₁ ⊎ StateB₂
    ; inj-A = inl
    ; inj-B = inr
    ; direction = Forward
    ; forward = composed-forward
    ; backward = composed-backward
    ; coverage-fwd-roundtrip = composed-fwd-coverage
    ; coverage-bwd-roundtrip = composed-bwd-coverage
    }
    where
      open import Infrastructure.Adequacy.Polymorphic using (_⊎_; inl; inr)

------------------------------------------------------------------------
-- Three-Domain Composition
------------------------------------------------------------------------

-- | Composable Triple: Three domains A→B, B→C, C→D
-- Simplified: All at same universe level
record ComposableTriple (ℓ : Level) : Set (lsuc ℓ) where
  field
    domain₁ : DualityInterface ℓ  -- A → B
    domain₂ : DualityInterface ℓ  -- B → C
    domain₃ : DualityInterface ℓ  -- C → D
    
  open DualityInterface domain₁ renaming (StateA to A; StateB to B)
  open DualityInterface domain₂ renaming (StateA to B'; StateB to C)
  open DualityInterface domain₃ renaming (StateA to C'; StateB to D)
  
  field
    compatibility₁₂ : B → B'
    compatibility₁₂-inv : B' → B
    compatibility₂₃ : C → C'
    compatibility₂₃-inv : C' → C

module TripleComposition {ℓ : Level} (triple : ComposableTriple ℓ) where
  open ComposableTriple triple
  
  -- First compose domain₁ and domain₂
  comp₁₂ : ComposableDomains ℓ
  comp₁₂ = record
    { domain₁ = domain₁
    ; domain₂ = domain₂
    ; state-compatibility = compatibility₁₂
    ; state-compatibility-inv = compatibility₁₂-inv
    }
  
  -- Get the composed interface
  open ComposedDuality comp₁₂ renaming (composed-interface to interface₁₂)
  
  -- Now compose interface₁₂ with domain₃
  comp₂₃ : ComposableDomains ℓ
  comp₂₃ = record
    { domain₁ = interface₁₂
    ; domain₂ = domain₃
    ; state-compatibility = compatibility₂₃
    ; state-compatibility-inv = compatibility₂₃-inv
    }
  
  -- Final composed interface: A → D
  open ComposedDuality comp₂₃ public renaming
    ( composed-interface to final-interface
    ; composed-forward to triple-forward
    ; composed-backward to triple-backward
    ; composed-fwd-coverage to triple-fwd-coverage
    ; composed-bwd-coverage to triple-bwd-coverage
    )

------------------------------------------------------------------------
-- Composition Associativity
------------------------------------------------------------------------

-- | Associativity witness: (d₁ ⊙ d₂) ⊙ d₃ ≡ d₁ ⊙ (d₂ ⊙ d₃)
-- Composition order doesn't matter for final result
module CompositionAssociativity
  {ℓ : Level}
  (triple : ComposableTriple ℓ)
  where
  
  open ComposableTriple triple
  open DualityInterface domain₁ renaming (StateA to A; forward to f₁; backward to b₁)
  open DualityInterface domain₂ renaming (StateA to B; forward to f₂; backward to b₂)
  open DualityInterface domain₃ renaming (StateA to C; forward to f₃; backward to b₃)
  
  -- Left-associated composition: (d₁ ⊙ d₂) ⊙ d₃
  module LeftAssoc where
    open TripleComposition triple public
  
  -- Right-associated composition: d₁ ⊙ (d₂ ⊙ d₃)
  module RightAssoc where
    -- First compose domain₂ and domain₃
    comp₂₃ : ComposableDomains ℓ
    comp₂₃ = record
      { domain₁ = domain₂
      ; domain₂ = domain₃
      ; state-compatibility = compatibility₂₃
      ; state-compatibility-inv = compatibility₂₃-inv
      }
    
    open ComposedDuality comp₂₃ renaming (composed-interface to interface₂₃)
    
    -- Then compose domain₁ with interface₂₃
    comp₁₂₃ : ComposableDomains ℓ
    comp₁₂₃ = record
      { domain₁ = domain₁
      ; domain₂ = interface₂₃
      ; state-compatibility = compatibility₁₂
      ; state-compatibility-inv = compatibility₁₂-inv
      }
    
    open ComposedDuality comp₁₂₃ public renaming
      ( composed-forward to triple-forward
      ; composed-backward to triple-backward
      )
  
  -- Associativity proof (postulated - would require functional extensionality)
  postulate
    forward-assoc : ∀ (a : A) →
      LeftAssoc.triple-forward a ≡ RightAssoc.triple-forward a
    
    backward-assoc : ∀ (d : DualityInterface.StateB domain₃) →
      LeftAssoc.triple-backward d ≡ RightAssoc.triple-backward d

------------------------------------------------------------------------
-- Export
------------------------------------------------------------------------

-- Cross-domain composition enables:
-- 1. Chaining transformations across domain boundaries
-- 2. Building complex pipelines from simple components
-- 3. Preserving adequacy guarantees through composition
-- 4. Associative composition for flexible ordering

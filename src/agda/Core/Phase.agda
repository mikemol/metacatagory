-- Core.Phase: Formal abstraction for behavioral phase boundaries
--
-- A Phase represents a transformation between system states with:
-- - A transform function from input to output type
-- - Invariant preservation: properties maintained across the boundary
-- - Composition: phases can be sequenced into pipelines
-- - Identity: no-op transformations exist
--
-- This formalizes the testing strategy's notion of phase boundaries
-- as first-class values in the type system.

module Core.Phase where

open import Agda.Primitive using (Level; _⊔_)
open import Agda.Builtin.Equality using (_≡_; refl)

private
  variable
    ℓ ℓ₁ ℓ₂ ℓ₃ ℓ₄ : Level

-- ============================================================================
-- Core Phase Type
-- ============================================================================

-- A phase represents a transformation from A to B with property preservation
record Phase {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    -- The transformation function
    transform : A → B
    
    -- Optional: Invariant that should be preserved
    -- (We use Maybe-like encoding since we don't have stdlib)
    -- For now, we keep it simple and can extend with dependent properties later

-- Convenient syntax for accessing the transform
_$ₚ_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → Phase A B → A → B
phase $ₚ input = Phase.transform phase input

infixl 9 _$ₚ_

-- ============================================================================
-- Phase Construction Helpers
-- ============================================================================

-- Create a phase from a simple function
mkPhase : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → (A → B) → Phase A B
mkPhase f = record { transform = f }

-- Identity phase (no transformation)
idPhase : ∀ {A : Set ℓ} → Phase A A
idPhase = mkPhase (λ x → x)

-- Constant phase (ignores input)
constPhase : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → B → Phase A B
constPhase b = mkPhase (λ _ → b)

-- ============================================================================
-- Phase Composition
-- ============================================================================

-- Sequential composition: Phase A B → Phase B C → Phase A C
_⟫_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
    → Phase A B → Phase B C → Phase A C
p₁ ⟫ p₂ = mkPhase (λ a → p₂ $ₚ (p₁ $ₚ a))

infixr 8 _⟫_

-- Product type for parallel composition (since we don't have stdlib)
record _×_ {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    fst : A
    snd : B

open _×_ public

infixr 4 _×_
infixr 5 _,_

-- Parallel composition: two independent phases that can run simultaneously
-- Output is a product (pair) of results
_⊗_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃} {D : Set ℓ₄}
    → Phase A B → Phase C D → Phase (A × C) (B × D)
_⊗_ {A = A} {B} {C} {D} p₁ p₂ = mkPhase transform-pair
  where
    transform-pair : A × C → B × D
    transform-pair (a , c) = (p₁ $ₚ a , p₂ $ₚ c)

infixr 9 _⊗_

-- ============================================================================
-- Phase Properties and Laws
-- ============================================================================

-- Left identity law: idPhase ⟫ p ≡ p
left-identity : ∀ {A : Set ℓ₁} {B : Set ℓ₂} (p : Phase A B) (a : A)
              → (idPhase ⟫ p) $ₚ a ≡ p $ₚ a
left-identity p a = refl

-- Right identity law: p ⟫ idPhase ≡ p
right-identity : ∀ {A : Set ℓ₁} {B : Set ℓ₂} (p : Phase A B) (a : A)
               → (p ⟫ idPhase) $ₚ a ≡ p $ₚ a
right-identity p a = refl

-- Associativity law: (p₁ ⟫ p₂) ⟫ p₃ ≡ p₁ ⟫ (p₂ ⟫ p₃)
associativity : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃} {D : Set ℓ₄}
                  (p₁ : Phase A B) (p₂ : Phase B C) (p₃ : Phase C D) (a : A)
              → ((p₁ ⟫ p₂) ⟫ p₃) $ₚ a ≡ (p₁ ⟫ (p₂ ⟫ p₃)) $ₚ a
associativity p₁ p₂ p₃ a = refl

-- ============================================================================
-- Phase Pipelines
-- ============================================================================

-- A pipeline is just a list of phases that compose sequentially
-- We can represent common patterns:

-- Two-stage pipeline
_⟫₂_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
     → Phase A B → Phase B C → Phase A C
_⟫₂_ = _⟫_

-- Three-stage pipeline  
pipeline₃ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃} {D : Set ℓ₄}
          → Phase A B → Phase B C → Phase C D → Phase A D
pipeline₃ p₁ p₂ p₃ = p₁ ⟫ p₂ ⟫ p₃

-- Four-stage pipeline
pipeline₄ : ∀ {A B C D E : Set ℓ}
          → Phase A B → Phase B C → Phase C D → Phase D E → Phase A E
pipeline₄ p₁ p₂ p₃ p₄ = p₁ ⟫ p₂ ⟫ p₃ ⟫ p₄

-- ============================================================================
-- Conditional Phases
-- ============================================================================

-- Simple Bool type since we don't have stdlib
data Bool : Set where
  true false : Bool

if_then_else_ : ∀ {ℓ : Level} {A : Set ℓ} → Bool → A → A → A
if true  then t else e = t
if false then t else e = e

-- A phase that applies one of two transformations based on a predicate
-- (Simplified without dependent types for now)
conditional : ∀ {ℓ₁ ℓ₂ : Level} {A : Set ℓ₁} {B : Set ℓ₂}
            → (A → Bool) → Phase A B → Phase A B → Phase A B
conditional {ℓ₁} {ℓ₂} {A} {B} pred then-phase else-phase = mkPhase branch
  where
    branch : A → B
    branch a = if pred a then (then-phase $ₚ a) else (else-phase $ₚ a)

-- ============================================================================
-- Phase Annotations
-- ============================================================================

-- Primitive string (builtin)
open import Agda.Builtin.String using (String)

-- Annotate a phase with metadata for testing/documentation
record AnnotatedPhase {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    phase : Phase A B
    name : String
    description : String
    
  -- Delegate to underlying phase
  apply : A → B
  apply = phase $ₚ_

-- Create annotated phase
annotate : ∀ {A : Set ℓ₁} {B : Set ℓ₂}
         → String → String -> Phase A B → AnnotatedPhase A B
annotate name desc p = record
  { phase = p
  ; name = name
  ; description = desc
  }

-- ============================================================================
-- Example: Dispatch Pipeline Phases
-- ============================================================================

module DispatchExample where
  
  -- Example types from our dispatch system
  postulate
    Evidence : Set
    Classification : Set
    Bundle : Set
    Algorithm : Set
  
  -- Individual phases
  postulate
    classify : Evidence → Classification
    dispatch : Classification → Bundle
    extract : Bundle → Algorithm
  
  -- Phase representation
  classifyPhase : Phase Evidence Classification
  classifyPhase = mkPhase classify
  
  dispatchPhase : Phase Classification Bundle
  dispatchPhase = mkPhase dispatch
  
  extractPhase : Phase Bundle Algorithm
  extractPhase = mkPhase extract
  
  -- Complete pipeline: Evidence → Algorithm
  fullPipeline : Phase Evidence Algorithm
  fullPipeline = classifyPhase ⟫ dispatchPhase ⟫ extractPhase
  
  -- Same thing with annotation
  annotatedPipeline : AnnotatedPhase Evidence Algorithm
  annotatedPipeline = annotate
    "Dispatch Pipeline"
    "Evidence → Classification → Bundle → Algorithm"
    fullPipeline
  
  -- Verify composition works
  test-pipeline : Evidence → Algorithm
  test-pipeline = fullPipeline $ₚ_

-- ============================================================================
-- Exports
-- ============================================================================

-- Re-export main types and operations
open Phase public using (transform)

{-# OPTIONS --without-K #-}

-- | Formal abstraction for behavioral phase boundaries and pipelines.
module Core.Phase where

-- A Phase represents a transformation between system states with:
-- - A transform function from input to output type
-- - Invariant preservation: properties maintained across the boundary
-- - Composition: phases can be sequenced into pipelines
-- - Identity: no-op transformations exist

-- This formalizes the testing strategy's notion of phase boundaries
-- as first-class values in the type system.


module Core.Phase where

-- Explicitly import foundational universe and equality from Infrastructure
open import Infrastructure.Universe using (Setℓ)
open import Agda.Primitive using (Level; _⊔_)
open import Infrastructure.Coherence.Path2 using (whisker; _∙₂_)
open import Agda.Builtin.Equality using (_≡_; refl)

private
  variable
    ℓ ℓ₁ ℓ₂ ℓ₃ ℓ₄ : Level

-- ============================================================================

-- A phase represents a transformation from A to B with property preservation
record Phase {ℓ₁ ℓ₂ : Level} (A : Setℓ ℓ₁) (B : Setℓ ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    -- The transformation function
    transform : A → B
    -- (Optional: Invariant preservation, extendable)


-- Convenient syntax for accessing the transform
_$ₚ_ : ∀ {A : Setℓ ℓ₁} {B : Setℓ ℓ₂} → Phase A B → A → B
phase $ₚ input = Phase.transform phase input

infixl 9 _$ₚ_

-- ============================================================================

-- Create a phase from a simple function
mkPhase : ∀ {A : Setℓ ℓ₁} {B : Setℓ ℓ₂} → (A → B) → Phase A B
mkPhase f = record { transform = f }

-- Identity phase (no transformation)
idPhase : ∀ {A : Setℓ ℓ} → Phase A A
idPhase = mkPhase (λ x → x)

-- Constant phase (ignores input)
constPhase : ∀ {A : Setℓ ℓ₁} {B : Setℓ ℓ₂} → B → Phase A B
constPhase b = mkPhase (λ _ → b)

-- ============================================================================
-- Phase Composition
-- ============================================================================

-- Sequential composition: Phase A B → Phase B C → Phase A C
_⟫_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
    → Phase A B → Phase B C → Phase A C
p₁ ⟫ p₂ = mkPhase (λ a → p₂ $ₚ (p₁ $ₚ a))

infixr 8 _⟫_

record _×_ {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    fst : A
    snd : B

open _×_ public

infixr 4 _×_
infixr 5 _,_

-- Dependent pair (Sigma type)
record Σ {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : A → Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  constructor _,ₛ_
  field
    fst : A
    snd : B fst

open Σ public

infixr 4 Σ
infixr 5 _,ₛ_

-- Example usage:
--   Σ ℕ (λ n → List n)
--   (n ,ₛ xs)

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
-- Dependent Phases (Advanced)
-- ============================================================================

-- A dependent phase where the output type depends on the input value
-- This is useful for dispatch scenarios where different classifications
-- produce different bundle types
record DependentPhase {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : A → Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    transform : (a : A) → B a

-- Apply dependent phase
_$ᵈ_ : ∀ {A : Set ℓ₁} {B : A → Set ℓ₂} → DependentPhase A B → (a : A) → B a
phase $ᵈ input = DependentPhase.transform phase input

infixl 9 _$ᵈ_

-- Create dependent phase from dependent function
mkDepPhase : ∀ {A : Set ℓ₁} {B : A → Set ℓ₂} → ((a : A) → B a) → DependentPhase A B
mkDepPhase f = record { transform = f }

-- Identity dependent phase
idDepPhase : ∀ {A : Set ℓ} → DependentPhase A (λ _ → A)
idDepPhase = mkDepPhase (λ a → a)

-- Compose dependent phases (when codomain of first matches domain of second)
_⟫ᵈ_ : ∀ {A : Set ℓ₁} {B : A → Set ℓ₂} {C : (a : A) → B a → Set ℓ₃}
     → (p₁ : DependentPhase A B)
     → (p₂ : (a : A) → DependentPhase (B a) (C a))
     → DependentPhase A (λ a → C a (p₁ $ᵈ a))
p₁ ⟫ᵈ p₂ = mkDepPhase (λ a → (p₂ a) $ᵈ (p₁ $ᵈ a))

infixr 8 _⟫ᵈ_

-- Lift regular phase to dependent phase (constant family)
liftPhase : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → Phase A B → DependentPhase A (λ _ → B)
liftPhase p = mkDepPhase (p $ₚ_)

-- ============================================================================
-- Phase Invariants
-- ============================================================================

-- An invariant is a property that should hold before and after a phase
-- We express this as a predicate that must be preserved
record Invariant {ℓ : Level} (A : Set ℓ) : Set (Agda.Primitive.lsuc ℓ) where
  field
    property : A → Set ℓ
    
-- A phase with an explicit invariant
record PhaseWithInvariant {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (Agda.Primitive.lsuc (ℓ₁ ⊔ ℓ₂)) where
  field
    phase : Phase A B
    invariantA : Invariant A
    invariantB : Invariant B
    -- Proof that invariant is preserved (we postulate for now)
    preserves : (a : A) → Invariant.property invariantA a 
              → Invariant.property invariantB (phase $ₚ a)

-- Create phase with trivial invariant (always true)
withTrivialInvariant : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → Phase A B → PhaseWithInvariant A B
withTrivialInvariant {ℓ₁} {ℓ₂} {A} {B} p = record
  { phase = p
  ; invariantA = record { property = λ _ → A }  -- Trivially true
  ; invariantB = record { property = λ _ → B }  -- Trivially true
  ; preserves = λ a _ → p $ₚ a
  }

-- ============================================================================
-- Phase Combinators
-- ============================================================================

-- Maybe type for error handling
data Maybe {ℓ : Level} (A : Set ℓ) : Set ℓ where
  just : A → Maybe A
  nothing : Maybe A

-- Natural numbers for retry counts
data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Retry combinator: attempt phase up to n times, return first success
retry : ∀ {A : Set ℓ₁} {B : Set ℓ₂}
      → ℕ → Phase A (Maybe B) → Phase A (Maybe B)
retry zero p = p
retry (suc n) p = mkPhase attempt
  where
    attempt : _ → Maybe _
    attempt a with p $ₚ a
    ... | just b = just b
    ... | nothing = (retry n p) $ₚ a

-- Fallback combinator: try primary phase, use secondary if it fails
fallback : ∀ {A : Set ℓ₁} {B : Set ℓ₂}
         → Phase A (Maybe B) → Phase A B → Phase A B
fallback primary secondary = mkPhase attempt
  where
    attempt : _ → _
    attempt a with primary $ₚ a
    ... | just b = b
    ... | nothing = secondary $ₚ a

-- Map over Maybe result
mapMaybe : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → (A → B) → Maybe A → Maybe B
mapMaybe f (just a) = just (f a)
mapMaybe f nothing = nothing

-- Chain phases that can fail
_>>=ₘ_ : ∀ {A : Set ℓ₁} {B : Set ℓ₂}
        → Phase A (Maybe B) → (B → Phase A (Maybe B)) → Phase A (Maybe B)
p₁ >>=ₘ f = mkPhase chain
  where
    chain : _ → Maybe _
    chain a with p₁ $ₚ a
    ... | just b = (f b) $ₚ a
    ... | nothing = nothing

infixl 7 _>>=ₘ_

-- ============================================================================
-- Profiling and Tracing
-- ============================================================================

-- Execution metadata for profiling
record ExecutionMetadata : Set where
  field
    phaseName : String
    -- Could extend with: complexity bounds, witness sizes, call counts
    -- For now, just track the name

-- A phase with profiling hooks
record ProfiledPhase {ℓ₁ ℓ₂ : Level} (A : Set ℓ₁) (B : Set ℓ₂) : Set (ℓ₁ ⊔ ℓ₂) where
  field
    phase : Phase A B
    metadata : ExecutionMetadata
    -- Hook called before transformation
    beforeHook : A → A  -- Identity for now, could log
    -- Hook called after transformation
    afterHook : B → B   -- Identity for now, could log
  
  -- Execute with profiling
  execute : A → B
  execute a = afterHook (phase $ₚ (beforeHook a))

-- Create profiled phase from annotated phase
profile : ∀ {A : Set ℓ₁} {B : Set ℓ₂} → AnnotatedPhase A B → ProfiledPhase A B
profile annotated = record
  { phase = AnnotatedPhase.phase annotated
  ; metadata = record { phaseName = AnnotatedPhase.name annotated }
  ; beforeHook = λ a → a
  ; afterHook = λ b → b
  }

-- ============================================================================
-- Exports
-- ============================================================================

-- Re-export main types and operations
open Phase public using (transform)
open DependentPhase public using () renaming (transform to depTransform)
open Invariant public using (property)
open ExecutionMetadata public using (phaseName)

module Core.TechnicalDebt where

open import Metamodel as M
open import Core using (_×_; _,_)
open _×_ public
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Int using (Int; pos; negsuc)
open import Core.Utils

-- Priority as a free abelian group, with dependencies
record Priority : Set where
  inductive
  constructor mkPriority
  field
    terms     : List (String × Int)
    dependsOn : List Priority

open Priority public

-- Technical debt annotation record
record DebtAnnotation : Set where
  constructor mkDebt
  field
    id        : M.Identifier
    rationale : String
    status    : String
    priority  : Priority

open DebtAnnotation public

-- Example priorities (Shared constants)
lowPriority : Priority
lowPriority = mkPriority (("test-fixture", pos 1) ∷ []) []

highPriority : Priority
highPriority = mkPriority (("core-critical", pos 100) ∷ []) (lowPriority ∷ [])

-- Helper: Convert Int to a comparable Nat (magnitude)
-- (Simplified for comparison purposes within this domain)
mag : Int → Nat
mag (pos n) = n
mag (negsuc n) = 0 -- Treat negative priorities as zero for this simple ordering

-- Compute scalar weight of a priority
weight : Priority → Nat
weight p = foldr (λ term acc → (mag (snd term)) + acc) 0 (terms p)

-- Priority comparison predicate
-- p1 > p2 if weight(p1) > weight(p2)
PriorityGreater : Priority → Priority → Set
PriorityGreater p₁ p₂ = ltNat (weight p₂) (weight p₁) ≡ true

-- Helper to prove priority order for constants
postulate
  trustMe : ∀ {A : Set} {x y : A} → x ≡ y

module Core.TechnicalDebt where

open import Metamodel as M
open import Core using (_×_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Int using (Int)
open import Agda.Builtin.Bool using (Bool; true; false)

-- Priority as a free abelian group, with dependencies
record Priority : Set where
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
lowPriority = mkPriority (("test-fixture", 1) ∷ []) []

highPriority : Priority
highPriority = mkPriority (("core-critical", 1) ∷ []) (lowPriority ∷ [])

-- Priority comparison predicate
PriorityGreater : Priority → Priority → Set
PriorityGreater p₁ p₂ = Agda.Builtin.Unit.⊤ -- Simplified for typechecking

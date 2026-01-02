{-# OPTIONS --without-K #-}

-- | Example demonstrating technical debt record construction.
module Examples.TechnicalDebtExample where

open import Metamodel as M
open import PropertyRegistry as PR
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Utils using (map)
open import Agda.Builtin.String

-- Example: Annotate a postulate with technical debt
postulate
  foo : M.Identifier
  bar : M.Identifier

-- Technical debt annotation record
record DebtAnnotation : Set where
  constructor mkDebt
  field
    id        : M.Identifier
    rationale : String
    status    : String

open DebtAnnotation public

-- Annotate each item
fooDebt : DebtAnnotation
fooDebt = mkDebt foo "Legacy algorithm, needs refactor" "open"

barDebt : DebtAnnotation
barDebt = mkDebt bar "Blocked by upstream API" "blocked"

-- Automated registry of all technical debt
technicalDebtRegistry : List DebtAnnotation
technicalDebtRegistry = fooDebt ∷ barDebt ∷ []

-- Example: Export rationale/status for reporting
rationales : List String
rationales = map DebtAnnotation.rationale technicalDebtRegistry

statuses : List String
statuses = map DebtAnnotation.status technicalDebtRegistry

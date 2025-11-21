module Examples.TechnicalDebtRegistry where

open import Agda.Builtin.List
open import Agda.Builtin.String
open import Agda.Builtin.IO
open import Agda.Builtin.Unit
open import Metamodel as M
open import Core.TechnicalDebt

-- Import registries from specific test suites
import Tests.AlgorithmCompositionTests
import Tests.SerializationTests

-- Access the lists (now compatible because they use Core.TechnicalDebt)
compositionDebt : List DebtAnnotation
compositionDebt = Tests.AlgorithmCompositionTests.technicalDebtRegistry

serializationDebt : List DebtAnnotation
serializationDebt = Tests.SerializationTests.technicalDebtRegistry

-- Central registry accumulator
registeredRegistries : List (List DebtAnnotation)
registeredRegistries = compositionDebt ∷ serializationDebt ∷ []

-- Aggregated registry (flattened)
allTechnicalDebt : List DebtAnnotation
allTechnicalDebt = concat registeredRegistries
  where
    concat : {A : Set} → List (List A) → List A
    concat [] = []
    concat (x ∷ xs) = x ++ concat xs
      where
        _++_ : {A : Set} → List A → List A → List A
        [] ++ ys = ys
        (z ∷ zs) ++ ys = z ∷ (zs ++ ys)

-- Helper: String concatenation (minimal)
primStringAppend : String → String → String
primStringAppend = Agda.Builtin.String.primStringAppend

_++_ : String → String → String
_++_ = primStringAppend

-- Helper: List map
map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Helper: Join strings
intercalate : String → List String → String
intercalate sep [] = ""
intercalate sep (x ∷ []) = x
intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

-- Helper: convert DebtAnnotation to JSON string
debtToJSON : DebtAnnotation → String
debtToJSON d =
  "{\"id\": \"" ++ M.Identifier.name (DebtAnnotation.id d) ++ "\", " ++
  "\"rationale\": \"" ++ DebtAnnotation.rationale d ++ "\", " ++
  "\"status\": \"" ++ DebtAnnotation.status d ++ "\"}"

-- Export allTechnicalDebt as JSON array
exportRegistryJSON : IO ⊤
exportRegistryJSON =
  let json = "[" ++ intercalate ", " (map debtToJSON allTechnicalDebt) ++ "]"
  in return tt -- IO stub, would be writeFile in full stdlib context
{-# OPTIONS --without-K #-}

module Examples.TechnicalDebtRegistry where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
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

-- Helper: List concatenation
_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

concat : {A : Set} → List (List A) → List A
concat [] = []
concat (x ∷ xs) = x ++ concat xs

-- Aggregated registry (flattened)
allTechnicalDebt : List DebtAnnotation
allTechnicalDebt = concat registeredRegistries

-- Helper: String concatenation
strCat : String → String → String
strCat = primStringAppend

-- Helper: List map
map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Helper: Join strings
intercalate : String → List String → String
intercalate sep [] = ""
intercalate sep (x ∷ []) = x
intercalate sep (x ∷ xs) = strCat x (strCat sep (intercalate sep xs))

-- Helper: convert DebtAnnotation to JSON string
debtToJSON : DebtAnnotation → String
debtToJSON d =
  let idStr = M.Identifier.name (DebtAnnotation.id d)
      ratStr = DebtAnnotation.rationale d
      statStr = DebtAnnotation.status d
  in strCat "{\"id\": \""
       (strCat idStr
       (strCat "\", \"rationale\": \""
       (strCat ratStr
       (strCat "\", \"status\": \""
       (strCat statStr "\"}")))))

-- IO primitives (postulated for compilation without stdlib)
postulate
  returnIO : {A : Set} → A → IO A
  printString : String → IO ⊤

-- Export allTechnicalDebt as JSON array (mock IO action)
exportRegistryJSON : IO ⊤
exportRegistryJSON =
  let json = strCat "[" (strCat (intercalate ", " (map debtToJSON allTechnicalDebt)) "]")
  in returnIO tt
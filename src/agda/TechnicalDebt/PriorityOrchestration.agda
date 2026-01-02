{-# OPTIONS --guardedness #-}

-- | Orchestration layer for priority strategies; ties logic/formatting to I/O with parameterized effects.
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Unit
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Int using (Int)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Primitive using (Level)
open import Core.Phase using (_×_; Σ)
open _×_

-- | Orchestration layer for priority strategies; ties logic/formatting to I/O.
--   Uses module parameterization for I/O operations (not postulates).
module TechnicalDebt.PriorityOrchestration
  -- I/O Operations (to be provided by FFI implementation)
  (writeFile : String → String → IO ⊤)
  (readFile : String → IO String)
  (fileExists : String → IO String)  -- returns "true" or "false"
  (putStrLn : String → IO ⊤)
  (reportSuccess : String → IO ⊤)
  (reportError : String → IO ⊤)
  (validateJSON : String → IO String)  -- returns "valid" or error message
  -- Formatting operations (from parameterized formatting module)
  (intToString : Int → String)
  -- Monad operations (to sequence I/O)
  (bind : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B)
  (pure : ∀ {ℓ} {A : Set ℓ} → A → IO A)
  where

-- Monad operators supplied as module parameters
-- bind: sequencing; pure: inject value into IO
-- This keeps I/O effects parameterized (no postulates in this module).
_>>=_ : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
_>>=_ = bind

return : ∀ {ℓ} {A : Set ℓ} → A → IO A
return = pure

_>>_ : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → IO B → IO B
_>>_ m n = m >>= λ _ → n

open import TechnicalDebt.Priorities
open import TechnicalDebt.PriorityMapping
import TechnicalDebt.PriorityFormatting
open module PriorityFormatting = TechnicalDebt.PriorityFormatting intToString
open import TechnicalDebt.DeferredItemsFormatting

------------------------------------------------------------------------
-- Minimal JSON rendering (pure string concatenation)
------------------------------------------------------------------------

infixr 5 _⊕_
_⊕_ : String → String → String
_⊕_ = primStringAppend

quoteStr : String → String
quoteStr s = "\"" ⊕ s ⊕ "\""

renderWeights : CategoryWeights → String
renderWeights w =
  "{" ⊕
  "\"postulate\":" ⊕ intToString (CategoryWeights.postulateWeight w) ⊕ "," ⊕
  "\"todo\":" ⊕ intToString (CategoryWeights.todoWeight w) ⊕ "," ⊕
  "\"fixme\":" ⊕ intToString (CategoryWeights.fixmeWeight w) ⊕ "," ⊕
  "\"deviation\":" ⊕ intToString (CategoryWeights.deviationWeight w) ⊕
  "}"

renderStrategyEntry : (String × PriorityStrategy) → String
renderStrategyEntry pair =
  let name  = _×_.fst pair
      strat = _×_.snd pair
  in quoteStr name ⊕ ":{\"weights\":" ⊕ renderWeights (strategyToWeights strat) ⊕ "}"

renderCommaList : List String → String
renderCommaList [] = ""
renderCommaList (x ∷ []) = x
renderCommaList (x ∷ xs) = x ⊕ "," ⊕ renderCommaList xs

renderStrategyMap : List (String × PriorityStrategy) → String
renderStrategyMap [] = "{}"
renderStrategyMap xs = "{" ⊕ renderCommaList (map renderStrategyEntry xs) ⊕ "}"

renderRoot : String → List (String × PriorityStrategy) → String
renderRoot active strategies =
  "{" ⊕
  "\"active\":" ⊕ quoteStr active ⊕ "," ⊕
  "\"strategies\":" ⊕ renderStrategyMap strategies ⊕
  "}"

strategyEntries : List (String × PriorityStrategy)
strategyEntries =
  (Core.Phase._,_ "default" defaultStrategy)
  ∷ (Core.Phase._,_ "ffiSafety" ffiSafetyStrategy)
  ∷ (Core.Phase._,_ "proofCompleteness" proofCompletenessStrategy)
  ∷ (Core.Phase._,_ "rapidDevelopment" rapidDevelopmentStrategy)
  ∷ (Core.Phase._,_ "production" productionStrategy)
  ∷ []

exportAllStrategiesAUDAXMarkdown : IO ⊤
exportAllStrategiesAUDAXMarkdown = do
  let audaxDoc = PriorityFormatting.formatAllStrategiesAUDAXDoc ((Core.Phase._,_ "default" defaultStrategy)
                                                              ∷ (Core.Phase._,_ "ffiSafety" ffiSafetyStrategy)
                                                              ∷ (Core.Phase._,_ "proofCompleteness" proofCompletenessStrategy)
                                                              ∷ (Core.Phase._,_ "rapidDevelopment" rapidDevelopmentStrategy)
                                                              ∷ (Core.Phase._,_ "production" productionStrategy)
                                                              ∷ [])
  let audaxStr = TechnicalDebt.DeferredItemsFormatting.audaxDocToMarkdown audaxDoc
  putStrLn "Exporting AUDAX Markdown for all strategies..."
  writeFile "build/priority_strategies_audax.md" audaxStr
  putStrLn "✓ Generated: build/priority_strategies_audax.md"
  reportSuccess "AUDAX Markdown export complete"

generateReferenceConfig : IO ⊤
generateReferenceConfig = do
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "PRIORITY STRATEGY ORCHESTRATION"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  putStrLn "LAYER STATUS:"
  putStrLn "  ✓ LOGIC LAYER (Agda):        TechnicalDebt.PriorityMapping"
  putStrLn "  ✓ FORMAT LAYER (Agda):       TechnicalDebt.PriorityFormatting"
  putStrLn "  ✓ ORCHESTRATION LAYER (Agda): TechnicalDebt.PriorityOrchestration"
  putStrLn ""
  putStrLn "Generating strategy profiles..."
  let jsonOutput = renderRoot "default" strategyEntries
  writeFile "build/priority_strategy_profiles.json" jsonOutput
  putStrLn "✓ Generated: build/priority_strategy_profiles.json"
  putStrLn ""
  validationResult ← validateJSON jsonOutput
  putStrLn (PriorityFormatting.concatStr "Validation: " validationResult)
  putStrLn ""
  reportSuccess "Priority strategy orchestration complete"

exportStrategyProfile : String → PriorityStrategy → IO ⊤
exportStrategyProfile strategyName strategy = do
  let filename = PriorityFormatting.concatStr (PriorityFormatting.concatStr "build/strategy_" strategyName) ".json"
  putStrLn (PriorityFormatting.concatStr (PriorityFormatting.concatStr "Exporting " strategyName) " strategy...")
  let jsonStr = "{" ⊕ "\"name\":" ⊕ quoteStr strategyName ⊕ ",\"weights\":" ⊕ renderWeights (strategyToWeights strategy) ⊕ "}"
  writeFile filename jsonStr
  putStrLn (PriorityFormatting.concatStr "✓ Generated: " filename)

exportAllStrategies : IO ⊤
exportAllStrategies = do
  putStrLn "Exporting individual strategy profiles..."
  putStrLn ""
  exportStrategyProfile "default" defaultStrategy
  exportStrategyProfile "ffiSafety" ffiSafetyStrategy
  exportStrategyProfile "proofCompleteness" proofCompletenessStrategy
  exportStrategyProfile "rapidDevelopment" rapidDevelopmentStrategy
  exportStrategyProfile "production" productionStrategy
  putStrLn ""
  reportSuccess "All individual strategies exported"

displayStrategyWeights : String → PriorityStrategy → IO ⊤
displayStrategyWeights strategyName strategy = do
  let weights = strategyToWeights strategy
  putStrLn (PriorityFormatting.concatStr "Strategy: " strategyName)
  putStrLn (PriorityFormatting.concatStr "  postulate: " (intToString (CategoryWeights.postulateWeight weights)))
  putStrLn (PriorityFormatting.concatStr "  todo: " (intToString (CategoryWeights.todoWeight weights)))
  putStrLn (PriorityFormatting.concatStr "  fixme: " (intToString (CategoryWeights.fixmeWeight weights)))
  putStrLn (PriorityFormatting.concatStr "  deviation: " (intToString (CategoryWeights.deviationWeight weights)))
  putStrLn ""

validateConfiguration : IO ⊤
validateConfiguration = do
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "CONFIGURATION VALIDATION"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  exists ← fileExists ".github/badges/weights.json"
  putStrLn (PriorityFormatting.concatStr "Current weights.json exists: " exists)
  putStrLn ""
  -- If exists, could compare with generated profiles
  -- (comparison logic would be implemented via FFI)
  reportSuccess "Validation complete"

main : IO ⊤
main = do
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║     AGDA PRIORITY STRATEGY ORCHESTRATION SYSTEM                  ║"
  putStrLn "║     Complete Logic-Formatting-Orchestration in Agda              ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""
  generateReferenceConfig
  putStrLn ""
  exportAllStrategies
  putStrLn ""
  exportAllStrategiesAUDAXMarkdown
  putStrLn ""
  validateConfiguration
  putStrLn ""
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "✓ ALL LAYERS OPERATING IN AGDA"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  reportSuccess "Orchestration system complete"

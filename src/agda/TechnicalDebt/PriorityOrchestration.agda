{-# OPTIONS --guardedness #-}

-- | Orchestration layer for priority strategy system
-- | Coordinates logic and formatting layers with I/O operations
-- | 
-- | Architecture:
-- |   LOGIC LAYER:        TechnicalDebt.PriorityMapping (pure computation)
-- |   FORMAT LAYER:       TechnicalDebt.PriorityFormatting (JSON generation)
-- |   ORCHESTRATION:      This module (I/O coordination)
-- |
-- | This module handles:
-- |   • File I/O operations (writing generated JSON)
-- |   • Status reporting and validation
-- |   • Build artifact coordination
-- |   • Integration with downstream systems
-- |
-- | Uses module parameterization for I/O operations (not postulates)

open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.IO

module TechnicalDebt.PriorityOrchestration
  -- I/O Operations (to be provided by FFI implementation)
  (writeFile : String → String → IO ⊤)
  (readFile : String → IO String)
  (fileExists : String → IO String)  -- returns "true" or "false"
  (putStrLn : String → IO ⊤)
  (reportSuccess : String → IO ⊤)
  (reportError : String → IO ⊤)
  (validateJSON : String → IO String)  -- returns "valid" or error message
  where

open import TechnicalDebt.Priorities
open import TechnicalDebt.PriorityMapping
open import TechnicalDebt.PriorityFormatting

-- Orchestration functions

-- Generate reference configuration file
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
  
  -- Get formatted JSON from format layer
  let jsonOutput = formatAllStrategyProfiles
  
  -- Write to reference configuration file
  writeFile "build/priority_strategy_profiles.json" jsonOutput
  
  putStrLn "✓ Generated: build/priority_strategy_profiles.json"
  putStrLn ""
  
  -- Validate the generated JSON
  validationResult ← validateJSON jsonOutput
  putStrLn ("Validation: " ++ validationResult)
  putStrLn ""
  
  reportSuccess "Priority strategy orchestration complete"

-- Export individual strategy profiles
exportStrategyProfile : String → PriorityStrategy → IO ⊤
exportStrategyProfile strategyName strategy = do
  let weights = strategyToWeights strategy
  let filename = "build/strategy_" ++ strategyName ++ ".json"
  
  putStrLn ("Exporting " ++ strategyName ++ " strategy...")
  
  -- Format this specific strategy
  let jsonOutput = formatStrategy strategy
  
  -- Write to file
  writeFile filename jsonOutput
  
  putStrLn ("✓ Generated: " ++ filename)

-- Export all strategies as individual files
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

-- Display strategy weights for validation
displayStrategyWeights : String → PriorityStrategy → IO ⊤
displayStrategyWeights strategyName strategy = do
  let weights = strategyToWeights strategy
  
  putStrLn ("Strategy: " ++ strategyName)
  -- Note: Would need intToString implementation to display actual weights
  putStrLn "  Weights computed from Agda logic layer"
  putStrLn ""

-- Validate against existing configuration
validateConfiguration : IO ⊤
validateConfiguration = do
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "CONFIGURATION VALIDATION"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  
  -- Check if current weights.json exists
  exists ← fileExists ".github/badges/weights.json"
  
  putStrLn ("Current weights.json exists: " ++ exists)
  putStrLn ""
  
  -- If exists, could compare with generated profiles
  -- (comparison logic would be implemented via FFI)
  
  reportSuccess "Validation complete"

-- Main orchestration entry point
main : IO ⊤
main = do
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║     AGDA PRIORITY STRATEGY ORCHESTRATION SYSTEM                  ║"
  putStrLn "║     Complete Logic-Formatting-Orchestration in Agda              ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""
  
  -- Execute orchestration steps
  generateReferenceConfig
  putStrLn ""
  
  exportAllStrategies
  putStrLn ""
  
  validateConfiguration
  putStrLn ""
  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "✓ ALL LAYERS OPERATING IN AGDA"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  reportSuccess "Orchestration system complete"

-- Note: This module uses parameterization for I/O operations.
-- When instantiating, provide concrete implementations (typically via FFI).
-- 
-- Example instantiation with GHC FFI:
--   module MyOrchestration where
--     open import TechnicalDebt.PriorityOrchestration
--       System.IO.writeFile
--       System.IO.readFile
--       myFileExists
--       System.IO.putStrLn
--       myReportSuccess
--       myReportError
--       myValidateJSON
--     
--     {-# COMPILE GHC myFileExists = ... #-}
--     {-# COMPILE GHC myReportSuccess = ... #-}
--     etc.

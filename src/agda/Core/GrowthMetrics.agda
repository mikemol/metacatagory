{-# OPTIONS --without-K #-}

-- Core.GrowthMetrics: Solution space growth rate instrumentation
-- PHASE-V.2: Track coordinate allocation patterns and solution space expansion

module Core.GrowthMetrics where

open import Metamodel as M
open import Core.Utils -- Uses consolidated utils
open import Agda.Builtin.String using (String)

-- ============================================================================
-- Phase 14: Solution Space Growth Instrumentation
-- ============================================================================

-- Coordinate allocation event
record CoordinateAllocation : Set where
  field
    coordinate : M.Coordinate
    timestamp : Nat
    context : String
    
-- Phase density
record PhaseDensity : Set where
  field
    phaseNumber : Nat
    objectCount : Nat
    firstAllocation : Maybe Nat
    lastAllocation : Maybe Nat
    
-- Y-coordinate distribution
record YCoordinateDistribution : Set where
  field
    phaseNumber : Nat
    minY : Nat
    maxY : Nat
    distinctYValues : Nat
    yValueGaps : List Nat

-- Growth rate metrics
record GrowthRate : Set where
  field
    totalAllocations : Nat
    phasesUsed : Nat
    averageObjectsPerPhase : Nat
    maxDensityPhase : Nat
    maxDensityCount : Nat
    sparsestPhase : Nat
    sparsestCount : Nat

-- Expansion pattern
data ExpansionPattern : Set where
  HorizontalGrowth : ExpansionPattern
  VerticalGrowth : ExpansionPattern
  BalancedGrowth : ExpansionPattern
  SparseGrowth : ExpansionPattern

-- Phase saturation
record PhaseSaturation : Set where
  field
    phaseNumber : Nat
    currentDensity : Nat
    estimatedCapacity : Maybe Nat
    saturationPercentage : Nat
    isSaturated : Bool

-- Growth snapshot
record GrowthSnapshot : Set where
  field
    snapshotTimestamp : Nat
    totalCoordinates : Nat
    activePhases : List PhaseDensity
    expansionPattern : ExpansionPattern
    saturatedPhases : List PhaseSaturation
    growthRate : GrowthRate

-- ============================================================================
-- Growth Metric Computation
-- ============================================================================

-- Calculate density for a specific phase
calculatePhaseDensity : Nat → List CoordinateAllocation → PhaseDensity
calculatePhaseDensity phase allocations = record
  { phaseNumber = phase
  ; objectCount = countPhaseAllocations phase allocations
  ; firstAllocation = findFirstAllocation phase allocations
  ; lastAllocation = findLastAllocation phase allocations
  }
  where
    countPhaseAllocations : Nat → List CoordinateAllocation → Nat
    countPhaseAllocations _ [] = zero
    countPhaseAllocations p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p 
         then suc (countPhaseAllocations p as) 
         else countPhaseAllocations p as
    
    findFirstAllocation : Nat → List CoordinateAllocation → Maybe Nat
    findFirstAllocation _ [] = nothing
    findFirstAllocation p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p 
         then just (CoordinateAllocation.timestamp a) 
         else findFirstAllocation p as
    
    findLastAllocation : Nat → List CoordinateAllocation → Maybe Nat
    findLastAllocation p allocations = findLastHelper p allocations nothing
      where
        findLastHelper : Nat → List CoordinateAllocation → Maybe Nat → Maybe Nat
        findLastHelper _ [] acc = acc
        findLastHelper p (a ∷ as) acc =
          let coord = CoordinateAllocation.coordinate a
          in if eqNat (M.Coordinate.x coord) p 
             then findLastHelper p as (just (CoordinateAllocation.timestamp a)) 
             else findLastHelper p as acc

-- Analyze Y-coordinate distribution
analyzeYDistribution : Nat → List CoordinateAllocation → YCoordinateDistribution
analyzeYDistribution phase allocations = record
  { phaseNumber = phase
  ; minY = computeMinY phase allocations
  ; maxY = computeMaxY phase allocations
  ; distinctYValues = countDistinctY phase allocations
  ; yValueGaps = []
  }
  where
    computeMinY : Nat → List CoordinateAllocation → Nat
    computeMinY _ [] = zero
    computeMinY p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p 
         then minNat (M.Coordinate.y coord) (computeMinY p as) 
         else computeMinY p as
    
    computeMaxY : Nat → List CoordinateAllocation → Nat
    computeMaxY _ [] = zero
    computeMaxY p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p 
         then maxNat (M.Coordinate.y coord) (computeMaxY p as) 
         else computeMaxY p as
    
    countDistinctY : Nat → List CoordinateAllocation → Nat
    countDistinctY _ [] = zero
    countDistinctY p allocations = countHelper p allocations []
      where
        countHelper : Nat → List CoordinateAllocation → List Nat → Nat
        countHelper _ [] seen = length seen
        countHelper p (a ∷ as) seen =
          let coord = CoordinateAllocation.coordinate a
              yVal = M.Coordinate.y coord
          in if eqNat (M.Coordinate.x coord) p 
             then (if member yVal seen then countHelper p as seen else countHelper p as (yVal ∷ seen))
             else countHelper p as seen

-- Compute overall growth rate
-- Uses divNat from Core.Utils (no termination pragma needed)
computeGrowthRate : List CoordinateAllocation → GrowthRate
computeGrowthRate allocations = record
  { totalAllocations = countAllocations allocations
  ; phasesUsed = countUniquePhases allocations
  ; averageObjectsPerPhase = divNat (countAllocations allocations) (maxNat (suc zero) (countUniquePhases allocations))
  ; maxDensityPhase = zero
  ; maxDensityCount = zero
  ; sparsestPhase = zero
  ; sparsestCount = zero
  }
  where
    countAllocations : List CoordinateAllocation → Nat
    countAllocations = length
    
    countUniquePhases : List CoordinateAllocation → Nat
    countUniquePhases allocations = countHelper allocations []
      where
        countHelper : List CoordinateAllocation → List Nat → Nat
        countHelper [] seen = length seen
        countHelper (a ∷ as) seen =
          let phase = M.Coordinate.x (CoordinateAllocation.coordinate a)
          in if member phase seen then countHelper as seen else countHelper as (phase ∷ seen)

-- Classify expansion pattern
classifyExpansionPattern : GrowthRate → ExpansionPattern
classifyExpansionPattern rate =
  let avgPerPhase = GrowthRate.averageObjectsPerPhase rate
      phaseCount = GrowthRate.phasesUsed rate
  in if ltNat avgPerPhase 3 then SparseGrowth
     else if ltNat phaseCount 5 then VerticalGrowth
     else if ltNat 10 avgPerPhase then VerticalGrowth else HorizontalGrowth

-- Check if a phase is approaching saturation
checkPhaseSaturation : PhaseDensity → PhaseSaturation
checkPhaseSaturation density = record
  { phaseNumber = PhaseDensity.phaseNumber density
  ; currentDensity = PhaseDensity.objectCount density
  ; estimatedCapacity = nothing
  ; saturationPercentage = zero
  ; isSaturated = false
  }

-- Create a growth snapshot
captureGrowthSnapshot : Nat → List CoordinateAllocation → GrowthSnapshot
captureGrowthSnapshot timestamp allocations =
  let rate = computeGrowthRate allocations
  in record
    { snapshotTimestamp = timestamp
    ; totalCoordinates = GrowthRate.totalAllocations rate
    ; activePhases = []
    ; expansionPattern = classifyExpansionPattern rate
    ; saturatedPhases = []
    ; growthRate = rate
    }

-- ============================================================================
-- Verification Helpers
-- ============================================================================

verifyGrowthRate : GrowthRate → Bool
verifyGrowthRate rate =
  let total = GrowthRate.totalAllocations rate
      phases = GrowthRate.phasesUsed rate
  in (ltNat zero total) && (ltNat zero phases)

verifyPhaseDensity : PhaseDensity → Bool
verifyPhaseDensity density = ltNat zero (PhaseDensity.objectCount density)

verifyGrowthSnapshot : GrowthSnapshot → Bool
verifyGrowthSnapshot snapshot = verifyGrowthRate (GrowthSnapshot.growthRate snapshot)

-- ============================================================================
-- Concrete Growth Tracking Instances
-- ============================================================================

metacatagoryGrowthHistory : List CoordinateAllocation
metacatagoryGrowthHistory =
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "dispatch-root" } ∷
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  record { coordinate = M.mkCoord 1 1 ; timestamp = 2 ; context = "field-extension" } ∷
  record { coordinate = M.mkCoord 2 0 ; timestamp = 3 ; context = "galois-group" } ∷
  record { coordinate = M.mkCoord 4 0 ; timestamp = 4 ; context = "yoneda-embedding" } ∷
  record { coordinate = M.mkCoord 4 1 ; timestamp = 5 ; context = "yoneda-iso" } ∷
  record { coordinate = M.mkCoord 13 0 ; timestamp = 6 ; context = "godel-sentence" } ∷
  record { coordinate = M.mkCoord 13 1 ; timestamp = 7 ; context = "incompleteness-first" } ∷
  record { coordinate = M.mkCoord 13 2 ; timestamp = 8 ; context = "incompleteness-second" } ∷
  []

metacatagoryGrowthSnapshot : GrowthSnapshot
metacatagoryGrowthSnapshot = captureGrowthSnapshot 9 metacatagoryGrowthHistory

phase13Density : PhaseDensity
phase13Density = calculatePhaseDensity 13 metacatagoryGrowthHistory

phase13YDistribution : YCoordinateDistribution
phase13YDistribution = analyzeYDistribution 13 metacatagoryGrowthHistory

metacatagoryGrowthRate : GrowthRate
metacatagoryGrowthRate = computeGrowthRate metacatagoryGrowthHistory

metacatagoryExpansionPattern : ExpansionPattern
metacatagoryExpansionPattern = classifyExpansionPattern metacatagoryGrowthRate

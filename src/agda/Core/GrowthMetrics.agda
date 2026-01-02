{-# OPTIONS --without-K #-}

-- | Growth metrics over algebraic structures and algorithms.
module Core.GrowthMetrics where

-- Core.GrowthMetrics: Solution space growth rate instrumentation
-- PHASE-V.2: Track coordinate allocation patterns and solution space expansion


-- Infrastructure imports for universe polymorphism and equality
open import Infrastructure.Universe using (Setℓ)
open import Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

open import Metamodel as M
open import Core.Utils -- Uses consolidated utils
open import Agda.Builtin.String using (String)

-- ============================================================================
-- Phase 14: Solution Space Growth Instrumentation
-- ============================================================================

-- | Event recording allocation of a coordinate at a timestamp with context.
record CoordinateAllocation : Set where
  field
    coordinate : M.Coordinate
    timestamp : Nat
    context : String

-- | Density summary for a single phase index.
record PhaseDensity : Set where
  field
    phaseNumber : Nat
    objectCount : Nat
    firstAllocation : Maybe Nat
    lastAllocation : Maybe Nat

-- | Distribution of Y coordinates within a phase.
record YCoordinateDistribution : Set where
  field
    phaseNumber : Nat
    minY : Nat
    maxY : Nat
    distinctYValues : Nat
    yValueGaps : List Nat
    
-- | Aggregate growth rate metrics across phases.
record GrowthRate : Set where
  field
    totalAllocations : Nat
    phasesUsed : Nat
    averageObjectsPerPhase : Nat
    maxDensityPhase : Nat
    maxDensityCount : Nat
    sparsestPhase : Nat
    sparsestCount : Nat
    
-- | High-level classification of expansion behavior.
data ExpansionPattern : Set where
  HorizontalGrowth : ExpansionPattern
  VerticalGrowth : ExpansionPattern
  BalancedGrowth : ExpansionPattern
  SparseGrowth : ExpansionPattern

-- | Saturation status of a single phase.
record PhaseSaturation : Set where
  field
    phaseNumber : Nat
    currentDensity : Nat
    estimatedCapacity : Maybe Nat
    saturationPercentage : Nat
    isSaturated : Bool

-- | Snapshot capturing current growth metrics and derived summaries.
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

-- | Calculate density statistics for a specific phase.
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

-- | Analyze Y-coordinate distribution for a given phase.
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
-- | Derive aggregate growth rate over all recorded allocations.
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

-- | Classify high-level expansion pattern from a growth rate.
classifyExpansionPattern : GrowthRate → ExpansionPattern
classifyExpansionPattern rate =
  let avgPerPhase = GrowthRate.averageObjectsPerPhase rate
      phaseCount = GrowthRate.phasesUsed rate
  in if ltNat avgPerPhase 3 then SparseGrowth
     else if ltNat phaseCount 5 then VerticalGrowth
     else if ltNat 10 avgPerPhase then VerticalGrowth else HorizontalGrowth

-- | Check if a phase is approaching saturation.
checkPhaseSaturation : PhaseDensity → PhaseSaturation
checkPhaseSaturation density = record
  { phaseNumber = PhaseDensity.phaseNumber density
  ; currentDensity = PhaseDensity.objectCount density
  ; estimatedCapacity = nothing
  ; saturationPercentage = zero
  ; isSaturated = false
  }

-- | Capture a growth snapshot at a timestamp from allocation history.
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

-- | Validate that a growth rate accounts for at least one allocation and phase.
verifyGrowthRate : GrowthRate → Bool
verifyGrowthRate rate =
  let total = GrowthRate.totalAllocations rate
      phases = GrowthRate.phasesUsed rate
  in (ltNat zero total) && (ltNat zero phases)

-- | Ensure a phase density counts at least one object.
verifyPhaseDensity : PhaseDensity → Bool
verifyPhaseDensity density = ltNat zero (PhaseDensity.objectCount density)

-- | Validate a growth snapshot by rechecking its rate.
verifyGrowthSnapshot : GrowthSnapshot → Bool
verifyGrowthSnapshot snapshot = verifyGrowthRate (GrowthSnapshot.growthRate snapshot)

-- ============================================================================
-- Concrete Growth Tracking Instances
-- ============================================================================

-- | Hand-authored timeline of notable allocations for the project.
metacatagoryGrowthHistory : List CoordinateAllocation
metacatagoryGrowthHistory =
  -- seed dispatch root
  record { coordinate = M.mkCoord 0 0 ; timestamp = 0 ; context = "dispatch-root" } ∷
  -- early algebra steps
  record { coordinate = M.mkCoord 1 0 ; timestamp = 1 ; context = "field-basic" } ∷
  -- extension step
  record { coordinate = M.mkCoord 1 1 ; timestamp = 2 ; context = "field-extension" } ∷
  -- galois branch
  record { coordinate = M.mkCoord 2 0 ; timestamp = 3 ; context = "galois-group" } ∷
  -- yoneda developments
  record { coordinate = M.mkCoord 4 0 ; timestamp = 4 ; context = "yoneda-embedding" } ∷
  -- yoneda isomorphism
  record { coordinate = M.mkCoord 4 1 ; timestamp = 5 ; context = "yoneda-iso" } ∷
  -- incompleteness branch
  record { coordinate = M.mkCoord 13 0 ; timestamp = 6 ; context = "godel-sentence" } ∷
  -- first incompleteness
  record { coordinate = M.mkCoord 13 1 ; timestamp = 7 ; context = "incompleteness-first" } ∷
  -- second incompleteness
  record { coordinate = M.mkCoord 13 2 ; timestamp = 8 ; context = "incompleteness-second" } ∷
  []

-- | Snapshot derived from the authored history.
metacatagoryGrowthSnapshot : GrowthSnapshot
metacatagoryGrowthSnapshot = captureGrowthSnapshot 9 metacatagoryGrowthHistory

-- | Density summary for phase 13.
phase13Density : PhaseDensity
phase13Density = calculatePhaseDensity 13 metacatagoryGrowthHistory

-- | Y-distribution for phase 13.
phase13YDistribution : YCoordinateDistribution
phase13YDistribution = analyzeYDistribution 13 metacatagoryGrowthHistory

-- | Growth rate computed from the authored history.
metacatagoryGrowthRate : GrowthRate
metacatagoryGrowthRate = computeGrowthRate metacatagoryGrowthHistory

-- | Expansion pattern classified from the computed growth rate.
metacatagoryExpansionPattern : ExpansionPattern
metacatagoryExpansionPattern = classifyExpansionPattern metacatagoryGrowthRate

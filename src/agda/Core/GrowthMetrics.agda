-- Core.GrowthMetrics: Solution space growth rate instrumentation
-- PHASE-V.2: Track coordinate allocation patterns and solution space expansion

module Core.GrowthMetrics where

open import Metamodel as M
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_; _*_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

-- If-then-else helper
if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

-- ============================================================================
-- Phase 14: Solution Space Growth Instrumentation
-- ============================================================================

-- Coordinate allocation event: records when a coordinate is assigned
record CoordinateAllocation : Set where
  field
    coordinate : M.Coordinate
    timestamp : Nat  -- Logical timestamp (allocation order)
    context : String  -- What triggered this allocation (e.g., "field-instance", "yoneda-proof")
    
-- Phase density: how many objects are allocated in a phase
record PhaseDensity : Set where
  field
    phaseNumber : Nat
    objectCount : Nat
    firstAllocation : Maybe Nat  -- Timestamp of first object in phase
    lastAllocation : Maybe Nat   -- Timestamp of last object in phase
    
-- Y-coordinate distribution within a phase
record YCoordinateDistribution : Set where
  field
    phaseNumber : Nat
    minY : Nat
    maxY : Nat
    distinctYValues : Nat
    yValueGaps : List Nat  -- Gaps in y-coordinate sequence

-- Growth rate metrics: how fast is solution space expanding?
record GrowthRate : Set where
  field
    totalAllocations : Nat
    phasesUsed : Nat
    averageObjectsPerPhase : Nat
    maxDensityPhase : Nat
    maxDensityCount : Nat
    sparsestPhase : Nat
    sparsestCount : Nat

-- Expansion pattern: characterize how coordinates spread across dimensions
data ExpansionPattern : Set where
  HorizontalGrowth : ExpansionPattern  -- New phases added frequently
  VerticalGrowth : ExpansionPattern    -- Existing phases get denser
  BalancedGrowth : ExpansionPattern    -- Mix of horizontal and vertical
  SparseGrowth : ExpansionPattern      -- Many phases, few objects each

-- Saturation indicator: is a phase approaching capacity?
record PhaseSaturation : Set where
  field
    phaseNumber : Nat
    currentDensity : Nat
    estimatedCapacity : Maybe Nat  -- If known
    saturationPercentage : Nat     -- 0-100
    isSaturated : Bool

-- Growth snapshot: point-in-time view of solution space state
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

-- Equality test for Nat
eqNat : Nat → Nat → Bool
eqNat zero zero = true
eqNat (suc m) (suc n) = eqNat m n
eqNat _ _ = false

-- Less than test for Nat
ltNat : Nat → Nat → Bool
ltNat _ zero = false
ltNat zero (suc _) = true
ltNat (suc m) (suc n) = ltNat m n

-- Min and max for Nat
minNat : Nat → Nat → Nat
minNat zero _ = zero
minNat _ zero = zero
minNat (suc m) (suc n) = suc (minNat m n)

maxNat : Nat → Nat → Nat
maxNat zero n = n
maxNat m zero = m
maxNat (suc m) (suc n) = suc (maxNat m n)

-- Subtraction for Nat
_-_ : Nat → Nat → Nat
zero - _ = zero
m - zero = m
suc m - suc n = m - n

-- Division for Nat (simplified non-terminating version)
{-# TERMINATING #-}
divNat : Nat → Nat → Nat
divNat _ zero = zero
divNat zero _ = zero
divNat m (suc n) = divHelper m (suc n) zero
  where
    divHelper : Nat → Nat → Nat → Nat
    divHelper zero _ acc = acc
    divHelper (suc m) d acc = if ltNat m d then_else_ acc (divHelper (m - d) d (suc acc))

-- Boolean and
_&&_ : Bool → Bool → Bool
true && b = b
false && _ = false

-- List length
length : {A : Set} → List A → Nat
length [] = zero
length (_ ∷ xs) = suc (length xs)

-- List membership
member : Nat → List Nat → Bool
member _ [] = false
member n (x ∷ xs) = if eqNat n x then_else_ true (member n xs)

-- Calculate density for a specific phase from allocation history
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
      in if eqNat (M.Coordinate.x coord) p then_else_ (suc (countPhaseAllocations p as)) (countPhaseAllocations p as)
    
    findFirstAllocation : Nat → List CoordinateAllocation → Maybe Nat
    findFirstAllocation _ [] = nothing
    findFirstAllocation p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p then_else_ (just (CoordinateAllocation.timestamp a)) (findFirstAllocation p as)
    
    findLastAllocation : Nat → List CoordinateAllocation → Maybe Nat
    findLastAllocation p allocations = findLastHelper p allocations nothing
      where
        findLastHelper : Nat → List CoordinateAllocation → Maybe Nat → Maybe Nat
        findLastHelper _ [] acc = acc
        findLastHelper p (a ∷ as) acc =
          let coord = CoordinateAllocation.coordinate a
          in if eqNat (M.Coordinate.x coord) p then_else_ (findLastHelper p as (just (CoordinateAllocation.timestamp a))) (findLastHelper p as acc)

-- Analyze Y-coordinate distribution for a phase
analyzeYDistribution : Nat → List CoordinateAllocation → YCoordinateDistribution
analyzeYDistribution phase allocations = record
  { phaseNumber = phase
  ; minY = computeMinY phase allocations
  ; maxY = computeMaxY phase allocations
  ; distinctYValues = countDistinctY phase allocations
  ; yValueGaps = []  -- Simplified: would compute actual gaps
  }
  where
    computeMinY : Nat → List CoordinateAllocation → Nat
    computeMinY _ [] = zero
    computeMinY p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p then_else_ (minNat (M.Coordinate.y coord) (computeMinY p as)) (computeMinY p as)
    
    computeMaxY : Nat → List CoordinateAllocation → Nat
    computeMaxY _ [] = zero
    computeMaxY p (a ∷ as) =
      let coord = CoordinateAllocation.coordinate a
      in if eqNat (M.Coordinate.x coord) p then_else_ (maxNat (M.Coordinate.y coord) (computeMaxY p as)) (computeMaxY p as)
    
    countDistinctY : Nat → List CoordinateAllocation → Nat
    countDistinctY _ [] = zero
    countDistinctY p allocations = countHelper p allocations []
      where
        countHelper : Nat → List CoordinateAllocation → List Nat → Nat
        countHelper _ [] seen = length seen
        countHelper p (a ∷ as) seen =
          let coord = CoordinateAllocation.coordinate a
              yVal = M.Coordinate.y coord
          in if eqNat (M.Coordinate.x coord) p then_else_
               (if member yVal seen then_else_ (countHelper p as seen) (countHelper p as (yVal ∷ seen)))
               (countHelper p as seen)

-- Compute overall growth rate from allocation history
computeGrowthRate : List CoordinateAllocation → GrowthRate
computeGrowthRate allocations = record
  { totalAllocations = countAllocations allocations
  ; phasesUsed = countUniquePhases allocations
  ; averageObjectsPerPhase = divNat (countAllocations allocations) (maxNat (suc zero) (countUniquePhases allocations))
  ; maxDensityPhase = zero  -- Simplified
  ; maxDensityCount = zero  -- Simplified
  ; sparsestPhase = zero    -- Simplified
  ; sparsestCount = zero    -- Simplified
  }
  where
    countAllocations : List CoordinateAllocation → Nat
    countAllocations [] = zero
    countAllocations (_ ∷ as) = suc (countAllocations as)
    
    countUniquePhases : List CoordinateAllocation → Nat
    countUniquePhases allocations = countHelper allocations []
      where
        countHelper : List CoordinateAllocation → List Nat → Nat
        countHelper [] seen = length seen
        countHelper (a ∷ as) seen =
          let phase = M.Coordinate.x (CoordinateAllocation.coordinate a)
          in if member phase seen then_else_ (countHelper as seen) (countHelper as (phase ∷ seen))

-- Classify expansion pattern based on growth metrics
classifyExpansionPattern : GrowthRate → ExpansionPattern
classifyExpansionPattern rate =
  let avgPerPhase = GrowthRate.averageObjectsPerPhase rate
      phaseCount = GrowthRate.phasesUsed rate
  in if ltNat avgPerPhase 3 then_else_ SparseGrowth
       (if ltNat phaseCount 5 then_else_ VerticalGrowth
          (if ltNat 10 avgPerPhase then_else_ VerticalGrowth HorizontalGrowth))

-- Check if a phase is approaching saturation
checkPhaseSaturation : PhaseDensity → PhaseSaturation
checkPhaseSaturation density = record
  { phaseNumber = PhaseDensity.phaseNumber density
  ; currentDensity = PhaseDensity.objectCount density
  ; estimatedCapacity = nothing  -- No hard limit in current design
  ; saturationPercentage = zero  -- Cannot compute without capacity
  ; isSaturated = false  -- Never saturated without capacity limit
  }

-- Create a growth snapshot from current allocation state
captureGrowthSnapshot : Nat → List CoordinateAllocation → GrowthSnapshot
captureGrowthSnapshot timestamp allocations =
  let rate = computeGrowthRate allocations
  in record
    { snapshotTimestamp = timestamp
    ; totalCoordinates = GrowthRate.totalAllocations rate
    ; activePhases = []  -- Would compute list of all phase densities
    ; expansionPattern = classifyExpansionPattern rate
    ; saturatedPhases = []
    ; growthRate = rate
    }

-- ============================================================================
-- Concrete Growth Tracking Instances
-- ============================================================================

-- Example allocation history for metacatagory project
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

-- Analyze metacatagory growth
metacatagoryGrowthSnapshot : GrowthSnapshot
metacatagoryGrowthSnapshot = captureGrowthSnapshot 9 metacatagoryGrowthHistory

-- Phase 13 (Gödel boundary) density
phase13Density : PhaseDensity
phase13Density = calculatePhaseDensity 13 metacatagoryGrowthHistory

-- Phase 13 Y-coordinate distribution
phase13YDistribution : YCoordinateDistribution
phase13YDistribution = analyzeYDistribution 13 metacatagoryGrowthHistory

-- Overall growth rate
metacatagoryGrowthRate : GrowthRate
metacatagoryGrowthRate = computeGrowthRate metacatagoryGrowthHistory

-- Expansion pattern classification
metacatagoryExpansionPattern : ExpansionPattern
metacatagoryExpansionPattern = classifyExpansionPattern metacatagoryGrowthRate

-- ============================================================================
-- Growth Verification Helpers
-- ============================================================================

-- Check if growth rate is within expected bounds
verifyGrowthRate : GrowthRate → Bool
verifyGrowthRate rate =
  let total = GrowthRate.totalAllocations rate
      phases = GrowthRate.phasesUsed rate
  in (ltNat zero total) && (ltNat zero phases)

-- Check if phase density is consistent
verifyPhaseDensity : PhaseDensity → Bool
verifyPhaseDensity density =
  ltNat zero (PhaseDensity.objectCount density)

-- Verify growth snapshot is well-formed
verifyGrowthSnapshot : GrowthSnapshot → Bool
verifyGrowthSnapshot snapshot =
  verifyGrowthRate (GrowthSnapshot.growthRate snapshot)

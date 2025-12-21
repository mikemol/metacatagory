{-# OPTIONS --without-K #-}

-- Tests.PathAggregatorTests: Validate integrated path + growth evolution

module Tests.PathAggregatorTests where

open import Core.PathAggregator
open import Metamodel as M
open import Core.GrowthMetrics as GM
open import Agda.Builtin.Equality using (_≡_; refl)
open import Core.Phase using (Bool; true; false)
open import Agda.Builtin.Nat using (Nat; zero; suc)

-- Test: Path snapshot validity
pathSnapshotValid : Bool
pathSnapshotValid = verifyPathSnapshot metacatagoryPathSnapshot

_ : pathSnapshotValid ≡ true
_ = refl

-- Test: Growth snapshot validity
growthSnapshotValid : Bool
growthSnapshotValid = GM.verifyGrowthSnapshot GM.metacatagoryGrowthSnapshot

_ : growthSnapshotValid ≡ true
_ = refl

-- Test: Evolution combined validity
combinedEvolutionValid : Bool
combinedEvolutionValid = PathGrowthEvolution.evolutionValid metacatagoryEvolution

_ : combinedEvolutionValid ≡ true
_ = refl

-- Test: Phase alignment correctness
phaseAlignmentCorrect : PathSnapshot.snapshotPhase alignedPathSnapshot ≡ GM.GrowthSnapshot.snapshotTimestamp GM.metacatagoryGrowthSnapshot
phaseAlignmentCorrect = refl

_ : phaseAlignmentCorrect ≡ refl
_ = refl

-- Test: Timeline length
timelineLengthCorrect : EvolutionTimeline.timelineLength evolutionTimeline ≡ 2
timelineLengthCorrect = refl

_ : timelineLengthCorrect ≡ refl
_ = refl

-- Test: Non-reflexive transformation composition source/target
transCompositionSourceOk : TransformationPath.source composedTransformation ≡ M.mkIdAt "t1" 1 0
transCompositionSourceOk = refl

transCompositionTargetOk : TransformationPath.target composedTransformation ≡ M.mkIdAt "t1''" 1 2
transCompositionTargetOk = refl

_ : transCompositionSourceOk ≡ refl
_ = refl
_ : transCompositionTargetOk ≡ refl
_ = refl

-- Summary: This suite validates linkage of global HoTT path closure with
-- growth metrics snapshot, ensuring aligned phase evolution tracking.

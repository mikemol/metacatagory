-- Tests/Index.agda
-- Comprehensive test suite validating behavioral phase boundaries
--
-- Testing Philosophy:
-- Each test suite validates transitions between behavioral phases,
-- ensuring type safety and correctness at architectural boundaries.

module Tests.Index where

-- Dispatch System Testing (9 phases)
-- Validates: Evidence → Classification → Dispatch → Algorithm Invocation
import Tests.DispatchBehaviorTests

-- Universal Property Testing (9 phases)
-- Validates: Algorithm → UMP → Categorical Structures → Bridge Coherence
import Tests.UniversalPropertyTests

-- Witness Construction Testing (12 phases)
-- Validates: Identifiers → Witnesses → Composites → Type Safety
import Tests.WitnessConstructionTests

-- Error Handling and Validation Testing (8 phases)
-- Validates: Type-level rejection, dispatch exhaustiveness, preconditions,
-- witness invariants, automorphism typing, bundle structure, property specificity,
-- and subfield directionality
import Tests.ErrorHandlingTests

-- Property Registry Testing (3 phases)
-- Validates: Stable identifiers and their generic consumption
import Tests.PropertyRegistryTests

-- Phase abstraction usage examples (informational)
import Tests.PhaseExamples

-- Advanced phase capabilities (dependent, invariants, combinators, profiling)
import Tests.AdvancedPhaseExamples

-- Algorithm composition validation (9 phases)
-- Validates: Multi-step pipelines, invariant preservation, UMP composition
import Tests.AlgorithmCompositionTests

-- Serialization and deserialization (9 phases)
-- Validates: Witness ↔ External conversion, roundtrip, structure preservation
import Tests.SerializationTests

-- Performance and complexity boundaries (9 phases)
-- Validates: Complexity classification, resource estimation, optimization points
import Tests.PerformanceBoundaryTests

-- Constructive witness framework (15 phases)
-- Validates: Computational witnesses, algorithms, correctness proofs, validation
import Tests.ConstructiveWitnessTests

-- Category of Phases examples (import triggers compilation of examples)
import Tests.PhaseCategoryExamplesRunner

-- Real-world algorithm implementations (small, computable examples)
import Tests.RealWorldAlgorithmsTests

-- Chapters 1–3 content (import triggers compilation of chapters)
import Tests.Chapters

-- Usage:
--   agda --no-main -i src/agda src/agda/Tests/Index.agda
--
-- This imports all test modules, triggering full compilation and validation.

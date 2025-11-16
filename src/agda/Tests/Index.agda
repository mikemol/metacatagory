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

-- Test Coverage Summary:
-- 
-- DispatchBehaviorTests covers:
--   Phase 1-2:  Evidence typing and classification construction
--   Phase 3-4:  Lazy instances and dispatch routing
--   Phase 5-6:  Auto dispatch and bundle extraction
--   Phase 7-9:  Algorithm invocation, lookup, and determinism
--
-- UniversalPropertyTests covers:
--   Phase 1-3:  Algorithm→UMP bridge and categorical structures
--   Phase 4-5:  Free constructions and Galois correspondence
--   Phase 6-7:  Algorithm selection and minimal polynomial terminality
--   Phase 8-9:  Splitting field initiality and composition preservation
--
-- WitnessConstructionTests covers:
--   Phase 1-3:  Identifier creation, field construction, extension building
--   Phase 4-5:  Polynomial witnesses and splitting fields
--   Phase 6-7:  Galois groups and automorphisms
--   Phase 8-9:  Composite structures and normal closures
--   Phase 10-12: Type safety, homomorphism witnesses, subfield verification

-- Usage:
--   agda --no-main -i src/agda src/agda/Tests/Index.agda
--
-- This imports all test modules, triggering full compilation and validation.

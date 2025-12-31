---
module: Tests.PerformanceBoundaryTests
kind: per-module
imports:
  - Core
  - Core.Phase
  - Core.AlgorithmComplexity  -- Phase III.1 (3.3)
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Core.AlgebraicAlgorithms
  - Core.Algorithms.Bundle
  - Core.Algorithms.Registry
  - Core.Algorithms.FiniteFields
  - Metamodel as M
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Primitive using (Level; _⊔_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (nothing)
  - GrowthAnalysis as GA using (metacatagoryHistory; categoricalBranchHistory; classicalBranchHistory)
  - Core.GrowthMetrics using
  - Core.Utils using (ltNat)
---

# Module: Tests.PerformanceBoundaryTests

**Source:** `src/agda/Tests/PerformanceBoundaryTests.agda`

## Dependencies

- Core
- Core.Phase
- Core.AlgorithmComplexity  -- Phase III.1 (3.3)
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Core.AlgebraicAlgorithms
- Core.Algorithms.Bundle
- Core.Algorithms.Registry
- Core.Algorithms.FiniteFields
- Metamodel as M
- Agda.Builtin.String using (String)
- Agda.Builtin.Unit using (⊤)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Primitive using (Level; _⊔_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (nothing)
- GrowthAnalysis as GA using (metacatagoryHistory; categoricalBranchHistory; classicalBranchHistory)
- Core.GrowthMetrics using
- Core.Utils using (ltNat)

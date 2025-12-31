---
module: Examples.InstrumentedAlgorithmDemo
kind: per-module
imports:
  - Algorithms.Instrumented
  - Core.Algorithms.Registry using (lookupAlgorithmBundle)
  - Core.Algorithms.Bundle using (AlgorithmBundle)
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.GrowthMetrics using (GrowthSnapshot; GrowthRate)
  - Algebra.Fields.Types using (GaloisGroup)
---

# Module: Examples.InstrumentedAlgorithmDemo

**Source:** `src/agda/Examples/InstrumentedAlgorithmDemo.agda`

## Dependencies

- Algorithms.Instrumented
- Core.Algorithms.Registry using (lookupAlgorithmBundle)
- Core.Algorithms.Bundle using (AlgorithmBundle)
- Algebra.Rings.Basic using (FieldDeclaration)
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.GrowthMetrics using (GrowthSnapshot; GrowthRate)
- Algebra.Fields.Types using (GaloisGroup)

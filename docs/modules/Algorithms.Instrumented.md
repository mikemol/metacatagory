---
module: Algorithms.Instrumented
kind: per-module
imports:
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Core.Algorithms.Bundle using (AlgorithmBundle)
  - Core.GrowthMetrics using
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.String using (String)
  - Core.AlgebraicAlgorithms using
  - Core.Limitations using (LimitationEvidence)
  - Algebra.Fields.Types using
  - Algebra.Foundation using (GroupDeclaration)
  - Core.Phase using (Maybe; nothing; just)
  - Core.Utils using (length)
  - GrowthAnalysis as GA
---

# Module: Algorithms.Instrumented

**Source:** `src/agda/Algorithms/Instrumented.agda`

## Dependencies

- Algebra.Rings.Basic using (FieldDeclaration)
- Core.Algorithms.Bundle using (AlgorithmBundle)
- Core.GrowthMetrics using
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.String using (String)
- Core.AlgebraicAlgorithms using
- Core.Limitations using (LimitationEvidence)
- Algebra.Fields.Types using
- Algebra.Foundation using (GroupDeclaration)
- Core.Phase using (Maybe; nothing; just)
- Core.Utils using (length)
- GrowthAnalysis as GA

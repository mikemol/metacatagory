---
module: Tests.AlgorithmSmokeTests
kind: per-module
imports:
  - Core
  - Algebra.Foundation
  - Algebra.Groups.Abelian
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Core.AlgebraicAlgorithms as CoreAlgo using
  - Core.Witnesses
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Algorithms.TestInstances using
---

# Module: Tests.AlgorithmSmokeTests

**Source:** `src/agda/Tests/AlgorithmSmokeTests.agda`

## Dependencies

- Core
- Algebra.Foundation
- Algebra.Groups.Abelian
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Core.AlgebraicAlgorithms as CoreAlgo using
- Core.Witnesses
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Algorithms.TestInstances using

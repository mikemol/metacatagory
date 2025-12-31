---
module: Tests.UniversalPropertyTests
kind: per-module
imports:
  - Core
  - Algebra.Foundation
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Core.AlgebraicAlgorithms
  - Core.UniversalProperties
  - Core.AlgorithmUniversality
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (Bool)
  - Core.Phase using (Bool; true)
---

# Module: Tests.UniversalPropertyTests

**Source:** `src/agda/Tests/UniversalPropertyTests.agda`

## Dependencies

- Core
- Algebra.Foundation
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Core.AlgebraicAlgorithms
- Core.UniversalProperties
- Core.AlgorithmUniversality
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (Bool)
- Core.Phase using (Bool; true)

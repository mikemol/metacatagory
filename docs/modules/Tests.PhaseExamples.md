---
module: Tests.PhaseExamples
kind: per-module
imports:
  - Core.Phase
  - Core
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Core.AlgebraicAlgorithms
  - Core.Algorithms.Bundle
  - Core.Algorithms.Registry
  - Core.Algorithms.FiniteFields
  - Core.Algorithms.NumberFields
  - Metamodel as M
  - Agda.Builtin.Equality using (_≡_; refl)
---

# Module: Tests.PhaseExamples

**Source:** `src/agda/Tests/PhaseExamples.agda`

## Dependencies

- Core.Phase
- Core
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Core.AlgebraicAlgorithms
- Core.Algorithms.Bundle
- Core.Algorithms.Registry
- Core.Algorithms.FiniteFields
- Core.Algorithms.NumberFields
- Metamodel as M
- Agda.Builtin.Equality using (_≡_; refl)

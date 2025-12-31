---
module: Tests.DispatchBehaviorTests
kind: per-module
imports:
  - Core
  - Algebra.Foundation
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Core.AlgebraicAlgorithms
  - Core.Algorithms.Bundle
  - Core.Algorithms.Registry
  - Core.Algorithms.FiniteFields
  - Core.Algorithms.NumberFields
  - Core.Algorithms.FunctionFields
  - Metamodel as M
  - Agda.Builtin.List using (List)
  - Core.Phase using (Σ; fst; snd; _,ₛ_)
  - Agda.Builtin.Equality using (_≡_; refl)
---

# Module: Tests.DispatchBehaviorTests

**Source:** `src/agda/Tests/DispatchBehaviorTests.agda`

## Dependencies

- Core
- Algebra.Foundation
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Core.AlgebraicAlgorithms
- Core.Algorithms.Bundle
- Core.Algorithms.Registry
- Core.Algorithms.FiniteFields
- Core.Algorithms.NumberFields
- Core.Algorithms.FunctionFields
- Metamodel as M
- Agda.Builtin.List using (List)
- Core.Phase using (Σ; fst; snd; _,ₛ_)
- Agda.Builtin.Equality using (_≡_; refl)

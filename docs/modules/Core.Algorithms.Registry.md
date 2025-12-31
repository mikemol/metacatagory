---
module: Core.Algorithms.Registry
kind: per-module
imports:
  - Core
  - Algebra.Foundation
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Core.AlgebraicAlgorithms
  - Core.Algorithms.Bundle
  - Algorithms.Adapters.BundleAdapter using (defaultAlgorithmBundle)
  - Algorithms.Basic
  - Core.Algorithms.FiniteFields
  - Core.Algorithms.NumberFields
  - Core.Algorithms.FunctionFields
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Σ; fst; snd; _,ₛ_)
---

# Module: Core.Algorithms.Registry

**Source:** `src/agda/Core/Algorithms/Registry.agda`

## Dependencies

- Core
- Algebra.Foundation
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Core.AlgebraicAlgorithms
- Core.Algorithms.Bundle
- Algorithms.Adapters.BundleAdapter using (defaultAlgorithmBundle)
- Algorithms.Basic
- Core.Algorithms.FiniteFields
- Core.Algorithms.NumberFields
- Core.Algorithms.FunctionFields
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Σ; fst; snd; _,ₛ_)

---
module: Core.ConstructiveWitnesses
kind: per-module
imports:
  - Agda.Primitive using (Level; lsuc)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core
  - Core.Phase
  - Core.AlgebraicAlgorithms
  - Core.Witnesses
  - Core.UniversalProperties
  - Algebra.Foundation
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Algebra.Groups.Basic
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Phase using (Bool; true; false)
  - Core.PolynomialsF2 as F2
---

# Module: Core.ConstructiveWitnesses

**Source:** `src/agda/Core/ConstructiveWitnesses.agda`

## Dependencies

- Agda.Primitive using (Level; lsuc)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core
- Core.Phase
- Core.AlgebraicAlgorithms
- Core.Witnesses
- Core.UniversalProperties
- Algebra.Foundation
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Algebra.Groups.Basic
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Phase using (Bool; true; false)
- Core.PolynomialsF2 as F2

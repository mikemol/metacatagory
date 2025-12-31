---
module: Core.AlgebraicAlgorithms
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Algebra.Groups.Basic
  - Algebra.Foundation
  - Core.Limitations
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (Maybe; just; nothing)
---

# Module: Core.AlgebraicAlgorithms

**Source:** `src/agda/Core/AlgebraicAlgorithms.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Algebra.Groups.Basic
- Algebra.Foundation
- Core.Limitations
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (Maybe; just; nothing)

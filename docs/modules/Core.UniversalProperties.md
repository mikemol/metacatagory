---
module: Core.UniversalProperties
kind: per-module
imports:
  - Agda.Primitive using (Level; lsuc)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core
  - Metamodel as M
  - Algebra.Foundation
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
---

# Module: Core.UniversalProperties

**Source:** `src/agda/Core/UniversalProperties.agda`

## Dependencies

- Agda.Primitive using (Level; lsuc)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core
- Metamodel as M
- Algebra.Foundation
- Algebra.Rings.Basic
- Algebra.Fields.Basic

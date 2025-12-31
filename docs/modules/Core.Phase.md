---
module: Core.Phase
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Agda.Primitive using (Level; _⊔_)
  - Infrastructure.Coherence.Path2 using (whisker; _∙₂_)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.String using (String)
---

# Module: Core.Phase

**Source:** `src/agda/Core/Phase.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Agda.Primitive using (Level; _⊔_)
- Infrastructure.Coherence.Path2 using (whisker; _∙₂_)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.String using (String)

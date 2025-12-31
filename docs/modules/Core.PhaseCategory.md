---
module: Core.PhaseCategory
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Agda.Primitive using (Level; lsuc; _⊔_)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Phase; _⟫_; idPhase; _$ₚ_; mkPhase; _×_; _,_; fst; snd; _⊗_)
---

# Module: Core.PhaseCategory

**Source:** `src/agda/Core/PhaseCategory.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Agda.Primitive using (Level; lsuc; _⊔_)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Phase; _⟫_; idPhase; _$ₚ_; mkPhase; _×_; _,_; fst; snd; _⊗_)

---
module: Core.GrowthMetrics
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Metamodel as M
  - Core.Utils -- Uses consolidated utils
  - Agda.Builtin.String using (String)
---

# Module: Core.GrowthMetrics

**Source:** `src/agda/Core/GrowthMetrics.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Metamodel as M
- Core.Utils -- Uses consolidated utils
- Agda.Builtin.String using (String)

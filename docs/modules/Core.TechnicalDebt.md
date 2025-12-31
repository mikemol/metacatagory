---
module: Core.TechnicalDebt
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Metamodel as M
  - Core using (_×_; _,_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Int using (Int; pos; negsuc)
  - Core.Utils
---

# Module: Core.TechnicalDebt

**Source:** `src/agda/Core/TechnicalDebt.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Metamodel as M
- Core using (_×_; _,_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Int using (Int; pos; negsuc)
- Core.Utils

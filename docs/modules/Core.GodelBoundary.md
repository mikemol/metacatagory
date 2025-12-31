---
module: Core.GodelBoundary
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core
  - Metamodel as M
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; zero; suc)
---

# Module: Core.GodelBoundary

**Source:** `src/agda/Core/GodelBoundary.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core
- Metamodel as M
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit using (⊤)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; zero; suc)

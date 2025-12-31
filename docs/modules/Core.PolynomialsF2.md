---
module: Core.PolynomialsF2
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Primitive using (Level; lzero)
---

# Module: Core.PolynomialsF2

**Source:** `src/agda/Core/PolynomialsF2.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Primitive using (Level; lzero)

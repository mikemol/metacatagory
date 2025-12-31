---
module: Core.Strings
kind: per-module
imports:
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.String using (String; primStringAppend; primShowNat)
  - Agda.Builtin.List using (List; []; _∷_)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
---

# Module: Core.Strings

**Source:** `src/agda/Core/Strings.agda`

## Dependencies

- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.String using (String; primStringAppend; primShowNat)
- Agda.Builtin.List using (List; []; _∷_)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

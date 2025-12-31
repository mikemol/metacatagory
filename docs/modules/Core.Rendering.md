---
module: Core.Rendering
kind: per-module
imports:
  - Agda.Builtin.String using (String)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core.Strings using (_++_; intercalate; natToString; quoteJSON; mapWithPrefix)
---

# Module: Core.Rendering

**Source:** `src/agda/Core/Rendering.agda`

## Dependencies

- Agda.Builtin.String using (String)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.Bool using (Bool; true; false)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core.Strings using (_++_; intercalate; natToString; quoteJSON; mapWithPrefix)

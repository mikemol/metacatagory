---
module: Core.IO
kind: per-module
imports:
  - Agda.Builtin.IO using () renaming (IO to IOᵇ)
  - Agda.Builtin.Unit using () renaming (⊤ to ⊤ᵇ; tt to ttᵇ)
  - Agda.Builtin.String using () renaming (String to Stringᵇ)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
---

# Module: Core.IO

**Source:** `src/agda/Core/IO.agda`

## Dependencies

- Agda.Builtin.IO using () renaming (IO to IOᵇ)
- Agda.Builtin.Unit using () renaming (⊤ to ⊤ᵇ; tt to ttᵇ)
- Agda.Builtin.String using () renaming (String to Stringᵇ)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Bool using (Bool; true; false)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)

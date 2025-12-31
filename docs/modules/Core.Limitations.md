---
module: Core.Limitations
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Metamodel as M
  - Agda.Builtin.String using (String)
  - Agda.Builtin.List   using (List; []; _∷_)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit   using (⊤; tt)
  - Agda.Builtin.Maybe  using (Maybe; just; nothing)
---

# Module: Core.Limitations

**Source:** `src/agda/Core/Limitations.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Metamodel as M
- Agda.Builtin.String using (String)
- Agda.Builtin.List   using (List; []; _∷_)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit   using (⊤; tt)
- Agda.Builtin.Maybe  using (Maybe; just; nothing)

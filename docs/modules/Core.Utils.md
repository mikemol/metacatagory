---
module: Core.Utils
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core.Phase public using (Bool; true; false)
  - Agda.Builtin.Nat public using (Nat; zero; suc; _+_; _*_; _-_)
  - Agda.Builtin.List public using (List; []; _∷_)
  - Agda.Builtin.String public using (String; primStringAppend)
  - Agda.Builtin.Equality public using (_≡_; refl)
  - Core.Phase public using (Maybe; just; nothing)
  - Core using (_×_; _,_)
---

# Module: Core.Utils

**Source:** `src/agda/Core/Utils.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core.Phase public using (Bool; true; false)
- Agda.Builtin.Nat public using (Nat; zero; suc; _+_; _*_; _-_)
- Agda.Builtin.List public using (List; []; _∷_)
- Agda.Builtin.String public using (String; primStringAppend)
- Agda.Builtin.Equality public using (_≡_; refl)
- Core.Phase public using (Maybe; just; nothing)
- Core using (_×_; _,_)

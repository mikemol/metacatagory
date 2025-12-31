---
module: Core.AdapterAutomation
kind: per-module
imports:
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Agda.Builtin.Unit using (⊤; tt)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.CategoricalAdapter
  - Agda.Builtin.Nat using (Nat; suc)
---

# Module: Core.AdapterAutomation

**Source:** `src/agda/Core/AdapterAutomation.agda`

## Dependencies

- Agda.Builtin.Nat using (Nat; zero; suc)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Agda.Builtin.Unit using (⊤; tt)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.CategoricalAdapter
- Agda.Builtin.Nat using (Nat; suc)

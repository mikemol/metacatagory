---
module: Core.AdapterReflection
kind: per-module
imports:
  - Agda.Builtin.Reflection renaming (bindTC to _>>=_)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.CategoricalAdapter
---

# Module: Core.AdapterReflection

**Source:** `src/agda/Core/AdapterReflection.agda`

## Dependencies

- Agda.Builtin.Reflection renaming (bindTC to _>>=_)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.CategoricalAdapter

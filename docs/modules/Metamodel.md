---
module: Metamodel
kind: per-module
imports:
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit     using (⊤; tt)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Nat      using (Nat)
  - Agda.Builtin.Char     using (Char)
  - Agda.Builtin.String   using (String)
  - Agda.Builtin.List     using (List; []; _∷_)
---

# Module: Metamodel

**Source:** `src/agda/Metamodel.agda`

## Dependencies

- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit     using (⊤; tt)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Nat      using (Nat)
- Agda.Builtin.Char     using (Char)
- Agda.Builtin.String   using (String)
- Agda.Builtin.List     using (List; []; _∷_)

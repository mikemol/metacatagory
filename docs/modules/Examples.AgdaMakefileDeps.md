---
module: Examples.AgdaMakefileDeps
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.Char using (Char)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Bool using (Bool; true; false)
---

# Module: Examples.AgdaMakefileDeps

**Source:** `src/agda/Examples/AgdaMakefileDeps.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.Char using (Char)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Bool using (Bool; true; false)

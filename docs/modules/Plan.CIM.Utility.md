---
module: Plan.CIM.Utility
kind: per-module
imports:
  - Agda.Primitive using (Level; lsuc)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.Nat using (Nat; zero; suc; _-_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - Agda.Builtin.Sigma using (Σ; _,_)
---

# Module: Plan.CIM.Utility

**Source:** `src/agda/Plan/CIM/Utility.agda`

## Dependencies

- Agda.Primitive using (Level; lsuc)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.Nat using (Nat; zero; suc; _-_)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- Agda.Builtin.Sigma using (Σ; _,_)

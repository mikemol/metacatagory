---
module: Chapter1.Level1
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Unit     using (⊤; tt)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.String using (String)
  - Metamodel as M
  - PropertyRegistry as P
  - Core.Phase using (_×_; _,_) public
---

# Module: Chapter1.Level1

**Source:** `src/agda/Chapter1/Level1.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Unit     using (⊤; tt)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.String using (String)
- Metamodel as M
- PropertyRegistry as P
- Core.Phase using (_×_; _,_) public

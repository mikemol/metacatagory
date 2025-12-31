---
module: Tests.CoverageReport
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; zero; suc; _+_)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
---

# Module: Tests.CoverageReport

**Source:** `src/agda/Tests/CoverageReport.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; zero; suc; _+_)
- Agda.Builtin.Unit using (⊤)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)

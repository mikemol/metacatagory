---
module: Tests.GrothendieckFibrationsChecklist
kind: per-module
imports:
  - Tests.ObligationAdapters as A
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (refl; _≡_)
  - Agda.Builtin.Unit using (⊤; tt)
  - Chapter2.Level2sub8 as C2S8
  - Chapter1.Level1sub3 as C1S3
  - Core.GrothendieckFibrations as GF
  - Metamodel as M
---

# Module: Tests.GrothendieckFibrationsChecklist

**Source:** `src/agda/Tests/GrothendieckFibrationsChecklist.agda`

## Dependencies

- Tests.ObligationAdapters as A
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (refl; _≡_)
- Agda.Builtin.Unit using (⊤; tt)
- Chapter2.Level2sub8 as C2S8
- Chapter1.Level1sub3 as C1S3
- Core.GrothendieckFibrations as GF
- Metamodel as M

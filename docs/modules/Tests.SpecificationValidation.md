---
module: Tests.SpecificationValidation
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Phase using (Bool; true; false)
  - Agda.Primitive using (Level; lzero)
  - Tests.GrothendieckFibrationsChecklist as GF
  - Tests.AbelianCategoriesChecklist as AC
  - Tests.SubobjectTheoryChecklist as ST
---

# Module: Tests.SpecificationValidation

**Source:** `src/agda/Tests/SpecificationValidation.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Phase using (Bool; true; false)
- Agda.Primitive using (Level; lzero)
- Tests.GrothendieckFibrationsChecklist as GF
- Tests.AbelianCategoriesChecklist as AC
- Tests.SubobjectTheoryChecklist as ST

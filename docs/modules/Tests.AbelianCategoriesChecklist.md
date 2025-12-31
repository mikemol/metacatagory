---
module: Tests.AbelianCategoriesChecklist
kind: per-module
imports:
  - Tests.ObligationAdapters as A
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (refl; _≡_)
  - Agda.Builtin.List using (List; []; _∷_)
  - Metamodel using (mkId)
---

# Module: Tests.AbelianCategoriesChecklist

**Source:** `src/agda/Tests/AbelianCategoriesChecklist.agda`

## Dependencies

- Tests.ObligationAdapters as A
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (refl; _≡_)
- Agda.Builtin.List using (List; []; _∷_)
- Metamodel using (mkId)

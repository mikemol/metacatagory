---
module: Tests.KanExtensionsChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤)
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
---

# Module: Tests.KanExtensionsChecklist

**Source:** `src/agda/Tests/KanExtensionsChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤)
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter

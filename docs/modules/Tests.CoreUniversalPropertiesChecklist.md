---
module: Tests.CoreUniversalPropertiesChecklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Tests.ObligationAdapters as A
  - Core.UniversalProperties as CUP
---

# Module: Tests.CoreUniversalPropertiesChecklist

**Source:** `src/agda/Tests/CoreUniversalPropertiesChecklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Metamodel as M
- Core.CategoricalAdapter
- Tests.ObligationAdapters as A
- Core.UniversalProperties as CUP

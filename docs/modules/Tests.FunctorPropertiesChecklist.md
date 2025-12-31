---
module: Tests.FunctorPropertiesChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Tests.ObligationAdapters as A
  - Metamodel as M
  - Chapter1.Level1sub2 as C1S2
---

# Module: Tests.FunctorPropertiesChecklist

**Source:** `src/agda/Tests/FunctorPropertiesChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Tests.ObligationAdapters as A
- Metamodel as M
- Chapter1.Level1sub2 as C1S2

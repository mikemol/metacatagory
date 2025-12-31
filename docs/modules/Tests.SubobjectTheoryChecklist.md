---
module: Tests.SubobjectTheoryChecklist
kind: per-module
imports:
  - Tests.ObligationAdapters as A
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (refl; _≡_)
  - Metamodel as M
---

# Module: Tests.SubobjectTheoryChecklist

**Source:** `src/agda/Tests/SubobjectTheoryChecklist.agda`

## Dependencies

- Tests.ObligationAdapters as A
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (refl; _≡_)
- Metamodel as M

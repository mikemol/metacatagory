---
module: Tests.GroupsFreeChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Chapter1.Level1 as C1L
  - Algebra.Foundation as AF
  - Algebra.Groups.Free as AGF
  - Tests.ObligationAdapters as A
---

# Module: Tests.GroupsFreeChecklist

**Source:** `src/agda/Tests/GroupsFreeChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.CategoricalAdapter
- Chapter1.Level1 as C1L
- Algebra.Foundation as AF
- Algebra.Groups.Free as AGF
- Tests.ObligationAdapters as A

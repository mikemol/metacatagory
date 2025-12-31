---
module: Tests.AlgebraChecklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.CategoricalAdapter
  - Tests.ObligationAdapters as A
  - Algebra.Foundation as AF
  - Algebra.Rings.Basic as AR
  - Chapter1.Level1 as C1L
---

# Module: Tests.AlgebraChecklist

**Source:** `src/agda/Tests/AlgebraChecklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.CategoricalAdapter
- Tests.ObligationAdapters as A
- Algebra.Foundation as AF
- Algebra.Rings.Basic as AR
- Chapter1.Level1 as C1L

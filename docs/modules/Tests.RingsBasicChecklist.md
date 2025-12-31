---
module: Tests.RingsBasicChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Chapter1.Level1 as C1L
  - Algebra.Foundation as AF
  - Algebra.Rings.Basic as AR
  - Algebra.Groups.Basic as AGB
  - Tests.ObligationAdapters as A
---

# Module: Tests.RingsBasicChecklist

**Source:** `src/agda/Tests/RingsBasicChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.CategoricalAdapter
- Chapter1.Level1 as C1L
- Algebra.Foundation as AF
- Algebra.Rings.Basic as AR
- Algebra.Groups.Basic as AGB
- Tests.ObligationAdapters as A

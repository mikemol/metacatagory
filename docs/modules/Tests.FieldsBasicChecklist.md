---
module: Tests.FieldsBasicChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Chapter1.Level1 as C1L
  - Algebra.Foundation as AF
  - Algebra.Rings.Basic as AR
  - Algebra.Fields.Basic as AFB
  - Tests.ObligationAdapters as A
---

# Module: Tests.FieldsBasicChecklist

**Source:** `src/agda/Tests/FieldsBasicChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Chapter1.Level1 as C1L
- Algebra.Foundation as AF
- Algebra.Rings.Basic as AR
- Algebra.Fields.Basic as AFB
- Tests.ObligationAdapters as A

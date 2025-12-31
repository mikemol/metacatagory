---
module: Tests.AdvancedFieldsChecklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Tests.ObligationAdapters as A
  - Algebra.Rings.Basic as AR
  - Algebra.Fields.Advanced as AFA
  - Algebra.Foundation as AF
  - Chapter1.Level1 as C1L
---

# Module: Tests.AdvancedFieldsChecklist

**Source:** `src/agda/Tests/AdvancedFieldsChecklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Metamodel as M
- Core.CategoricalAdapter
- Tests.ObligationAdapters as A
- Algebra.Rings.Basic as AR
- Algebra.Fields.Advanced as AFA
- Algebra.Foundation as AF
- Chapter1.Level1 as C1L

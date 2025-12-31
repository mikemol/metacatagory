---
module: Tests.GroupsAbelianChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Chapter1.Level1 as C1L
  - Algebra.Foundation as AF
  - Algebra.Groups.Abelian as AGA
  - Tests.ObligationAdapters as A
---

# Module: Tests.GroupsAbelianChecklist

**Source:** `src/agda/Tests/GroupsAbelianChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.CategoricalAdapter
- Chapter1.Level1 as C1L
- Algebra.Foundation as AF
- Algebra.Groups.Abelian as AGA
- Tests.ObligationAdapters as A

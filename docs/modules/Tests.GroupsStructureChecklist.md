---
module: Tests.GroupsStructureChecklist
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
  - Algebra.Groups.Structure as AGS
  - Algebra.Groups.Basic as AGB
  - Tests.ObligationAdapters as A
---

# Module: Tests.GroupsStructureChecklist

**Source:** `src/agda/Tests/GroupsStructureChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.CategoricalAdapter
- Chapter1.Level1 as C1L
- Algebra.Foundation as AF
- Algebra.Groups.Free as AGF
- Algebra.Groups.Structure as AGS
- Algebra.Groups.Basic as AGB
- Tests.ObligationAdapters as A

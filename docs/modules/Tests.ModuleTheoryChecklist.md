---
module: Tests.ModuleTheoryChecklist
kind: per-module
imports:
  - Agda.Builtin.Equality
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤)
  - Metamodel as M
  - Algebra.Foundation as AF
  - Algebra.Rings.Basic as AR
  - Algebra.Modules.Basic as AM
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
  - Chapter1.Level1 as C1L
---

# Module: Tests.ModuleTheoryChecklist

**Source:** `src/agda/Tests/ModuleTheoryChecklist.agda`

## Dependencies

- Agda.Builtin.Equality
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit using (⊤)
- Metamodel as M
- Algebra.Foundation as AF
- Algebra.Rings.Basic as AR
- Algebra.Modules.Basic as AM
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter
- Chapter1.Level1 as C1L

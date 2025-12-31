---
module: Tests.TensorProductChecklist
kind: per-module
imports:
  - Agda.Builtin.Equality
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Algebra.Foundation as AF
  - Chapter1.Level1 as C1L
  - Algebra.Groups.Abelian as AGA
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
---

# Module: Tests.TensorProductChecklist

**Source:** `src/agda/Tests/TensorProductChecklist.agda`

## Dependencies

- Agda.Builtin.Equality
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Algebra.Foundation as AF
- Chapter1.Level1 as C1L
- Algebra.Groups.Abelian as AGA
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter

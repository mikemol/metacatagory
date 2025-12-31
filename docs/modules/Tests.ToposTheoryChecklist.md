---
module: Tests.ToposTheoryChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (refl; _≡_)
  - Agda.Builtin.Unit using (⊤; tt)
  - Core.CategoricalAdapter
  - Tests.ToposObligationAdapters as A
  - Metamodel as M
  - Chapter3.Level3sub2 as S2
  - Chapter1.Level1sub3 as C1S3
---

# Module: Tests.ToposTheoryChecklist

**Source:** `src/agda/Tests/ToposTheoryChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (refl; _≡_)
- Agda.Builtin.Unit using (⊤; tt)
- Core.CategoricalAdapter
- Tests.ToposObligationAdapters as A
- Metamodel as M
- Chapter3.Level3sub2 as S2
- Chapter1.Level1sub3 as C1S3

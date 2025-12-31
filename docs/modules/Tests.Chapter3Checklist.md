---
module: Tests.Chapter3Checklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Metamodel as M
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.CategoricalAdapter
  - Tests.ObligationAdapters as A
  - Chapter3.Level3sub1 as S1
  - Chapter3.Level3sub2 as S2
  - Chapter1.Level1sub3 as C1S3
---

# Module: Tests.Chapter3Checklist

**Source:** `src/agda/Tests/Chapter3Checklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Metamodel as M
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.CategoricalAdapter
- Tests.ObligationAdapters as A
- Chapter3.Level3sub1 as S1
- Chapter3.Level3sub2 as S2
- Chapter1.Level1sub3 as C1S3

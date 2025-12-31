---
module: Tests.Chapter2Checklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.List using (List; []; _∷_)
  - Metamodel as M
  - Chapter2.Level2sub1 as S1
  - Chapter2.Level2sub2 as S2
  - Chapter2.Level2sub3 as S3
  - Chapter2.Level2sub4 as S4
  - Chapter2.Level2sub5 as S5
  - Chapter2.Level2sub6 as S6
  - Chapter2.Level2sub7 as S7
  - Chapter2.Level2sub8 as S8
  - Chapter1.Level1sub3 as C1S3
  - Chapter1.Level1sub6 as C1S6
  - Chapter1.Level1 as C1L
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
---

# Module: Tests.Chapter2Checklist

**Source:** `src/agda/Tests/Chapter2Checklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.List using (List; []; _∷_)
- Metamodel as M
- Chapter2.Level2sub1 as S1
- Chapter2.Level2sub2 as S2
- Chapter2.Level2sub3 as S3
- Chapter2.Level2sub4 as S4
- Chapter2.Level2sub5 as S5
- Chapter2.Level2sub6 as S6
- Chapter2.Level2sub7 as S7
- Chapter2.Level2sub8 as S8
- Chapter1.Level1sub3 as C1S3
- Chapter1.Level1sub6 as C1S6
- Chapter1.Level1 as C1L
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter

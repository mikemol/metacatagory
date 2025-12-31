---
module: Tests.ObligationAdapters
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Equality using (_≡_)
  - Metamodel as M
  - Chapter1.Level1 as C1L
  - Chapter1.Level1sub2 as C1S2
  - Chapter1.Level1sub3 as C1S3
  - Chapter1.Level1sub4 as C1S4
  - Chapter1.Level1sub5 as C1S5
  - Chapter1.Level1sub6 as C1S6
  - Chapter1.Level1sub8 as C1S8
  - Chapter2.Level2sub1 as C2S1
  - Chapter2.Level2sub2 as C2S2
  - Chapter2.Level2sub3 as C2S3
  - Chapter2.Level2sub4 as C2S4
  - Chapter2.Level2sub5 as C2S5
  - Chapter2.Level2sub6 as S6
  - Chapter2.Level2sub7 as C2S7
  - Chapter2.Level2sub8 as C2S8
  - Chapter3.Level3sub1 as C3S1
  - Chapter3.Level3sub2 as C3S2
  - Agda.Primitive using (Level; lzero; lsuc)
  - Core.CategoricalAdapter
  - Algebra.Foundation as AFo
  - Algebra.Enrichment as AE
  - Algebra.Rings.Basic as AR
  - Algebra.Modules.Basic as AM
  - Algebra.Fields.Basic as AFB
  - Algebra.Fields.Advanced as AFA
  - Algebra.Groups.Basic as AGB
  - Algebra.Groups.Free as AGF
  - Algebra.Groups.Structure as AGS
  - Algebra.Groups.Abelian as AGA
  - Core.UniversalProperties as CUP
  - Chapter1.Level1 as C
  - Chapter2.Level2sub6 as Enriched
  - Core.Phase using (Bool)
---

# Module: Tests.ObligationAdapters

**Source:** `src/agda/Tests/ObligationAdapters.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Equality using (_≡_)
- Metamodel as M
- Chapter1.Level1 as C1L
- Chapter1.Level1sub2 as C1S2
- Chapter1.Level1sub3 as C1S3
- Chapter1.Level1sub4 as C1S4
- Chapter1.Level1sub5 as C1S5
- Chapter1.Level1sub6 as C1S6
- Chapter1.Level1sub8 as C1S8
- Chapter2.Level2sub1 as C2S1
- Chapter2.Level2sub2 as C2S2
- Chapter2.Level2sub3 as C2S3
- Chapter2.Level2sub4 as C2S4
- Chapter2.Level2sub5 as C2S5
- Chapter2.Level2sub6 as S6
- Chapter2.Level2sub7 as C2S7
- Chapter2.Level2sub8 as C2S8
- Chapter3.Level3sub1 as C3S1
- Chapter3.Level3sub2 as C3S2
- Agda.Primitive using (Level; lzero; lsuc)
- Core.CategoricalAdapter
- Algebra.Foundation as AFo
- Algebra.Enrichment as AE
- Algebra.Rings.Basic as AR
- Algebra.Modules.Basic as AM
- Algebra.Fields.Basic as AFB
- Algebra.Fields.Advanced as AFA
- Algebra.Groups.Basic as AGB
- Algebra.Groups.Free as AGF
- Algebra.Groups.Structure as AGS
- Algebra.Groups.Abelian as AGA
- Core.UniversalProperties as CUP
- Chapter1.Level1 as C
- Chapter2.Level2sub6 as Enriched
- Core.Phase using (Bool)

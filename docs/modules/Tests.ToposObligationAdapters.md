---
module: Tests.ToposObligationAdapters
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Primitive using (Level; lzero; lsuc)
  - Metamodel as M
  - Chapter1.Level1sub3 as C1S3
  - Chapter3.Level3sub2 as C3S2
  - Core.CategoricalAdapter
---

# Module: Tests.ToposObligationAdapters

**Source:** `src/agda/Tests/ToposObligationAdapters.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_)
- Agda.Builtin.Unit using (⊤)
- Agda.Primitive using (Level; lzero; lsuc)
- Metamodel as M
- Chapter1.Level1sub3 as C1S3
- Chapter3.Level3sub2 as C3S2
- Core.CategoricalAdapter

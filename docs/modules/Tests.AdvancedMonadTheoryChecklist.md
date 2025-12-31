---
module: Tests.AdvancedMonadTheoryChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (refl; _≡_)
  - Agda.Builtin.Unit using (⊤)
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
  - Chapter2.Level2sub4 as C2S4
---

# Module: Tests.AdvancedMonadTheoryChecklist

**Source:** `src/agda/Tests/AdvancedMonadTheoryChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (refl; _≡_)
- Agda.Builtin.Unit using (⊤)
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter
- Chapter2.Level2sub4 as C2S4

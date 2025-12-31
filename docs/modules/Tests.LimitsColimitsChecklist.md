---
module: Tests.LimitsColimitsChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤)
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
---

# Module: Tests.LimitsColimitsChecklist

**Source:** `src/agda/Tests/LimitsColimitsChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤)
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter

---
module: Tests.YonedaChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤)
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter
  - Core.Yoneda as Y
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Metamodel as M
---

# Module: Tests.YonedaChecklist

**Source:** `src/agda/Tests/YonedaChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤)
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter
- Core.Yoneda as Y
- Algebra.Rings.Basic using (FieldDeclaration)
- Metamodel as M

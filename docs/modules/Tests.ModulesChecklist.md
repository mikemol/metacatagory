---
module: Tests.ModulesChecklist
kind: per-module
imports:
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Metamodel as M
  - Core.CategoricalAdapter
  - Tests.ObligationAdapters as A
  - Algebra.Foundation as AF
  - Algebra.Groups.Basic as GB
  - Algebra.Groups.Abelian as GA
  - Algebra.Rings.Basic as AR
  - Algebra.Modules.Basic as AM
  - Chapter1.Level1 as C1L
---

# Module: Tests.ModulesChecklist

**Source:** `src/agda/Tests/ModulesChecklist.agda`

## Dependencies

- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Metamodel as M
- Core.CategoricalAdapter
- Tests.ObligationAdapters as A
- Algebra.Foundation as AF
- Algebra.Groups.Basic as GB
- Algebra.Groups.Abelian as GA
- Algebra.Rings.Basic as AR
- Algebra.Modules.Basic as AM
- Chapter1.Level1 as C1L

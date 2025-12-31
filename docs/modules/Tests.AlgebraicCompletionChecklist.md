---
module: Tests.AlgebraicCompletionChecklist
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Builtin.Nat as N
  - Agda.Builtin.String as S
  - Metamodel as M
  - Chapter1.Level1 as C1L
  - Algebra.Foundation as AFo
  - Algebra.Rings.Basic as AR
  - Algebra.Fields.Basic as AFB
  - Algebra.Groups.Basic as AGB
  - Algebra.Groups.Abelian as AGA
  - Algebra.Modules.Basic as AM
  - Tests.ObligationAdapters as A
  - Core.CategoricalAdapter as Core
---

# Module: Tests.AlgebraicCompletionChecklist

**Source:** `src/agda/Tests/AlgebraicCompletionChecklist.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤)
- Agda.Builtin.Nat as N
- Agda.Builtin.String as S
- Metamodel as M
- Chapter1.Level1 as C1L
- Algebra.Foundation as AFo
- Algebra.Rings.Basic as AR
- Algebra.Fields.Basic as AFB
- Algebra.Groups.Basic as AGB
- Algebra.Groups.Abelian as AGA
- Algebra.Modules.Basic as AM
- Tests.ObligationAdapters as A
- Core.CategoricalAdapter as Core

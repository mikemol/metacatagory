---
module: Tests.ErrorAsSpecificationTests
kind: per-module
imports:
  - Metamodel as M
  - Core.Limitations
  - Core
  - Core.AlgebraicAlgorithms
  - Core.Witnesses
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Agda.Builtin.String using (String)
  - Agda.Builtin.List   using (List; []; _∷_)
  - Core.Phase using (Bool; true; false)
  - Core.Phase using (Maybe; just; nothing)
  - Agda.Builtin.Equality using (_≡_; refl)
---

# Module: Tests.ErrorAsSpecificationTests

**Source:** `src/agda/Tests/ErrorAsSpecificationTests.agda`

## Dependencies

- Metamodel as M
- Core.Limitations
- Core
- Core.AlgebraicAlgorithms
- Core.Witnesses
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Agda.Builtin.String using (String)
- Agda.Builtin.List   using (List; []; _∷_)
- Core.Phase using (Bool; true; false)
- Core.Phase using (Maybe; just; nothing)
- Agda.Builtin.Equality using (_≡_; refl)

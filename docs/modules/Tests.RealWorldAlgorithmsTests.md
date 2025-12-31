---
module: Tests.RealWorldAlgorithmsTests
kind: per-module
imports:
  - Agda.Primitive using (Level)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Core.Phase using
  - Examples.RealWorldAlgorithms as R
---

# Module: Tests.RealWorldAlgorithmsTests

**Source:** `src/agda/Tests/RealWorldAlgorithmsTests.agda`

## Dependencies

- Agda.Primitive using (Level)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Core.Phase using
- Examples.RealWorldAlgorithms as R

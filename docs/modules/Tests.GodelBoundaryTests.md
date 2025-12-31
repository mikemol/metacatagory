---
module: Tests.GodelBoundaryTests
kind: per-module
imports:
  - Core.GodelBoundary as GB
  - Metamodel as M
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Nat using (Nat; zero; suc)
---

# Module: Tests.GodelBoundaryTests

**Source:** `src/agda/Tests/GodelBoundaryTests.agda`

## Dependencies

- Core.GodelBoundary as GB
- Metamodel as M
- Core.Phase using (Bool; true; false)
- Agda.Builtin.String using (String)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Nat using (Nat; zero; suc)

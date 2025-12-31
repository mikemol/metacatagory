---
module: Tests.SerializationTests
kind: per-module
imports:
  - Core
  - Core.Phase
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Core.AlgebraicAlgorithms
  - Core.Algorithms.Bundle
  - Metamodel as M
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Equality using (_≡_)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (Maybe; just; nothing)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Phase using (Bool; true; false)
  - Core.TechnicalDebt
---

# Module: Tests.SerializationTests

**Source:** `src/agda/Tests/SerializationTests.agda`

## Dependencies

- Core
- Core.Phase
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Core.AlgebraicAlgorithms
- Core.Algorithms.Bundle
- Metamodel as M
- Agda.Builtin.String using (String)
- Agda.Builtin.Equality using (_≡_)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (Maybe; just; nothing)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Phase using (Bool; true; false)
- Core.TechnicalDebt

---
module: Tests.ConstructiveWitnessTests
kind: per-module
imports:
  - Core
  - Core.Phase
  - Core.Witnesses
  - Core.ConstructiveWitnesses
  - Core.AlgebraicAlgorithms
  - Core.AlgorithmUniversality hiding (proof)
  - Core.UniversalProperties
  - Algebra.Foundation
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Phase using (Bool; true; false)
  - Core.PolynomialsF2 as PF2
---

# Module: Tests.ConstructiveWitnessTests

**Source:** `src/agda/Tests/ConstructiveWitnessTests.agda`

## Dependencies

- Core
- Core.Phase
- Core.Witnesses
- Core.ConstructiveWitnesses
- Core.AlgebraicAlgorithms
- Core.AlgorithmUniversality hiding (proof)
- Core.UniversalProperties
- Algebra.Foundation
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Phase using (Bool; true; false)
- Core.PolynomialsF2 as PF2

---
module: Tests.AlgorithmCompositionTestsMinimal
kind: per-module
imports:
  - Metamodel as M
  - Core.Phase
  - Core.AlgebraicAlgorithms
  - Core.UniversalProperties
  - Core.Algorithms.Bundle
  - Core.Algorithms.Registry using (FieldClassification)
  - Core.TechnicalDebt
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic using (GaloisGroup; SplittingField)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Nat as N using (Nat; suc)
---

# Module: Tests.AlgorithmCompositionTestsMinimal

**Source:** `src/agda/Tests/AlgorithmCompositionTestsMinimal.agda`

## Dependencies

- Metamodel as M
- Core.Phase
- Core.AlgebraicAlgorithms
- Core.UniversalProperties
- Core.Algorithms.Bundle
- Core.Algorithms.Registry using (FieldClassification)
- Core.TechnicalDebt
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic using (GaloisGroup; SplittingField)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Nat as N using (Nat; suc)

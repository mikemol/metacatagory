---
module: Core.AlgorithmCorrectness
kind: per-module
imports:
  - Agda.Primitive using (Level; lsuc)
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Core
  - Core.Phase
  - Core.Witnesses
  - Core.ConstructiveWitnesses
  - Core.AlgebraicAlgorithms
  - Algebra.Foundation
  - Algebra.Rings.Basic using (FieldDeclaration)
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Algebra.Groups.Basic
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Maybe; nothing; just)
  - Core.Phase using (Bool; true; false)
---

# Module: Core.AlgorithmCorrectness

**Source:** `src/agda/Core/AlgorithmCorrectness.agda`

## Dependencies

- Agda.Primitive using (Level; lsuc)
- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Core
- Core.Phase
- Core.Witnesses
- Core.ConstructiveWitnesses
- Core.AlgebraicAlgorithms
- Algebra.Foundation
- Algebra.Rings.Basic using (FieldDeclaration)
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Algebra.Groups.Basic
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Maybe; nothing; just)
- Core.Phase using (Bool; true; false)

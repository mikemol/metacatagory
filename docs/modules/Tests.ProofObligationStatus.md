---
module: Tests.ProofObligationStatus
kind: per-module
imports:
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Core.Phase using (true)
  - Metamodel as M
  - Core.AlgorithmCorrectness
  - Core.ConstructiveWitnesses
  - Examples.AlgorithmCorrectnessExamples as Ex
---

# Module: Tests.ProofObligationStatus

**Source:** `src/agda/Tests/ProofObligationStatus.agda`

## Dependencies

- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Core.Phase using (true)
- Metamodel as M
- Core.AlgorithmCorrectness
- Core.ConstructiveWitnesses
- Examples.AlgorithmCorrectnessExamples as Ex

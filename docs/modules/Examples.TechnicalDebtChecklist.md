---
module: Examples.TechnicalDebtChecklist
kind: per-module
imports:
  - Metamodel as M
  - PropertyRegistry as PR
  - Agda.Builtin.List
  - Agda.Builtin.String
  - Agda.Builtin.Int
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Sigma using (Σ; _,_)
  - Core.Utils using (map)
  - Tests.AlgorithmCompositionTests using (technicalDebtRegistry)
  - Core.TechnicalDebt using (DebtAnnotation; Priority; PriorityGreater; highPriority; lowPriority)
---

# Module: Examples.TechnicalDebtChecklist

**Source:** `src/agda/Examples/TechnicalDebtChecklist.agda`

## Dependencies

- Metamodel as M
- PropertyRegistry as PR
- Agda.Builtin.List
- Agda.Builtin.String
- Agda.Builtin.Int
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Sigma using (Σ; _,_)
- Core.Utils using (map)
- Tests.AlgorithmCompositionTests using (technicalDebtRegistry)
- Core.TechnicalDebt using (DebtAnnotation; Priority; PriorityGreater; highPriority; lowPriority)

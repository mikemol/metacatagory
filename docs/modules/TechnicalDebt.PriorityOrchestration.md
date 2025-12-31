---
module: TechnicalDebt.PriorityOrchestration
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (_×_; Σ)
  - Agda.Builtin.String
  - Agda.Builtin.Unit
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Int using (Int)
  - Agda.Primitive using (Level)
  - TechnicalDebt.Priorities
  - TechnicalDebt.PriorityMapping
  - TechnicalDebt.PriorityFormatting
  - TechnicalDebt.DeferredItemsFormatting
---

# Module: TechnicalDebt.PriorityOrchestration

**Source:** `src/agda/TechnicalDebt/PriorityOrchestration.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (_×_; Σ)
- Agda.Builtin.String
- Agda.Builtin.Unit
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Int using (Int)
- Agda.Primitive using (Level)
- TechnicalDebt.Priorities
- TechnicalDebt.PriorityMapping
- TechnicalDebt.PriorityFormatting
- TechnicalDebt.DeferredItemsFormatting

---
module: TechnicalDebt.PriorityMapping
kind: per-module
imports:
  - TechnicalDebt.Priorities
  - Core.TechnicalDebt using (Priority; weight)
  - Core using (_×_; _,_)
  - Agda.Builtin.Int using (Int; pos; negsuc)
---

# Module: TechnicalDebt.PriorityMapping

**Source:** `src/agda/TechnicalDebt/PriorityMapping.agda`

## Dependencies

- TechnicalDebt.Priorities
- Core.TechnicalDebt using (Priority; weight)
- Core using (_×_; _,_)
- Agda.Builtin.Int using (Int; pos; negsuc)

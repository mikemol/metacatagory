---
module: TechnicalDebt.Priorities
kind: per-module
imports:
  - Core.TechnicalDebt using (Priority; mkPriority)
  - Core using (_×_; _,_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Int using (Int; pos)
  - Agda.Builtin.List using (List; _∷_; [])
---

# Module: TechnicalDebt.Priorities

**Source:** `src/agda/TechnicalDebt/Priorities.agda`

## Dependencies

- Core.TechnicalDebt using (Priority; mkPriority)
- Core using (_×_; _,_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Int using (Int; pos)
- Agda.Builtin.List using (List; _∷_; [])

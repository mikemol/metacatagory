---
module: GrowthAnalysis
kind: per-module
imports:
  - Core.GrowthMetrics using
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Core.Utils using (length; map)
  - Metamodel as M
---

# Module: GrowthAnalysis

**Source:** `src/agda/GrowthAnalysis.agda`

## Dependencies

- Core.GrowthMetrics using
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Core.Utils using (length; map)
- Metamodel as M

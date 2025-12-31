---
module: Plan.CIM.GrammarBridge
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Plan.CIM.Utility using (map; Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric)
  - Plan.CIM.PandocAST
---

# Module: Plan.CIM.GrammarBridge

**Source:** `src/agda/Plan/CIM/GrammarBridge.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.Bool using (Bool; true; false)
- Plan.CIM.Utility using (map; Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric)
- Plan.CIM.PandocAST

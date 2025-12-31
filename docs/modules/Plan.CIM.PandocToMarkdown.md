---
module: Plan.CIM.PandocToMarkdown
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.Bool using (Bool; true)
  - Agda.Builtin.Equality using (_≡_; refl)
  - Plan.CIM.Utility using (map; _++_; TransformationSystem; CoherenceWitness; Path; refl-path; EmergentMetric)
  - Plan.CIM.PandocAST
---

# Module: Plan.CIM.PandocToMarkdown

**Source:** `src/agda/Plan/CIM/PandocToMarkdown.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.Bool using (Bool; true)
- Agda.Builtin.Equality using (_≡_; refl)
- Plan.CIM.Utility using (map; _++_; TransformationSystem; CoherenceWitness; Path; refl-path; EmergentMetric)
- Plan.CIM.PandocAST

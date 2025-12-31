---
module: Plan.CIM.Structure
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; _+_)
  - Agda.Builtin.Bool using (Bool; true)
  - Plan.CIM.Utility using (Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric; BraidedInheritanceFunctor; map; Path; refl-path)
  - Plan.CIM.PandocAST
  - Plan.CIM.PandocProtocols
  - Plan.CIM.PandocToMarkdown
---

# Module: Plan.CIM.Structure

**Source:** `src/agda/Plan/CIM/Structure.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; _+_)
- Agda.Builtin.Bool using (Bool; true)
- Plan.CIM.Utility using (Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric; BraidedInheritanceFunctor; map; Path; refl-path)
- Plan.CIM.PandocAST
- Plan.CIM.PandocProtocols
- Plan.CIM.PandocToMarkdown

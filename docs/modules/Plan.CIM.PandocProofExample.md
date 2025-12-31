---
module: Plan.CIM.PandocProofExample
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.Bool using (Bool; true)
  - Plan.CIM.Utility using (map; _++_)
  - Plan.CIM.PandocAST
  - Plan.CIM.PandocToMarkdown
  - Plan.CIM.Structure
  - Plan.CIM.GrammarBridge
---

# Module: Plan.CIM.PandocProofExample

**Source:** `src/agda/Plan/CIM/PandocProofExample.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.Bool using (Bool; true)
- Plan.CIM.Utility using (map; _++_)
- Plan.CIM.PandocAST
- Plan.CIM.PandocToMarkdown
- Plan.CIM.Structure
- Plan.CIM.GrammarBridge

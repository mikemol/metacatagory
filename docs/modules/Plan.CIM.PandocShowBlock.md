---
module: Plan.CIM.PandocShowBlock
kind: per-module
imports:
  - Agda.Builtin.List using (List; _∷_; [])
  - Agda.Builtin.Nat using (Nat; suc; zero; _<_; _-_)
  - Agda.Builtin.String
  - Agda.Builtin.Bool using (Bool; true; false)
  - Plan.CIM.PandocAST using (Block)
  - Plan.CIM.PandocShowInline using (showInlines)
---

# Module: Plan.CIM.PandocShowBlock

**Source:** `src/agda/Plan/CIM/PandocShowBlock.agda`

## Dependencies

- Agda.Builtin.List using (List; _∷_; [])
- Agda.Builtin.Nat using (Nat; suc; zero; _<_; _-_)
- Agda.Builtin.String
- Agda.Builtin.Bool using (Bool; true; false)
- Plan.CIM.PandocAST using (Block)
- Plan.CIM.PandocShowInline using (showInlines)

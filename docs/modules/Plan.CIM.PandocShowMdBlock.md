---
module: Plan.CIM.PandocShowMdBlock
kind: per-module
imports:
  - Agda.Builtin.Nat using (Nat; _<_; zero; suc; _-_)
  - Plan.CIM.PandocShowBlock using (showNat)
  - Agda.Builtin.List using (List; _∷_; [])
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.String
  - Plan.CIM.PandocAST using (MdBlock)
  - Plan.CIM.PandocShowInline using (showMdInlines)
---

# Module: Plan.CIM.PandocShowMdBlock

**Source:** `src/agda/Plan/CIM/PandocShowMdBlock.agda`

## Dependencies

- Agda.Builtin.Nat using (Nat; _<_; zero; suc; _-_)
- Plan.CIM.PandocShowBlock using (showNat)
- Agda.Builtin.List using (List; _∷_; [])
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.String
- Plan.CIM.PandocAST using (MdBlock)
- Plan.CIM.PandocShowInline using (showMdInlines)

---
module: Plan.CIM.DocumentSynthesis
kind: per-module
imports:
  - Agda.Builtin.String using (String; primStringAppend; primStringEquality)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.Char using (Char)
  - Plan.CIM.Utility using (_++_; map)
  - Plan.CIM.FrameworkMetadata
  - Plan.CIM.PandocAST
---

# Module: Plan.CIM.DocumentSynthesis

**Source:** `src/agda/Plan/CIM/DocumentSynthesis.agda`

## Dependencies

- Agda.Builtin.String using (String; primStringAppend; primStringEquality)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.Char using (Char)
- Plan.CIM.Utility using (_++_; map)
- Plan.CIM.FrameworkMetadata
- Plan.CIM.PandocAST

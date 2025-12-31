---
module: Plan.CIM.RoadmapExporter
kind: per-module
imports:
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc; _+_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - Plan.CIM.Utility hiding (_++_; map)
  - Plan.CIM.PandocAST using (PandocDoc; MarkdownDoc; Block; Inline; MdBlock)
  - Plan.CIM.PandocShowInline using (showInlines; showMdInlines)
  - Plan.CIM.PandocShowBlock using (showBlock; showBlocks; showBlockLists)
  - Plan.CIM.PandocShowMdBlock using (showMdBlock; showMdBlocks; showMdBlockLists)
  - Plan.CIM.DocumentationContent
  - Plan.CIM.DocumentSynthesis
---

# Module: Plan.CIM.RoadmapExporter

**Source:** `src/agda/Plan/CIM/RoadmapExporter.agda`

## Dependencies

- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc; _+_)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- Plan.CIM.Utility hiding (_++_; map)
- Plan.CIM.PandocAST using (PandocDoc; MarkdownDoc; Block; Inline; MdBlock)
- Plan.CIM.PandocShowInline using (showInlines; showMdInlines)
- Plan.CIM.PandocShowBlock using (showBlock; showBlocks; showBlockLists)
- Plan.CIM.PandocShowMdBlock using (showMdBlock; showMdBlocks; showMdBlockLists)
- Plan.CIM.DocumentationContent
- Plan.CIM.DocumentSynthesis

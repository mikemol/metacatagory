---
module: Plan.CIM.MarkdownParse
kind: per-module
imports:
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Char using (Char; primCharEquality)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.String using (String)
  - Plan.CIM.PandocAST
  - Plan.CIM.PandocToMarkdown using (pandocDocToMarkdown)
  - Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)
---

# Module: Plan.CIM.MarkdownParse

**Source:** `src/agda/Plan/CIM/MarkdownParse.agda`

## Dependencies

- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Char using (Char; primCharEquality)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.String using (String)
- Plan.CIM.PandocAST
- Plan.CIM.PandocToMarkdown using (pandocDocToMarkdown)
- Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)

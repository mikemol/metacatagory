---
module: Plan.CIM.MarkdownNormalize
kind: per-module
imports:
  - Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)
  - Agda.Builtin.List   using (List; []; _∷_)
  - Agda.Builtin.Unit   using (⊤; tt)
  - Agda.Builtin.IO     using (IO)
  - Agda.Builtin.Bool   using (Bool; true; false)
  - Agda.Builtin.Char   using (Char)
  - Agda.Builtin.Nat    using (Nat; zero; suc)
  - Plan.CIM.PandocAST
  - Plan.CIM.MarkdownParse
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
  - System.Environment (getArgs)
---

# Module: Plan.CIM.MarkdownNormalize

**Source:** `src/agda/Plan/CIM/MarkdownNormalize.agda`

## Dependencies

- Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)
- Agda.Builtin.List   using (List; []; _∷_)
- Agda.Builtin.Unit   using (⊤; tt)
- Agda.Builtin.IO     using (IO)
- Agda.Builtin.Bool   using (Bool; true; false)
- Agda.Builtin.Char   using (Char)
- Agda.Builtin.Nat    using (Nat; zero; suc)
- Plan.CIM.PandocAST
- Plan.CIM.MarkdownParse
- qualified Data.Text as T
- qualified Data.Text.IO as TIO
- System.Environment (getArgs)

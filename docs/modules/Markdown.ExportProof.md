---
module: Markdown.ExportProof
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Markdown.Normalization
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤)
  - Core.Utils
---

# Module: Markdown.ExportProof

**Source:** `src/agda/Markdown/ExportProof.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Markdown.Normalization
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤)
- Core.Utils

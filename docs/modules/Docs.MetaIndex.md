---
module: Docs.MetaIndex
kind: per-module
imports:
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.List using (List; []; _∷_)
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
---

# Module: Docs.MetaIndex

**Source:** `src/agda/Docs/MetaIndex.agda`

## Dependencies

- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.List using (List; []; _∷_)
- qualified Data.Text as T
- qualified Data.Text.IO as TIO

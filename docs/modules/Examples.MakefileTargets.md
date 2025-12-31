---
module: Examples.MakefileTargets
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend; primStringEquality)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - Agda.Builtin.IO using (IO)
  - qualified System.Directory as Dir
  - qualified System.FilePath as FP
  - qualified Data.Text as T
---

# Module: Examples.MakefileTargets

**Source:** `src/agda/Examples/MakefileTargets.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend; primStringEquality)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- Agda.Builtin.IO using (IO)
- qualified System.Directory as Dir
- qualified System.FilePath as FP
- qualified Data.Text as T

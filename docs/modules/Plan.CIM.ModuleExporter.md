---
module: Plan.CIM.ModuleExporter
kind: per-module
imports:
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Core.Utils using (map)
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
  - qualified System.Directory as Dir
  - qualified System.FilePath as FP
  - Data.List (isPrefixOf, nub, sortBy, isSuffixOf)
  - Data.Ord (comparing)
  - Data.Char (isSpace)
  - Control.Exception (catch, SomeException)
---

# Module: Plan.CIM.ModuleExporter

**Source:** `src/agda/Plan/CIM/ModuleExporter.agda`

## Dependencies

- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.String using (String)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Bool using (Bool; true; false)
- Core.Utils using (map)
- qualified Data.Text as T
- qualified Data.Text.IO as TIO
- qualified System.Directory as Dir
- qualified System.FilePath as FP
- Data.List (isPrefixOf, nub, sortBy, isSuffixOf)
- Data.Ord (comparing)
- Data.Char (isSpace)
- Control.Exception (catch, SomeException)

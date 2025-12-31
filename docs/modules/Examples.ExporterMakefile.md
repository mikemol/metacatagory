---
module: Examples.ExporterMakefile
kind: per-module
imports:
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
  - qualified Data.Map.Strict as M
  - qualified Data.List as L
  - Data.Maybe (fromMaybe)
  - qualified System.Directory as Dir
  - Examples.AgdaFileScanFFI
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Core.Utils using (map)
  - Core.CategoricalAdapter
  - Core.AdapterAutomation
  - Metamodel
  - Examples.AgdaMakefileDeps using (_++_; ModuleName; primStringEquality; primStringSplit;
  - Examples.MakefileTargets using (MakefileTarget; TargetCategory; allCategories;
---

# Module: Examples.ExporterMakefile

**Source:** `src/agda/Examples/ExporterMakefile.agda`

## Dependencies

- qualified Data.Text as T
- qualified Data.Text.IO as TIO
- qualified Data.Map.Strict as M
- qualified Data.List as L
- Data.Maybe (fromMaybe)
- qualified System.Directory as Dir
- Examples.AgdaFileScanFFI
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.String using (String)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Bool using (Bool; true; false)
- Core.Utils using (map)
- Core.CategoricalAdapter
- Core.AdapterAutomation
- Metamodel
- Examples.AgdaMakefileDeps using (_++_; ModuleName; primStringEquality; primStringSplit;
- Examples.MakefileTargets using (MakefileTarget; TargetCategory; allCategories;

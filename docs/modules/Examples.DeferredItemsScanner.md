---
module: Examples.DeferredItemsScanner
kind: per-module
imports:
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.Nat using (Nat; _+_; zero; suc)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Maybe using (Maybe; just; nothing)
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
  - qualified System.Process as Proc
  - qualified Data.Time.Clock as Clock
  - qualified Data.Time.Format as TimeF
  - qualified System.Environment
  - Core.Strings
  - Core.IO String ⊤ tt IO _>>=_ _>>_ return primWriteFile primReadFile primAppendFile primPutStr primPutStrLn primGetLine
  - Core.Rendering
---

# Module: Examples.DeferredItemsScanner

**Source:** `src/agda/Examples/DeferredItemsScanner.agda`

## Dependencies

- Agda.Builtin.IO using (IO)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.Nat using (Nat; _+_; zero; suc)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Maybe using (Maybe; just; nothing)
- qualified Data.Text as T
- qualified Data.Text.IO as TIO
- qualified System.Process as Proc
- qualified Data.Time.Clock as Clock
- qualified Data.Time.Format as TimeF
- qualified System.Environment
- Core.Strings
- Core.IO String ⊤ tt IO _>>=_ _>>_ return primWriteFile primReadFile primAppendFile primPutStr primPutStrLn primGetLine
- Core.Rendering

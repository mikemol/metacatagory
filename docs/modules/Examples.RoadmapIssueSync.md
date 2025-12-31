---
module: Examples.RoadmapIssueSync
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤; tt)
  - qualified Data.Text as T
  - qualified Data.Text.IO as TIO
  - qualified Data.Aeson as Aeson
  - qualified Data.ByteString.Lazy as BSL
  - qualified Network.HTTP.Client as HTTP
  - qualified Network.HTTP.Client.TLS as HTTPS
  - qualified Network.HTTP.Types.Status as HTTPStatus
  - qualified System.Environment as Env
  - Control.Monad (forM_, when)
  - Data.Maybe (fromMaybe)
  - qualified Data.Text.Encoding as TE
---

# Module: Examples.RoadmapIssueSync

**Source:** `src/agda/Examples/RoadmapIssueSync.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤; tt)
- qualified Data.Text as T
- qualified Data.Text.IO as TIO
- qualified Data.Aeson as Aeson
- qualified Data.ByteString.Lazy as BSL
- qualified Network.HTTP.Client as HTTP
- qualified Network.HTTP.Client.TLS as HTTPS
- qualified Network.HTTP.Types.Status as HTTPStatus
- qualified System.Environment as Env
- Control.Monad (forM_, when)
- Data.Maybe (fromMaybe)
- qualified Data.Text.Encoding as TE

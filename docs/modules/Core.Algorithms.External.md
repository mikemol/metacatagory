---
module: Core.Algorithms.External
kind: per-module
imports:
  - Core
  - Algebra.Foundation
  - Algebra.Rings.Basic
  - Algebra.Fields.Basic
  - Algebra.Fields.Advanced
  - Core.AlgebraicAlgorithms
  - Core.Witnesses
  - Core.Algorithms.Bundle
  - Metamodel as M
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Core.Phase using (Maybe; nothing; just)
  - Agda.Primitive using (Level; lzero; lsuc; _⊔_)
  - Algorithms.Basic
---

# Module: Core.Algorithms.External

**Source:** `src/agda/Core/Algorithms/External.agda`

## Dependencies

- Core
- Algebra.Foundation
- Algebra.Rings.Basic
- Algebra.Fields.Basic
- Algebra.Fields.Advanced
- Core.AlgebraicAlgorithms
- Core.Witnesses
- Core.Algorithms.Bundle
- Metamodel as M
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Core.Phase using (Maybe; nothing; just)
- Agda.Primitive using (Level; lzero; lsuc; _⊔_)
- Algorithms.Basic

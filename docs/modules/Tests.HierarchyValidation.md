---
module: Tests.HierarchyValidation
kind: per-module
imports:
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Unit using (⊤; tt)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Tests.CoverageReport using (WellFoundedIndex; mkIndex; _<ᵢ_)
---

# Module: Tests.HierarchyValidation

**Source:** `src/agda/Tests/HierarchyValidation.agda`

## Dependencies

- Core.Phase using (Bool; true; false)
- Agda.Builtin.Unit using (⊤; tt)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Tests.CoverageReport using (WellFoundedIndex; mkIndex; _<ᵢ_)

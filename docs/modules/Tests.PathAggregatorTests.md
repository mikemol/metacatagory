---
module: Tests.PathAggregatorTests
kind: per-module
imports:
  - Core.PathAggregator
  - Metamodel as M
  - Core.GrowthMetrics as GM
  - Agda.Builtin.Equality using (_≡_; refl)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.Nat using (Nat; zero; suc)
---

# Module: Tests.PathAggregatorTests

**Source:** `src/agda/Tests/PathAggregatorTests.agda`

## Dependencies

- Core.PathAggregator
- Metamodel as M
- Core.GrowthMetrics as GM
- Agda.Builtin.Equality using (_≡_; refl)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.Nat using (Nat; zero; suc)

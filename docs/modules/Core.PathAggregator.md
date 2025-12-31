---
module: Core.PathAggregator
kind: per-module
imports:
  - Infrastructure.Universe using (Setℓ)
  - Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
  - Metamodel as M
  - Core.Phase
  - Agda.Builtin.Equality using (_≡_; refl)
  - Agda.Builtin.Nat using (Nat; zero; suc)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (Bool; true; false)
  - Agda.Builtin.String using (String)
  - Agda.Primitive using (Level; lzero; lsuc; _⊔_)
  - Core.GrowthMetrics as GM  -- Link growth metrics
---

# Module: Core.PathAggregator

**Source:** `src/agda/Core/PathAggregator.agda`

## Dependencies

- Infrastructure.Universe using (Setℓ)
- Infrastructure.Coherence.Path2 using (_≡_; refl; whisker; _∙₂_)
- Metamodel as M
- Core.Phase
- Agda.Builtin.Equality using (_≡_; refl)
- Agda.Builtin.Nat using (Nat; zero; suc)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (Bool; true; false)
- Agda.Builtin.String using (String)
- Agda.Primitive using (Level; lzero; lsuc; _⊔_)
- Core.GrowthMetrics as GM  -- Link growth metrics

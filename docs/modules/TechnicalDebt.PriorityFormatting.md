---
module: TechnicalDebt.PriorityFormatting
kind: per-module
imports:
  - Agda.Builtin.Int using (Int)
  - Agda.Builtin.String using (String; primStringAppend)
  - TechnicalDebt.DeferredItemsFormatting using (mkListLike; AUDAXBlock; AUDAXDoc; AUDAXInline; ListLike)
  - TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
  - Agda.Builtin.List using (List; []; _∷_)
  - TechnicalDebt.Priorities using (PriorityStrategy)
  - Agda.Primitive using (Level; _⊔_)
  - Core.Phase using (_×_; Σ; _,_)
---

# Module: TechnicalDebt.PriorityFormatting

**Source:** `src/agda/TechnicalDebt/PriorityFormatting.agda`

## Dependencies

- Agda.Builtin.Int using (Int)
- Agda.Builtin.String using (String; primStringAppend)
- TechnicalDebt.DeferredItemsFormatting using (mkListLike; AUDAXBlock; AUDAXDoc; AUDAXInline; ListLike)
- TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
- Agda.Builtin.List using (List; []; _∷_)
- TechnicalDebt.Priorities using (PriorityStrategy)
- Agda.Primitive using (Level; _⊔_)
- Core.Phase using (_×_; Σ; _,_)

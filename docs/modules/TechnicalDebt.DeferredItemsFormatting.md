---
module: TechnicalDebt.DeferredItemsFormatting
kind: per-module
imports:
  - Agda.Builtin.String using (String; primShowNat; primStringAppend)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (_×_; _,_)
  - TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)
---

# Module: TechnicalDebt.DeferredItemsFormatting

**Source:** `src/agda/TechnicalDebt/DeferredItemsFormatting.agda`

## Dependencies

- Agda.Builtin.String using (String; primShowNat; primStringAppend)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (_×_; _,_)
- TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)

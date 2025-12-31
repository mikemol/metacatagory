---
module: TechnicalDebt.DeferredItemsFormatting_AUDAX
kind: per-module
imports:
  - Agda.Builtin.String using (String; primShowNat)
  - Agda.Builtin.Nat using (Nat)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (_×_; _,_)
  - TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)
---

# Module: TechnicalDebt.DeferredItemsFormatting_AUDAX

**Source:** `src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda`

## Dependencies

- Agda.Builtin.String using (String; primShowNat)
- Agda.Builtin.Nat using (Nat)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (_×_; _,_)
- TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)

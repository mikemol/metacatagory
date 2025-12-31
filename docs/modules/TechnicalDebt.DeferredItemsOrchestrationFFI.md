---
module: TechnicalDebt.DeferredItemsOrchestrationFFI
kind: per-module
imports:
  - Agda.Builtin.IO using (IO)
  - Agda.Builtin.Unit using (⊤)
  - Agda.Builtin.Nat using (Nat; zero; suc; _+_)
  - Agda.Builtin.String using (String; primStringAppend)
  - Agda.Builtin.List using (List; []; _∷_)
  - Core.Phase using (_×_; fst; snd)
  - TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts)
  - TechnicalDebt.DeferredItemsFormatting using (AUDAXDoc; AUDAXBlock; AUDAXInline; formatDeferredItemsAUDAXDoc; countsAsFields; natToString; ListLike; Header; Para; CodeBlock; BlockQuote; ListBlock; Table; Field; Raw; Null)
---

# Module: TechnicalDebt.DeferredItemsOrchestrationFFI

**Source:** `src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda`

## Dependencies

- Agda.Builtin.IO using (IO)
- Agda.Builtin.Unit using (⊤)
- Agda.Builtin.Nat using (Nat; zero; suc; _+_)
- Agda.Builtin.String using (String; primStringAppend)
- Agda.Builtin.List using (List; []; _∷_)
- Core.Phase using (_×_; fst; snd)
- TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts)
- TechnicalDebt.DeferredItemsFormatting using (AUDAXDoc; AUDAXBlock; AUDAXInline; formatDeferredItemsAUDAXDoc; countsAsFields; natToString; ListLike; Header; Para; CodeBlock; BlockQuote; ListBlock; Table; Field; Raw; Null)

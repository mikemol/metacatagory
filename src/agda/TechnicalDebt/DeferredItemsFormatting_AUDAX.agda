{-# OPTIONS --without-K #-}

module TechnicalDebt.DeferredItemsFormatting_AUDAX where

open import Agda.Builtin.String using (String; primShowNat)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Phase using (_×_; _,_)

-- Reusable record type for list-like structures
record ListLike (A : Set) : Set where
  constructor mkListLike
  field
    items : List A

-- AUDAXInline type
data AUDAXInline : Set where
  Str    : String → AUDAXInline
  Emph   : ListLike AUDAXInline → AUDAXInline
  Strong : ListLike AUDAXInline → AUDAXInline
  Code   : String → AUDAXInline
  Link   : ListLike AUDAXInline → String → AUDAXInline
  Image  : ListLike AUDAXInline → String → AUDAXInline
  Space  : AUDAXInline
  Break  : AUDAXInline

-- AUDAXBlock type
data AUDAXBlock : Set where
  Para       : ListLike AUDAXInline → AUDAXBlock
  Header     : Nat → ListLike AUDAXInline → AUDAXBlock
  CodeBlock  : String → AUDAXBlock
  BlockQuote : ListLike AUDAXBlock → AUDAXBlock
  ListBlock  : ListLike (ListLike AUDAXBlock) → AUDAXBlock
  Table      : ListLike String → ListLike (ListLike AUDAXInline) → AUDAXBlock
  Field      : String → String → AUDAXBlock
  Raw        : String → AUDAXBlock
  Null       : AUDAXBlock

-- AUDAXDoc record type
record AUDAXDoc : Set where
  field
    blocks : ListLike AUDAXBlock
    meta   : String

open import TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)

-- Primitive Nat → String conversion for reuse
natToString : Nat → String
natToString = primShowNat

-- Build a structured AUDAXDoc from detected counts
formatDeferredItemsAUDAXDoc : DeferredItemCounts → AUDAXDoc
formatDeferredItemsAUDAXDoc counts =
  let open DeferredItemCounts counts
      total = totalDeferred counts
      tableHeader = mkListLike ("Category" ∷ "Count" ∷ [])
      tableRows = mkListLike (
        mkListLike (Str "DeviationLog" ∷ Str (natToString deviationLog) ∷ []) ∷
        mkListLike (Str "Postulates"   ∷ Str (natToString postulates)   ∷ []) ∷
        mkListLike (Str "TODO"         ∷ Str (natToString todo)         ∷ []) ∷
        mkListLike (Str "PLANNED"      ∷ Str (natToString planned)      ∷ []) ∷
        mkListLike (Str "FIXME"        ∷ Str (natToString fixme)        ∷ []) ∷
        mkListLike (Str "Total"        ∷ Str (natToString total)        ∷ []) ∷
        [] )
      blocks = mkListLike (
        Header 1 (mkListLike (Str "Deferred Items Report" ∷ [])) ∷
        Header 2 (mkListLike (Str "DeviationLog Entries" ∷ [])) ∷
        Field "DeviationLog" (natToString deviationLog) ∷
        Header 2 (mkListLike (Str "Postulates" ∷ [])) ∷
        Field "Postulates" (natToString postulates) ∷
        Header 2 (mkListLike (Str "TODO Items" ∷ [])) ∷
        Field "TODO" (natToString todo) ∷
        Header 2 (mkListLike (Str "PLANNED Items" ∷ [])) ∷
        Field "PLANNED" (natToString planned) ∷
        Header 2 (mkListLike (Str "FIXME Items" ∷ [])) ∷
        Field "FIXME" (natToString fixme) ∷
        Header 2 (mkListLike (Str "Summary" ∷ [])) ∷
        Table tableHeader tableRows ∷
        [] )
  in record { blocks = blocks ; meta = "AUDAX deferred-items export" }

-- Extract counts as key/value pairs (for JSON rendering, if needed)
countsAsFields : DeferredItemCounts → List (String × String)
countsAsFields counts =
  let open DeferredItemCounts counts
      total = totalDeferred counts
  in ("deviation_log" , natToString deviationLog) ∷
     ("postulates"    , natToString postulates)   ∷
     ("todo"          , natToString todo)         ∷
     ("planned"       , natToString planned)      ∷
     ("fixme"         , natToString fixme)        ∷
     ("total"         , natToString total)        ∷ []

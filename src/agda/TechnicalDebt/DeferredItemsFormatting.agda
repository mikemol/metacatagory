{-# OPTIONS --without-K #-}

-- | Formatting helpers for deferred item summaries.
module TechnicalDebt.DeferredItemsFormatting where

open import Agda.Builtin.String using (String; primShowNat; primStringAppend)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Phase using (_×_; _,_)

map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

infixr 5 _++_
_++_ : String → String → String
_++_ = primStringAppend

concatMap : ∀ {A : Set} → (A → String) → List A → String
concatMap f [] = ""
concatMap f (x ∷ xs) = f x ++ concatMap f xs

-- Helper: join strings with separator
joinWith : String → List String → String
joinWith sep [] = ""
joinWith sep (x ∷ []) = x
joinWith sep (x ∷ xs) = x ++ sep ++ joinWith sep xs

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

{-# TERMINATING #-}
mutual
  AUDAXInlineListToMarkdown : List AUDAXInline → String
  AUDAXInlineListToMarkdown [] = ""
  AUDAXInlineListToMarkdown (x ∷ xs) = AUDAXInlineToMarkdown x ++ AUDAXInlineListToMarkdown xs

  AUDAXInlineToMarkdown : AUDAXInline → String
  AUDAXInlineToMarkdown (Str s) = s
  AUDAXInlineToMarkdown (Emph xs) = "*" ++ AUDAXInlineListToMarkdown (ListLike.items xs) ++ "*"
  AUDAXInlineToMarkdown (Strong xs) = "**" ++ AUDAXInlineListToMarkdown (ListLike.items xs) ++ "**"
  AUDAXInlineToMarkdown (Code s) = "`" ++ s ++ "`"
  AUDAXInlineToMarkdown (Link xs url) = "[" ++ AUDAXInlineListToMarkdown (ListLike.items xs) ++ "](" ++ url ++ ")"
  AUDAXInlineToMarkdown (Image xs url) = "![" ++ AUDAXInlineListToMarkdown (ListLike.items xs) ++ "](" ++ url ++ ")"
  AUDAXInlineToMarkdown Space = " "
  AUDAXInlineToMarkdown Break = "\n"

-- Implementation of table rendering (after mutual block)
renderMarkdownTable : ListLike String → ListLike (ListLike AUDAXInline) → String
renderMarkdownTable header rows =
  let headerStrs = ListLike.items header
      separator = joinWith " | " (map (λ _ → "---") headerStrs)
      headerRow = joinWith " | " headerStrs
      rowStrs = map renderRow (ListLike.items rows)
      allRows = joinWith "\n" rowStrs
  in "| " ++ headerRow ++ " |\n| " ++ separator ++ " |\n" ++ allRows ++ "\n\n"
  where
    renderRow : ListLike AUDAXInline → String
    renderRow cells = "| " ++ joinWith " | " (map AUDAXInlineToMarkdown (ListLike.items cells)) ++ " |"

{-# TERMINATING #-}
mutual
  AUDAXBlockListToMarkdown : List AUDAXBlock → String
  AUDAXBlockListToMarkdown [] = ""
  AUDAXBlockListToMarkdown (b ∷ bs) = AUDAXBlockToMarkdown b ++ AUDAXBlockListToMarkdown bs

  AUDAXBlockToMarkdown : AUDAXBlock → String
  AUDAXBlockToMarkdown (Para inlines) = AUDAXInlineListToMarkdown (ListLike.items inlines) ++ "\n\n"
  AUDAXBlockToMarkdown (Header n inlines) = "# " ++ AUDAXInlineListToMarkdown (ListLike.items inlines) ++ "\n\n"
  AUDAXBlockToMarkdown (CodeBlock code) = "```\n" ++ code ++ "\n```\n\n"
  AUDAXBlockToMarkdown (BlockQuote blocks) = "> " ++ AUDAXBlockListToMarkdown (ListLike.items blocks)
  AUDAXBlockToMarkdown (ListBlock blockss) = concatMap (\bs → "- " ++ AUDAXBlockListToMarkdown (ListLike.items bs)) (ListLike.items blockss)
  AUDAXBlockToMarkdown (Table header rows) = renderMarkdownTable header rows
  AUDAXBlockToMarkdown (Field k v) = k ++ ": " ++ v ++ "\n"
  AUDAXBlockToMarkdown (Raw s) = s ++ "\n"
  AUDAXBlockToMarkdown Null = ""

audaxDocToMarkdown : AUDAXDoc → String
audaxDocToMarkdown doc =
  AUDAXBlockListToMarkdown (ListLike.items (AUDAXDoc.blocks doc)) ++ "\n" ++ AUDAXDoc.meta doc ++ "\n"

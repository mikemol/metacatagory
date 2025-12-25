{-# OPTIONS --without-K #-}

module TechnicalDebt.DeferredItemsFormatting where

open import Agda.Builtin.String using (String; primShowNat)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Sigma using (Σ; _,_)

infixr 4 _×_
_×_ : Set → Set → Set
_×_ A B = Σ A (λ _ → B)
open import TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts; totalDeferred)

-- Primitive Nat → String conversion for reuse
natToString : Nat → String
natToString = primShowNat

-- Structured document representation (no concrete syntax here)
data Doc : Set where
  Heading : Nat → String → Doc
  Field   : String → String → Doc
  Table   : List (String × String) → Doc
  Concat  : List Doc → Doc

-- Build a structured document from detected counts (data only)
formatDeferredItemsDoc : DeferredItemCounts → Doc
formatDeferredItemsDoc counts =
  let open DeferredItemCounts counts
      total = totalDeferred counts
      rows = ("DeviationLog" , natToString deviationLog) ∷
             ("Postulates"  , natToString postulates)  ∷
             ("TODO"        , natToString todo)        ∷
             ("PLANNED"     , natToString planned)     ∷
             ("FIXME"       , natToString fixme)       ∷
             ("Total"       , natToString total)       ∷ []
  in Concat
       (Heading 1 "Deferred Items Report" ∷
        Heading 2 "DeviationLog Entries" ∷
        Field "DeviationLog" (natToString deviationLog) ∷
        Heading 2 "Postulates" ∷
        Field "Postulates" (natToString postulates) ∷
        Heading 2 "TODO Items" ∷
        Field "TODO" (natToString todo) ∷
        Heading 2 "PLANNED Items" ∷
        Field "PLANNED" (natToString planned) ∷
        Heading 2 "FIXME Items" ∷
        Field "FIXME" (natToString fixme) ∷
        Heading 2 "Summary" ∷
        Table rows ∷
        [] )

-- Extract counts as key/value pairs (useful for JSON rendering)
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

{-# OPTIONS --without-K #-}

module TechnicalDebt.DeferredItemsDetection where

open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)

-- Deferred item counts
record DeferredItemCounts : Set where
  field
    deviationLog : Nat
    postulates : Nat
    todo : Nat
    planned : Nat
    fixme : Nat

-- Total count
totalDeferred : DeferredItemCounts → Nat
totalDeferred c =
  DeferredItemCounts.deviationLog c +
  DeferredItemCounts.postulates c +
  DeferredItemCounts.todo c +
  DeferredItemCounts.planned c +
  DeferredItemCounts.fixme c

-- Empty baseline
zeroCounts : DeferredItemCounts
zeroCounts = record
  { deviationLog = 0
  ; postulates = 0
  ; todo = 0
  ; planned = 0
  ; fixme = 0
  }

-- Item entry (for collecting detailed results)
record DeferredItemEntry : Set where
  field
    file : String
    line : Nat
    content : String

-- Summary with detailed entries
record DeferredItemsSummary : Set where
  field
    counts : DeferredItemCounts
    deviationLogEntries : List DeferredItemEntry
    postulateEntries : List DeferredItemEntry
    todoEntries : List DeferredItemEntry
    plannedEntries : List DeferredItemEntry
    fixmeEntries : List DeferredItemEntry

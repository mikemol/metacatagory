{-# OPTIONS --without-K #-}

-- | Minimal AUDAX inline test harness.
module MinimalAUDAXInlineTest where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)

-- Minimal copy of AUDAXInline
record ListLike (A : Set) : Set where
  constructor mkListLike
  field
    items : List A

data AUDAXInline : Set where
  Str  : String → AUDAXInline
  Emph : ListLike AUDAXInline → AUDAXInline

-- Minimal test function
renderInline : AUDAXInline → String
renderInline (Str s) = s
renderInline (Emph xs) = "*" -- Just return a marker for Emph

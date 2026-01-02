{-# OPTIONS --without-K --cubical-compatible #-}

-- | Show/pretty-print Markdown-specific block nodes.
module Plan.CIM.PandocShowMdBlock where

open import Agda.Builtin.Nat using (Nat; _<_; zero; suc; _-_)
open import Plan.CIM.PandocShowBlock using (showNat)
open import Agda.Builtin.List using (List; _∷_; [])
open import Agda.Builtin.Bool using (Bool; true; false)

open import Agda.Builtin.String
open import Plan.CIM.PandocAST using (MdBlock)
open Plan.CIM.PandocAST
open import Plan.CIM.PandocShowInline using (showMdInlines)

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

mutual
  showMdBlock : MdBlock → String
  showMdBlock (MdPara ys) = _++_ "MdPara " (showMdInlines ys)
  showMdBlock (MdHeader n ys) = _++_ (_++_ (_++_ "MdHeader " (showNat n)) " ") (showMdInlines ys)
  showMdBlock (MdCodeBlock s) = _++_ "MdCodeBlock " s
  showMdBlock (MdList items) = _++_ (_++_ "MdList [" (showMdBlockLists items)) "]"
  showMdBlock (MdOrderedList items) = _++_ (_++_ "MdOrderedList [" (showMdBlockLists items)) "]"
  showMdBlock (MdQuote qs) = _++_ (_++_ "MdQuote [" (showMdBlocks qs)) "]"
  showMdBlock MdRule = "MdRule"
  showMdBlock (MdRaw s) = _++_ "MdRaw " s
  showMdBlock MdNull = "MdNull"
  showMdBlock MdEOB = "MdEOB"
  showMdBlock MdSBB = "MdSBB"

  showMdBlocks : List MdBlock → String
  showMdBlocks [] = ""
  showMdBlocks (b ∷ []) = showMdBlock b
  showMdBlocks (b ∷ bs) = _++_ (_++_ (showMdBlock b) ", ") (showMdBlocks bs)

  showMdBlockLists : List (List MdBlock) → String
  showMdBlockLists [] = ""
  showMdBlockLists (xs ∷ []) = _++_ (_++_ "[" (showMdBlocks xs)) "]"
  showMdBlockLists (xs ∷ ys) = _++_ (_++_ (_++_ "[" (showMdBlocks xs)) "], ") (showMdBlockLists ys)


{-# TERMINATING #-}
div : Nat → Nat → Nat
div n m with n < m
... | true = zero
... | false = suc (div (n - m) m)

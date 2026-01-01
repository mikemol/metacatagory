
{-# OPTIONS --without-K --cubical-compatible #-}

-- | Show/pretty-print block-level Pandoc AST nodes.
module Plan.CIM.PandocShowBlock where

open import Agda.Builtin.List using (List; _∷_; [])
open import Agda.Builtin.Nat using (Nat; suc; zero; _<_; _-_)
open import Agda.Builtin.String
open import Agda.Builtin.Bool using (Bool; true; false)

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

index : List String → Nat → String
index [] _ = "?"
index (x ∷ xs) zero = x
index (x ∷ xs) (suc n) = index xs n

div : Nat → Nat → Nat
{-# TERMINATING #-}
div n m with n < m
... | true = zero
... | false = suc (div (n - m) m)

mod : Nat → Nat → Nat
{-# TERMINATING #-}
mod n m with n < m
... | true = n
... | false = mod (n - m) m

digits : List String
digits = "0" ∷ "1" ∷ "2" ∷ "3" ∷ "4" ∷ "5" ∷ "6" ∷ "7" ∷ "8" ∷ "9" ∷ []

mutual
  showNat : Nat → String
  showNat zero = "0"
  showNat n = showNat' n

  {-# TERMINATING #-}
  showNat' : Nat → String
  showNat' zero = "0"
  showNat' n with n < 10
  ... | true = index digits n
  ... | false = _++_ (showNat' (div n 10)) (index digits (mod n 10))

open import Plan.CIM.PandocAST using (Block)
open Plan.CIM.PandocAST
open import Plan.CIM.PandocShowInline using (showInlines)

mutual
  showBlock : Block → String
  showBlock (Para xs) = _++_ "Para " (showInlines xs)
  showBlock (Plain xs) = _++_ "Plain " (showInlines xs)
  showBlock (Header n xs) = _++_ (_++_ (_++_ "Header " (showNat n)) " ") (showInlines xs)
  showBlock (CodeBlock s) = _++_ "CodeBlock " s
  showBlock (RawBlock s) = _++_ "RawBlock " s
  showBlock (BlockQuote bs) = _++_ (_++_ "BlockQuote [" (showBlocks bs)) "]"
  showBlock (OrderedList xs) = _++_ (_++_ "OrderedList [" (showBlockLists xs)) "]"
  showBlock (BulletList xs) = _++_ (_++_ "BulletList [" (showBlockLists xs)) "]"
  showBlock HorizontalRule = "HorizontalRule"
  showBlock (Table s) = _++_ "Table " s
  showBlock Null = "Null"

  showBlocks : List Block → String
  showBlocks [] = ""
  showBlocks (b ∷ []) = showBlock b
  showBlocks (b ∷ bs) = _++_ (_++_ (showBlock b) ", ") (showBlocks bs)

  showBlockLists : List (List Block) → String
  showBlockLists [] = ""
  showBlockLists (xs ∷ []) = _++_ (_++_ "[" (showBlocks xs)) "]"
  showBlockLists (xs ∷ ys) = _++_ (_++_ (_++_ "[" (showBlocks xs)) "], ") (showBlockLists ys)

{-# OPTIONS --without-K #-}

open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)

-- | Common string manipulation utilities
-- Consolidates string operations used across the codebase
-- FFI primitives provided as module parameters - no postulates required
module Core.Strings
  (primNatToString : Nat → String)
  where

-- ==========================================================
-- String Concatenation
-- ==========================================================

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- ==========================================================
-- String Joining
-- ==========================================================

-- Join list of strings with separator
intercalate : String → List String → String
intercalate sep [] = ""
intercalate sep (x ∷ []) = x
intercalate sep (x ∷ xs) = x ++ (sep ++ intercalate sep xs)

-- Join list of strings with no separator
concat : List String → String
concat = intercalate ""

-- Join with newlines
unlines : List String → String
unlines = intercalate "\n"

-- Join with spaces
unwords : List String → String
unwords = intercalate " "

-- ==========================================================
-- Nat to String Conversion
-- ==========================================================

-- Convert natural number to string (using provided primitive)
natToString : Nat → String
natToString zero = "0"
natToString n = primNatToString n

-- Alias for compatibility
showNat : Nat → String
showNat = natToString

-- ==========================================================
-- String Quoting
-- ==========================================================

-- Quote string for JSON
quoteJSON : String → String
quoteJSON s = "\"" ++ (s ++ "\"")

-- Quote string for shell
quoteShell : String → String
quoteShell s = "'" ++ (s ++ "'")

-- ==========================================================
-- String Prefixing
-- ==========================================================

-- Add prefix to each string in list
mapWithPrefix : String → List String → List String
mapWithPrefix prefix [] = []
mapWithPrefix prefix (x ∷ xs) = (prefix ++ x) ∷ mapWithPrefix prefix xs

-- Indent each line by given number of spaces
indent : Nat → List String → List String
indent zero lines = lines
indent (suc n) lines = mapWithPrefix " " (indent n lines)

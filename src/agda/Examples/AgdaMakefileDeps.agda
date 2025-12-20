module Examples.AgdaMakefileDeps where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.Nat using (Nat; zero; suc; _==_)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.IO using (IO)
open import Core.Utils using (length; if_then_else_)

postulate
  words : String → List String
  readFileLines : String → IO (List String)
  primStringToList : String → List Char
  primStringLength : String → Nat
  primStringSlice : Nat → Nat → String → String
  primStringEquality : String → String → Bool
  primStringWords : String → List String
  dummyIO : IO String

{-# COMPILE GHC words = words #-}
{-# COMPILE GHC readFileLines = \path -> fmap lines (readFile path) #-}

-- Placeholder: Agda file scanning (skeleton for now)
listAgdaFiles : String → List String
listAgdaFiles _ = []

-- Placeholder: Make rule generation (skeleton for now)
{-# TERMINATING #-}
makeRuleForFile : String → IO String
makeRuleForFile _ = dummyIO

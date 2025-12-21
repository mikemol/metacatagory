{-# OPTIONS --without-K #-}

module Examples.AgdaFileScanFFI where

open import Agda.Builtin.List using (List; [])
open import Agda.Builtin.String using (String)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)

-- FFI stub: list all .agda files under src/agda recursively
postulate
  listAgdaFiles : String → IO (List String)
{-# COMPILE GHC listAgdaFiles = listAgdaFiles #-}

-- Usage note:
-- Ensure listDirectoryRecursiveAgda.hs is compiled and available as a Haskell module exposing 'listAgdaFiles'.
-- The Agda FFI will call this function to list all .agda files under the given root.

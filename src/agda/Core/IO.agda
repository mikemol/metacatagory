{-# OPTIONS --without-K #-}

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)

-- | Common IO operations and monadic combinators
-- Consolidates IO patterns used across executable Agda modules
-- FFI primitives provided as module parameters - no postulates required
module Core.IO
  -- FFI Primitives as module parameters
  (_>>=_ : {A B : Set} → IO A → (A → IO B) → IO B)
  (_>>_ : {A B : Set} → IO A → IO B → IO B)
  (return : {A : Set} → A → IO A)
  (primWriteFile : String → String → IO ⊤)
  (primReadFile : String → IO String)
  (primAppendFile : String → String → IO ⊤)
  (primPutStr : String → IO ⊤)
  (primPutStrLn : String → IO ⊤)
  (primGetLine : IO String)
  where

-- ==========================================================
-- File IO Operations (using provided primitives)
-- ==========================================================

writeFile : String → String → IO ⊤
writeFile = primWriteFile

readFile : String → IO String
readFile = primReadFile

appendFile : String → String → IO ⊤
appendFile = primAppendFile

-- ==========================================================
-- Console IO Operations (using provided primitives)
-- ==========================================================

putStr : String → IO ⊤
putStr = primPutStr

putStrLn : String → IO ⊤
putStrLn = primPutStrLn

getLine : IO String
getLine = primGetLine

-- ==========================================================
-- List Operations with IO
-- ==========================================================

-- Map IO operation over list (for side effects)
mapM_ : {A : Set} → (A → IO ⊤) → List A → IO ⊤
mapM_ f [] = return tt
mapM_ f (x ∷ xs) = f x >> mapM_ f xs

-- Map IO operation over list (collecting results)
mapM : {A B : Set} → (A → IO B) → List A → IO (List B)
mapM f [] = return []
mapM f (x ∷ xs) = f x >>= λ y → mapM f xs >>= λ ys → return (y ∷ ys)

-- Sequence list of IO actions
sequence_ : {A : Set} → List (IO A) → IO ⊤
sequence_ [] = return tt
sequence_ (x ∷ xs) = x >> sequence_ xs

-- Execute action when condition is true
when : Bool → IO ⊤ → IO ⊤
when true action = action
when false action = return tt

-- Execute action unless condition is true
unless : Bool → IO ⊤ → IO ⊤
unless true action = return tt
unless false action = action

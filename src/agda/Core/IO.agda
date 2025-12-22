{-# OPTIONS --without-K #-}

-- | Common IO operations and monadic combinators
-- Consolidates IO patterns used across executable Agda modules
module Core.IO where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Bool using (Bool; true; false)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
#-}

-- ==========================================================
-- IO Monad Combinators
-- ==========================================================

postulate
  _>>=_ : {A B : Set} → IO A → (A → IO B) → IO B
  _>>_ : {A B : Set} → IO A → IO B → IO B
  return : {A : Set} → A → IO A

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC _>>_ = \_ _ -> (>>) #-}
{-# COMPILE GHC return = \_ -> pure #-}

-- ==========================================================
-- File IO Operations
-- ==========================================================

postulate
  writeFile : String → String → IO ⊤
  readFile : String → IO String
  appendFile : String → String → IO ⊤

{-# FOREIGN GHC
writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = TIO.writeFile (T.unpack path) content

readFileAdapter :: T.Text -> IO T.Text
readFileAdapter path = TIO.readFile (T.unpack path)

appendFileAdapter :: T.Text -> T.Text -> IO ()
appendFileAdapter path content = TIO.appendFile (T.unpack path) content
#-}

{-# COMPILE GHC writeFile = writeFileAdapter #-}
{-# COMPILE GHC readFile = readFileAdapter #-}
{-# COMPILE GHC appendFile = appendFileAdapter #-}

-- ==========================================================
-- Console IO Operations
-- ==========================================================

postulate
  putStr : String → IO ⊤
  putStrLn : String → IO ⊤
  getLine : IO String

{-# COMPILE GHC putStr = putStr . T.unpack #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}
{-# COMPILE GHC getLine = fmap T.pack getLine #-}

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

-- Import Bool from Core.Phase or Agda.Builtin.Bool
open import Agda.Builtin.Bool using (Bool; true; false)

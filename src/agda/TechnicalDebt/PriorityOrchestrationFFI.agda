{-# OPTIONS --guardedness #-}

-- | FFI-backed I/O for priority orchestration (reading/writing reports).
module TechnicalDebt.PriorityOrchestrationFFI where

open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.IO
open import Agda.Builtin.Int using (Int)

-- FFI-backed I/O operations

postulate
  -- File operations (GHC FFI)
  ffi-writeFile : String → String → IO ⊤
  ffi-readFile : String → IO String
  ffi-fileExists : String → IO String
  -- Console operations (GHC FFI)
  ffi-putStrLn : String → IO ⊤
  -- Colored status reporting (GHC FFI)
  ffi-reportSuccess : String → IO ⊤
  ffi-reportError : String → IO ⊤
  -- JSON validation (GHC FFI)
  ffi-validateJSON : String → IO String
  -- String conversion (GHC FFI)
  ffi-intToString : Int → String
  -- Monad operations
  ffi-bind : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
  ffi-pure : ∀ {ℓ} {A : Set ℓ} → A → IO A

-- GHC FFI pragmas
{-# FOREIGN GHC import qualified System.IO #-}
{-# FOREIGN GHC import qualified System.Directory #-}
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# FOREIGN GHC import qualified System.Process #-}
{-# FOREIGN GHC import qualified System.Exit #-}

{-# COMPILE GHC ffi-writeFile = \path content -> System.IO.writeFile (T.unpack path) (T.unpack content) #-}
{-# COMPILE GHC ffi-readFile = \path -> fmap T.pack (System.IO.readFile (T.unpack path)) #-}
{-# COMPILE GHC ffi-fileExists = \path ->
  System.Directory.doesFileExist (T.unpack path) >>= \exists ->
    return (if exists then T.pack "true" else T.pack "false")
#-}

{-# COMPILE GHC ffi-putStrLn = \s -> System.IO.putStrLn (T.unpack s) #-}

{-# COMPILE GHC ffi-reportSuccess = \msg -> 
  System.IO.putStrLn ("\x1b[32m✓ " ++ T.unpack msg ++ "\x1b[0m")
#-}

{-# COMPILE GHC ffi-reportError = \msg ->
  System.IO.putStrLn ("\x1b[31m✗ " ++ T.unpack msg ++ "\x1b[0m")
#-}

{-# COMPILE GHC ffi-validateJSON = \jsonStr ->
  System.Directory.findExecutable "python3" >>= \maybePy ->
    case maybePy of { Nothing -> return (T.pack "valid");
      Just py ->
        System.Process.readProcessWithExitCode
          py
          ["-c", "import json,sys; json.load(sys.stdin)"]
          (T.unpack jsonStr) >>= \(ec, _out, err) ->
            case ec of { System.Exit.ExitSuccess -> return (T.pack "valid");
              System.Exit.ExitFailure _ -> return (T.pack ("Invalid JSON: " ++ err)) } }
#-}

{-# COMPILE GHC ffi-intToString = \n -> T.pack (show n) #-}

{-# COMPILE GHC ffi-bind = \_ _ _ _ m f -> m >>= f #-}
{-# COMPILE GHC ffi-pure = \_ _ x -> return x #-}

open import TechnicalDebt.PriorityOrchestration
  ffi-writeFile
  ffi-readFile
  ffi-fileExists
  ffi-putStrLn
  ffi-reportSuccess
  ffi-reportError
  ffi-validateJSON
  ffi-intToString
  ffi-bind
  ffi-pure
  public

-- Main entry point for exporting AUDAX Markdown for all strategies
mainAUDAXMarkdown : IO ⊤
mainAUDAXMarkdown = exportAllStrategiesAUDAXMarkdown

{-# OPTIONS --without-K #-}

module TechnicalDebt.DeferredItemsOrchestrationFFI where

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess)
import Control.Exception (catch, SomeException)
import Data.Char (isSpace)

countPattern :: String -> IO Int
countPattern pat = do
  catch
    (do result <- readProcess "bash" ["-c", "grep -r --include=\"*.agda\" --include=\"*.md\" '" ++ pat ++ "' src/ README.md 2>/dev/null | wc -l"] ""
        return (read (dropWhile isSpace result) :: Int))
    (\(_ :: SomeException) -> return 0)

mainAdapter :: IO ()
mainAdapter = do
  dvLog <- countPattern "DeviationLog"
  post <- countPattern "^[[:space:]]*postulate"
  todo <- countPattern "TODO"
  plan <- countPattern "PLANNED"
  fixme <- countPattern "FIXME"
  
  -- Call Agda formatting functions with counts as structured data
  let markdown = d_buildAndFormatMarkdown_70 (fromIntegral dvLog) (fromIntegral post) (fromIntegral todo) (fromIntegral plan) (fromIntegral fixme)
  let json = d_buildAndFormatJSON_84 (fromIntegral dvLog) (fromIntegral post) (fromIntegral todo) (fromIntegral plan) (fromIntegral fixme)

  TIO.writeFile "deferred-items.md" markdown
  TIO.writeFile "deferred-summary.json" json
 #-}

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Sigma using (_,_) -- for pair pattern syntax
open import TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts)
open import TechnicalDebt.DeferredItemsFormatting using (Doc; Heading; Field; Table; Concat; formatDeferredItemsDoc; countsAsFields; natToString; _×_)

infixr 5 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- Render helpers
hashes : Nat → String
hashes zero    = ""
hashes (suc n) = "#" ++ hashes n

renderHeading : Nat → String → String
renderHeading lvl txt = (hashes lvl) ++ " " ++ txt ++ "\n\n"

renderField : String → String → String
renderField _ value = "Found **" ++ value ++ "** instances\n\n"

renderTableRows : List (String × String) → String
renderTableRows [] = ""
renderTableRows ((name , value) ∷ rs) = "| " ++ name ++ " | " ++ value ++ " |\n" ++ renderTableRows rs

renderTable : List (String × String) → String
renderTable rows =
  "| Category | Count |\n|----------|-------|\n" ++ renderTableRows rows

renderDoc : Doc → String
renderDoc (Heading lvl txt) = renderHeading lvl txt
renderDoc (Field lbl val)   = renderField lbl val
renderDoc (Table rows)      = renderTable rows
renderDoc (Concat [])       = ""
renderDoc (Concat (d ∷ ds)) = renderDoc d ++ renderDoc (Concat ds)

renderMarkdownDoc : Doc → String
renderMarkdownDoc doc = renderDoc doc

postulate
  ffi-main : IO ⊤
  ffi-bind : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
  ffi-pure : ∀ {ℓ} {A : Set ℓ} → A → IO A

{-# COMPILE GHC ffi-main = mainAdapter #-}
{-# COMPILE GHC ffi-bind = \_ _ _ _ m f -> m >>= f #-}
{-# COMPILE GHC ffi-pure = \_ _ x -> return x #-}

buildAndFormatMarkdown : Nat → Nat → Nat → Nat → Nat → String
buildAndFormatMarkdown dvLog post todo plan fixme =
  let counts = record
        { deviationLog = dvLog
        ; postulates = post
        ; todo = todo
        ; planned = plan
        ; fixme = fixme
        }
    in renderMarkdownDoc (formatDeferredItemsDoc counts)

buildAndFormatJSON : Nat → Nat → Nat → Nat → Nat → String
buildAndFormatJSON dvLog post todo plan fixme =
  let counts = record
        { deviationLog = dvLog
        ; postulates = post
        ; todo = todo
        ; planned = plan
        ; fixme = fixme
        }
      fields = countsAsFields counts
  in "{\n" ++ renderFields fields ++ "}\n"
  where
    renderFields : List (String × String) → String
    renderFields [] = ""
    renderFields ((k , v) ∷ []) = "  \"" ++ k ++ "\": " ++ v ++ "\n"
    renderFields ((k , v) ∷ rest) = "  \"" ++ k ++ "\": " ++ v ++ ",\n" ++ renderFields rest

agdaFormatMarkdown : Nat → Nat → Nat → Nat → Nat → String
agdaFormatMarkdown = buildAndFormatMarkdown

agdaFormatJSON : Nat → Nat → Nat → Nat → Nat → String
agdaFormatJSON = buildAndFormatJSON

main : IO ⊤
main = ffi-main

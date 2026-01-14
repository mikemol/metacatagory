{-# OPTIONS --without-K #-}

-- | FFI helpers to orchestrate deferred-item detection via external tools.
module TechnicalDebt.DeferredItemsOrchestrationFFI where

{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# FOREIGN GHC import qualified Data.Text.IO as TIO #-}
{-# FOREIGN GHC import System.Process (readProcess) #-}
{-# FOREIGN GHC import Control.Exception (catch, SomeException) #-}
{-# FOREIGN GHC import Data.Char (isSpace) #-}


open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.String using (String; primStringAppend)


postulate
  ffi-main : IO ⊤
  ffi-bind : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
{-# COMPILE GHC ffi-bind = \_ _ _ _ m f -> m >>= f #-}



postulate
  countPattern : String → IO Nat
{-# COMPILE GHC countPattern = \pat ->
  let pat' = Data.Text.unpack pat in
  Control.Exception.catch
    (System.Process.readProcess "bash" ["-c", "grep -r --include=\"*.agda\" --include=\"*.md\" '" ++ pat' ++ "' src/ README.md 2>/dev/null | wc -l"] "")
    (\(_ :: Control.Exception.SomeException) -> return "0")
  >>= \result -> return (fromIntegral (read (dropWhile Data.Char.isSpace result) :: Int))
  #-}


open import Agda.Builtin.String using (String; primStringAppend)

postulate
  writeFile# : String → String → IO ⊤
{-# COMPILE GHC writeFile# = \path content ->
  let path' = Data.Text.unpack path;
      content' = Data.Text.unpack content
  in TIO.writeFile path' (T.pack content') >> return ()
  #-}
writeFile = writeFile#

-- ...existing code...



open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Phase using (_×_; fst; snd)
open import TechnicalDebt.DeferredItemsDetection using (DeferredItemCounts)
open import TechnicalDebt.DeferredItemsFormatting using (AUDAXDoc; AUDAXBlock; AUDAXInline; formatDeferredItemsAUDAXDoc; countsAsFields; natToString; ListLike; Header; Para; CodeBlock; BlockQuote; ListBlock; Table; Field; Raw; Null)
open TechnicalDebt.DeferredItemsFormatting using (Str; Emph; Strong; Code; Link; Image; Space; Break)

infixr 5 _++_
_++_ : String → String → String
_++_ = primStringAppend


audaxHashes : Nat → String
audaxHashes zero    = ""
audaxHashes (suc n) = "#" ++ audaxHashes n




{-# TERMINATING #-}
mutual
  renderFields : List (String × String) → String
  renderFields [] = ""
  renderFields (x ∷ []) = "  \"" ++ fst x ++ "\": " ++ snd x ++ "\n"
  renderFields (x ∷ rest) = "  \"" ++ fst x ++ "\": " ++ snd x ++ ",\n" ++ renderFields rest

  audaxRenderDoc : AUDAXDoc → String
  audaxRenderDoc doc = audaxRenderBlocks (ListLike.items (AUDAXDoc.blocks doc))

  -- Local map for List (Agda.Builtin.List does not export map)
  map : ∀ {a b} {A : Set a} {B : Set b} → (A → B) → List A → List B
  map f [] = []
  map f (x ∷ xs) = f x ∷ map f xs

  audaxRenderInline : AUDAXInline → String
  audaxRenderInline (Str s) = s
  audaxRenderInline (Emph xs) = "*" ++ audaxRenderInlines (ListLike.items xs) ++ "*"
  audaxRenderInline (Strong xs) = "**" ++ audaxRenderInlines (ListLike.items xs) ++ "**"
  audaxRenderInline (Code s) = "`" ++ s ++ "`"
  audaxRenderInline (Link xs url) = "[" ++ audaxRenderInlines (ListLike.items xs) ++ "](" ++ url ++ ")"
  audaxRenderInline (Image xs url) = "![" ++ audaxRenderInlines (ListLike.items xs) ++ "](" ++ url ++ ")"
  audaxRenderInline Space = " "
  audaxRenderInline Break = "  \n"

  audaxRenderInlines : List AUDAXInline → String
  audaxRenderInlines [] = ""
  audaxRenderInlines (x ∷ xs) = audaxRenderInline x ++ audaxRenderInlines xs

  audaxRenderBlock : AUDAXBlock → String
  audaxRenderBlock (Header n xs)       = audaxHashes n ++ " " ++ audaxRenderInlines (ListLike.items xs) ++ "\n\n"
  audaxRenderBlock (Para xs)           = audaxRenderInlines (ListLike.items xs) ++ "\n\n"
  audaxRenderBlock (CodeBlock s)       = "```\n" ++ s ++ "\n```\n\n"
  audaxRenderBlock (BlockQuote bs)     = "> " ++ audaxRenderBlocks (ListLike.items bs)
  audaxRenderBlock (ListBlock bss)     = audaxRenderListBlocks (map ListLike.items (ListLike.items bss))
  audaxRenderBlock (Table header rows) = audaxRenderTable (ListLike.items header) (map ListLike.items (ListLike.items rows))
  audaxRenderBlock (Field lbl val)     = lbl ++ ": " ++ val ++ "\n\n"
  audaxRenderBlock (Raw s)             = s ++ "\n\n"
  audaxRenderBlock Null                = ""

  audaxRenderBlocks : List AUDAXBlock → String
  audaxRenderBlocks [] = ""
  audaxRenderBlocks (b ∷ bs) = audaxRenderBlock b ++ audaxRenderBlocks bs

  audaxRenderListBlocks : List (List AUDAXBlock) → String
  audaxRenderListBlocks [] = ""
  audaxRenderListBlocks (bs ∷ rest) = "- " ++ audaxRenderBlocks bs ++ audaxRenderListBlocks rest

  audaxRenderTable : List String → List (List AUDAXInline) → String
  audaxRenderTable header rows =
    let headerRow = "| " ++ audaxJoin " | " header ++ " |\n"
        sepRow = "|" ++ audaxJoin "|" (map (λ _ → "---") header) ++ "|\n"
        bodyRows = audaxRenderTableRows rows
    in headerRow ++ sepRow ++ bodyRows

  audaxRenderTableRows : List (List AUDAXInline) → String
  audaxRenderTableRows [] = ""
  audaxRenderTableRows (row ∷ rest) = "| " ++ audaxJoin " | " (map audaxRenderInline row) ++ " |\n" ++ audaxRenderTableRows rest

  audaxJoin : String → List String → String
  audaxJoin _ [] = ""
  audaxJoin _ (x ∷ []) = x
  audaxJoin sep (x ∷ xs) = x ++ sep ++ audaxJoin sep xs





buildAndFormatMarkdown : Nat → Nat → Nat → Nat → Nat → String
buildAndFormatMarkdown dvLog post todo plan fixme =
  let counts = record
        { deviationLog = dvLog
        ; postulates = post
        ; todo = todo
        ; planned = plan
        ; fixme = fixme
        }
      audaxDoc = formatDeferredItemsAUDAXDoc counts
  in audaxRenderDoc audaxDoc


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


agdaFormatMarkdown : Nat → Nat → Nat → Nat → Nat → String
agdaFormatMarkdown = buildAndFormatMarkdown

agdaFormatJSON : Nat → Nat → Nat → Nat → Nat → String
agdaFormatJSON = buildAndFormatJSON

main : IO ⊤
main =
  ffi-bind (countPattern "DeviationLog") (\dvLog →
  ffi-bind (countPattern "^[[:space:]]*postulate") (\post →
  ffi-bind (countPattern "TODO") (\todo →
  ffi-bind (countPattern "PLANNED") (\plan →
  ffi-bind (countPattern "FIXME") (\fixme →
    let markdown = buildAndFormatMarkdown dvLog post todo plan fixme
        json = buildAndFormatJSON dvLog post todo plan fixme
    in ffi-bind (writeFile "build/reports/deferred-items.md" markdown) (\_ →
       ffi-bind (writeFile "docs/status/deferred-items.md" markdown) (\_ →
         writeFile "build/reports/deferred-summary.json" json)))))))

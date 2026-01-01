{-# OPTIONS --without-K --cubical --guardedness #-}

-- | Normalize markdown text for consistent downstream processing.
module Plan.CIM.MarkdownNormalize where

open import Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)
open import Agda.Builtin.List   using (List; []; _∷_)
open import Agda.Builtin.Unit   using (⊤; tt)
open import Agda.Builtin.IO     using (IO)
open import Agda.Builtin.Bool   using (Bool; true; false)
open import Agda.Builtin.Char   using (Char)
open import Agda.Builtin.Nat    using (Nat; zero; suc)
open import Plan.CIM.PandocAST
open import Plan.CIM.MarkdownParse

------------------------------------------------------------------------
-- Simple string utilities
------------------------------------------------------------------------

infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

reverse : ∀ {ℓ} {A : Set ℓ} → List A → List A
reverse = revAcc []
  where
    revAcc : ∀ {ℓ} {A : Set ℓ} → List A → List A → List A
    revAcc acc []       = acc
    revAcc acc (x ∷ xs) = revAcc (x ∷ acc) xs

joinLines : List String → String
joinLines [] = ""
joinLines (x ∷ []) = x
joinLines (x ∷ xs) = primStringAppend x (primStringAppend "\n" (joinLines xs))

isNewline : Char → Bool
isNewline '\n' = true
isNewline _    = false

splitLines : String → List String
splitLines s = go [] [] (primStringToList s)
  where
    go : List Char → List String → List Char → List String
    go acc rev []       = primStringFromList (reverse acc) ∷ reverse rev
    go acc rev (c ∷ cs) with isNewline c
    ... | true  = go [] (primStringFromList (reverse acc) ∷ rev) cs
    ... | false = go (c ∷ acc) rev cs

prefixLines : String → String → String
prefixLines pref s = joinLines (map (primStringAppend pref) (splitLines s))

renderInlines : List MdInline → String
renderInlines [] = ""
renderInlines (x ∷ xs) = renderInline x ++ renderInlines xs
  where
    renderInline : MdInline → String
    renderInline (MdStr s)      = s
    renderInline (MdEmph ys)    = "_" ++ renderInlines ys ++ "_"
    renderInline (MdStrong ys)  = "**" ++ renderInlines ys ++ "**"
    renderInline (MdCode s)     = "`" ++ s ++ "`"
    renderInline MdSpace        = " "
    renderInline MdBreak        = "\n"
    renderInline MdEOL          = "\n"
    renderInline (MdLink ys u)  = "[" ++ renderInlines ys ++ "](" ++ u ++ ")"
    renderInline (MdImage ys u) = "![" ++ renderInlines ys ++ "](" ++ u ++ ")"

repeatHash : Nat → String
repeatHash zero    = ""
repeatHash (suc n) = "#" ++ repeatHash n

mutual
  renderBlocks : List MdBlock → String
  renderBlocks bs = renderBlocksWith "" bs

  renderBlocksWith : String → List MdBlock → String
  renderBlocksWith _ [] = ""
  renderBlocksWith indent (b ∷ bs) = renderBlockWith indent b ++ delim bs ++ renderBlocksWith indent bs

  delim : List MdBlock → String
  delim []       = ""
  delim (_ ∷ _)  = "\n\n"

  renderBlockWith : String → MdBlock → String
  renderBlockWith indent (MdPara ys)     = renderInlines ys
  renderBlockWith indent (MdHeader n ys) = repeatHash n ++ " " ++ renderInlines ys
  renderBlockWith indent (MdCodeBlock s) = "```text\n" ++ s ++ "\n```"
  renderBlockWith indent (MdList items)  = renderList indent items
  renderBlockWith indent (MdOrderedList items) = renderOrdered indent items
  renderBlockWith indent (MdQuote qs)    = renderQuote indent qs
  renderBlockWith indent MdRule          = "---"
  renderBlockWith indent (MdRaw s)       = s
  renderBlockWith _      MdNull          = ""
  renderBlockWith _      MdEOB           = ""
  renderBlockWith _      MdSBB           = ""

  renderList : String → List (List MdBlock) → String
  renderList indent [] = ""
  renderList indent (item ∷ xs) = indent ++ "* " ++ renderItem indent item ++ listTail indent xs

  renderOrdered : String → List (List MdBlock) → String
  renderOrdered indent [] = ""
  renderOrdered indent (item ∷ xs) = indent ++ "1. " ++ renderItem indent item ++ listTail indent xs

  renderItem : String → List MdBlock → String
  renderItem _ [] = ""
  renderItem indent (ib ∷ ibs) = renderBlockWith (indent ++ "  ") ib ++ itemTail indent ibs

  itemTail : String → List MdBlock → String
  itemTail _ [] = ""
  itemTail indent (ib ∷ ibs) = "\n" ++ indent ++ "  " ++ renderBlockWith (indent ++ "  ") ib ++ itemTail indent ibs

  listTail : String → List (List MdBlock) → String
  listTail _ [] = ""
  listTail indent (item ∷ xs) = "\n" ++ renderList indent (item ∷ xs)

  renderQuote : String → List MdBlock → String
  renderQuote indent qs = prefixLines (indent ++ "> ") (renderBlocksWith indent qs)

renderDoc : MarkdownDoc → String
renderDoc doc = renderBlocks (MarkdownDoc.blocks doc) ++ "\n"

------------------------------------------------------------------------
-- IO bindings
------------------------------------------------------------------------

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

readFileAdapter :: T.Text -> IO T.Text
readFileAdapter path = TIO.readFile (T.unpack path)

writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = TIO.writeFile (T.unpack path) content

putStrLnAdapter :: T.Text -> IO ()
putStrLnAdapter = TIO.putStrLn

getArgsAdapter :: IO [T.Text]
getArgsAdapter = map T.pack <$> getArgs
#-}

postulate
  _>>=_    : {A B : Set} → IO A → (A → IO B) → IO B
  return   : {A : Set} → A → IO A
  readFile : String → IO String
  writeFile : String → String → IO ⊤
  putStrLn : String → IO ⊤
  getArgs  : IO (List String)

{-# COMPILE GHC _>>=_ = \_ _ -> (>>=) #-}
{-# COMPILE GHC return = \_ -> return #-}
{-# COMPILE GHC readFile = readFileAdapter #-}
{-# COMPILE GHC writeFile = writeFileAdapter #-}
{-# COMPILE GHC putStrLn = putStrLnAdapter #-}
{-# COMPILE GHC getArgs = getArgsAdapter #-}

------------------------------------------------------------------------
-- Normalization logic
------------------------------------------------------------------------

normalizeFile : String → IO ⊤
normalizeFile path =
  readFile path >>= λ contents →
  let lines = splitLines contents
      rendered = renderDoc (parseLinesToMarkdown lines)
  in writeFile path rendered

processArgs : List String → IO ⊤
processArgs [] = return tt
processArgs (p ∷ ps) =
  putStrLn ("markdown-normalize: processing " ++ p) >>= λ _ →
  normalizeFile p >>= λ _ → processArgs ps

main : IO ⊤
main = getArgs >>= processArgs

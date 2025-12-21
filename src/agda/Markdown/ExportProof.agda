{-# OPTIONS --without-K #-}

module Markdown.ExportProof where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Markdown.Normalization
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Core.Utils

{-# FOREIGN GHC import qualified Data.Text as T #-}

postulate
  putStrLn : String → IO ⊤

{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

_++_ : String → String → String
_++_ = primStringAppend

infixr 20 _++_

-- Simple nat-to-string conversion
natToString : Nat → String
natToString zero = "0"
natToString (suc n) = primStringAppend "s(" (primStringAppend (natToString n) ")")

toString : Nat → String
toString = natToString

-- Example: Export a proof trace as a JSON string

showPandocBlock : PandocBlock → String
showPandocBlock (Heading n s) = "Heading(" ++ toString n ++ "," ++ s ++ ")"
showPandocBlock (Para s) = "Para(" ++ s ++ ")"
showPandocBlock (BulletList xs) = "BulletList(...)"
showPandocBlock (OrderedList xs) = "OrderedList(...)"
showPandocBlock (CodeBlock s) = "CodeBlock(" ++ s ++ ")"
showPandocBlock HorizontalRule = "HorizontalRule"
showPandocBlock (BlockQuote xs) = "BlockQuote(...)"
showPandocBlock (RawBlock s) = "RawBlock(" ++ s ++ ")"

showMarkdownBlock : MarkdownBlock → String
showMarkdownBlock (AtxHeading n s) = "AtxHeading(" ++ toString n ++ "," ++ s ++ ")"
showMarkdownBlock (Paragraph s) = "Paragraph(" ++ s ++ ")"
showMarkdownBlock (ListItem xs) = "ListItem(...)"
showMarkdownBlock (UnorderedList xs) = "List(...)"
showMarkdownBlock (OrderedList xs) = "OrderedList(...)"
showMarkdownBlock (CodeBlock s) = "CodeBlock(" ++ s ++ ")"
showMarkdownBlock HorizontalRule = "HorizontalRule"
showMarkdownBlock (BlockQuote xs) = "BlockQuote(...)"

proofStepToJSON : ProofStep → String
proofStepToJSON step =
  "{\"rule\": \"" ++ ProofStep.rule step ++ "\", " ++
  "\"input\": \"" ++ showPandocBlock (ProofStep.input step) ++ "\", " ++
  "\"output\": \"" ++ showMarkdownBlock (ProofStep.output step) ++ "\"}"

-- Export the full proof trace as a JSON array
exportProofTrace : List ProofStep → String
exportProofTrace steps =
  "[" ++ intercalate "," (map proofStepToJSON steps) ++ "]"

-- Example IO action to write proof trace to file
writeProofTrace : List ProofStep → String → IO ⊤
writeProofTrace steps filename =
  putStrLn (exportProofTrace steps)
  -- For real file output, use Agda's FFI or a Haskell backend

-- Usage: Call writeProofTrace Proof "proof.json" in your normalization pipeline

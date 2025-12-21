{-# OPTIONS --without-K #-}

module Markdown.Normalization where

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Utils using (map)

-- Pandoc AST (simplified)
data PandocBlock : Set where
  Heading : Nat → String → PandocBlock
  Para    : String → PandocBlock
  BulletList : List (List PandocBlock) → PandocBlock
  OrderedList : List (List PandocBlock) → PandocBlock
  CodeBlock : String → PandocBlock
  HorizontalRule : PandocBlock
  BlockQuote : List PandocBlock → PandocBlock
  RawBlock : String → PandocBlock
  -- ... add more as needed ...

-- Strict Markdown AST (lint-compliant)
data MarkdownBlock : Set where
  AtxHeading : Nat → String → MarkdownBlock
  Paragraph  : String → MarkdownBlock
  ListItem   : List MarkdownBlock → MarkdownBlock
  UnorderedList : List (List MarkdownBlock) → MarkdownBlock
  OrderedList : List (List MarkdownBlock) → MarkdownBlock
  CodeBlock  : String → MarkdownBlock
  HorizontalRule : MarkdownBlock
  BlockQuote : List MarkdownBlock → MarkdownBlock
  -- ... add more as needed ...

-- Transformation rules (as functions)
{-# TERMINATING #-}
normalizeBlock : PandocBlock → MarkdownBlock
normalizeBlock (Heading n s) = AtxHeading n s
normalizeBlock (Para s) = Paragraph s
normalizeBlock (BulletList xs) = UnorderedList (map (map normalizeBlock) xs)
normalizeBlock (OrderedList xs) = OrderedList (map (map normalizeBlock) xs)
normalizeBlock (CodeBlock s) = CodeBlock s
normalizeBlock HorizontalRule = HorizontalRule
normalizeBlock (BlockQuote xs) = BlockQuote (map normalizeBlock xs)
normalizeBlock (RawBlock s) = Paragraph s

-- Proof object: a trace of transformations
record ProofStep : Set where
  constructor mkProofStep
  field
    rule   : String
    input  : PandocBlock
    output : MarkdownBlock

Proof : List ProofStep
Proof = [] -- to be constructed by normalization

-- Example: normalization function with proof trace
data NormResult : Set where
  norm : MarkdownBlock → List ProofStep → NormResult

normalizeWithProof : PandocBlock → NormResult
normalizeWithProof b =
  let out = normalizeBlock b in
  norm out (mkProofStep "normalizeBlock" b out ∷ [])

-- Extend with more rules, block types, and proof steps as needed.

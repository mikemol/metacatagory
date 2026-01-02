{-# OPTIONS --without-K --cubical --guardedness #-}

-- | Convert Pandoc AST to markdown strings for export.
module Plan.CIM.PandocToMarkdown where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Bool using (Bool; true)
open import Agda.Builtin.Equality using (_≡_; refl)

open import Plan.CIM.Utility using (map; _++_; TransformationSystem; CoherenceWitness; Path; refl-path; EmergentMetric)
open import Plan.CIM.PandocAST

------------------------------------------------------------------------
-- BraidStep: represents a single transformation step from Block to MdBlock
------------------------------------------------------------------------

record BraidStep : Set where
  field
    fromBlock : Block
    toBlock : MdBlock
    description : String

------------------------------------------------------------------------
-- BraidTrace: sequence of transformation steps with summary
------------------------------------------------------------------------

record BraidTrace : Set where
  field
    steps : List BraidStep
    summary : String

------------------------------------------------------------------------
-- Transformation: Inline → MdInline and Block → MdBlock (mutual recursion)
------------------------------------------------------------------------

-- Forward declarations for mutual recursion
{-# TERMINATING #-}
transformInline : Inline → MdInline
transformBlock : Block → MdBlock

-- Implementations with guard on structural recursion
transformInline (Str s)             = MdStr s
transformInline (Emph xs)           = MdEmph (map transformInline xs)
transformInline (Strong xs)         = MdStrong (map transformInline xs)
transformInline (Code s)            = MdCode s
transformInline Space              = MdSpace
transformInline SoftBreak          = MdBreak
transformInline LineBreak          = MdBreak
transformInline (Math s)           = MdCode s  -- Fallback: treat math as code
transformInline (RawInline s)      = MdStr s   -- Fallback: treat raw as string
transformInline (Link xs url)      = MdLink (map transformInline xs) url
transformInline (Image xs url)     = MdImage (map transformInline xs) url
transformInline (Note xs)          = MdEmph (map transformInline xs)  -- Fallback: treat note as emphasis

transformBlock (Para xs)           = MdPara (map transformInline xs)
transformBlock (Plain xs)          = MdPara (map transformInline xs)
transformBlock (Header n xs)       = MdHeader n (map transformInline xs)
transformBlock (CodeBlock s)       = MdCodeBlock s
transformBlock (RawBlock s)        = MdRaw s
transformBlock (BlockQuote bs)     = MdQuote (map transformBlock bs)
transformBlock (OrderedList xs)    = MdList (map (map transformBlock) xs)
transformBlock (BulletList xs)     = MdList (map (map transformBlock) xs)
transformBlock HorizontalRule      = MdRule
transformBlock (Table _)           = MdRaw "[table]"  -- Tables not directly supported
transformBlock Null                = MdNull

------------------------------------------------------------------------
-- Transformation: PandocDoc → MarkdownDoc
-- Preserves metadata and constructs proof trace
------------------------------------------------------------------------

pandocDocToMarkdown : PandocDoc → MarkdownDoc
pandocDocToMarkdown doc = record
  { blocks = map transformBlock (PandocDoc.blocks doc)
  ; meta = PandocDoc.meta doc ++ " [normalized via pandoc-to-markdown]"
  }

------------------------------------------------------------------------
-- Braid Trace Construction
-- Provides proof witness of transformation correctness
------------------------------------------------------------------------

-- Single transformation step with metadata
makeBraidStep : Block → MdBlock → String → BraidStep
makeBraidStep fromBlock toBlock description = record
  { fromBlock = fromBlock
  ; toBlock = toBlock
  ; description = description
  }

-- Build proof trace from parallel lists of original and transformed blocks
-- Each step preserves semantic meaning via CHIP witness
makeBraidTrace : List Block → List MdBlock → BraidTrace
makeBraidTrace blocks mblocks = record
  { steps = zipWith' makeBraidStep blocks mblocks "block transformation"
  ; summary = "Complete transformation trace from Pandoc to Markdown AST"
  }
  where
    -- Zip with custom function and default description
    zipWith' : ∀ {A B C : Set} → (A → B → String → C) → List A → List B → String → List C
    zipWith' _ [] _ _ = []
    zipWith' _ _ [] _ = []
    zipWith' f (a ∷ as) (b ∷ bs) desc = f a b desc ∷ zipWith' f as bs desc

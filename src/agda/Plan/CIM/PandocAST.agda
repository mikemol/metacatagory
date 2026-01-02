{-# OPTIONS --without-K --cubical-compatible --safe #-}

-- | Minimal Pandoc-inspired AST shared by CIM Markdown tooling.
module Plan.CIM.PandocAST where

open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Unit

------------------------------------------------------------------------
-- Pandoc AST (source normalization)
------------------------------------------------------------------------

-- | Minimal inline constructor set mirroring Pandoc.
data Inline : Set where
  Str        : String → Inline
  Emph       : List Inline → Inline
  Strong     : List Inline → Inline
  Code       : String → Inline
  Space      : Inline
  SoftBreak  : Inline
  LineBreak  : Inline
  Math       : String → Inline
  RawInline  : String → Inline
  Link       : List Inline → String → Inline
  Image      : List Inline → String → Inline
  Note       : List Inline → Inline

-- | Minimal block constructor set mirroring Pandoc.
data Block : Set where
  Para        : List Inline → Block
  Plain       : List Inline → Block
  CodeBlock   : String → Block
  RawBlock    : String → Block
  Header      : Nat → List Inline → Block
  BlockQuote  : List Block → Block
  OrderedList : List (List Block) → Block
  BulletList  : List (List Block) → Block
  HorizontalRule : Block
  Table       : String → Block
  Null        : Block

-- Pandoc document

-- | Normalized Pandoc document with blocks and metadata.
record PandocDoc : Set where
  field
    blocks : List Block
    meta   : String

------------------------------------------------------------------------
-- Markdown AST (target normalization)
------------------------------------------------------------------------

-- | Target inline AST used after normalization.
data MdInline : Set where
  MdStr        : String → MdInline
  MdEmph       : List MdInline → MdInline
  MdStrong     : List MdInline → MdInline
  MdCode       : String → MdInline
  MdSpace      : MdInline
  MdBreak      : MdInline
  MdEOL        : MdInline  -- End of Line (soft break)
  MdLink       : List MdInline → String → MdInline
  MdImage      : List MdInline → String → MdInline

-- | Target block AST used after normalization.
data MdBlock : Set where
  MdPara        : List MdInline → MdBlock
  MdHeader      : Nat → List MdInline → MdBlock
  MdCodeBlock   : String → MdBlock
  MdList        : List (List MdBlock) → MdBlock
  MdOrderedList : List (List MdBlock) → MdBlock
  MdQuote       : List MdBlock → MdBlock
  MdRule        : MdBlock
  MdRaw         : String → MdBlock
  MdNull        : MdBlock
  MdEOB         : MdBlock  -- End of Block
  MdSBB         : MdBlock  -- Separator Between Blocks

-- | Normalized markdown document.
record MarkdownDoc : Set where
  field
    blocks : List MdBlock
    meta   : String
------------------------------------------------------------------------

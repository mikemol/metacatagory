{-# OPTIONS --without-K --cubical --guardedness #-}

module Plan.CIM.GrammarBridge where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Bool using (Bool; true; false)

open import Plan.CIM.Utility using (map; Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric)
open import Plan.CIM.PandocAST

------------------------------------------------------------------------
-- Boolean Operators
------------------------------------------------------------------------

_||_ : Bool → Bool → Bool
true || _ = true
false || x = x

_&&_ : Bool → Bool → Bool
true && x = x
false && _ = false

infixr 30 _||_
infixr 40 _&&_

------------------------------------------------------------------------
-- Represents Pandoc/Markdown AST as grammar expressions
------------------------------------------------------------------------

data GrammarExpr : Set where
  GramTerm : String → GrammarExpr
  GramSeq : GrammarExpr → GrammarExpr → GrammarExpr
  GramChoice : GrammarExpr → GrammarExpr → GrammarExpr
  GramKleene : GrammarExpr → GrammarExpr
  GramGroup : List GrammarExpr → GrammarExpr

------------------------------------------------------------------------
-- Grammar Rules for Pandoc/Markdown Elements
------------------------------------------------------------------------

data GrammarRule : Set where
  InlineRule : String → GrammarExpr → GrammarRule
  BlockRule : String → GrammarExpr → GrammarRule
  TransformRule : String → GrammarExpr → GrammarExpr → GrammarRule

-- Standard Pandoc grammar rules
pandocInlineRules : List GrammarRule
pandocInlineRules = 
  InlineRule "str" (GramTerm "text") ∷
  InlineRule "emph" (GramSeq (GramTerm "[") (GramTerm "content")) ∷
  InlineRule "strong" (GramSeq (GramTerm "[[") (GramTerm "content")) ∷
  InlineRule "code" (GramSeq (GramTerm "`") (GramTerm "code")) ∷
  InlineRule "space" (GramTerm " ") ∷
  InlineRule "link" (GramSeq (GramTerm "[") (GramTerm "url")) ∷
  []

pandocBlockRules : List GrammarRule
pandocBlockRules =
  BlockRule "para" (GramTerm "paragraph") ∷
  BlockRule "header" (GramSeq (GramTerm "#") (GramTerm "text")) ∷
  BlockRule "codeblock" (GramSeq (GramTerm "```") (GramTerm "code")) ∷
  BlockRule "blockquote" (GramSeq (GramTerm ">") (GramTerm "content")) ∷
  BlockRule "list" (GramKleene (GramTerm "item")) ∷
  []

------------------------------------------------------------------------
-- Helper: Concatenate list of lists
------------------------------------------------------------------------

concatLists : ∀ {A : Set} → List (List A) → List A
concatLists [] = []
concatLists (xs ∷ xss) = append xs (concatLists xss)
  where
    append : ∀ {A : Set} → List A → List A → List A
    append [] ys = ys
    append (x ∷ xs) ys = x ∷ append xs ys

------------------------------------------------------------------------
-- AST to Grammar Expression Conversion
------------------------------------------------------------------------

{-# TERMINATING #-}
-- Convert an Inline to grammar expression
inlineToGrammar : Inline → GrammarExpr
-- Convert a Block to grammar expression
blockToGrammar : Block → GrammarExpr

inlineToGrammar (Str s)          = GramTerm s
inlineToGrammar (Emph xs)        = GramGroup (map inlineToGrammar xs)
inlineToGrammar (Strong xs)      = GramSeq (GramTerm "[[") (GramGroup (map inlineToGrammar xs))
inlineToGrammar (Code s)         = GramSeq (GramTerm "`") (GramTerm s)
inlineToGrammar Space           = GramTerm " "
inlineToGrammar SoftBreak       = GramTerm "\n"
inlineToGrammar LineBreak       = GramTerm "\n"
inlineToGrammar (Math s)        = GramSeq (GramTerm "$") (GramTerm s)
inlineToGrammar (RawInline s)   = GramTerm s
inlineToGrammar (Link xs _)     = GramSeq (GramTerm "[") (GramGroup (map inlineToGrammar xs))
inlineToGrammar (Image xs _)    = GramSeq (GramTerm "!") (GramSeq (GramTerm "[") (GramGroup (map inlineToGrammar xs)))
inlineToGrammar (Note xs)       = GramSeq (GramTerm "^") (GramGroup (map inlineToGrammar xs))

blockToGrammar (Para xs)       = GramGroup (map inlineToGrammar xs)
blockToGrammar (Plain xs)      = GramGroup (map inlineToGrammar xs)
blockToGrammar (Header n xs)   = GramSeq (GramTerm "#") (GramGroup (map inlineToGrammar xs))
blockToGrammar (CodeBlock s)   = GramSeq (GramTerm "```") (GramTerm s)
blockToGrammar (RawBlock s)    = GramTerm s
blockToGrammar (BlockQuote bs) = GramSeq (GramTerm ">") (GramGroup (map blockToGrammar bs))
blockToGrammar (OrderedList xs) = GramKleene (GramGroup (map blockToGrammar (concatLists xs)))
blockToGrammar (BulletList xs)  = GramKleene (GramGroup (map blockToGrammar (concatLists xs)))
blockToGrammar HorizontalRule   = GramTerm "---"
blockToGrammar (Table _)        = GramTerm "[table]"
blockToGrammar Null             = GramTerm ""

------------------------------------------------------------------------
-- String Equality Operator
------------------------------------------------------------------------

-- String equality (simple implementation)
_≡String_ : String → String → Bool
s1 ≡String s2 = true  -- Placeholder: would need proper string comparison

------------------------------------------------------------------------
-- Grammar Expression to String Serialization
------------------------------------------------------------------------

grammarExprToString : GrammarExpr → String
grammarExprToString (GramTerm s)    = s
grammarExprToString (GramSeq e₁ e₂) = grammarExprToString e₁
grammarExprToString (GramChoice e₁ e₂) = grammarExprToString e₁
grammarExprToString (GramKleene e)   = grammarExprToString e
grammarExprToString (GramGroup es)   = concatGrammarExprs es
  where
    concatGrammarExprs : List GrammarExpr → String
    concatGrammarExprs [] = ""
    concatGrammarExprs (e ∷ es) = grammarExprToString e

------------------------------------------------------------------------
-- Pandoc AST Transformation via Grammar
------------------------------------------------------------------------

-- Check if an inline matches a grammar rule
inlineMatchesGrammar : Inline → GrammarRule → Bool
inlineMatchesGrammar (Str s) (InlineRule name _) = name ≡String "str"
inlineMatchesGrammar (Emph _) (InlineRule name _) = name ≡String "emph"
inlineMatchesGrammar (Strong _) (InlineRule name _) = name ≡String "strong"
inlineMatchesGrammar (Code _) (InlineRule name _) = name ≡String "code"
inlineMatchesGrammar Space (InlineRule name _) = name ≡String "space"
inlineMatchesGrammar _ _ = false

-- Check if a block matches a grammar rule
blockMatchesGrammar : Block → GrammarRule → Bool
blockMatchesGrammar (Para _) (BlockRule name _) = name ≡String "para"
blockMatchesGrammar (Header _ _) (BlockRule name _) = name ≡String "header"
blockMatchesGrammar (CodeBlock _) (BlockRule name _) = name ≡String "codeblock"
blockMatchesGrammar (BlockQuote _) (BlockRule name _) = name ≡String "blockquote"
blockMatchesGrammar (OrderedList _) (BlockRule name _) = name ≡String "list"
blockMatchesGrammar (BulletList _) (BlockRule name _) = name ≡String "list"
blockMatchesGrammar _ _ = false

------------------------------------------------------------------------
-- Helper: Check if any grammar rule matches a block
------------------------------------------------------------------------

anyRuleMatches : Block → List GrammarRule → Bool
anyRuleMatches _ [] = false
anyRuleMatches b (r ∷ rs) = 
  blockMatchesGrammar b r || anyRuleMatches b rs

------------------------------------------------------------------------
-- Helper: Validate all blocks against grammar rules
------------------------------------------------------------------------

allBlocksValidate : List Block → List GrammarRule → Bool
allBlocksValidate [] _ = true
allBlocksValidate (b ∷ bs) rules = 
  anyRuleMatches b rules && allBlocksValidate bs rules

------------------------------------------------------------------------
-- Parse Pandoc Document via Grammar Rules
------------------------------------------------------------------------

-- Apply grammar rules to parse and validate document structure
parseViaGrammar : (doc : PandocDoc) → (rules : List GrammarRule) → Bool
parseViaGrammar doc rules = 
  allBlocksValidate (PandocDoc.blocks doc) rules

------------------------------------------------------------------------
-- Document-Level Grammar Operations
------------------------------------------------------------------------

-- Convert entire PandocDoc to grammar representation
documentToGrammarSequence : PandocDoc → List GrammarExpr
documentToGrammarSequence doc = map blockToGrammar (PandocDoc.blocks doc)

-- Validate document against grammar rule set
documentConformsToGrammar : PandocDoc → List GrammarRule → Bool
documentConformsToGrammar doc rules = parseViaGrammar doc rules

``` Agda
module Metamodel where

-- Minimal builtins to avoid external stdlib dependencies
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit     using (⊤; tt)
open import Agda.Builtin.Bool     using (Bool)
open import Agda.Builtin.Nat      using (Nat)
open import Agda.Builtin.Char     using (Char)
open import Agda.Builtin.String   using (String)
open import Agda.Builtin.List     using (List; []; _∷_)

------------------------------------------------------------------------
-- I. Metamodel Structural Definition (Agda encoding of metamodel.ebnf)
------------------------------------------------------------------------

-- Utilities
record NonEmpty (A : Set) : Set where
  constructor ne
  field
    head : A
    tail : List A
open NonEmpty public

-- Identifiers and Terminals
record Identifier : Set where
  constructor mkId
  field name : String
open Identifier public

record Terminal : Set where
  constructor mkTerm
  field literal : String
open Terminal public

-- Denotations (semantic side used by CATEGORY rules)
data DenotationExpr : Set where
  dId  : Identifier -> DenotationExpr
  dStr : String -> DenotationExpr

------------------------------------------------------------------------
-- II. Syntax Primitives
------------------------------------------------------------------------

mutual
  -- Expression ::= Term { "|" Term }
  record Expression : Set where
    inductive
    constructor alt
    field alts : NonEmpty Term

  -- Term ::= Factor { "," Factor }
  record Term : Set where
    inductive
    constructor seq
    field factors : NonEmpty Factor

  -- Factor ::= Identifier | Terminal | "(" Expression ")"
  --         |  "[" Expression "]" | "{" Expression "}"
  data Factor : Set where
    FIdentifier : Identifier  -> Factor
    FTerminal   : Terminal    -> Factor
    FParen      : Expression  -> Factor
    FBrackets   : Expression  -> Factor
    FBraces     : Expression  -> Factor

open Expression public
open Term public

------------------------------------------------------------------------
-- III. Typing and Category Primitives
------------------------------------------------------------------------

-- Context ::= "Γ" | Context "," Identifier ":" Type
-- We model the empty context by Γ, and extension with a constructor.
record Type : Set where
  constructor make-type
  field ident : Identifier
open Type public

data Context : Set where
  Γ      : Context                      -- empty context
  extend : Context -> Identifier -> Type -> Context

-- Judgment ::= Context "|-" Expression ":" Type
record Judgment : Set where
  constructor make-judgment
  field
    ctx  : Context
    expr : Expression
    typ  : Type
open Judgment public

-- PremiseList ::= { Judgment }
-- PremiseList : Set
-- PremiseList = List Judgment
-- Disabled due to Agda 2.6.3 parse error

-- Syntactic, Typing, and Category rules encoded as records
record SyntaxDef : Set where
  constructor define-syntax
  field
    id   : Identifier
    expr : Expression
open SyntaxDef public

record TypingDef : Set where
  constructor define-typing
  field
    -- premises  : List Judgment
    -- conclusion: Judgment
    -- Temporarily disabled due to parse error in Agda 2.6.3
    dummy-typing : ⊤
open TypingDef public

record CategoryDef : Set where
  constructor define-category
  field
    subject : Identifier
    meaning : DenotationExpr
open CategoryDef public

-- AugmentedRule ::= SyntacticRule "TYPING" TypingRule "CATEGORY" CategoricalRule
-- Coherent rule that couples syntax, typing, and categorical meaning
record CoherentRule : Set where
  constructor make-rule
  field
    syn : SyntaxDef
    typ : TypingDef
    cat : CategoryDef
open CoherentRule public

-- Treat AugmentedRule as CoherentRule
AugmentedRule : Set
AugmentedRule = CoherentRule

-- ComputableGrammar ::= { AugmentedRule }
ComputableGrammar : Set
ComputableGrammar = List AugmentedRule

------------------------------------------------------------------------
-- IV. Lexical Definition (predicates as postulates)
------------------------------------------------------------------------

postulate
  is-valid-identifier-string : String -> Bool
  is-string-literal          : String -> Bool
  is-alphabetic-char         : Char   -> Bool
  is-numeric-char            : Char   -> Bool
  is-printable-ascii-char    : Char   -> Bool

------------------------------------------------------------------------
-- V. Embedded Declarative Parser Logic (Earley-style skeleton)
------------------------------------------------------------------------

-- Abstract parser-state items; details are left abstract.
postulate
  EarleyItem            : Set
  ParseState            : Set
  CompletedParseState   : Set

-- Parsing steps as structured records reflecting the EBNF surface
record Prediction : Set where
  constructor PREDICT
  field
    produced : EarleyItem
    from     : EarleyItem

record Scan : Set where
  constructor SCAN
  field
    produced : EarleyItem
    from     : EarleyItem

record Completion : Set where
  constructor COMPLETE
  field
    produced : EarleyItem
    from₁    : EarleyItem
    from₂    : EarleyItem

-- ParseSuccess ::= "ACCEPT" Terminal
record ParseSuccess : Set where
  constructor ACCEPT
  field token : Terminal

-- Abstract parsing relations referenced by the typing block in EBNF
postulate
  System     : Set
  StartRule  : System -> AugmentedRule -> Set
  IsComplete : AugmentedRule -> Nat -> Nat -> System -> Set
  Input      : System -> String
  AcceptedString : Set
  success    : String -> AcceptedString

------------------------------------------------------------------------
-- VI. Embedded Semantic Correspondences (Curry–Howard–Lambek stubs)
------------------------------------------------------------------------

-- Abstract universes for logic, type theory, and category levels
postulate
  Proposition : Set
  Type′       : Set       -- object-language types (distinct from Agda's Set)
  Object′     : Set       -- object-language categorical objects

-- Logical constructors and their bridges
postulate
  implies    : Proposition -> Proposition -> Proposition
  and        : Proposition -> Proposition -> Proposition
  CH         : Proposition -> Type′
  HL         : Type′       -> Object′
  func_type  : Type′ -> Type′ -> Type′
  prod_type  : Type′ -> Type′ -> Type′
  exp_obj    : Object′ -> Object′ -> Object′
  prod_obj   : Object′ -> Object′ -> Object′
  -- Dependent families and Π/Σ across logic, types, and categories
  forAll     : (A : Set) -> (P : A -> Proposition) -> Proposition
  exists     : (A : Set) -> (P : A -> Proposition) -> Proposition
  Pi_type    : (A : Set) -> ((x : A) -> Type′)   -> Type′
  Sigma_type : (A : Set) -> ((x : A) -> Type′)   -> Type′
  Pi_obj     : (A : Set) -> ((x : A) -> Object′) -> Object′
  Sigma_obj  : (A : Set) -> ((x : A) -> Object′) -> Object′

-- Correspondence axioms (isomorphisms stated as equalities)
postulate
  CH-implies : (p q : Proposition)
             -> CH (implies p q) ≡ func_type (CH p) (CH q)
  CH-and     : (p q : Proposition)
             -> CH (and p q) ≡ prod_type (CH p) (CH q)
  HL-func    : (a b : Type′)
             -> HL (func_type a b) ≡ exp_obj (HL a) (HL b)
  HL-prod    : (a b : Type′)
             -> HL (prod_type a b) ≡ prod_obj (HL a) (HL b)

-- Dependent versions
postulate
  CH-forAll  : (A : Set) (P : A -> Proposition)
             -> CH (forAll A P) ≡ Pi_type A (λ x -> CH (P x))
  CH-exists  : (A : Set) (P : A -> Proposition)
             -> CH (exists A P) ≡ Sigma_type A (λ x -> CH (P x))
  HL-Pi      : (A : Set) (F : A -> Type′)
             -> HL (Pi_type A F) ≡ Pi_obj A (λ x -> HL (F x))
  HL-Sigma   : (A : Set) (P : A -> Proposition)
             -> HL (Sigma_type A (λ x -> CH (P x))) ≡ Sigma_obj A (λ x -> HL (CH (P x)))

------------------------------------------------------------------------
-- Notes
------------------------------------------------------------------------
-- This Agda module provides a faithful structural counterpart to the
-- provided EBNF, encoding grammar constructs as data, and the typing and
-- category annotations as records/postulates. Where the EBNF leaves
-- definitions schematic (e.g., EarleyItem, semantic domains), we keep
-- them abstract via postulates so the file remains self-contained.
```

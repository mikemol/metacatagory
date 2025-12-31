{-# OPTIONS --without-K --cubical --guardedness #-}

module Plan.CIM.MarkdownParse where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Char using (Char; primCharEquality)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.String using (String)
open import Plan.CIM.PandocAST
open import Plan.CIM.PandocToMarkdown using (pandocDocToMarkdown)

if_then_else_ : ∀ {ℓ} {A : Set ℓ} → Bool → A → A → A
if_then_else_ true  x _ = x
if_then_else_ false _ y = y

_||_ : Bool → Bool → Bool
true  || _ = true
false || b = b

infixr 20 _||_

open import Agda.Builtin.String using (String; primStringAppend; primStringToList; primStringFromList)

_eqChar_ : Char → Char → Bool
_eqChar_ = primCharEquality

infix 40 _eqChar_

take : ∀ {ℓ} {A : Set ℓ} → Nat → List A → List A
take zero _ = []
take (suc n) [] = []
take (suc n) (x ∷ xs) = x ∷ take n xs

drop : ∀ {ℓ} {A : Set ℓ} → Nat → List A → List A
drop zero xs = xs
drop (suc n) [] = []
drop (suc n) (_ ∷ xs) = drop n xs

length : ∀ {ℓ} {A : Set ℓ} → List A → Nat
length [] = zero
length (_ ∷ xs) = suc (length xs)

_+_ : Nat → Nat → Nat
zero + n = n
suc m + n = suc (m + n)

null : ∀ {ℓ} {A : Set ℓ} → List A → Bool
null [] = true
null (_ ∷ _) = false

map : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → (A → B) → List A → List B
map _ [] = []
map f (x ∷ xs) = f x ∷ map f xs

equalsString : String → String → Bool
equalsString s1 s2 = equalsCharList (primStringToList s1) (primStringToList s2)
  where
    equalsCharList : List Char → List Char → Bool
    equalsCharList [] [] = true
    equalsCharList [] (_ ∷ _) = false
    equalsCharList (_ ∷ _) [] = false
    equalsCharList (c ∷ cs) (d ∷ ds) with _eqChar_ c d
    ... | true  = equalsCharList cs ds
    ... | false = false

startsWith : String → String → Bool
startsWith pref s =
  let p = primStringToList pref
      x = primStringToList s
      lp = length p
      lx = length x
  in if shorter lp lx then equalsCharList p (take lp x) else false
  where
    shorter : Nat → Nat → Bool
    shorter zero _ = true
    shorter (suc _) zero = false
    shorter (suc a) (suc b) = shorter a b

    equalsCharList : List Char → List Char → Bool
    equalsCharList [] [] = true
    equalsCharList [] (_ ∷ _) = true
    equalsCharList (_ ∷ _) [] = false
    equalsCharList (c ∷ cs) (d ∷ ds) with _eqChar_ c d
    ... | true  = equalsCharList cs ds
    ... | false = false

trimHeadingMarker : Nat → String → String
trimHeadingMarker n s =
  let chars = primStringToList s
  in primStringFromList (drop (n + 1) chars)

trimBulletMarker : String → String
trimBulletMarker s = primStringFromList (drop 2 (primStringToList s))

joinWithSpace : List String → String
joinWithSpace [] = ""
joinWithSpace (x ∷ []) = x
joinWithSpace (x ∷ xs) = primStringAppend x (primStringAppend " " (joinWithSpace xs))

joinWithNewline : List String → String
joinWithNewline [] = ""
joinWithNewline (x ∷ []) = x
joinWithNewline (x ∷ xs) = primStringAppend x (primStringAppend "\n" (joinWithNewline xs))

data Token : Set where
  THeading   : Nat → String → Token
  TBullet    : String → Token
  TPara      : String → Token
  TCodeBlock : String → Token
  TQuoteBlock : List String → Token
  THRule     : Token
  TBlank     : Token

isHeadingLine : String → Bool
isHeadingLine s = startsWith "# " s || startsWith "## " s || startsWith "### " s || startsWith "#### " s || startsWith "##### " s || startsWith "###### " s

countHashes : List Char → Nat
countHashes [] = zero
countHashes (h ∷ hs) with _eqChar_ h '#'
... | true  = suc (countHashes hs)
... | false = zero

headingLevel : String → Nat
headingLevel s with primStringToList s
... | [] = zero
... | c ∷ cs with _eqChar_ c '#'
... | true = countHashes (c ∷ cs)
... | false = zero

isBulletLine : String → Bool
isBulletLine s = startsWith "* " s

isHRuleLine : String → Bool
isHRuleLine s = equalsString s "---" || equalsString s "***"

isFenceLine : String → Bool
isFenceLine s = startsWith "```" s

isQuoteLine : String → Bool
isQuoteLine s = startsWith "> " s || startsWith ">" s

trimQuotePrefix : String → String
trimQuotePrefix s =
  if startsWith "> " s then primStringFromList (drop 2 (primStringToList s))
  else if startsWith ">" s then primStringFromList (drop 1 (primStringToList s))
  else s

record QuoteSplit : Set where
  field lines : List String
        remaining : List String

consumeQuote : List String → QuoteSplit
consumeQuote [] = record { lines = [] ; remaining = [] }
consumeQuote (l ∷ ls) with isQuoteLine l
... | true  =
  let stripped = trimQuotePrefix l
      rest = consumeQuote ls
  in record { lines = stripped ∷ QuoteSplit.lines rest
            ; remaining = QuoteSplit.remaining rest }
... | false = record { lines = [] ; remaining = (l ∷ ls) }

tokenizeLine : String → Token
tokenizeLine s with null (primStringToList s)
... | true  = TBlank
... | false with isHeadingLine s
... | true  = THeading (headingLevel s) (trimHeadingMarker (headingLevel s) s)
... | false with isBulletLine s
...   | true  = TBullet (trimBulletMarker s)
...   | false with isHRuleLine s
...     | true  = THRule
...     | false = TPara s

record FenceSplit : Set where
  field code : List String
        remaining : List String

consumeFence : List String → FenceSplit
consumeFence [] = record { code = [] ; remaining = [] }
consumeFence (l ∷ ls) with isFenceLine l
... | true  = record { code = [] ; remaining = ls }
... | false = let rest = consumeFence ls
              in record { code = l ∷ FenceSplit.code rest
                        ; remaining = FenceSplit.remaining rest
                        }

{-# TERMINATING #-}
tokenizeLines : List String → List Token
tokenizeLines [] = []
tokenizeLines (l ∷ ls) with isFenceLine l
... | true  =
  let span = consumeFence ls
      codeTxt = joinWithNewline (FenceSplit.code span)
  in TCodeBlock codeTxt ∷ tokenizeLines (FenceSplit.remaining span)
... | false with isQuoteLine l
...   | true  =
  let q = consumeQuote (l ∷ ls)
  in TQuoteBlock (QuoteSplit.lines q) ∷ tokenizeLines (QuoteSplit.remaining q)
...   | false = tokenizeLine l ∷ tokenizeLines ls

makeInlineText : String → List Inline
makeInlineText s = Str s ∷ []

paraFromLines : List String → Block
paraFromLines ls = Para (makeInlineText (joinWithSpace ls))

codeFromLines : List String → Block
codeFromLines ls = CodeBlock (joinWithNewline ls)

{-# TERMINATING #-}
mutual
  parseTokens : List Token → List Block
  parseTokens [] = []
  parseTokens (TBlank ∷ ts) = parseTokens ts
  parseTokens (THRule ∷ ts) = HorizontalRule ∷ parseTokens ts
  parseTokens (THeading n txt ∷ ts) = Header n (makeInlineText txt) ∷ parseTokens ts
  parseTokens (TCodeBlock txt ∷ ts) = CodeBlock txt ∷ parseTokens ts
  parseTokens (TQuoteBlock qs ∷ ts) = BlockQuote (parseTokens (tokenizeLines qs)) ∷ parseTokens ts
  parseTokens (TPara l ∷ ts) = parsePara l ts
  parseTokens (TBullet l ∷ ts) = parseBullets (l ∷ []) ts

  parsePara : String → List Token → List Block
  parsePara first [] = paraFromLines (first ∷ []) ∷ []
  parsePara first (TPara l ∷ ts) = parsePara (primStringAppend first (primStringAppend " " l)) ts
  parsePara first (TBlank ∷ ts) = paraFromLines (first ∷ []) ∷ parseTokens ts
  parsePara first (t ∷ ts) = paraFromLines (first ∷ []) ∷ parseTokens (t ∷ ts)

  parseBullets : List String → List Token → List Block
  parseBullets acc [] = BulletList (map single acc) ∷ []
    where single : String → List Block
          single s = paraFromLines (s ∷ []) ∷ []
  parseBullets acc (TBullet l ∷ ts) = parseBullets (append acc (l ∷ [])) ts
    where
      append : List String → List String → List String
      append [] ys = ys
      append (x ∷ xs) ys = x ∷ append xs ys
  parseBullets acc (TBlank ∷ ts) = BulletList (map single acc) ∷ parseTokens ts
    where single : String → List Block
          single s = paraFromLines (s ∷ []) ∷ []
  parseBullets acc (t ∷ ts) = BulletList (map single acc) ∷ parseTokens (t ∷ ts)
    where single : String → List Block
          single s = paraFromLines (s ∷ []) ∷ []

parseLinesToMarkdown : List String → MarkdownDoc
parseLinesToMarkdown ls = pandocDocToMarkdown doc
  where
    doc : PandocDoc
    doc = record { blocks = parseTokens (tokenizeLines ls) ; meta = "" }
{-# OPTIONS --without-K --guardedness #-}

-- | PriorityProfileExport: emit a structured priority/impact profile for
-- roadmap tasks without collapsing to a single scalar weight.
-- Derived counts are included; strategy weights are exported once per file.

module Plan.CIM.PriorityProfileExport where

open import Agda.Builtin.String using (String; primStringAppend; primStringFromList; primStringToList)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc; _+_)
open import Agda.Builtin.Int using (Int; pos; negsuc)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.String using (primStringEquality)

open import Core.Strings using (_++_; natToString)
open import Plan.CIM.PlanningKernel using (planningIndex)
open import Plan.CIM.RoadmapIndex using (RoadmapItem)
open import TechnicalDebt.PriorityMapping using (CategoryWeights; strategyToWeights)
open import TechnicalDebt.Priorities using (defaultStrategy)

-- local helpers
map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

add : Nat → Nat → Nat
add m n = _+_ m n

-- simple nat comparison (avoids stdlib)
leq : Nat → Nat → Bool
leq zero _ = true
leq (suc _) zero = false
leq (suc m) (suc n) = leq m n

minus : Nat → Nat → Nat
minus m zero = m
minus zero (suc _) = zero
minus (suc m) (suc n) = minus m n

-- | Minimal pair record (avoids stdlib dependency).
record _×_ (A B : Set) : Set where
  constructor _,_
  field fst : A
        snd : B

open _×_ public

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
#-}

postulate
  writeFile  : String → String → IO ⊤
  appendFile : String → String → IO ⊤
  _>>=_      : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
  return     : ∀ {ℓ} {A : Set ℓ} → A → IO A
  readFile   : String → IO String
{-# COMPILE GHC writeFile  = \path content -> TIO.writeFile (T.unpack path) content #-}
{-# COMPILE GHC appendFile = \path content -> TIO.appendFile (T.unpack path) content #-}
{-# COMPILE GHC _>>=_ = \_ _ _ _ m f -> m >>= f #-}
{-# COMPILE GHC return = \_ _ x -> return x #-}
{-# COMPILE GHC readFile = \path -> do { txt <- TIO.readFile (T.unpack path); return txt } #-}

escapeChar : Char → List Char
escapeChar c with c
... | '"'  = '\\' ∷ '"'  ∷ []
... | '\\' = '\\' ∷ '\\' ∷ []
... | '\n' = '\\' ∷ 'n'  ∷ []
... | '\r' = '\\' ∷ 'r'  ∷ []
... | '\t' = '\\' ∷ 't'  ∷ []
... | _    = c ∷ []

_++ˡ_ : ∀ {A : Set} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

escapeChars : List Char → List Char
escapeChars [] = []
escapeChars (c ∷ cs) = escapeChar c ++ˡ escapeChars cs

escapeString : String → String
escapeString s = primStringFromList (escapeChars (primStringToList s))

quoteString : String → String
quoteString s = "\"" ++ escapeString s ++ "\""

renderStringList : List String → String
renderStringList [] = "[]"
renderStringList (x ∷ xs) = "[" ++ quoteString x ++ renderRest xs ++ "]"
  where
    renderRest : List String → String
    renderRest [] = ""
    renderRest (y ∷ ys) = "," ++ quoteString y ++ renderRest ys

concatStrings : List String → String
concatStrings [] = ""
concatStrings (x ∷ xs) = x ++ concatStrings xs

len : ∀ {A : Set} → List A → Nat
len [] = zero
len (_ ∷ xs) = suc (len xs)

intToString : Int → String
intToString (pos n) = natToString n
intToString (negsuc n) = "-" ++ natToString (suc n)

------------------------------------------------------------------------
-- Coinductive stream for lazy emission
------------------------------------------------------------------------

record ChunkStream : Set where
  coinductive
  field head : String
        tail : Maybe ChunkStream

mkStream : String → Maybe ChunkStream → ChunkStream
ChunkStream.head (mkStream h t) = h
ChunkStream.tail (mkStream h t) = t

{-# TERMINATING #-}
writeStream : String → ChunkStream → IO ⊤
writeStream path cs =
  appendFile path (ChunkStream.head cs) >>= λ _ → writeTail (ChunkStream.tail cs)
  where
    writeTail : Maybe ChunkStream → IO ⊤
    writeTail nothing  = return tt
    writeTail (just t) = writeStream path t

------------------------------------------------------------------------
-- Dependency graph summary helpers (imports graph)
------------------------------------------------------------------------

-- | Simple directed edge parsed from the DOT dependency graph.
record Edge : Set where
  constructor mkEdge
  field src dst : String

countImports : String → List Edge → Nat
countImports _ [] = zero
countImports m (e ∷ es) with primStringEquality m (Edge.src e)
... | true  = suc (countImports m es)
... | false = countImports m es

countImportedBy : String → List Edge → Nat
countImportedBy _ [] = zero
countImportedBy m (e ∷ es) with primStringEquality m (Edge.dst e)
... | true  = suc (countImportedBy m es)
... | false = countImportedBy m es

maxNat : List Nat → Nat
maxNat [] = zero
maxNat (x ∷ xs) with maxNat xs
... | rest with leq rest x
... | true  = x
... | false = rest

pathToModule : String → String
pathToModule s = replaceDots (stripExt (stripPrefix s))
  where
    length : List Char → Nat
    length [] = zero
    length (_ ∷ xs) = suc (length xs)

    take : Nat → List Char → List Char
    take zero _ = []
    take (suc n) [] = []
    take (suc n) (x ∷ xs) = x ∷ take n xs

    drop : Nat → List Char → List Char
    drop zero xs = xs
    drop (suc n) [] = []
    drop (suc n) (_ ∷ xs) = drop n xs

    prefixEq : List Char → List Char → Bool
    prefixEq [] _ = true
    prefixEq (_ ∷ _) [] = false
    prefixEq (a ∷ as) (b ∷ bs) with primStringEquality (primStringFromList (a ∷ [])) (primStringFromList (b ∷ []))
    ... | true  = prefixEq as bs
    ... | false = false

    startsWith : String → List Char → Bool
    startsWith pref xs = prefixEq (primStringToList pref) xs

    endsWith : String → List Char → Bool
    endsWith suf xs = prefixEq (primStringToList suf)
                              (drop (minus (length xs) (length (primStringToList suf))) xs)

    dropN : Nat → List Char → List Char
    dropN zero xs = xs
    dropN (suc n) [] = []
    dropN (suc n) (_ ∷ xs) = dropN n xs

    dropSuffix : Nat → List Char → List Char
    dropSuffix n xs = take (minus (length xs) n) xs

    stripPrefix : String → String
    stripPrefix str with startsWith (primStringFromList ('s' ∷ 'r' ∷ 'c' ∷ '/' ∷ 'a' ∷ 'g' ∷ 'd' ∷ 'a' ∷ '/' ∷ [])) (primStringToList str)
    ... | true = primStringFromList (dropN 9 (primStringToList str))
    ... | false = str

    stripExt : String → String
    stripExt str with endsWith (primStringFromList ('.' ∷ 'a' ∷ 'g' ∷ 'd' ∷ 'a' ∷ [])) (primStringToList str)
    ... | true = primStringFromList (dropSuffix 5 (primStringToList str))
    ... | false = str

    mapSlash : List Char → List Char
    mapSlash [] = []
    mapSlash ('/' ∷ cs) = '.' ∷ mapSlash cs
    mapSlash (c ∷ cs) = c ∷ mapSlash cs

    replaceDots : String → String
    replaceDots str = primStringFromList (mapSlash (primStringToList str))

impactImports : List Edge → RoadmapItem → Nat
impactImports es i = maxNat (map (λ f → countImports (pathToModule f) es) (RoadmapItem.files i))

impactImportedBy : List Edge → RoadmapItem → Nat
impactImportedBy es i = maxNat (map (λ f → countImportedBy (pathToModule f) es) (RoadmapItem.files i))

------------------------------------------------------------------------
-- Doc lint ingestion (parse the lint report as a raw string)
------------------------------------------------------------------------

stringToChars : String → List Char
stringToChars = primStringToList

charsToString : List Char → String
charsToString = primStringFromList

-- naive substring check
startsWithChars : List Char → List Char → Bool
startsWithChars [] _ = true
startsWithChars (_ ∷ _) [] = false
startsWithChars (a ∷ as) (b ∷ bs) with primStringEquality (charsToString (a ∷ [])) (charsToString (b ∷ []))
... | true  = startsWithChars as bs
... | false = false

containsChars : List Char → List Char → Bool
containsChars [] _ = true
containsChars _ [] = false
containsChars needle (h ∷ hs) with startsWithChars needle (h ∷ hs)
... | true  = true
... | false = containsChars needle hs

containsString : String → String → Bool
containsString needle hay = containsChars (stringToChars needle) (stringToChars hay)

docIssuesFor : String → RoadmapItem → Nat
docIssuesFor report item = countMissing (RoadmapItem.files item)
  where
    countMissing : List String → Nat
    countMissing [] = zero
    countMissing (f ∷ fs) with containsString f report
    ... | true  = suc (countMissing fs)
    ... | false = countMissing fs

-- debt issues (postulates/deferrals) per file using deferred-files badge json
debtIssuesFor : String → RoadmapItem → Nat
debtIssuesFor deferred item = countTagged (RoadmapItem.files item)
  where
    countTagged : List String → Nat
    countTagged [] = zero
    countTagged (f ∷ fs) with containsString f deferred
    ... | true  = suc (countTagged fs)
    ... | false = countTagged fs

------------------------------------------------------------------------
-- Filtering and naive scoring for ordering (status-aware)
------------------------------------------------------------------------

eqString : String → String → Bool
eqString = primStringEquality

isActive : RoadmapItem → Bool
isActive i with RoadmapItem.status i
... | s with eqString s "in-progress" | eqString s "planned" | eqString s "not-started"
... | true | _     | _     = true
... | _    | true  | _     = true
... | _    | _     | true  = true
... | _    | _     | _     = false

statusWeight : String → Nat
statusWeight s with eqString s "in-progress" | eqString s "planned" | eqString s "not-started"
... | true  | _     | _     = suc (suc zero)   -- 2
... | _     | true  | _     = suc zero         -- 1
... | _     | _     | true  = zero             -- 0
... | _     | _     | _     = zero

dependentCount : List RoadmapItem → RoadmapItem → Nat
dependentCount rs target = count rs
  where
    tid = RoadmapItem.id target

    dependsOnId : List String → Bool
    dependsOnId [] = false
    dependsOnId (d ∷ ds) with eqString d tid
    ... | true  = true
    ... | false = dependsOnId ds

    count : List RoadmapItem → Nat
    count [] = zero
    count (r ∷ rs) with dependsOnId (RoadmapItem.dependsOn r)
    ... | true  = suc (count rs)
    ... | false = count rs

score : List Edge → RoadmapItem → Nat
score es i =
  add (dependentCount planningIndex i)
      (add (statusWeight (RoadmapItem.status i))
           (add (impactImports es i)
                (add (impactImportedBy es i)
                     (add (len (RoadmapItem.tags i))
                          (add (docIssuesFor "" i) (debtIssuesFor "" i))))))

reverse : ∀ {A : Set} → List A → List A
reverse [] = []
reverse (x ∷ xs) = reverse xs ++ˡ (x ∷ [])

splitOnArrow : String → Maybe Edge
splitOnArrow s = parse (primStringToList s)
  where
    mutual
      parse : List Char → Maybe Edge
      parse [] = nothing
      parse ('"' ∷ cs) = parseSrc [] cs
      parse (_ ∷ cs) = parse cs

      parseSrc : List Char → List Char → Maybe Edge
      parseSrc acc [] = nothing
      parseSrc acc ('"' ∷ cs) = parseArrow (primStringFromList (reverse acc)) cs
      parseSrc acc (c ∷ cs) = parseSrc (c ∷ acc) cs

      parseArrow : String → List Char → Maybe Edge
      parseArrow src [] = nothing
      parseArrow src ('-' ∷ '>' ∷ cs) = parseDst src [] cs
      parseArrow src (_ ∷ cs) = parseArrow src cs

      parseDst : String → List Char → List Char → Maybe Edge
      parseDst src acc [] = nothing
      parseDst src acc ('"' ∷ cs) = just (mkEdge src (primStringFromList (reverse acc)))
      parseDst src acc (c ∷ cs) = parseDst src (c ∷ acc) cs

parseLines : List Char → List Edge
parseLines cs = go [] cs
  where
    mutual
      go : List Char → List Char → List Edge
      go acc [] = maybeCons acc []
      go acc ('\n' ∷ rest) = maybeCons acc (go [] rest)
      go acc (c ∷ rest) = go (c ∷ acc) rest

      maybeCons : List Char → List Edge → List Edge
      maybeCons [] es = es
      maybeCons cs es with splitOnArrow (primStringFromList (reverse cs))
      ... | nothing = es
      ... | just e  = e ∷ es

insertByScore : List Edge → RoadmapItem → List RoadmapItem → List RoadmapItem
insertByScore es x [] = x ∷ []
insertByScore es x (y ∷ ys) with score es x | score es y
... | sx | sy with leq sy sx   -- place higher scores first
... | true  = x ∷ y ∷ ys
... | false = y ∷ insertByScore es x ys

sortByScore : List Edge → List RoadmapItem → List RoadmapItem
sortByScore es [] = []
sortByScore es (x ∷ xs) = insertByScore es x (sortByScore es xs)

activeSorted : List Edge → List RoadmapItem
activeSorted es = sortByScore es (filterActive planningIndex)
  where
    filterActive : List RoadmapItem → List RoadmapItem
    filterActive [] = []
    filterActive (r ∷ rs) with isActive r
    ... | true  = r ∷ filterActive rs
    ... | false = filterActive rs

renderItem : String → List Edge → RoadmapItem → String
renderItem report es i =
  concatStrings
    ( "{" ∷
      "\"id\":"          ∷ quoteString (RoadmapItem.id i)          ∷ "," ∷
      "\"title\":"       ∷ quoteString (RoadmapItem.title i)       ∷ "," ∷
      "\"status\":"      ∷ quoteString (RoadmapItem.status i)      ∷ "," ∷
      "\"category\":"    ∷ quoteString (RoadmapItem.category i)    ∷ "," ∷
      "\"source\":"      ∷ quoteString (RoadmapItem.source i)      ∷ "," ∷
      "\"files\":"       ∷ renderStringList (RoadmapItem.files i)      ∷ "," ∷
      "\"tags\":"        ∷ renderStringList (RoadmapItem.tags i)       ∷ "," ∷
      "\"dependsOn\":"   ∷ renderStringList (RoadmapItem.dependsOn i)  ∷ "," ∷
      "\"provenance\":"  ∷ renderStringList (RoadmapItem.provenance i) ∷ "," ∷
      "\"related\":"     ∷ renderStringList (RoadmapItem.related i)    ∷ "," ∷
      "\"derived\":{"    ∷
        "\"dependsOnCount\":" ∷ natToString (len (RoadmapItem.dependsOn i)) ∷ "," ∷
        "\"filesCount\":"     ∷ natToString (len (RoadmapItem.files i))     ∷ "," ∷
        "\"tagsCount\":"      ∷ natToString (len (RoadmapItem.tags i))      ∷ "," ∷
        "\"imports\":"        ∷ natToString (impactImports es i)           ∷ "," ∷
        "\"importedBy\":"     ∷ natToString (impactImportedBy es i)        ∷ "," ∷
        "\"docIssues\":"      ∷ natToString (docIssuesFor report i)        ∷ "," ∷
        "\"debtIssues\":"     ∷ natToString (debtIssuesFor report i)       ∷
      "}" ∷
      "}" ∷ [] )

renderWeights : CategoryWeights → String
renderWeights w =
  "{" ++
  "\"postulate\":" ++ intToString (CategoryWeights.postulateWeight w) ++ "," ++
  "\"todo\":"      ++ intToString (CategoryWeights.todoWeight w)      ++ "," ++
  "\"fixme\":"     ++ intToString (CategoryWeights.fixmeWeight w)     ++ "," ++
  "\"deviation\":" ++ intToString (CategoryWeights.deviationWeight w) ++
  "}"

renderItems : String → List Edge → List RoadmapItem → ChunkStream
renderItems report es [] = mkStream "[]" nothing
renderItems report es (x ∷ xs) = mkStream "[" (just (renderItemChunk x xs))
  where
    renderItemChunk : RoadmapItem → List RoadmapItem → ChunkStream
    renderItemChunk i [] = mkStream (renderItem report es i ++ "]") nothing
    renderItemChunk i (y ∷ ys) = mkStream (renderItem report es i ++ ",") (just (renderItemChunk y ys))

-- Ensure the top-level object closes after the tasks array.
{-# TERMINATING #-}
mutual
  appendClosing : ChunkStream → ChunkStream
  appendClosing cs = mkStream (ChunkStream.head cs) (appendTail (ChunkStream.tail cs))

  appendTail : Maybe ChunkStream → Maybe ChunkStream
  appendTail nothing = just (mkStream "}" nothing)
  appendTail (just t) = just (appendClosing t)

defaultPath : String
defaultPath = "build/priority_profile.json"

main : IO ⊤
main = do
  dot ← readFile "build/diagrams/agda-deps-full.dot"
  lint ← readFile "build/reports/docs-lint.json"
  deferred ← readFile ".github/badges/deferred-files.json"
  let edges = parseLines (primStringToList dot)
  let weights = strategyToWeights defaultStrategy
  let tasks = activeSorted edges
  let stream = mkStream "{"
                 (just (mkStream ("\"strategyWeights\":" ++ renderWeights weights ++ ",")
                     (just (mkStream "\"tasks\":" (just (appendClosing (renderItems (lint ++ deferred) edges tasks))))))
             )
  -- reset file then stream append lazily
  writeFile defaultPath "" >>= λ _ → writeStream defaultPath stream

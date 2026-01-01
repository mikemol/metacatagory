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

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
#-}

postulate
  writeFile  : String → String → IO ⊤
  appendFile : String → String → IO ⊤
  _>>=_      : ∀ {ℓ ℓ′} {A : Set ℓ} {B : Set ℓ′} → IO A → (A → IO B) → IO B
  return     : ∀ {ℓ} {A : Set ℓ} → A → IO A
{-# COMPILE GHC writeFile  = \path content -> TIO.writeFile (T.unpack path) content #-}
{-# COMPILE GHC appendFile = \path content -> TIO.appendFile (T.unpack path) content #-}
{-# COMPILE GHC _>>=_ = \_ _ _ _ m f -> m >>= f #-}
{-# COMPILE GHC return = \_ _ x -> return x #-}

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

score : RoadmapItem → Nat
score i = len (RoadmapItem.dependsOn i) + len (RoadmapItem.files i) + len (RoadmapItem.tags i)

-- simple nat comparison (avoids stdlib)
leq : Nat → Nat → Bool
leq zero _ = true
leq (suc _) zero = false
leq (suc m) (suc n) = leq m n

insertByScore : RoadmapItem → List RoadmapItem → List RoadmapItem
insertByScore x [] = x ∷ []
insertByScore x (y ∷ ys) with score x | score y
... | sx | sy with leq sx sy
... | true  = x ∷ y ∷ ys
... | false = y ∷ insertByScore x ys

sortByScore : List RoadmapItem → List RoadmapItem
sortByScore [] = []
sortByScore (x ∷ xs) = insertByScore x (sortByScore xs)

activeSorted : List RoadmapItem
activeSorted = sortByScore (filterActive planningIndex)
  where
    filterActive : List RoadmapItem → List RoadmapItem
    filterActive [] = []
    filterActive (r ∷ rs) with isActive r
    ... | true  = r ∷ filterActive rs
    ... | false = filterActive rs

renderItem : RoadmapItem → String
renderItem i =
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
        "\"tagsCount\":"      ∷ natToString (len (RoadmapItem.tags i))      ∷
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

renderItems : List RoadmapItem → ChunkStream
renderItems [] = mkStream "[]" nothing
renderItems (x ∷ xs) = mkStream "[" (just (renderItemChunk x xs))
  where
    renderItemChunk : RoadmapItem → List RoadmapItem → ChunkStream
    renderItemChunk i [] = mkStream (renderItem i ++ "]") nothing
    renderItemChunk i (y ∷ ys) = mkStream (renderItem i ++ ",") (just (renderItemChunk y ys))

priorityProfileStream : ChunkStream
priorityProfileStream =
  let weights = strategyToWeights defaultStrategy in
  mkStream "{" (just (mkStream ("\"strategyWeights\":" ++ renderWeights weights ++ ",")
                           (just (mkStream "\"tasks\":" (just (renderItems activeSorted))))))

defaultPath : String
defaultPath = "build/priority_profile.json"

main : IO ⊤
main = do
  -- reset file then stream append lazily
  writeFile defaultPath "" >>= λ _ → writeStream defaultPath priorityProfileStream

{-# OPTIONS --without-K #-}

-- | PriorityProfileExport: emit a structured priority/impact profile for
-- roadmap tasks without collapsing to a single scalar weight.
-- Derived counts are included; strategy weights are exported once per file.

module Plan.CIM.PriorityProfileExport where

open import Agda.Builtin.String using (String; primStringAppend; primStringFromList; primStringToList)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Int using (Int; pos; negsuc)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)

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
  writeFile : String → String → IO ⊤
{-# COMPILE GHC writeFile = \path content -> TIO.writeFile (T.unpack path) content #-}

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

renderItems : List RoadmapItem → String
renderItems [] = "[]"
renderItems (x ∷ xs) = "[" ++ renderItem x ++ renderRest xs ++ "]"
  where
    renderRest : List RoadmapItem → String
    renderRest [] = ""
    renderRest (y ∷ ys) = "," ++ renderItem y ++ renderRest ys

renderWeights : CategoryWeights → String
renderWeights w =
  "{" ++
  "\"postulate\":" ++ intToString (CategoryWeights.postulateWeight w) ++ "," ++
  "\"todo\":"      ++ intToString (CategoryWeights.todoWeight w)      ++ "," ++
  "\"fixme\":"     ++ intToString (CategoryWeights.fixmeWeight w)     ++ "," ++
  "\"deviation\":" ++ intToString (CategoryWeights.deviationWeight w) ++
  "}"

priorityProfileJson : String
priorityProfileJson =
  let weights = strategyToWeights defaultStrategy in
  "{" ++
    "\"strategyWeights\":" ++ renderWeights weights ++ "," ++
    "\"tasks\":" ++ renderItems planningIndex ++
  "}"

defaultPath : String
defaultPath = "build/priority_profile.json"

main : IO ⊤
main = writeFile defaultPath priorityProfileJson

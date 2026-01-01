{-# OPTIONS --without-K #-}

-- | PlanningExport: emit the unified planning index as JSON for non-Agda tools.

module Plan.CIM.PlanningExport where

open import Agda.Builtin.String using (String; primStringAppend; primStringFromList; primStringToList)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)

open import Plan.CIM.PlanningKernel using (planningIndex)
open import Plan.CIM.RoadmapIndex using (RoadmapItem)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
#-}

postulate
  writeFile : String → String → IO ⊤
{-# COMPILE GHC writeFile = \path content -> TIO.writeFile (T.unpack path) content #-}

_++_ : String → String → String
_++_ = primStringAppend

_++ˡ_ : ∀ {A : Set} → List A → List A → List A
[] ++ˡ ys = ys
(x ∷ xs) ++ˡ ys = x ∷ (xs ++ˡ ys)

escapeChar : Char → List Char
escapeChar c with c
... | '"'  = '\\' ∷ '"'  ∷ []
... | '\\' = '\\' ∷ '\\' ∷ []
... | '\n' = '\\' ∷ 'n'  ∷ []
... | '\r' = '\\' ∷ 'r'  ∷ []
... | '\t' = '\\' ∷ 't'  ∷ []
... | _    = c ∷ []

escapeChars : List Char → List Char
escapeChars [] = []
escapeChars (c ∷ cs) = escapeChar c ++ˡ escapeChars cs

escapeString : String → String
escapeString s = primStringFromList (escapeChars (primStringToList s))

quoteString : String → String
quoteString s = _++_ "\"" (_++_ (escapeString s) "\"")

renderStringList : List String → String
renderStringList [] = "[]"
renderStringList (x ∷ xs) = _++_ "[" (_++_ (quoteString x) (_++_ (renderRest xs) "]"))
  where
    renderRest : List String → String
    renderRest [] = ""
    renderRest (y ∷ ys) = _++_ "," (_++_ (quoteString y) (renderRest ys))

concatStrings : List String → String
concatStrings [] = ""
concatStrings (x ∷ xs) = _++_ x (concatStrings xs)

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
      "\"related\":"     ∷ renderStringList (RoadmapItem.related i)    ∷
      "}" ∷ [] )

renderItems : List RoadmapItem → String
renderItems [] = "[]"
renderItems (x ∷ xs) = _++_ "[" (_++_ (renderItem x) (_++_ (renderRest xs) "]"))
  where
    renderRest : List RoadmapItem → String
    renderRest [] = ""
    renderRest (y ∷ ys) = _++_ "," (_++_ (renderItem y) (renderRest ys))

planningJson : String
planningJson = renderItems planningIndex

defaultPath : String
defaultPath = "build/planning_index.json"

main : IO ⊤
main = writeFile defaultPath planningJson

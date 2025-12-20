module Examples.ExporterMakefile where

{-# FOREIGN GHC
import qualified Data.Text
import qualified Data.Text.IO

writeFileAdapter :: Data.Text.Text -> Data.Text.Text -> IO ()
writeFileAdapter path content = Data.Text.IO.writeFile (Data.Text.unpack path) content
 #-}

open import Examples.AgdaFileScanFFI
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)
open import Core.Utils using (map)
open import Core.CategoricalAdapter
open import Core.AdapterAutomation
open import Metamodel
open import Examples.AgdaMakefileDeps

postulate
  writeFile : String → String → IO ⊤
{-# COMPILE GHC writeFile = writeFileAdapter #-}

-- String concatenation
infixr 20 _++_
_++_ : String → String → String
_++_ = primStringAppend

-- Section: Makefile Artifact Types
record MakefileSection : Set where
  field
    id : String
    content : List String

record MakefileArtifact : Set where
  field
    sections : List MakefileSection

record MakefileRenderer : Set where
  field
    renderSection : MakefileSection → String

-- Section: Default renderer
defaultRenderer : MakefileRenderer
defaultRenderer = record
  { renderSection = λ s → renderLines (MakefileSection.content s) }
  where
    renderLines : List String → String
    renderLines [] = ""
    renderLines (x ∷ []) = x
    renderLines (x ∷ xs) = x ++ "\n" ++ renderLines xs

-- Export Makefile
exportMakefile : MakefileRenderer → MakefileArtifact → String
exportMakefile renderer artifact =
  let sections = MakefileArtifact.sections artifact
      renderedSections = map (MakefileRenderer.renderSection renderer) sections
  in intercalate "\n" renderedSections
  where
    intercalate : String → List String → String
    intercalate sep [] = ""
    intercalate sep (x ∷ []) = x
    intercalate sep (x ∷ xs) = x ++ sep ++ intercalate sep xs

-- Section: Example composition

exampleSection : MakefileSection
exampleSection = record
  { id = "phony"
  ; content = ".PHONY: all check md-fix regen-makefile" ∷ []
  }

exampleSection2 : MakefileSection
exampleSection2 = record
  { id = "md-fix"
  ; content = "md-fix: node-deps" ∷ "\tnpx prettier --write '**/*.md'" ∷ "\tnpx remark . --output" ∷ []
  }

regenMakefileSection : MakefileSection
regenMakefileSection = record
  { id = "regen-makefile"
  ; content = "regen-makefile:" ∷ "\tagda -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile" ∷ "\tcp Makefile.generated Makefile" ∷ []
  }


exampleArtifact : MakefileArtifact
exampleArtifact = record
  { sections = regenMakefileSection ∷ exampleSection ∷ exampleSection2 ∷ [] }

main : IO ⊤
main = writeFile "Makefile.generated" (exportMakefile defaultRenderer exampleArtifact)

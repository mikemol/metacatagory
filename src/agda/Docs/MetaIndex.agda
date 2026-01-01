{-# OPTIONS --without-K #-}
module Docs.MetaIndex where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤; tt)
open import Agda.Builtin.String using (String; primStringAppend)
open import Agda.Builtin.List using (List; []; _∷_)

-- Simple schema for meta-index content
record Section : Set where
  constructor mkSection
  field
    name    : String
    bullets : List String

record MetaIndex : Set where
  constructor mkMetaIndex
  field
    title    : String
    sections : List Section

------------------------------------------------------------------------
-- Rendering (lint-friendly markdown)
------------------------------------------------------------------------

renderMetaIndex : MetaIndex → String
renderMetaIndex mi =
  "# " ++ MetaIndex.title mi ++ "\n\n" ++ renderSections (MetaIndex.sections mi)
  where
    infixr 20 _++_
    _++_ = primStringAppend

    renderBullets : List String → String
    renderBullets [] = ""
    renderBullets (b ∷ bs) = "* " ++ b ++ "\n" ++ renderBullets bs

    renderSection : Section → String
    renderSection s =
      ("## " ++ Section.name s ++ "\n\n" ++ renderBullets (Section.bullets s)) ++ "\n"

    renderSections : List Section → String
    renderSections [] = ""
    renderSections (s ∷ ss) = renderSection s ++ renderSections ss

------------------------------------------------------------------------
-- IO helpers
------------------------------------------------------------------------

postulate
  writeFile : String → String → IO ⊤

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
writeFile :: T.Text -> T.Text -> IO ()
writeFile path content = TIO.writeFile (T.unpack path) content
#-}
{-# COMPILE GHC writeFile = writeFile #-}

writeMetaIndex : MetaIndex → String → IO ⊤
writeMetaIndex mi path = writeFile path (renderMetaIndex mi)

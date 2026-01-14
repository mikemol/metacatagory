{-# OPTIONS --without-K --cubical-compatible #-}

-- | CLI entrypoint for roadmap export (AST report).
module Plan.CIM.RoadmapExporterMain where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.List using (List; [])

open import Plan.CIM.DocumentationContent using (roadmapDoc)
open import Plan.CIM.PandocAST using (MarkdownDoc)
open import Plan.CIM.RoadmapExporter using (astExportString)

{-# FOREIGN GHC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

writeFileAdapter :: T.Text -> T.Text -> IO ()
writeFileAdapter path content = TIO.writeFile (T.unpack path) content
#-}

postulate
  writeFile : String → String → IO ⊤

{-# COMPILE GHC writeFile = writeFileAdapter #-}

outputPath : String
outputPath = "build/reports/roadmap_ast.txt"

main : IO ⊤
main =
  let markdownDoc = record { blocks = [] ; meta = "" }
      payload = astExportString roadmapDoc markdownDoc
  in writeFile outputPath payload

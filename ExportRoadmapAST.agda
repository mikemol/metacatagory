{-# OPTIONS --without-K --cubical-compatible #-}

module ExportRoadmapAST where

open import Agda.Builtin.Unit
open import Agda.Builtin.IO
open import Plan.CIM.RoadmapExporter

main : IO ⊤
main =
  let pd = roadmapDoc
      md = pandocDocToMarkdown pd
  in exportASTs pd md "build/reports/roadmap-ast.txt"

{-# OPTIONS --without-K --cubical-compatible --guardedness #-}

-- | Protocol definitions for moving between Pandoc AST and CIM structures.
module Plan.CIM.PandocProtocols where

open import Plan.CIM.PandocAST
open import Plan.CIM.Utility
open import Plan.CIM.CHIPConformance

open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Primitive using (Level; lzero; lsuc)
open import Agda.Builtin.Nat using (Nat; _+_)

open import Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; makeBraidStep; makeBraidTrace; transformBlock; pandocDocToMarkdown)

------------------------------------------------------------------------
-- CHIP Enforcement: All block and document definitions use PhaseAmbiguity, TransformationSystem, CoherenceWitness
------------------------------------------------------------------------

-- CHIP-enforced block ambiguity and transformation system
blockAmb : PhaseAmbiguity Block MdBlock
blockAmb = record { valA = Para [] ; valB = transformBlock (Para []) ; phase = 0 }

blockTransSys : TransformationSystem Block MdBlock
blockTransSys = record { Step = BraidStep ; cost = λ _ → 1 }

blockCoherence : CoherenceWitness blockAmb blockTransSys
blockCoherence = record
  { proofPath = trans-step
      (makeBraidStep (PhaseAmbiguity.valA blockAmb) (PhaseAmbiguity.valB blockAmb) "block transform")
      refl-path
  ; metric = mkMetric 1 1
  }

-- CHIP-enforced document ambiguity and transformation system
docAmb : PhaseAmbiguity PandocDoc MarkdownDoc
docAmb = record
  { valA = record { blocks = [] ; meta = "" }
  ; valB = pandocDocToMarkdown (record { blocks = [] ; meta = "" })
  ; phase = 0
  }

docTransSys : TransformationSystem PandocDoc MarkdownDoc
docTransSys = record { Step = BraidTrace ; cost = λ _ → 1 }

docCoherence : CoherenceWitness docAmb docTransSys
docCoherence = record
  { proofPath = trans-step
      (makeBraidTrace (PandocDoc.blocks (PhaseAmbiguity.valA docAmb))
                      (map transformBlock (PandocDoc.blocks (PhaseAmbiguity.valA docAmb))))
      refl-path
  ; metric = mkMetric (countBlocks (PandocDoc.blocks (PhaseAmbiguity.valA docAmb)))
                     (countBlocks (PandocDoc.blocks (PhaseAmbiguity.valA docAmb)))
  }
  where
    countBlocks : List Block → Nat
    countBlocks [] = 0
    countBlocks (_ ∷ bs) = 1 + countBlocks bs

------------------------------------------------------------------------
-- End of PandocProtocols

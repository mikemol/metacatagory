{-# OPTIONS --without-K --cubical --guardedness #-}

-- | Protocol definitions for moving between Pandoc AST and CIM structures.
module Plan.CIM.PandocProtocols where

open import Plan.CIM.PandocAST
open import Plan.CIM.Utility
open import Plan.CIM.CHIPConformance

open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Primitive using (Level; lzero; lsuc)

------------------------------------------------------------------------
-- CHIP Enforcement: All block and document definitions use PhaseAmbiguity, TransformationSystem, CoherenceWitness
------------------------------------------------------------------------

-- CHIP-enforced block ambiguity and transformation system
blockAmb : PhaseAmbiguity Block MdBlock
blockAmb = record { valA = Para [] ; valB = MdPara [] ; phase = 0 }

blockTransSys : TransformationSystem Block MdBlock
blockTransSys = record { Step = String ; cost = λ s → 1 }

blockCoherence : CoherenceWitness blockAmb blockTransSys
blockCoherence = record { proofPath = refl-path ; metric = record { magnitude = 1 } }

-- CHIP-enforced document ambiguity and transformation system
docAmb : PhaseAmbiguity PandocDoc MarkdownDoc
docAmb = record { valA = record { blocks = [] ; meta = "" } ; valB = record { blocks = [] ; meta = "" } ; phase = 0 }

docTransSys : TransformationSystem PandocDoc MarkdownDoc
docTransSys = record { Step = String ; cost = λ s → 1 }

docCoherence : CoherenceWitness docAmb docTransSys
docCoherence = record { proofPath = refl-path ; metric = record { magnitude = 1 } }

------------------------------------------------------------------------
-- End of PandocProtocols

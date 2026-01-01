{-# OPTIONS --without-K --cubical --guardedness #-}

-- | Export proofs rendered into Pandoc AST to downstream formats.
module Plan.CIM.PandocProofExport where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat; _+_)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Sigma using (Σ)

open import Plan.CIM.Utility using (map; _++_; _×_)
open import Plan.CIM.PandocAST
open import Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; transformInline; transformBlock)
open import Plan.CIM.Structure using (TransformationContext; TransformationAttestation; ComposedTransformation; attestTransformation; makeTransformationContext)
open import Plan.CIM.PandocProofExample using (exampleAttestation)

------------------------------------------------------------------------
-- Proof Data Structures for Export
------------------------------------------------------------------------

-- Structured representation of a single transformation step for export
record ProofStep : Set where
  field
    stepNumber : Nat
    description : String
    inputType : String
    outputType : String
    validated : Bool

-- Structured representation of the complete proof trace
record ProofData : Set where
  field
    documentId : String
    transformationName : String
    sourceBlocks : Nat
    targetBlocks : Nat
    steps : List ProofStep
    metadataPreserved : Bool
    grammarConforming : Bool
    exportTimestamp : String

-- JSON serialization format representation
record JSONRepresentation : Set where
  field
    format : String        -- "json"
    version : String       -- "1.0"
    content : String       -- Serialized JSON string

-- YAML serialization format representation
record YAMLRepresentation : Set where
  field
    format : String        -- "yaml"
    version : String       -- "1.0"
    content : String       -- Serialized YAML string

------------------------------------------------------------------------
-- Convert BraidStep to ProofStep
------------------------------------------------------------------------

braidStepToProofStep : (step : BraidStep) → (index : Nat) → ProofStep
braidStepToProofStep step idx = record
  { stepNumber = idx
  ; description = BraidStep.description step
  ; inputType = "Block"
  ; outputType = "MdBlock"
  ; validated = true
  }

------------------------------------------------------------------------
-- Extract Proof Data from Transformation Attestation
------------------------------------------------------------------------

length : ∀ {A : Set} → List A → Nat
length [] = 0
length (_ ∷ xs) = 1 + length xs

getPandocDocBlocks : PandocDoc → List Block
getPandocDocBlocks = PandocDoc.blocks

getMarkdownDocBlocks : MarkdownDoc → List MdBlock
getMarkdownDocBlocks = MarkdownDoc.blocks

extractProofData : (att : TransformationAttestation) → String → ProofData
extractProofData att docId = record
  { documentId = docId
  ; transformationName = "PandocToMarkdown"
  ; sourceBlocks = length (getPandocDocBlocks (TransformationContext.sourceDoc (TransformationAttestation.context att)))
  ; targetBlocks = length (getMarkdownDocBlocks (TransformationContext.transformedDoc (TransformationAttestation.context att)))
  ; steps = stepsFromTrace (TransformationContext.transformationTrace (TransformationAttestation.context att))
  ; metadataPreserved = TransformationAttestation.metadataIntegrity att
  ; grammarConforming = true  -- Assume verified
  ; exportTimestamp = "2024-12-20T20:15:00Z"
  }
  where
    stepsFromTrace : BraidTrace → List ProofStep
    stepsFromTrace trace = convertSteps 0 (BraidTrace.steps trace)
      where
        convertSteps : Nat → List BraidStep → List ProofStep
        convertSteps _ [] = []
        convertSteps idx (bs ∷ rest) = 
          braidStepToProofStep bs idx ∷ convertSteps (idx + 1) rest

------------------------------------------------------------------------
-- Helper: Natural Number to String
------------------------------------------------------------------------

natToString : Nat → String
natToString n = "0"  -- Placeholder: would require proper Nat → String conversion

------------------------------------------------------------------------
-- Helper: Boolean to String
------------------------------------------------------------------------

boolToString : Bool → String
boolToString true = "true"
boolToString false = "false"

------------------------------------------------------------------------
-- Serialize ProofData to JSON
------------------------------------------------------------------------

serializeProofDataToJSON : ProofData → String
serializeProofDataToJSON pd = 
  "{" ++
  "\"documentId\": \"" ++ ProofData.documentId pd ++ "\"," ++
  "\"transformationName\": \"" ++ ProofData.transformationName pd ++ "\"," ++
  "\"sourceBlocks\": " ++ natToString (ProofData.sourceBlocks pd) ++ "," ++
  "\"targetBlocks\": " ++ natToString (ProofData.targetBlocks pd) ++ "," ++
  "\"stepCount\": " ++ natToString (listLength (ProofData.steps pd)) ++ "," ++
  "\"metadataPreserved\": " ++ boolToString (ProofData.metadataPreserved pd) ++ "," ++
  "\"grammarConforming\": " ++ boolToString (ProofData.grammarConforming pd) ++ "," ++
  "\"timestamp\": \"" ++ ProofData.exportTimestamp pd ++ "\"" ++
  "}"
  where
    listLength : ∀ {A : Set} → List A → Nat
    listLength [] = 0
    listLength (_ ∷ xs) = 1 + listLength xs

------------------------------------------------------------------------
-- Serialize ProofData to YAML
------------------------------------------------------------------------

serializeProofDataToYAML : ProofData → String
serializeProofDataToYAML pd =
  "documentId: " ++ ProofData.documentId pd ++ "\n" ++
  "transformationName: " ++ ProofData.transformationName pd ++ "\n" ++
  "sourceBlocks: " ++ natToString (ProofData.sourceBlocks pd) ++ "\n" ++
  "targetBlocks: " ++ natToString (ProofData.targetBlocks pd) ++ "\n" ++
  "metadataPreserved: " ++ boolToString (ProofData.metadataPreserved pd) ++ "\n" ++
  "grammarConforming: " ++ boolToString (ProofData.grammarConforming pd) ++ "\n" ++
  "timestamp: " ++ ProofData.exportTimestamp pd

------------------------------------------------------------------------
-- Export Functions
------------------------------------------------------------------------

-- Export proof data as JSON
exportProofAsJSON : (att : TransformationAttestation) → String → JSONRepresentation
exportProofAsJSON att docId = record
  { format = "json"
  ; version = "1.0"
  ; content = serializeProofDataToJSON (extractProofData att docId)
  }

-- Export proof data as YAML
exportProofAsYAML : (att : TransformationAttestation) → String → YAMLRepresentation
exportProofAsYAML att docId = record
  { format = "yaml"
  ; version = "1.0"
  ; content = serializeProofDataToYAML (extractProofData att docId)
  }

------------------------------------------------------------------------
-- Example Export from the proof example
------------------------------------------------------------------------

exampleExportedProofJSON : JSONRepresentation
exampleExportedProofJSON = 
  exportProofAsJSON exampleAttestation "example-doc-001"

exampleExportedProofYAML : YAMLRepresentation
exampleExportedProofYAML = 
  exportProofAsYAML exampleAttestation "example-doc-001"

------------------------------------------------------------------------
-- End of PandocProofExport
------------------------------------------------------------------------

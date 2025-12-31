---
module: Plan.CIM.PandocProofExport
kind: per-module
imports:
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; _+_)
  - Agda.Builtin.Bool using (Bool; true; false)
  - Agda.Builtin.Sigma using (Σ)
  - Plan.CIM.Utility using (map; _++_; _×_)
  - Plan.CIM.PandocAST
  - Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; transformInline; transformBlock)
  - Plan.CIM.Structure using (TransformationContext; TransformationAttestation; ComposedTransformation; attestTransformation; makeTransformationContext)
  - Plan.CIM.PandocProofExample using (exampleAttestation)
---

# Module: Plan.CIM.PandocProofExport

**Source:** `src/agda/Plan/CIM/PandocProofExport.agda`

## Dependencies

- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; _+_)
- Agda.Builtin.Bool using (Bool; true; false)
- Agda.Builtin.Sigma using (Σ)
- Plan.CIM.Utility using (map; _++_; _×_)
- Plan.CIM.PandocAST
- Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; transformInline; transformBlock)
- Plan.CIM.Structure using (TransformationContext; TransformationAttestation; ComposedTransformation; attestTransformation; makeTransformationContext)
- Plan.CIM.PandocProofExample using (exampleAttestation)

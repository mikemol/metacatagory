---
module: Plan.CIM.CHIPRecomposed
kind: per-module
imports:
  - Agda.Primitive using (Level)
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.String using (String)
  - Agda.Builtin.Nat using (Nat; _+_)
  - Plan.CIM.Utility using (Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric; Path; refl-path; trans-step; map; _++_)
  - Plan.CIM.PandocAST
  - Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; transformInline; transformBlock)
  - Plan.CIM.PandocProtocols using (blockAmb; blockTransSys; docAmb; docTransSys)
  - Plan.CIM.GrammarBridge using (GrammarExpr; blockToGrammar; inlineToGrammar)
---

# Module: Plan.CIM.CHIPRecomposed

**Source:** `src/agda/Plan/CIM/CHIPRecomposed.agda`

## Dependencies

- Agda.Primitive using (Level)
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.String using (String)
- Agda.Builtin.Nat using (Nat; _+_)
- Plan.CIM.Utility using (Ambiguity; TransformationSystem; CoherenceWitness; EmergentMetric; Path; refl-path; trans-step; map; _++_)
- Plan.CIM.PandocAST
- Plan.CIM.PandocToMarkdown using (BraidStep; BraidTrace; transformInline; transformBlock)
- Plan.CIM.PandocProtocols using (blockAmb; blockTransSys; docAmb; docTransSys)
- Plan.CIM.GrammarBridge using (GrammarExpr; blockToGrammar; inlineToGrammar)

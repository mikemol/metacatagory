---
module: Plan.CIM.CHIPConformance
kind: per-module
imports:
  - Agda.Builtin.String
  - Agda.Builtin.List using (List; []; _∷_)
  - Agda.Builtin.Unit
  - Agda.Builtin.Nat using (Nat; _+_)
  - Agda.Builtin.Equality using (_≡_)
  - Agda.Primitive using (Level; lzero; lsuc)
  - Agda.Builtin.Sigma using (Σ; _,_)
  - Plan.CIM.Utility using (PhaseAmbiguity; TransformationSystem; EmergentMetric; Path; CoherenceWitness; BraidedInheritanceFunctor; BraidedSPPF; map; _×_; packed-node)
  - Agda.Builtin.Nat using (Nat)
---

# Module: Plan.CIM.CHIPConformance

**Source:** `src/agda/Plan/CIM/CHIPConformance.agda`

## Dependencies

- Agda.Builtin.String
- Agda.Builtin.List using (List; []; _∷_)
- Agda.Builtin.Unit
- Agda.Builtin.Nat using (Nat; _+_)
- Agda.Builtin.Equality using (_≡_)
- Agda.Primitive using (Level; lzero; lsuc)
- Agda.Builtin.Sigma using (Σ; _,_)
- Plan.CIM.Utility using (PhaseAmbiguity; TransformationSystem; EmergentMetric; Path; CoherenceWitness; BraidedInheritanceFunctor; BraidedSPPF; map; _×_; packed-node)
- Agda.Builtin.Nat using (Nat)

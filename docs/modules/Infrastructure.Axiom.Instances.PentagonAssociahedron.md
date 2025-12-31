---
module: Infrastructure.Axiom.Instances.PentagonAssociahedron
kind: per-module
imports:
  - Agda.Primitive using (Level; _⊔_)
  - Agda.Builtin.Nat using (Nat)
  - Infrastructure.Arity.BinTree using (BinTree)
  - Infrastructure.Polytopes.Associahedron using (Path; _++_; ++-assoc)
  - Infrastructure.Polytopes.Associahedron.Triangulation using (PentagonFan; pentagon-from-fan)
  - Infrastructure.Axiom.Adequacy using (PathAlgebra)
  - Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)
---

# Module: Infrastructure.Axiom.Instances.PentagonAssociahedron

**Source:** `src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda`

## Dependencies

- Agda.Primitive using (Level; _⊔_)
- Agda.Builtin.Nat using (Nat)
- Infrastructure.Arity.BinTree using (BinTree)
- Infrastructure.Polytopes.Associahedron using (Path; _++_; ++-assoc)
- Infrastructure.Polytopes.Associahedron.Triangulation using (PentagonFan; pentagon-from-fan)
- Infrastructure.Axiom.Adequacy using (PathAlgebra)
- Infrastructure.Axiom.Instance using (AxiomInstance; FramedFace)

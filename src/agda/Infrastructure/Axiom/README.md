# Infrastructure.Axiom (Adequacy Layer)

This directory formalizes the project invariant:

> Axioms are not postulates. They are *template parameters* whose associated
> theorems must be *constructible* from the generators provided.

Core idea:

- A **Face** is an "axiom-to-be": two parallel boundary paths with same endpoints.
- A **Solver** is a theorem-generator that fills a Face from a **Kit**.
- **Adequacy** is the property that the kit *suffices* to generate the required fills.

Concrete kits (triangles, whiskering, naturality, etc.) live in higher Infrastructure modules
(e.g. Polytopes/Associahedron/Triangulation.agda, Braiding/HexagonTriangulation.agda) and should
instantiate `Solver`.

# The Source Lattice: An Atlas

> **Coordinate System:** Source Root
> **Parent Index:** $(7, 3)$
> **Context:** Physical manifestation of the Coherence Induction Metacategory.

This directory contains the reified proof terms of the system. It is organized as a **Graded Vector Space**, where each directory represents a distinct dimension of the solution topology.

-----

## 1. Dimension 0: The Operational Kernel (`Core/`)

* **Role:** The foundational 0-Cells and 1-Cells of the system.
* **Key Artifacts:**
    * [GodelBoundary.agda](Core/GodelBoundary.agda): The reified limit of the system (Technical Debt type).
    * [Phase.agda](Core/Phase.agda): The definition of the 1-Cell (Transformation).
    * [UniversalProperties.agda](Core/UniversalProperties.agda): The mandates for logical consistency.

## 2. Dimension 1: The Algebraic Content (`Algebra/`)

* **Role:** The 2-Cell content that fills the operational units.
* **Sub-Manifolds:**
    * **Groups:** [Structure.agda](Algebra/Groups/Structure.agda), [Abelian.agda](Algebra/Groups/Abelian.agda)
    * **Rings:** [Basic.agda](Algebra/Rings/Basic.agda)
    * **Fields:** [Advanced.agda](Algebra/Fields/Advanced.agda)
    * **Enrichment:** [Enrichment.agda](Algebra/Enrichment.agda) (Category Theory bindings).

## 3. Dimension 2: The Pedagogical Lattice (`Chapter*/`)

* **Role:** The progressive expansion of the solution space. These directories track the "Game State" of the repository's evolution.
* **Progression:**
    * **Level 1:** [Level1Index.agda](Chapter1/Level1Index.agda) - Foundations.
    * **Level 2:** [Level2Index.agda](Chapter2/Level2Index.agda) - Intermediate Structures.
    * **Level 3:** [Level3Index.agda](Chapter3/Level3Index.agda) - Advanced Braiding.

## 4. Dimension 3: The Boundary Constraints (`Tests/`)

* **Role:** The "Walls" of the solution space. These are **Checklists** that must be satisfied.
* **Topology:**
    * **Core Constraints:** [CoreUniversalPropertiesChecklist.agda](Tests/CoreUniversalPropertiesChecklist.agda)
    * **Algebra Constraints:** [GroupsAbelianChecklist.agda](Tests/GroupsAbelianChecklist.agda)
    * **Advanced Theory:** [ToposTheoryChecklist.agda](Tests/ToposTheoryChecklist.agda)

## 5. Navigation Axioms (How to Read)

1.  **Indices are Truth:** If you are lost, find the `Index.agda` file in your current directory. It braids the local files into a coherent thread.
2.  **Types are Definitions:** To understand a concept, look at its `data` or `record` definition, not just its implementation.
3.  **Holes are Tasks:** Any `{! !}` (hole) you encounter is a request for contribution (see `GodelBoundary`).
# The Group Manifold: A Local Atlas

> **Coordinate System:** Dimension 2 (Algebraic Content)
> **Parent Index:** $(8, 1)$
> **Context:** Reified Algebraic Structures within the Lattice.

This directory contains the formal definitions and proofs regarding **Group Theory**, treated here as the study of **Symmetrical 1-Types** (Groupoids with a single object).

***

## 1. The Structural Core

*   **[Structure.agda](Structure.agda):** The axiomatic definition of a Group Object.
    *   *Universal Property:* A Monoid with Inverses.
    *   *HoTT Interpretation:* The fundamental group of a space.

## 2. Sub-Spaces & Variations

*   **[Abelian.agda](Abelian.agda):** The Commutative Sub-Manifold.
    *   *Constraint:* $a \cdot b \equiv b \cdot a$ (Path Commutativity).
*   **[Free.agda](Free.agda):** The Free Group Construction.
    *   *Adjunction:* Left adjoint to the Forgetful Functor.

## 3. Integration

*   **[Basic.agda](Basic.agda):** Foundational lemmas and basic theorems braided into the core logic.

## 4. Navigation

To ascend back to the Algebraic Root, see [../Index.agda](../Index.agda).

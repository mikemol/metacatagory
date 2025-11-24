# The Constraint Manifold: Specification Boundaries

> **Coordinate System:** Dimension 3 (Verification)
> **Parent Index:** $(8, 3)$
> **Context:** The Topological Walls of the Solution Space.

This directory contains the **Checklists** and **Witnesses** that bound the system. In the Coherence Induction Metacategory, a "Test" is not a mere assertion; it is a **Homological Witness** verifying that a specific region of the lattice is contractible (consistent and hole-free).

***

## 1. Foundational Constraints (The 0-Cell Boundary)

These checklists verify the integrity of the Operational Kernel.

*   **[CoreUniversalPropertiesChecklist.agda](CoreUniversalPropertiesChecklist.agda):** Verifies that 0-Morphisms satisfy the axioms (Product, Coproduct, Terminal).
*   **[GodelBoundaryTests.agda](GodelBoundaryTests.agda):** Probes the limits of the `TechnicalDebt` type.
*   **[WitnessConstructionTests.agda](WitnessConstructionTests.agda):** Verifies the extraction of constructive evidence.

## 2. Algebraic Constraints (The 2-Cell Boundary)

These verify the correctness of the Algebraic structures.

*   **Group Theory:** [GroupsStructureChecklist.agda](GroupsStructureChecklist.agda), [GroupsAbelianChecklist.agda](GroupsAbelianChecklist.agda), [GroupsFreeChecklist.agda](GroupsFreeChecklist.agda).
*   **Ring Theory:** [RingsBasicChecklist.agda](RingsBasicChecklist.agda).
*   **Field Theory:** [FieldsBasicChecklist.agda](FieldsBasicChecklist.agda), [PolynomialFieldExtensionsChecklist.agda](PolynomialFieldExtensionsChecklist.agda).
*   **Vector Spaces:** [VectorSpaceChecklist.agda](VectorSpaceChecklist.agda).

## 3. Category Theoretic Constraints (The n-Morphism Boundary)

These verify the high-level categorical machinery.

*   **[YonedaChecklist.agda](YonedaChecklist.agda):** Verifies the Yoneda Embedding (The preservation of structure).
*   **[ToposTheoryChecklist.agda](ToposTheoryChecklist.agda):** Verifies the logic of the Topos (Subobject Classifiers).
*   **[FunctorPropertiesChecklist.agda](FunctorPropertiesChecklist.agda):** Verifies 1-Cell mapping properties.
*   **[MonadAdjunctionChecklist.agda](MonadAdjunctionChecklist.agda):** Verifies the resolution of computational effects.

## 4. System & Performance Constraints

These verify the computational properties of the lattice implementation.

*   **[SerializationTests.agda](SerializationTests.agda):** Ensures the lattice can be persisted.
*   **[PerformanceBoundaryTests.agda](PerformanceBoundaryTests.agda):** Measures the computational cost of the metric functor.
*   **[ErrorAsSpecificationTests.agda](ErrorAsSpecificationTests.agda):** Verifies that errors are treated as First-Class Types.

## 5. How to Add a New Test or Checklist

1.  Create a new `.agda` file in this directory, following the naming convention `<Feature>Checklist.agda` or `<Feature>Tests.agda`.
2.  At the top of the file, document its purpose, expected outcomes, and any relevant references.
3.  Import the relevant modules and define your assertions or witnesses.
4.  Add your new file to the appropriate section above and update this README.
5.  Run `make check-tests` to verify your additions.
6.  If your test is a checklist, ensure it covers all relevant cases and edge conditions.

## 6. Interpreting Failures and Troubleshooting

*   **Type Errors:** Usually indicate a mismatch in expected structure or a missing witness. Check imports and type signatures.
*   **Unsolved Metavariables (`{! !}`):** Indicates incomplete proofs or missing constructive evidence. Fill in the holes or defer with a TODO.
*   **Checklist Failures:** Review the assertion and ensure all dependencies are satisfied. Use smaller, isolated tests to debug.
*   **Performance Issues:** If a test is slow, consider refactoring for efficiency or splitting into smaller units.

## 7. Best Practices

*   Keep checklists focused and modular.
*   Document the rationale for each test and checklist.
*   Use descriptive names for assertions and witnesses.
*   Regularly run `make check-tests` and review generated reports.

## 8. How to Verify

To run the full constraint solver across the manifold:

```bash
make test_report
```

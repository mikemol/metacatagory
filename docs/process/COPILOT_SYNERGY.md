# Copilot Synergy SPPF Hierarchy

## Onboarding and Architectural Review

* **Review Architecture Document**

  * Path: `ARCHITECTURE.md`   \* Required for all contributors and LLMs before making changes or proposals.   \* Subtasks:     \* Study the SPPF-modeled architecture nodes for composability, recursive revisiting, and roadmap-driven integration.     \* Cross-reference onboarding tips and examples for each architectural pattern.     \* Ensure new modules, refactors, and proposals align with architectural principles and update the roadmap as needed.     \* Reference major roadmap nodes in `Utility.agda` for recursive revisiting: `exampleUnifiedTopologicalParserRoadmap`, `exampleDimensionalReliefRoadmap`, `examplePolytopeManifestRoadmap`, `exampleElasticityOfMeaningRoadmap`.

## Root Instruction Node: Maintain Synergy with Roadmap

* Path: `src/agda/Plan/CIM/Utility.agda`

* Subtasks:

  * Parse roadmap object and extract actionable steps.   \* Identify status, dependencies, and rationale for each step.   \* Inherit context from previous roadmap updates.

* Reference `ARCHITECTURE.md` for architectural context and composability patterns.

* Ensure roadmap steps are consistent with architectural nodes and onboarding guidance.

* Cross-link roadmap steps to major nodes in `Utility.agda` for recursive revisiting and composability.

* Branches:

  * Decomposition: Use roadmap to break down tasks into composable subtasks.   \* Integration: Reference roadmap for protocol record updates and module connections.   \* Inheritance: Propagate context and rationale from roadmap to new actions.

* Use onboarding tips and examples from `ARCHITECTURE.md` to guide decomposition and integration.

* Subtasks:

  * Update roadmap status fields as tasks are completed or advanced.   \* Annotate rationale and context for each status change.   \* Branch: Link completed steps to next actionable nodes.

* Subtasks:

  * When proposing new actions, cite relevant roadmap nodes and branches.   \* Inherit dependencies and context from referenced steps.   \* Compose new instructions as extensions or refinements of existing roadmap paths.

## Python Type Hinting Guidance

**Use built-in generics for containers, and `Any` from `typing` for unconstrained types:**

For modern Python (3.9+), prefer built-in types for containers:

    def foo(bar: dict[str, int], baz: list[str]) -> set[int]:         ...

For unconstrained types, use `Any` from the `typing` module:

    from typing import Any     def bar(data: dict[str, Any]) -> None:         ...

Avoid deprecated `Dict`, `List`, `Set`, and `Tuple` from the `typing` module unless you need compatibility with older Python versions or advanced features (e.g., `TypeVar`).

This prevents 'undefined name' errors and ensures your code is future-proof and PEP-compliant.

* Confirm proposals are consistent with SPPF nodes and onboarding guidance in `ARCHITECTURE.md`.

## Inheritance and Multipath Structure

* Architectural review and onboarding are recursive: revisit `ARCHITECTURE.md` and roadmap as new patterns and modules are introduced. Cross-reference major roadmap nodes in `Utility.agda` for context and traceability.

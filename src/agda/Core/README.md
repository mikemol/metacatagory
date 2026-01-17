# Core

Foundational constructions used across Plan.CIM, Algebra, and Tests.
This directory hosts the phase/category backbone and general-purpose
infrastructure that other modules import.

Primary entry points:
- `Phase.agda` / `PhaseCategory.agda` for phase theory primitives.
- `BraidTree.agda` for braid composition utilities.
- `UniversalProperties.agda` / `UniversalConstructionKernel.agda` for categorical scaffolding.
- `Yoneda.agda` for representable functor patterns.

Status:
- Some modules still carry deferred items (postulates/TODOs). See
  `docs/status/DEFERRED-TRACKING.md` for the current ledger.

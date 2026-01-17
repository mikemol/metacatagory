# Meta-Index: Plan.CIM.CHIPConformance

## Scope

- Protocol conformance utilities for CHIP, including graded metrics and SPPF nodes.

## Key elements

- `GradedVectorSpace` record (dimensions, `EmergentMetric`).
- Helpers: `mapGVS`, `sumMetric`, `composeBraids`, `makeSPPFNode`.
- `composeBraids` composes braid inheritance and aggregates metrics.
- Uses PhaseAmbiguity, TransformationSystem, BraidedSPPF utilities.

## Dependencies

- `Plan.CIM.Utility` (metrics, braids, SPPF helpers) and CHIP protocol definitions.

## Update triggers

- New metrics, braid functors, or SPPF node forms.
- Protocol boundary changes in roadmap/architecture.
- Parsing or category-theory refinements that affect conformance.

## Technical debt / status

- Track protocol and SPPF evolution in deferred-items.md and ROADMAP.md.

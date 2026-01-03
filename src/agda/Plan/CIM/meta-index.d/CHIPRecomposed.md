# Meta-Index: Plan.CIM.CHIPRecomposed

## Scope

- Block-level recomposition and tracing for CHIP protocols.

## Key elements

- `recomposeBlockCoherence`, `composeBraidTraces`, `buildSteps`.
- Imports Utility (Ambiguity, TransformationSystem, CoherenceWitness, metrics, path ops) and document adapters (PandocAST/ToMarkdown/Protocols/GrammarBridge).

## Dependencies

- Transformation and braid utilities from `Plan.CIM.Utility`; document adapters for block handling.

## Update triggers

- New block types or protocol transformations.
- Changes to coherence/braid protocols in roadmap/architecture.

## Technical debt / status

- Track protocol/trace evolution in deferred-items.md and ROADMAP.md.

# Meta-Index: Core.Phase

## Scope

- Compositional transformations between states; phases as first-class morphisms.

## Key elements

- `Phase` record with helpers `mkPhase`, `idPhase`, `constPhase`.
- Application operator `_$ₚ_`.

## Dependencies

- Infrastructure.Universe and Infrastructure.Coherence.Path2.

## Update triggers

- Changes to phase composition rules or helper constructors.
- Adjustments to infrastructure equality/universe layers.
- Roadmap items that alter protocol pipelines using phases.

## Technical debt / status

- Track any refactors or new infrastructure tie-ins in deferred-items.md and ROADMAP.md.

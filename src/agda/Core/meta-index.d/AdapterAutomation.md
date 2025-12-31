# Meta-Index: Core.AdapterAutomation

## Scope

* Categorical adapter patterns (standard/enhanced/legacy wrappers) and helpers for protocol migration.

## Key elements

* `HasCategorical` record; adapter variants `StandardAdapter`, `EnhancedAdapter`, `LegacyAdapterWrapper`.
* Constructors `mkStandardAdapter`, `mkEnhancedAdapter`, `upgradeLegacyAdapter`; check `isFilledStandard`.

## Dependencies

* Infrastructure.Universe and Infrastructure.Coherence.Path2 (universe alias, equality/whiskering).

## Update triggers

* Changes to adapter records or construction helpers.
* Migration or categorical interface updates in Core.
* Shifts in infrastructure equality/universe layers.

## Technical debt / status

* Universe polymorphism not yet generalized to `Setℓ`; track in deferred-items.md and ROADMAP.md.

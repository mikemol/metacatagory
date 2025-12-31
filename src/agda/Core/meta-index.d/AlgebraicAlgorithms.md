# Meta-Index: Core.AlgebraicAlgorithms

## Scope

* Shared algebraic decision/structure records for computational algebra workflows.

## Key elements

* Decision type `Dec`.
* Packed structures: `packedMagmaBase/SemigroupBase/MonoidBase`, with `…Ext` chains.

## Dependencies

* Infrastructure.Universe and Infrastructure.Coherence.Path2.

## Update triggers

* Changes to packed structure definitions or decision types.
* New algebraic algorithm requirements in Core or Plan/CIM.
* Infrastructure shifts (universe/equality).

## Technical debt / status

* Universe polymorphism not yet generalized to `Setℓ`; track in deferred-items.md and ROADMAP.md.

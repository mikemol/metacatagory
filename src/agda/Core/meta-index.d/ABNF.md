# Meta-Index: Core.ABNF

## Scope

* Formal ABNF syntax (RFC 5234) as an inductive AST for protocol grammars.

## Key elements

* `ABNF` data type; example `abnfDigit`.
* List helpers `_++_`, `concatMap` for grammar assembly.

## Dependencies

* Infrastructure.Universe (universe alias) and Infrastructure.Coherence.Path2 (whiskering/equality).

## Update triggers

* Grammar constructor or combinator changes.
* Shifts in infrastructure equality/universe layers.
* Roadmap items that adjust parsing or grammar transformation workflows.

## Technical debt / status

* Universe polymorphism not yet generalized to `Setℓ`; track in deferred-items.md and ROADMAP.md.

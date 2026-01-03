# Meta-Index: Plan.CIM.Ambiguity

## Scope

- Weighted ambiguity modeling with provenance and external tension oracles.

## Key elements

- `WeightedOption` record (value, weight, provenance).
- `Ambiguity` data (determinate, superposition, conflict).
- Parameters: `TensionOracle` FFI hook; `LeftIdentityProof` (monad law).

## Dependencies

- Plan.CIM.Utility; any consuming FFI surface for tension.

## Update triggers

- New ambiguity forms or changes to weighted option structure.
- FFI oracle adjustments or law-proof requirements.
- Roadmap/architecture updates for ambiguity handling.

## Technical debt / status

- FFI oracle integration is deferred/parameterized; document integrations in deferred-items.md and ROADMAP.md.

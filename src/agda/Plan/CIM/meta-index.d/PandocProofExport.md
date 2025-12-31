# Meta-Index: Plan.CIM.PandocProofExport

## Scope

* Proof export utilities targeting Pandoc/Markdown.

## Key elements

* Transformations from proof structures to document AST; helpers for formatting proofs.

## Dependencies

* `Plan.CIM.PandocAST`, rendering modules, and proof data structures.

## Update triggers

* Schema changes in proof data or document layout.
* New proof features requiring export hooks.

## Technical debt / status

* Track export coverage and formatting debt in deferred-items.md and ROADMAP.md.

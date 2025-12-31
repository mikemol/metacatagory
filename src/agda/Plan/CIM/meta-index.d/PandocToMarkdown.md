# Meta-Index: Plan.CIM.PandocToMarkdown

## Scope

* Conversion from Pandoc AST to Markdown text.

## Key elements

* AST traversal and rendering helpers that target Markdown output.

## Dependencies

* `Plan.CIM.PandocAST` and rendering modules.

## Update triggers

* New AST variants or Markdown dialect adjustments.
* Downstream pipeline changes requiring different output shape.

## Technical debt / status

* Track conversion edge cases and dialect differences in deferred-items.md and ROADMAP.md.

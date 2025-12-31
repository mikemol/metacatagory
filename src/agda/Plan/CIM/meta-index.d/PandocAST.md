# Meta-Index: Plan.CIM.PandocAST

## Scope

* Pandoc/Markdown AST used for document normalization and transformation.

## Key elements

* `Inline` variants (Str, Emph, Strong, Code, Space, SoftBreak, LineBreak, Math, RawInline, Link, Image, Note).
* `Block` variants (Para, Plain, CodeBlock, RawBlock, Header, BlockQuote, OrderedList, BulletList, HorizontalRule, Table, Null).

## Dependencies

* Pandoc/Markdown document model references; consumes Plan/CIM utilities.

## Update triggers

* Adding/removing block or inline variants.
* Document model changes affecting normalization or rendering.
* Roadmap/architecture updates for document processing.

## Technical debt / status

* Track AST evolution and extensibility requirements in deferred-items.md and ROADMAP.md.

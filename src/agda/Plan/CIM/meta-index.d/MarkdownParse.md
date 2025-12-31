# Meta-Index: Plan.CIM.MarkdownParse

## Scope

* Parsing Markdown into project AST forms.

## Key elements

* Parsers for Markdown constructs feeding `Plan.CIM.PandocAST`.

## Dependencies

* Markdown grammar helpers and AST definitions.

## Update triggers

* Grammar changes or new block/inline forms.
* Downstream normalization or rendering shifts that require parser updates.

## Technical debt / status

* Track unsupported constructs and TODOs in deferred-items.md and ROADMAP.md.

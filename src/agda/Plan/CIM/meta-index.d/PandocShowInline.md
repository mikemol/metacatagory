# Meta-Index: Plan.CIM.PandocShowInline

## Scope

- Rendering helpers for Pandoc `Inline` and `MdInline` lists.

## Key elements

- `showInlines`, `showMdInlines`; string append helper `_++_`.

## Dependencies

- `Plan.CIM.PandocAST` and related normalization/rendering modules.

## Update triggers

- New inline variants or rendering patterns.
- Changes in document model or downstream formatting expectations.

## Technical debt / status

- Track rendering gaps or dialect-specific tweaks in deferred-items.md and ROADMAP.md.

# Meta-Index: Plan.CIM.PandocShowBlock

## Scope

- Rendering helpers for Pandoc `Block` values and lists.

## Key elements

- `showBlock`, `showBlocks`, `showBlockLists`; numeric helpers `showNat/showNat'`; utilities `index`, `digits`, `_++_`.

## Dependencies

- `Plan.CIM.PandocAST` and related normalization/rendering modules.

## Update triggers

- New block variants or rendering patterns.
- Changes in document model or downstream formatting expectations.

## Technical debt / status

- Track rendering gaps or dialect-specific tweaks in deferred-items.md and ROADMAP.md.

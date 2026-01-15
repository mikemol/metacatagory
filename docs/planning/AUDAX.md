
---

# AUDAX Unified Doc Algebra: Design Draft

## Rationale

The unified AUDAX Doc algebra should:

* Support all structures needed by current pipelines (headings, fields, tables, lists, code, blockquotes, links, images, etc.)
* Be extensible for future needs (e.g., HTML, LaTeX, semantic annotations)
* Allow pure, compositional renderers for Markdown, JSON, HTML, etc.
* Enable structural comparison and transformation between document trees.

## Proposed Core Types (Agda-style pseudocode)

```agda
data AUDAXInline : Set where
  Str        : String → AUDAXInline
  Emph       : List AUDAXInline → AUDAXInline
  Strong     : List AUDAXInline → AUDAXInline
  Code       : String → AUDAXInline
  Link       : List AUDAXInline → String → AUDAXInline
  Image      : List AUDAXInline → String → AUDAXInline
  Space      : AUDAXInline
  Break      : AUDAXInline

data AUDAXBlock : Set where
  Para        : List AUDAXInline → AUDAXBlock
  Header      : Nat → List AUDAXInline → AUDAXBlock
  CodeBlock   : String → AUDAXBlock
  BlockQuote  : List AUDAXBlock → AUDAXBlock
  ListBlock   : List (List AUDAXBlock) → AUDAXBlock
  Table       : List String → List (List AUDAXInline) → AUDAXBlock
  Field       : String → String → AUDAXBlock
  Raw         : String → AUDAXBlock
  Null        : AUDAXBlock

record AUDAXDoc : Set where
  field
    blocks : List AUDAXBlock
    meta   : String
```

## Design Notes

* This structure unifies the expressiveness of PandocAST with the simplicity of the custom Doc types.
* `Field` and `Table` are included to support reporting use cases.
* `meta` field allows for extensible metadata (e.g., provenance, tags).
* Renderers can be written to target Markdown, JSON, HTML, etc., by pattern-matching on these constructors.
* Future extensions (e.g., semantic tags, cross-references) can be added as new constructors.

## Next Steps

* Review this draft for completeness and alignment with all current use cases.
* Prototype the AUDAXDoc type in Agda and implement a minimal Markdown renderer.
* Plan migration of one pipeline (deferred-items or priority) to use AUDAXDoc.

[//]: # (AUDAX Document Algebra Inventory)

# Document Algebra Inventory (AUDAX Step 2)

This section catalogs all current document/formatting algebras and their usages in the metacatagory project, as a foundation for unification under AUDAX.

## 1. PandocAST ([Plan.CIM.PandocAST.agda])

* **Types:** `Inline`, `Block`, `PandocDoc`, `MdInline`, `MdBlock`, `MarkdownDoc`
* **Purpose:** Rich, extensible document algebra for representing normalized documents (source and target forms).
* **Usages:**
  * Markdown parsing and normalization ([Plan.CIM.MarkdownParse.agda])
  * Transformation and rendering ([Plan.CIM.PandocToMarkdown.agda])
  * Intended as the basis for AUDAX unification.

## 2. DeferredItems Doc ([TechnicalDebt.DeferredItemsFormatting.agda])

* **Type:** `Doc` (with `Heading`, `Field`, `Table`, `Concat`)
* **Purpose:** Structured reporting for deferred-items pipeline (markdown, JSON renderers).
* **Usages:**
  * Deferred items report generation ([TechnicalDebt.DeferredItemsOrchestrationFFI.agda])
  * Markdown and JSON output for deferred-summary.

## 3. Priority Doc ([TechnicalDebt.PriorityFormatting.agda])

* **Type:** `Doc` (with `Heading`, `Field`, `Table`, `Concat`)
* **Purpose:** Structured reporting for priority strategy pipeline (markdown, JSON renderers).
* **Usages:**
  * Priority strategy report generation.
  * Markdown and JSON output for strategy profiles.

## 4. Renderer Functions

* **Markdown/JSON renderers** exist for each custom Doc type (see above modules).
* **PandocAST** has transformation and normalization functions (e.g., `pandocDocToMarkdown`).

## 5. Other Formatting/Reporting

* Some legacy or ad-hoc formatting may exist in scripts or older modules (to be identified during refactor).

---

**Next Steps:**

* Analyze structural differences and overlaps between PandocAST and custom Doc types.
* Propose a unified algebra (AUDAX) that subsumes all current use cases.
* Refactor at least one pipeline to use the unified algebra.

**References:**

* [Plan.CIM.PandocAST.agda](../../src/agda/Plan/CIM/PandocAST.agda)
* [TechnicalDebt.DeferredItemsFormatting.agda](../../src/agda/TechnicalDebt/DeferredItemsFormatting.agda)
* [TechnicalDebt.PriorityFormatting.agda](../../src/agda/TechnicalDebt/PriorityFormatting.agda)
* [Plan.CIM.PandocToMarkdown.agda](../../src/agda/Plan/CIM/PandocToMarkdown.agda)
* [Plan.CIM.MarkdownParse.agda](../../src/agda/Plan/CIM/MarkdownParse.agda)

# AUDAX: Agda Unified Document Algebra Initiative

## Scope and Goals

AUDAX aims to unify all document-producing and reporting pipelines in the metacatagory project under a single, extensible document algebra. This will:

* Eliminate ad-hoc formatting logic and duplicated Doc types.
* Enable rich, structured output (tables, lists, links, etc.) for all reporting and documentation.
* Make it easy to add new output formats (Markdown, HTML, LaTeX, JSON, etc.) by writing new renderers.
* Support structural comparison, transformation, and cohomology between document-producing modules.

## Objectives

1. Inventory and analyze all existing document/formatting algebras (PandocAST, custom Doc types, etc.).
2. Design a unified Doc algebra (likely based on or extending PandocAST) to serve as the common foundation.
3. Refactor at least one major pipeline (deferred-items or priority) to emit AUDAX Doc values.
4. Implement pure renderers from AUDAX Doc to Markdown, JSON, and optionally HTML.
5. Integrate AUDAX into the planning and work tracking infrastructure.

## Work Plan

See ROADMAP-INDEX.md and docs/status/DEFERRED-TRACKING.md for integration points and status. The following steps are tracked in the project todo list:

1. Define AUDAX scope and goals (this document)
2. Inventory existing document algebras
3. Design unified Doc algebra (AUDAX)
4. Refactor one pipeline to AUDAX
5. Implement renderers for AUDAX
6. Integrate AUDAX into planning infrastructure

## Rationale

A unified document algebra will:

* Increase maintainability and extensibility
* Enable richer, more consistent documentation and reporting
* Support advanced features like cross-pipeline cohomology and transformation
* Reduce technical debt from duplicated formatting logic

## References

* [Plan.CIM.PandocAST](../../src/agda/Plan/CIM/PandocAST.agda)
* [TechnicalDebt/DeferredItemsFormatting.agda](../../src/agda/TechnicalDebt/DeferredItemsFormatting.agda)
* [TechnicalDebt/PriorityFormatting.agda](../../src/agda/TechnicalDebt/PriorityFormatting.agda)
* [docs/planning/ROADMAP-INDEX.md](ROADMAP-INDEX.md)
* [docs/status/DEFERRED-TRACKING.md](../status/DEFERRED-TRACKING.md)

# EBNF-to-Agda Conventions (scalable pattern)

This repository encodes each EBNF production as a lightweight Agda record/enum and threads proofs through a unified, typed proof layer. The goal is to scale across chapters (`src/ebnf/1`, `2`, `3`, …) with minimal friction.

## Module layout

- `Metamodel.agda` — base syntactic layer (Identifier, Expression, etc.)
- `Core.agda` — stable public surface; re-exports `Metamodel` and the unified proof system from Chapter 1 (`Level1`). Import `Core` for downstream modules to avoid churn.
- `1/*.agda` — Chapter 1 translations. `1/Index.agda` re-exports chapter modules.
- `2/*.agda` — Chapter 2 translations. `2/Index.agda` aggregates.
- `3/*.agda` — Chapter 3 translations. `3/Index.agda` aggregates.

## Unified proof layer

Centralized in Chapter 1 (`Level1`):

### Core datatypes

| Construct                                         | Purpose                                                                                            |
| ------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `Subject`                                         | Indexes what a proposition talks about (object, functor, limit diagram, internal structure, etc.). |
| `AxiomName`                                       | Tags the axiom/theorem instance (associativity, Yoneda, universal property, coherence).            |
| `AxiomProp : Subject -> AxiomName -> Proposition` | Object-language proposition looked up by `(Subject, AxiomName)` pair.                              |
| `Holds : Proposition -> Set`                      | Interprets object-language into an Agda type to inhabit.                                           |
| `Proof subject ax`                                | Shorthand = `Holds (AxiomProp subject ax)`.                                                        |

### Extended Subjects (higher & internal layers)

Added for Chapters 7–8:

| Subject constructor            | Meaning                                                               |
| ------------------------------ | --------------------------------------------------------------------- |
| `TwoCategoryS C`               | Coherence assertions about 2-category `C`.                            |
| `LaxFunctorS F C D`            | Coherence (associativity/unitality) for lax functor `F : C -> D`.     |
| `BicategoryS B`                | Bicategory-level associator & unitor constraints.                     |
| `DistributorS A B D`           | Bimodule/profunctor composition context.                              |
| `InternalCategoryS IC ambient` | Internal category `IC` living in ambient category `ambient`.          |
| `InternalDiagramS D IC`        | Internal diagram `D` valued in internal category `IC`.                |
| `InternalLimitS L D IC`        | Internal limit object `L` for diagram `D` in internal category `IC`.  |
| `InternalColimitS L D IC`      | Internal colimit object `L` for diagram `D` (dual of internal limit). |

### Extended Axiom/Theorem Names

New tags introduced:

`TwoCatAssociativityName`, `TwoCatUnitalityName`, `LaxAssociativityName`, `LaxUnitalityName`, `LimitHierarchyName`, `CauchyViaDistributorsName`, `InternalAssociativityName`, `InternalLeftUnitName`, `InternalRightUnitName`, `YonedaLemmaName`, `InternalLimitUniversalName`, `InternalColimitDualityName`, `InternalColimitUniversalName`.

These pair with subjects via `AxiomProp` equations (e.g. `AxiomProp (InternalCategoryS IC ambient) InternalAssociativityName ≡ InternalAssocProp IC ambient`).

### Proposition Constructors

For every new axiom name a matching constructor exists (e.g. `TwoCatAssociativityProp`, `InternalLimitUniversalProp`, `InternalColimitUniversalProp`). This forms the stable vocabulary referenced by bridges.

### Bridge Postulates Pattern

Each chapter introduces lightweight records reflecting syntactic data; a bridge postulate injects that record into the proof layer. Pattern:

```agda
postulate
   someBridge
      : (rec : LocalAxiomOrStructure)
      -> (parameters : …)
      -> Proof (Subject …parameters…) AxiomNameTag
```

Examples (Chapter 7 & 8):

```agda
twoCatAssociativityProof : (ax : AssociativityAxiomTwoCat) -> (C : Identifier)
                                    -> Proof (TwoCategoryS C) TwoCatAssociativityName

internalYonedaLemmaProof : (thm : InternalYonedaLemma) -> (IC ambient : Identifier)
                                     -> Proof (InternalCategoryS IC ambient) YonedaLemmaName

internalColimitUniversalProof : (col : InternalColimitDeclaration) -> (L D IC : Identifier)
                                             -> Proof (InternalColimitS L D IC) InternalColimitUniversalName
```

### Internal Limit / Colimit Duality

We encode both universal properties separately:

- Limit: `InternalLimitS` + `InternalLimitUniversalName` + `InternalLimitUniversalProp`.
- Colimit: `InternalColimitS` + `InternalColimitUniversalName` + `InternalColimitUniversalProp` (bridge added after introducing explicit cocone/colimit records).
- Duality meta-level: `InternalColimitDualityName` expresses correctness of dual inference.

This separation lets one assert colimit existence independently of deriving it purely from a limit duality postulate.

### Evolving Towards Implementations

Currently all semantic witnesses are `postulate`s. When replacing with constructive proofs:

1. Define semantic structures (e.g. explicit composition operations) in a dedicated semantics module.
2. Replace `postulate`d `AxiomProp` equalities with definitional equalities or derived lemmas.
3. Refactor bridge postulates into actual functions producing inhabitants of `Holds …`.

The indexing (`Subject`, `AxiomName`) remains stable, minimizing downstream diff.

## Per-EBNF file pattern

For each EBNF file, create a sibling Agda module:

1. Structural records/enums mirroring productions.
2. Comments copy key CATEGORY prose for traceability.
3. Optional bridge postulates to `Core.Proof` of the form:
   - `bridgeX : (decl : …) -> (ax : …) -> Proof (Subject params) AxiomNameTag`
4. If needed, add `AxiomName`/`Subject`/`AxiomProp` equations in `Level1` (or a future `Proof.Core`) rather than per-file repetition.

## Import guidance

- New chapter modules should `open import Core` to depend on a stable surface.
- Downstream aggregation: add re-exports to `N/Index.agda` (e.g., `EBNF1.Index`) for consumers.

## Scalability notes

- Keep chapters independent by limiting changes to `Level1` when introducing new `Subject`s or `AxiomName`s.
- Prefer bridge postulates in the chapter modules over duplicating proof machinery.
- When ready for semantics, introduce a small `CategoryCore.agda` and redirect `AxiomProp` from postulates to definitions incrementally.
- For higher/internal extensions, always: (1) add `Subject`, (2) add `AxiomName`, (3) add proposition constructor, (4) add `AxiomProp` equation, (5) add bridge postulate in chapter file.
- Dual constructions (limits/colimits) should keep separate universal property tags to avoid conflating existential and duality inferences.

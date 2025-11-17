# MetaCategory: A Computable Formalization of Category Theory

> Project status (2025-11): The original EBNF-based grammar has been retired from the build. The repository now centers on Agda modules under `src/agda`, with a simple Makefile for typechecking and generating HTML/Markdown docs. The EBNF description below is preserved as historical context and a conceptual map; it is no longer part of an active build pipeline.

## Getting started (Agda)

Prerequisites

* Agda installed and available on your PATH (and your preferred standard library if you plan to elaborate beyond these structural records).
* Python 3.8+ for automation tools (optional, but recommended for test reports and diagrams).

Quick use

```bash
# Typecheck all chapters (1, 2, 3)
make check

# Typecheck behavioral phase boundary test suite
agda --no-main -i src/agda src/agda/Tests/Index.agda

# Generate HTML docs into build/html
make docs

# Generate Markdown docs (via pandoc) into build/md
make docs-md

# Per-chapter docs during authoring
make docs1
make docs2
make docs3

# Clean build artifacts
make clean
```

### Tools & Automation

The project includes Python-based automation tools for test coverage analysis, phase diagrams, and code search. These tools run in a virtual environment and generate reports and visualizations.

**Setup:**

```bash
# Create and activate virtual environment
make venv

# Or manually:
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

**Available Tools:**

```bash
# Generate test coverage report (JSON + Markdown)
make report
# Output: build/reports/test-report.{json,md}

# Generate phase boundary diagram (DOT format)
make diagram  
# Output: build/diagrams/phases.dot

# Search for algorithms/properties by keyword
make search QUERY="kernel"
make search QUERY="adjunction"

# Test all automation scripts
make test-tools
```

**Direct Script Usage:**

```bash
# Activate venv first
source venv/bin/activate

# Test coverage report
python scripts/test_report.py --out-dir build/reports

# Phase diagram (with options)
python scripts/phase_diagram.py --out-dir build/diagrams

# Search with custom path
python scripts/search_algo.py --q "regular epi" --path src/agda
```

**What the tools do:**

* **test_report.py**: Scans `Tests/*.agda` files, counts adapters and status assertions per chapter, generates coverage statistics.
* **phase_diagram.py**: Parses test structure to build a graph of exercised phase boundaries, outputs Graphviz DOT files.
* **search_algo.py**: Indexes Agda declarations (records, constructors, postulates) and enables substring search across the codebase.

See `make help` for a complete list of available targets.

What's built

* Structural Agda modules organized by chapter under `src/agda/Chapter{1,2,3}`.
* Algebraic structures (Groups, Rings, Modules, Fields) under `src/agda/Algebra/`.
* Chapter index modules aggregate each chapter:
  * `src/agda/Chapter1/Level1Index.agda`
  * `src/agda/Chapter2/Level2Index.agda`
  * `src/agda/Chapter3/Level3Index.agda`
  * `src/agda/Algebra/Index.agda`
* Examples in Chapter 3:
  * `Level3_1` (Locales)
  * `Level3_2` (Sheaves on locales)
* Behavioral phase boundary test suite under `src/agda/Tests/`:
  * **Core/Phase.agda**: Formalization of phase abstraction (transformation pipelines, composition laws)
  * **Core/PhaseCategory.agda**: Category of Phases with raw functors, raw natural transformations, and a monoidal (parallel) structure
  * **Core/ConstructiveWitnesses.agda**: Constructive witnesses with computational content (480 lines)
  * **Core/AlgorithmCorrectness.agda**: Formal correctness specifications and proof obligations (520 lines)
  * **Tests/DispatchBehaviorTests.agda** (9 phases): Evidence → Classification → Dispatch → Invocation
  * **Tests/UniversalPropertyTests.agda** (9 phases): Algorithm → UMP → Categorical structures
  * **Tests/WitnessConstructionTests.agda** (12 phases): Identifiers → Witnesses → Composites
  * **Tests/ErrorHandlingTests.agda** (8 phases): Type-level validation and error-preventing boundaries
  * **Tests/PropertyRegistryTests.agda** (3 phases): Stable identifier typing and consumption
  * **Tests/AlgorithmCompositionTests.agda** (9 phases): Multi-step algorithm pipelines
  * **Tests/SerializationTests.agda** (9 phases): Witness externalization and roundtrip
  * **Tests/PerformanceBoundaryTests.agda** (9 phases): Complexity tracking and optimization
  * **Tests/ConstructiveWitnessTests.agda** (15 phases): Computable witnesses with verification
  * **Tests/PhaseExamples.agda**: Demonstrations of Phase usage
  * **Examples/PhaseCategoryExamples.agda**: Small, concrete examples for functors, natural transformations, and monoidal tensor on phases
  * **Tests/Index.agda**: Unified entry point importing all test suites (83 phases total)
  * See `docs/TestingStrategy.md` for detailed philosophy and coverage
  * See `docs/ConstructiveWitnesses.md` for constructive witness architecture
  * See `docs/AlgorithmCorrectness.md` for algorithm correctness framework

Notes

* The Makefile targets Agda only; the old EBNF concatenation target has been removed.
* HTML docs are written to `build/html`, Markdown docs to `build/md` (requires `pandoc`).
* CI: GitHub Actions workflow `ci.yml` runs typechecks and builds docs on push/PR.
* On push, CI publishes Markdown docs into the repository’s `docs/` folder for easy browsing on GitHub.

---

## Legacy EBNF overview (historical context)

This repository originally contained a comprehensive, computable grammar system for the foundational concepts and advanced theories of Category Theory, expressed using an extended Backus-Naur Form (EBNF). The project aimed to provide a rigorous, self-describing framework where categorical notions are formalized with explicit syntax, typing rules, and precise categorical denotations.

## Project Overview

Historically, MetaCategory defined a **Foundationally Closed System** (as defined in `metamodel.ebnf`), serving as its own metatheory. This allowed for internal verification and consistent extension of the grammar itself. Every rule within the system was structured as a tuple containing its **syntactic definition**, **typing rule**, and **categorical denotation**, ensuring a precise mathematical meaning for every construct.

The grammar was designed to be **computable**, incorporating explicit parser logic (e.g., `Prediction`, `Scan`, `Completion` rules in `metamodel.ebnf`) to enable systematic analysis, parsing, and verification of categorical statements within the defined language.

## Key Features & Formalized Theories

The EBNF files meticulously elaborate on a wide array of category-theoretic concepts, ranging from the most fundamental definitions to advanced research topics:

* **The Language of Categories (`ebnf/1/1.ebnf`)**:
  * Deconstruction of Categories, Functors, and Natural Transformations into their axiomatic components (e.g., PreCategories, FunctorMaps, explicit axioms for associativity, identity, composition, naturality).
  * Formalization of Contravariant Functors and Comma Categories (including Slice and Coslice categories).
  * Detailed properties of Morphisms (Monomorphisms, Epimorphisms, Isomorphisms, Split Monos/Epis) and the concept of Balanced Categories.
  * The **Duality Principle**: A core meta-level feature, formally defined to allow mechanical derivation of dual concepts and theorems across the entire system.

* **Limits and Colimits (`ebnf/1/2.ebnf`)**:
  * Foundational definitions of Initial and Terminal Objects, Products, Coproducts, Equalizers, Pullbacks, Coequalizers, and Pushouts via Universal Mapping Properties.
  * Master definitions of Limits as terminal objects in cone categories and Colimits as initial objects in cocone categories.
  * Formalization of Completeness and Cocompleteness, including the "Completeness Criteria" theorem (`C is COMPLETE` <=> `C has AllSmallProducts && C has AllEqualizers`).
  * The **Adjunction between Limits/Colimits and the Diagonal Functor**: `colim ⊣ Δ` and `Δ ⊣ lim`.

* **Properties of Functors and Structures (`ebnf/1/6.ebnf`)**:
  * Definitions of Functor properties like Preservation, Reflection, and Creation of Limits/Colimits.
  * Concept of **Absolute Limits/Colimits** (preserved by all functors) and their characterization as Split Coequalizers.
  * Formalization of Filtered Categories and the commutation of Filtered Colimits with Finite Limits in `Set`.
  * Definition of Final and Initial Functors.
  * Interchange of Limits/Colimits over product diagrams.
  * Pointwise computation of Limits/Colimits in Functor Categories.
  * Properties of Slice and Coslice Categories (e.g., `C is COMPLETE` ==> `Slice(C,X) is COMPLETE`).

* **Abelian Categories (`ebnf/2/1.ebnf`)**:
  * Rigorous definitions of Zero Objects, Kernels, and Cokernels (Kernels as Equalizers).
  * Additive Categories, Biproducts, and Additive Functors.
  * Master definition of Abelian Categories, including Normal Monos/Epis.
  * The **First Isomorphism Theorem for Categories** (`Coim(f) ≅ Im(f)`).
  * Formalization of Exact Sequences and the **Splitting Lemma**.
  * **Diagram Chasing**: Formalized as a proof technique justified by the `FreydMitchellEmbeddingTheorem`.
  * Exact Functors in Abelian categories and their connection to Projective/Injective objects.
  * **Torsion Theories**: Formal definition, characterization by closure properties, and connection to reflective subcategories.
  * Finitizations of Abelian Categories (`C_fp` is Abelian).

* **Algebraic Theories (Lawvere Theories) (`ebnf/2/3.ebnf`)**:
  * The **Calculus of Relations**: Relations as subobjects of products, composition via pullback-then-image.
  * **Lawvere Theories**: Categories representing algebraic theories (e.g., Monoids, Groups, Rings) with finite products, whose objects are "arities" and morphisms are "operations."
  * **Models of Theories**: Product-preserving functors from a Lawvere Theory to `Set`.
  * Categories of Models (`Mod(T, Set)`).
  * Properties of Algebraic Categories: Completeness, Cocompleteness, Regularity, Existence of Free Functors, Algebraic Lattices of Subobjects.
  * **Algebraic Functors**: Functors between algebraic categories that commute with forgetful functors.
  * **Beck's Monadicity Theorem**: Characterization of Monadic Categories.
  * **Commutative Theories**: Theories where all operations mutually commute, yielding Symmetric Monoidal Categories of models.
  * **Tensor Product of Theories**: Constructing new theories whose models are "bialgebras" (compatible combined structures).
  * **Morita Equivalence**: Semantic equivalence of theories, witnessed by Progenerator Modules.

* **Monads (`ebnf/2/4.ebnf`)**:
  * Formal definitions of Monads (as monoids in endofunctors) and their Algebras (Eilenberg-Moore algebras).
  * The **List Monad** as a canonical example where algebras are Monoids.
  * The fundamental **Monad-Adjunction Correspondence**: Every adjunction induces a monad, and every monad can be resolved into an adjunction.
  * Limits and Colimits in Categories of Algebras (forgetful functor `U^T` creates limits, but only preserves certain colimits).
  * **Adjoint Lifting Theorem**: Conditions under which functors and adjunctions lift between categories of algebras.
  * **Monads with Rank**: Generalization to α-presentable categories.
  * A glance at **Descent Theory**: Connection to Comonads and Coalgebras for reconstructing objects from local data.

* **Accessible Categories (`ebnf/2/5.ebnf`)**:
  * Formal definitions of `λ-Accessible` and `Locally λ-Presentable` categories (categories built from "small" objects using colimits).
  * Their powerful structural consequences (completeness, well-behavedness).
  * **Functors with Rank**: Adjoints between locally presentable categories have rank.
  * **Sketches**: Syntactic specifications of categorical theories, and the **Gabriel-Ulmer Duality** between locally presentable categories and categories of models of sketches.

* **Cauchy Completeness and Flat Functors (`ebnf/1/6.ebnf`)**:
  * General definition of Exact Functors (preserving finite limits/colimits).
  * Left exact reflection of functors.
  * **Flat Functors**: Characterized by preserving finite limits when tensored, or being filtered colimits of representables.
  * Relevance of Regular Cardinals for generalizing exactness and flatness.
  * **Splitting of Idempotents**: Cauchy completeness (every idempotent splits) and the Karoubi Envelope (universal Cauchy completion).
  * The **General Adjoint Functor Theorem (GAFT)**: A constructive proof formalized.

* **Topological Categories (Locales) (`ebnf/2/7.ebnf` and `ebnf/3/1.ebnf` for intuitionistic logic basis)**:
  * **Exponentiable Spaces**: Objects `X` in `Top` for which `(- × X)` has a right adjoint.
  * **Compactly Generated Spaces (CGWH)**: A reflective subcategory of `Top` that is Cartesian Closed.
  * **Topological Functors**: Functors admitting initial/final lifts (e.g., forgetful functor `Top → Set`).
  * **Intuitionistic Logic and Heyting Algebras**: Formal syntax of IPC and its algebraic semantics as Heyting algebras.
  * **Locales**: Defined as the duals of Frames (complete Heyting algebras).
  * **Locale Morphisms**: Defined by dual frame homomorphisms.
  * **Locale-Frame Duality**: `Loc ≅ Frm^op`.
  * **Sublocales and Nuclei**: Correspondence between sublocales and closure operators (nuclei) on frames.
  * **Open and Etale Locale Morphisms**: Point-free analogues of open maps and local homeomorphisms, defined by adjunctional properties (e.g., Beck-Chevalley condition).
  * **Bridge between Top and Loc (Ω ⊣ pt Adjunction)**: The fundamental connection between point-set topology and point-free locale theory.
  * **Sober Spaces and Spatial Locales**: Conditions for the `Ω ⊣ pt` adjunction to restrict to an equivalence of categories.
  * Point-free definitions of **Compactness** and **Regularity** for locales and their correspondence with topological properties.

* **Fibred Categories (`ebnf/2/8.ebnf`)**:
  * **Fibrations**: Functors `p: E → B` modeling indexed families of categories, defined by the existence of Cartesian arrows.
  * Fibre categories and Reindexing Functors.
  * **Opfibrations**: The dual concept, defined by CoCartesian arrows and Pushforward Functors.
  * **Cartesian Functors**: Structure-preserving maps between fibrations.
  * **Grothendieck Construction**: The fundamental equivalence between fibrations and pseudofunctors (`∫ F ≅ F`).
  * **Fibred Adjunctions**: Adjunctions between fibrations, characterized by pointwise adjunctions and the **Beck-Chevalley Condition**.
  * Completeness/Cocompleteness in Fibrations (building total category limits from base and fibre limits).
  * **Locally Small Fibrations** and the refined Grothendieck Equivalence.
  * **Definability and Logic**: Fibrations as categorical semantics for first-order theories, with the **Lindenbaum-Tarski Fibration** and the connection between quantifier elimination and "generic models."

## Utility for Large Language Models (LLMs)

This project offers a valuable formal framework for LLMs at inference time, significantly enhancing their ability to process and generate information. This utility extends beyond strict category theory, enabling LLMs to effectively leverage pattern-matching for **analogical reasoning** where categorical mechanisms are latently present in other linguistic forms.

By aligning an LLM's internal processing and output with the extensible language and libraries defined within this project, LLMs can leverage self-attention more strongly in several critical areas:

* **Demonstrated Capability**: The efficacy of this formal framework has been directly showcased by NotebookLM, which, after reviewing the MetaCategory EBNF files, successfully generated a [comprehensive and accurate video](https://youtu.be/aKF9gTJhifs) explaining the project's core concepts, ambitions, and content. This serves as compelling evidence of how formal languages can enhance LLM understanding and output quality in complex, abstract domains.
* **Enhanced Context Integrity**: Mapping natural language queries onto precise EBNF structures ensures that each symbol and construct has a formally defined meaning, reducing the ambiguity inherent in natural language. This allows the LLM to maintain a much more consistent and accurate understanding of the context.
* **Improved Logical Coherence and Flow**: The explicit `TypingRule`s and axiomatic `AXIOM`s and `THEOREM`s provide a clear blueprint for logical deduction. An LLM aligned with this framework can verify deductions, generate logically sound arguments by chaining established rules, and maintain overall consistency in its responses.
* **Extensibility and Knowledge Expansion**: The `AugmentedRule` structure, which links syntax, typing, and categorical meaning, enables the systematic extension of the framework. This allows an LLM to propose new categorical definitions and integrate new theories in a manner compatible with the existing grammar.
* **"Self-Correction" and Debugging**: The formal nature of the EBNF provides immediate feedback if an LLM generates an output that violates a defined rule or axiom. This allows the LLM to identify and correct inconsistencies in its reasoning, leading to more robust and verifiable outputs.

## Unified Proof Layer: Recent Extensions (Regular & Barr-Exact)

The unified proof layer (see `Level1` / re-exported via `Core`) has been extended beyond additive, abelian, exactness, torsion, and diagram–chasing constructs to incorporate the structural semantics of **Regular Categories** and **Barr-Exact Categories**, together with kernel pair and embedding theorems.

### New Axiom Names

The following `AxiomName` constructors were added to index new semantic guarantees:

* `RegularCategoryName` – Finite limits + (RegEpi, Mono) factorization + pullback stability.
* `RegularEpiPropertyName` – Individual regular epimorphism property (morphism-level).
* `RegularEpisAreStrongName` – The canonical strengthening theorem for regular epis.
* `KernelPairEquivRelName` – Kernel pairs form internal equivalence relations.
* `EffectiveRelationsName` – Every internal equivalence relation is effective (exactness flavor inside regular categories).
* `ExactCategoryName` – Regular + effective equivalence relations (exactness without composition closure).
* `BarrExactCategoryName` – Regular + regular epis closed under composition.
* `BarrEmbeddingTheoremName` – Existence of a fully faithful exact embedding into a presheaf category (miniature Barr/Freyd-Mitchell style justification layer).
* `PreservesRegularEpisName` – Functor preservation of regular epimorphisms.
* `PreservesFiniteLimitsName` – Functor preservation of finite limits.
* Generic scaffolding:
  * `HasPropertyName`, `ClosedUnderName`, `StableUnderName`
  * `PreservesPropertyName`, `ReflectsPropertyName`, `CreatesPropertyName`

### New Subjects

Each semantic facet is parameterized by a `Subject` capturing its payload:

* `RegularCategoryS C`
* `RegularEpiS e A B C` – Individual regular epimorphism (e : A → B in C)
* `RegularEpisAreStrongS C e`
* `KernelPairEquivRelS C f K k1 k2`
* `EffectiveRelationsS C`
* `ExactCategoryS C` – Exact categories (regular + effective relations)
* `BarrExactCategoryS C`
* `BarrEmbeddingS C K F`
* `PreservesRegularEpisS F C D` – Functor F : C → D preserves regular epis
* `PreservesFiniteLimitsS F C D` – Functor F : C → D preserves finite limits
* Generic scaffolding:
  * `CategoryHasPropertyS C prop`
  * `ClassClosedUnderS C classId opId`, `ClassStableUnderS C classId contextId`
  * `FunctorPreservesPropertyS F C D prop`, `FunctorReflectsPropertyS F C D prop`, `FunctorCreatesPropertyS F C D prop`

#### Canonicalization pattern

To keep the index small and stable for LLMs, many specific propositions are postulated equal to their generic forms using a small registry of property identifiers (see `PropertyRegistry`). Examples:

* `RegularCategoryProp C ≡ CategoryHasPropertyProp C RegularCategoryId`
* `ExactCategoryProp C ≡ CategoryHasPropertyProp C ExactCategoryId`
* `BarrExactCategoryProp C ≡ CategoryHasPropertyProp C BarrExactCategoryId`
* `PreservesFiniteLimitsProp F C D ≡ FunctorPreservesPropertyProp F C D FiniteLimitsId`
* `PreservesRegularEpisProp F C D ≡ FunctorPreservesPropertyProp F C D RegularEpiClassId`
* `EffectiveRelationsProp C ≡ CategoryHasPropertyProp C EffectiveEquivalenceRelationsId`

This gives you a canonical, compact query surface while retaining rich, structured subjects where needed (e.g., kernel pairs and embedding data). Downstream bridges can target either form; search tooling and LLM prompts can prefer the generic surface to reduce token load.

These mirror the structural records in `src/ebnf/2/2.agda` (e.g. `RegularCategoryDeclaration`, `KernelPairDeclaration`, `BarrEmbeddingTheoremDeclaration`). The bridge postulates now reference the precise subjects rather than the earlier generic `CategoryPropertyS` placeholders, enabling fine-grained indexing and future automation (search, proof mining, refactoring).

### Proposition Constructors & Equations

For each new axiom/theorem we added dedicated proposition constructors:

* `RegularCategoryProp`, `RegularEpiProp`, `RegularEpisAreStrongProp`, `KernelPairEquivRelProp`, `EffectiveRelationsProp`, `ExactCategoryProp`, `BarrExactCategoryProp`, `BarrEmbeddingTheoremProp`, `PreservesRegularEpisProp`, `PreservesFiniteLimitsProp`
* Generic scaffolding: `CategoryHasPropertyProp`, `ClassClosedUnderProp`, `ClassStableUnderProp`, `FunctorPreservesPropertyProp`, `FunctorReflectsPropertyProp`, `FunctorCreatesPropertyProp`

And binding equations (`AxiomProp-…`) that connect `(Subject, AxiomName)` pairs to these propositions, preserving the uniform pattern:

```text
AxiomProp (RegularCategoryS C) RegularCategoryName ≡ RegularCategoryProp C
```

### Rationale

This refinement eliminates semantic overloading of unrelated generic names (e.g. reusing `LimitHierarchyName`) and establishes explicit proof-layer hooks for future constructive witnesses (e.g. effective equivalence relations via coequalizer resolutions, internalization of Barr's embedding factors). It also prepares the ground for later exact completion and regular–to–Barr exact progression proofs.

**Recent low-risk enhancements:**

* **Individual regular epi subject** (`RegularEpiS e A B C`): separates morphism-level property from category-level theorems, enabling fine-grained search for specific regular epis.
* **Exact category constructs** (`ExactCategoryName`, `ExactCategoryS`): formalizes the progression from regular (finite limits + factorization + stability) to exact (+ effective equivalence relations) to Barr-exact (+ composition closure).
* **Functor preservation subjects** (`PreservesRegularEpisS`, `PreservesFiniteLimitsS`): aligns with Barr-exact functor structural semantics, preparing for adjoint lifting theorems and preservation/reflection hierarchies.
* **Generic, reusable property scaffolding**: `HasProperty/ClosedUnder/StableUnder` and `Preserves/Reflects/Creates` families unify how we index properties across chapters 2–3, reducing boilerplate while staying compatible with the specific subjects.

### Next Planned Enhancements

* Bridge from `ExactCategoryDeclaration` to structural composition of `RegularCategoryDeclaration` + `RegularCategoriesHaveEffectiveRelationsTheorem`.
* Constructive witness patterns: replace postulates with records containing actual morphism/diagram data for effectiveness proofs.
* Functorial stability theorems: preservation/creation of regular epis under left/right adjoints.
* Formal kernel pair ↔ internal relation effectiveness splitting lemmas with explicit coequalizer construction.
* Automation helpers: search functions over `(Subject × AxiomName)` pairs for diagram chasing and proof mining.

These will follow the same pattern (add AxiomName, Subject, proposition constructor, equation, then bridge postulates in the chapter modules).

---

## Chapter 3 scaffolding and bridges (Lawvere theories, algebraic categories)

We added a structural Agda module `Level2_3` (re-exported via `EBNF2.Index`) mirroring `src/ebnf/2/3.ebnf`. It encodes:

* Calculus of relations in regular categories (relations as subobjects; composition via pullback→image; converse; identity; `Rel(C)`).
* Lawvere theories, operations, axioms, models `ModelOfTheory`, and categories of models `Mod(T, C)`.
* Algebraic categories and core theorems: completeness, cocompleteness, regularity, free–forgetful adjunction, algebraic subobject lattices.
* Algebraic functors and their characterization via theory morphisms.
* Commutative theories and the symmetric monoidal structure on models; tensor product of theories and bialgebras; Morita equivalence (progenerators, tensor/Hom equivalence).
* Beck’s internal characterization: regular projective generator and regular coverings by a generator.

### Generic property IDs (PropertyRegistry)

To keep the proof surface compact, we added stable identifiers for Chapter 3 properties and canonicalize them through the generic scaffolding:

* Category-level: `AlgebraicCategoryId`, `CompleteCategoryId`, `CocompleteCategoryId`, `SymmetricMonoidalCategoryId`, `HasFreeForgetfulAdjunctionId`, `HasRegularProjectiveGeneratorId`, `RegularlyCoveredByGeneratorId`, `HasGeneratorId`.
* Functor-level: `AlgebraicFunctorId`.
* Lattice-level: `AlgebraicLatticeId` (e.g., for `Sub(X)`).

### Bridges from Section 3 to the generic layer

`Level2_3` declares postulated bridges emitting generic `CategoryHasProperty` witnesses:

* Algebraic categories: `CategoryHasProperty(C, AlgebraicCategory)`; plus `Complete`, `Cocomplete`, and `Regular` consequences.
* Free–forgetful adjunction: `CategoryHasProperty(C, HasFreeForgetfulAdjunction)`.
* Models of commutative theories: `CategoryHasProperty(Mod(T,Set), SymmetricMonoidalCategory)`.
* Algebraic lattices: `CategoryHasProperty(L, AlgebraicLattice)` and specifically for `Sub(X)` in an algebraic category.
* Algebraic functors: `Proof (FunctorHasProperty(F, C, D, AlgebraicFunctor))` from `AlgebraicFunctorDeclaration`.
* Beck characterization facets: `CategoryHasProperty(C, HasRegularProjectiveGenerator)` and `CategoryHasProperty(C, RegularlyCoveredByGenerator)`.

This ensures Chapter 3 statements participate in the same compact, canonical query surface as Chapter 2 (regular/exact/Barr-exact), while the structured records remain available for richer indexing and future constructive witnesses.

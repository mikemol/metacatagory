{-# OPTIONS --without-K --cubical-compatible --safe #-}

module Plan.CIM.DocumentationContent where

open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using (Nat; zero; suc)

open import Plan.CIM.Utility using (_++_; map)
open import Plan.CIM.PandocAST

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

h : Nat → List Inline → Block
h n inls = Header n inls

p : List Inline → Block
p = Para

plain : List Inline → Block
plain = Plain

codeBlock : String → Block
codeBlock = CodeBlock

text : String → Inline
text = Str

space : Inline
space = Space

breakLine : Inline
breakLine = LineBreak

ul : List (List Block) → Block
ul = BulletList

bullets : List String → Block
bullets xs = ul (map (λ s → plain (text s ∷ []) ∷ []) xs)

------------------------------------------------------------------------
-- README content as structured PandocDoc
------------------------------------------------------------------------

readmeDoc : PandocDoc
readmeDoc = record
  { blocks =
      h 1 (text "The Metacategory: A Homotopical Algebra System" ∷ []) ∷
     h 2 (text "Formalizing the Axiom of Well-Founded Indexed Composition" ∷ []) ∷
      h 2 (text "1. Overview" ∷ []) ∷
      p (text "This repository contains a formal verification and computational framework implemented in Agda. "
         ∷ text "It unifies Abstract Algebra, Category Theory, and Constructive Algorithmics into a single, self-referential Directed Acyclic Graph (DAG)." ∷ []) ∷
      p (text "The system is not merely a library of mathematical proofs; it is a mathematical operating system. "
         ∷ text "It operates under the Curry-Howard-Lambek (CHL) correspondence, unifying lexical scope (syntax), semantic composition (logic), and categorical morphisms (structure)." ∷ []) ∷
      h 3 (text "The Core Axioms" ∷ []) ∷
      p (text "The architecture is strictly bound by the following formally internalized principles:" ∷ []) ∷
      p (text "1. The Axiom of Well-Founded Indexed Composition: Every node N is assigned a static coordinate (x, y). "
         ∷ text "A composite node N_n may only depend on constituents N_i where (x_i, y_i) < (x_n, y_n). This enforces a global DAG structure, preventing circular definitions." ∷ []) ∷
      p (text "2. The Axiom of Universal Reification: Every concept—whether a data structure, a proof, or an ambiguity—is reified as an indexed Identifier." ∷ []) ∷
      p (text "3. The Axiom of Gödelian Boundedness: The system explicitly models its own incompleteness. "
         ∷ text "Limit objects (e.g., unprovable statements, infinite regress) are reified as nodes within the solution space." ∷ []) ∷
      h 2 (text "2. The Ontological Stack" ∷ []) ∷
      p (text "The system is stratified into phases, representing the evolution from static definitions to dynamic execution." ∷ []) ∷
      h 3 (text "Layer I: The Substrate (Core)" ∷ []) ∷
      p (text "This layer defines the physics of the system." ∷ []) ∷
      p (text "* Metamodel.agda: Defines Identifier, Coordinate, and the ordering relation <ᶜ. This is the syntactic bedrock." ∷ []) ∷
      p (text "* Core.Phase: Reifies transformation as a first-class citizen. A Phase A B is a morphism in the Category of Phases, allowing for sequential and parallel composition of algorithmic steps." ∷ []) ∷
      p (text "* Core.GodelBoundary: A formal acknowledgment of the system's limits. It constructs witnesses for self-referential paradoxes and reifies the gap between the system and its self-model." ∷ []) ∷
      p (text "* Core.PathAggregator: Implements Homotopy Type Theory (HoTT) principles. It aggregates individual serialization roundtrips into a GlobalClosureWitness, proving that the system's coordinate geometry is invariant under transformation." ∷ []) ∷
      h 3 (text "Layer II: The Algebraic Pillars (Algebra)" ∷ []) ∷
      p (text "This hierarchy builds the objects of the universe." ∷ []) ∷
      p (text "* Hierarchy: Magma → Semigroup → Monoid → Group → Ring → Field." ∷ []) ∷
      p (text "* Modules & Algebras: Extends rings to LeftModule, VectorSpace, and RAlgebra." ∷ []) ∷
      p (text "* Feature: These are not just typeclasses; they are deeply nested records containing Constructive Witnesses. A FieldExtension carries proofs of its degree, basis, and separability." ∷ []) ∷
      h 3 (text "Layer III: The Categorical Pillars (Chapter1, Chapter2, Chapter3)" ∷ []) ∷
      p (text "This hierarchy builds the laws of the universe." ∷ []) ∷
      p (text "* Chapter1 (Fundamentals): Limits, Colimits, Adjunctions, Kan Extensions." ∷ []) ∷
      p (text "* Chapter2 (Structure): Abelian Categories, Regular Categories, Monads, Fibrations." ∷ []) ∷
      p (text "* Chapter3 (Topos Theory): Locales, Sheaves, Ω-sets." ∷ []) ∷
      p (text "* Deep Integration: These modules define Universal Properties. For example, KernelAsEqualizer defines the algebraic kernel strictly as a categorical limit." ∷ []) ∷
      h 2 (text "3. The Unified Bridge: Taking the Product" ∷ []) ∷
      p (text "The system's power lies in the intersection of the Algebraic and Categorical pillars. This is achieved via the Adapter Pattern." ∷ []) ∷
      h 3 (text "Core.CategoricalAdapter" ∷ []) ∷
      p (text "This module provides a universal interface CategoricalAdapter T that wraps any algebraic structure T. "
         ∷ text "It creates a morphism from the Unit type to T, effectively treating specific algebraic instances as objects in a generalized category." ∷ []) ∷
      h 3 (text "Tests.ObligationAdapters" ∷ []) ∷
      p (text "This is the proving ground. It systematically maps algebraic constructs to categorical requirements." ∷ []) ∷
      p (text "* Example: It proves that an Algebra.Modules.Basic.KernelOfModuleHomomorphism satisfies the Chapter2.KernelAsEqualizerDefinition." ∷ []) ∷
      p (text "* Mechanism: It uses Indexed Adapters to carry the proof that status ≡ true, ensuring that every algebraic feature is categorically sound." ∷ []) ∷
      h 2 (text "4. Development Roadmaps" ∷ []) ∷
      []
  ; meta = ""
  }

------------------------------------------------------------------------
-- CONTRIBUTING content as structured PandocDoc
------------------------------------------------------------------------

contributingDoc : PandocDoc
contributingDoc = record
  { blocks =
      h 1 (text "Contributing to MetaCategory" ∷ []) ∷
      p (text "Welcome! This guide will help you get started as a contributor to the MetaCategory project. "
         ∷ text "Please read carefully to ensure your contributions are effective and consistent with project standards." ∷ []) ∷
      h 2 (text "Getting Started" ∷ []) ∷
      p (text "* Clone the repository and set up your environment using the provided Makefile targets." ∷ []) ∷
      p (text "* Review the documentation in README.md, ROADMAP.md, and the relevant subdirectory manuals (see src/agda/*/README.md)." ∷ []) ∷
      p (text "* Install dependencies:" ∷ []) ∷
      codeBlock "make venv\nmake node-deps" ∷
      h 2 (text "Coding Standards" ∷ []) ∷
      p (text "* Agda code should follow the conventions in the onboarding and module manuals." ∷ []) ∷
      p (text "* Python scripts should be formatted with black and linted with flake8." ∷ []) ∷
      p (text "* Markdown should be auto-formatted and linted using make md-fix and make md-lint." ∷ []) ∷
      p (text "* Commit messages should be clear and reference relevant issues or roadmap items." ∷ []) ∷
      h 2 (text "Adding Tests & Checklists" ∷ []) ∷
      p (text "* Place new checklists in src/agda/Tests/ and update the relevant README." ∷ []) ∷
      p (text "* Use the checklist/test philosophy described in src/agda/Tests/README.md." ∷ []) ∷
      p (text "* Run make check-tests to verify your additions." ∷ []) ∷
      p (text "* Document the purpose and expected outcome of each test in its file header." ∷ []) ∷
      h 2 (text "Documentation" ∷ []) ∷
      p (text "* Update or add documentation in the appropriate README.md or manual." ∷ []) ∷
      p (text "* For new modules, include a README describing its purpose, key files, and navigation." ∷ []) ∷
      p (text "* Link to relevant Makefile targets and automation scripts where appropriate." ∷ []) ∷
      h 2 (text "Interpreting Reports & Metrics" ∷ []) ∷
      p (text "* Generated reports (e.g., top-offenders.md) highlight technical debt and deferred items." ∷ []) ∷
      p (text "* Use these reports to prioritize refactoring and documentation efforts." ∷ []) ∷
      p (text "* See .github/scripts/README.md for details on automation and metrics." ∷ []) ∷
      h 2 (text "Submitting Changes" ∷ []) ∷
      p (text "1. Fork the repository and create a feature branch." ∷ []) ∷
      p (text "2. Make your changes and commit with a descriptive message." ∷ []) ∷
      p (text "3. Run all relevant Makefile targets to verify code, tests, and docs." ∷ []) ∷
      p (text "4. Open a pull request, referencing related issues or roadmap items." ∷ []) ∷
      p (text "5. Respond to review feedback and iterate as needed." ∷ []) ∷
      h 2 (text "Community & Support" ∷ []) ∷
      p (text "* For questions, open a discussion or issue on GitHub." ∷ []) ∷
      p (text "* See CREDITS.md for contributors and acknowledgments." ∷ []) ∷
      p (text "Thank you for helping build MetaCategory!" ∷ []) ∷
      []
  ; meta = ""
  }

------------------------------------------------------------------------
-- NAVIGATION content as structured PandocDoc
------------------------------------------------------------------------

navigationDoc : PandocDoc
navigationDoc = record
  { blocks =
      h 1 (text "MetaCategory Global Navigation" ∷ []) ∷
      p (text "This document provides a high-level map of the repository's documentation and code structure, helping contributors and users find relevant manuals, guides, and modules quickly." ∷ []) ∷
      h 2 (text "Top-Level Documentation" ∷ []) ∷
      p (text "* README.md: System overview, axioms, architecture, and ontological stack." ∷ []) ∷
      p (text "* ROADMAP.md: Project roadmap, phase status, and planning." ∷ []) ∷
      p (text "* CONTRIBUTING.md: Contribution guidelines, onboarding, coding standards, and PR process." ∷ []) ∷
      p (text "* DEFERRED-TRACKING.md: Technical debt and deferred items tracking." ∷ []) ∷
      p (text "* COPILOT_SYNERGY.md: AI and Copilot integration notes." ∷ []) ∷
      p (text "* CREDITS.md: Contributors and acknowledgments." ∷ []) ∷
      p (text "* ingest.md: Data ingestion notes." ∷ []) ∷
      p (text "* testing.md: Testing philosophy and details." ∷ []) ∷
      h 2 (text "Source Atlas & Manuals" ∷ []) ∷
      p (text "* src/agda/README.md: Atlas of the source lattice, dimensional breakdown." ∷ []) ∷
      p (text "* src/agda/Core/README.md: Core physics, axioms, limits, and algorithms." ∷ []) ∷
      p (text "* src/agda/Algebra/README.md: Algebraic structures and navigation." ∷ []) ∷
      p (text "* src/agda/Algebra/Groups/README.md: Abelian groups, enrichment, Grothendieck connection." ∷ []) ∷
      p (text "* src/agda/Chapter1/README.md: Level 1 onboarding and curriculum." ∷ []) ∷
      p (text "* src/agda/Tests/README.md: Checklist/test philosophy and boundaries." ∷ []) ∷
      h 2 (text "Automation & Scripts" ∷ []) ∷
      p (text ".github/scripts/README.md: Automation philosophy, metric functor, topology visualizer, debt surveyor, and Makefile targets." ∷ []) ∷
      h 2 (text "Generated Reports" ∷ []) ∷
      p (text ".github/badges/top-offenders.md: Technical debt offenders report." ∷ []) ∷
      h 2 (text "Navigation Tips" ∷ []) ∷
      p (text "* Each major directory contains a README.md with local context and navigation." ∷ []) ∷
      p (text "* Use the Makefile targets for automation, documentation, and reporting (see .github/scripts/README.md)." ∷ []) ∷
      p (text "* For onboarding, start with src/agda/Chapter1/README.md and CONTRIBUTING.md." ∷ []) ∷
      p (text "* For technical debt and deferred items, see DEFERRED-TRACKING.md and generated reports." ∷ []) ∷
      p (text "* For roadmap and planning, see ROADMAP.md." ∷ []) ∷
      p (text "For further questions, see CONTRIBUTING.md or open an issue/discussion on GitHub." ∷ []) ∷
      []
  ; meta = ""
  }

------------------------------------------------------------------------
-- ROADMAP content as structured PandocDoc (static preface; dynamics added in exporter)
------------------------------------------------------------------------

roadmapDoc : PandocDoc
roadmapDoc = record
   { blocks =
         h 1 (text "Metacatagory Development Roadmap" ∷ []) ∷

         h 2 (text "Overview" ∷ []) ∷
         p (text "This roadmap is synthesized from tracked work items and structured roadmap steps. Generated by the documentation exporter to keep it aligned with repository state." ∷ []) ∷

         h 2 (text "Status Snapshot" ∷ []) ∷
         bullets (
            "Agda: 2.8.0 (project setting)" ∷
            "Docs regenerated via make docs" ∷
            "Deferred items tracked in DEFERRED-TRACKING.md" ∷
            []
         ) ∷

         h 2 (text "How to Regenerate" ∷ []) ∷
         codeBlock "make docs\nmake md-lint" ∷

         h 2 (text "Notes" ∷ []) ∷
         p (text "This document appends structured roadmap steps and deferred snapshots at render time." ∷ []) ∷
         []
   ; meta = ""
   }

------------------------------------------------------------------------
-- DEFERRED-TRACKING content as structured PandocDoc
------------------------------------------------------------------------

deferredTrackingDoc : PandocDoc
deferredTrackingDoc = record
   { blocks =
         h 1 (text "Deferred Items Tracking" ∷ []) ∷

         h 2 (text "Overview" ∷ []) ∷
         p (text "Tracks deferred work items identified during the Agda 2.8.0 migration and refactoring efforts. Counts reflect deferred-summary.json snapshots." ∷ []) ∷
         p (text "Source: deferred-summary.json (auto-generated)" ∷ []) ∷
         bullets ("Totals: 567 items" ∷ "Postulates: 351" ∷ "TODOs: 155" ∷ "Other markers (FIXME/HACK/etc.): 61" ∷ []) ∷

         h 2 (text "Tracking Strategy" ∷ []) ∷

         h 3 (text "High-Priority Deferred Items" ∷ []) ∷
         p (text "Items blocking core functionality or Plan.CIM completion." ∷ []) ∷
         ul (
            (plain (text "[ ] CHIPCoreRecompose.agda (8-line stub); blocked by Core re-enable" ∷ []) ∷
             p (text "Effort: 100-150 lines; Timeline: after Core restored; Ref: ROADMAP.md Phase 2.1" ∷ []) ∷ []) ∷
            (plain (text "[ ] PandocProtocols.agda stubs (blockAmb, blockTransSys, blockCoherence, docAmb, docTransSys, docCoherence)" ∷ []) ∷
             p (text "Effort: 50-100 lines semantic impls; Ref: ROADMAP.md Phase 2.3" ∷ []) ∷ []) ∷
            (plain (text "[ ] CHIPConformance.agda stubs (makeSPPFNode, composeBraids refinement)" ∷ []) ∷
             p (text "Effort: 50-80 lines; Ref: ROADMAP.md Phase 2.3" ∷ []) ∷ []) ∷
            (plain (text "[ ] Core.disabled/ modules (23 modules)" ∷ []) ∷
             p (text "Phase theory, categorical foundations, adjunction/limits; Effort: 1-2 weeks; Ref: ROADMAP.md Phase 3.2" ∷ []) ∷ []) ∷
            (plain (text "[ ] Algebra.disabled/ subset (for Core deps)" ∷ []) ∷
             p (text "Groups/rings/modules/fields; Effort: 3-5 days; Ref: ROADMAP.md Phase 3.2" ∷ []) ∷ []) ∷
            []
         ) ∷

         h 3 (text "Medium-Priority Deferred Items" ∷ []) ∷
         p (text "Improves functionality but not blocking major milestones." ∷ []) ∷
         ul (
            (plain (text "[ ] Examples.disabled/ modules (13 modules)" ∷ []) ∷
             p (text "TechnicalDebtRegistry and demonstration examples; Effort: 1-3 days/module; Ref: ROADMAP.md Phase 3.1" ∷ []) ∷ []) ∷
            (plain (text "[ ] Plan.CIM integration tests" ∷ []) ∷
             p (text "E2E transformation pipeline, grammar conformance, proof export; Effort: 100-150 lines; Ref: ROADMAP.md Phase 2.2" ∷ []) ∷ []) ∷
            (plain (text "[ ] Plan.CIM documentation" ∷ []) ∷
             p (text "Architecture/API/tutorials; Effort: 1-2 days; Ref: ROADMAP.md Phase 4.1" ∷ []) ∷ []) ∷
            []
         ) ∷

         h 3 (text "Low-Priority Deferred Items" ∷ []) ∷
         p (text "Nice-to-have improvements, cleanup, or exploratory work." ∷ []) ∷
         ul (
            (plain (text "[ ] Utility-broken.agda recovery" ∷ []) ∷
             p (text "Extract 77 roadmap examples; fix syntax; merge into Plan.CIM.Utility.agda; Effort: 4-6 hours; Ref: intake/refactoring-roadmap-migration.md" ∷ []) ∷ []) ∷
            (plain (text "[ ] Script modernization" ∷ []) ∷
             p (text "Python 3.10+ features, type hints, error handling; Effort: 2-3 days; Ref: scripts/" ∷ []) ∷ []) ∷
            (plain (text "[ ] CHIP protocol semantic enrichment" ∷ []) ∷
             p (text "Ambiguity detection, transformation metrics, coherence validation; Research 1-2 weeks; Deferred to ROADMAP-DRAFT.md" ∷ []) ∷ []) ∷
            []
         ) ∷

         h 2 (text "Deferred Item Sources" ∷ []) ∷
         h 3 (text "By Module Category" ∷ []) ∷
         codeBlock "Core.disabled/          : ~80 postulates, ~30 TODOs\nAlgebra.disabled/       : ~45 postulates, ~20 TODOs\nExamples.disabled/      : ~25 postulates, ~15 TODOs\nPlan.CIM/               : ~15 TODOs (stubs)\nScripts/                : ~40 TODOs (improvements)\nDocumentation/          : ~50 TODOs (missing sections)\nOther                   : ~106 postulates, ~35 TODOs" ∷

         h 3 (text "By Type" ∷ []) ∷
         codeBlock "Postulates (351):\n  - Unproven theorems awaiting formal proofs\n  - Placeholder functions for disabled modules\n  - Abstract interface requirements\n\nTODOs (155):\n  - Code improvements and refactoring\n  - Missing implementations\n  - Documentation gaps\n\nOther markers (61):\n  - FIXME: Known bugs or issues\n  - HACK: Temporary workarounds\n  - NOTE: Important context or warnings" ∷

         h 2 (text "Tracking Process" ∷ []) ∷
         h 3 (text "Adding New Deferred Items" ∷ []) ∷
         ul (map (λ s → plain (text s ∷ []) ∷ [])
            ("Add inline marker: -- TODO: [category] description or postulate name : Type" ∷
             "Run make deferred-items to refresh deferred-summary.json" ∷
             "If high/medium priority, add to sections above" ∷
             "Reference ROADMAP.md if it blocks a phase" ∷ [])) ∷

         h 3 (text "Resolving Deferred Items" ∷ []) ∷
         ul (map (λ s → plain (text s ∷ []) ∷ [])
            ("Remove marker or replace postulate with implementation" ∷
             "Run make deferred-items to refresh counts" ∷
             "Check off item in this document" ∷
             "Update ROADMAP.md if a milestone is unblocked" ∷ [])) ∷

         h 3 (text "Periodic Review" ∷ []) ∷
         ul (map (λ s → plain (text s ∷ []) ∷ [])
            ("Weekly: review high-priority items and blockers" ∷
             "Monthly: reprioritize medium/low items" ∷
             "Per milestone: regenerate deferred-summary.json and verify counts" ∷ [])) ∷

         h 2 (text "Tools" ∷ []) ∷
         h 3 (text "Generate Deferred Summary" ∷ []) ∷
         codeBlock "make deferred-items\n# Outputs: deferred-summary.json" ∷

         h 3 (text "Search for Deferred Items" ∷ []) ∷
         codeBlock "# TODOs\ngrep -r \"TODO\" src/agda/\n\n# Postulates\ngrep -r \"postulate\" src/agda/\n\n# All markers\ngrep -rE \"TODO|FIXME|HACK|XXX|NOTE\" src/agda/" ∷

         h 3 (text "Flag Unannotated Technical Debt" ∷ []) ∷
         codeBlock "make Flag Unannotated Technical Debt\n# Or: python3 scripts/flag_unannotated_debt.py" ∷

         h 2 (text "Cross-References" ∷ []) ∷
         bullets (
            "Primary roadmap: ROADMAP.md" ∷
            "Worklog: intake/refactoring-roadmap-migration.md" ∷
            "Generated data: deferred-summary.json (gitignored)" ∷
            "Build tasks: .vscode/tasks.json" ∷
            []
         ) ∷

         h 2 (text "Status Snapshot" ∷ []) ∷
         p (text "Last updated: 2025-12-20; Next review: after Phase 1 cleanup (ROADMAP.md); Totals: 567 (351 postulates, 155 TODOs, 61 other)" ∷ []) ∷
         []
   ; meta = ""
   }

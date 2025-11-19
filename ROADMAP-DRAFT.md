# MetaCategory Consolidated Roadmap (Draft)

Status Date: 2025-11-19
Sources Integrated: `testing.md`, `docs/ARCHITECTURE.md`, `README.md`, `DEFERRED-TRACKING.md`, `ingest.md`
Scope: Full depth / full breadth – phases, architectural follow‑ups, ingestion curriculum, deferred items, future research vectors.

---
## 1. High-Level Phase Status Snapshot

| Phase | Name / Focus                                    | Current State            | Source of Truth                                | Notes                                                           |
| ----- | ----------------------------------------------- | ------------------------ | ---------------------------------------------- | --------------------------------------------------------------- |
| 0     | Primal Correction (Constructive UMP proofs)     | COMPLETE                 | `testing.md`                                   | Product/Coproduct/Equalizer/Pullback/Pushout stubs replaced     |
| I     | Foundational Indexing & Spec Validation         | COMPLETE                 | `testing.md`                                   | Subobject checklist fixed; Topos validation deferred separately |
| II    | Universal Property & Algorithm Coherence        | COMPLETE (Focused Index) | `testing.md`                                   | Global logical closure Phase 2.5 achieved                       |
| III.1 | Complexity Classification                       | COMPLETE                 | `testing.md` / `Core/AlgorithmComplexity.agda` | Ordering `_ ≤ᶜ _` validated                                     |
| III.2 | Advanced Compositional Path / DAG Validation    | COMPLETE                 | `Tests/AlgorithmCompositionTests.agda`         | Multi-step pipeline + ordering checks                           |
| III.3 | Error-as-Specification Integration              | COMPLETE                 | `Core/Limitations.agda` / tests                | `AlgorithmResult` + aggregation tests                           |
| III.4 | Serialization Roundtrip / HoTT Path Isomorphism | COMPLETE                 | `Tests/SerializationTests.agda`                | ExternalIdentifier bidirectional coordinate preservation        |
| III.5 | ToposTheoryChecklist Alignment (Optional)       | DEFERRED                 | `testing.md`                                   | Non-blocking; constructor alignment                             |
| IV.1  | Adjunction Coherence (Free-Forgetful)           | PLANNED                  | `testing.md`                                   | Construct unit/counit natural iso                               |
| IV.2  | Yoneda Lemma Structural Proof                   | PLANNED                  | `testing.md`                                   | Simple functor instance witnessing isomorphism                  |
| IV.3  | Grothendieck Fibration Verification             | PLANNED                  | `testing.md`                                   | Indexed family correctness + Beck-Chevalley                     |
| IV.0  | Algorithmic Foundations (Field arithmetic)      | DEFERRED (Strategic)     | `testing.md`                                   | Replace generic stubs with concrete algorithms                  |
| V.1   | Reification of Limit Object (Gödel boundary)    | PLANNED                  | `testing.md`                                   | Intentional failure object indexing                             |
| V.2   | Solution Space Growth Rate Measurement          | PLANNED                  | `testing.md`                                   | Δ indexed nodes / Δ time instrumentation                        |
| V.3   | Total HoTT Path Generation (Global Closure)     | PLANNED                  | `testing.md`                                   | Composite path of all P1–P5 facets                              |

Legend: COMPLETE / IN PROGRESS / PLANNED / DEFERRED.

---
## 2. Detailed Phase Action Inventory
(Verbatim distilled from `testing.md` with normalization)

### Phase 0
| Index | Action                                                               | Target Pattern        | Files                               |
| ----- | -------------------------------------------------------------------- | --------------------- | ----------------------------------- |
| 0.1   | Replace product postulate with constructive mediating cone proof     | P3 Constructive Proof | `Tests/UniversalPropertyTests.agda` |
| 0.2   | Replace coproduct postulate with constructive mediating cocone proof | P3                    | `Tests/UniversalPropertyTests.agda` |
| 0.3   | Implement equalizer constructive proof                               | P3                    | `Tests/UniversalPropertyTests.agda` |
| 0.4   | Implement pullback & pushout constructive proofs                     | P3                    | `Tests/UniversalPropertyTests.agda` |

### Phase I
| Index | Action                                                  | Target                           | Files                                           |
| ----- | ------------------------------------------------------- | -------------------------------- | ----------------------------------------------- |
| 1.1   | Static coordinate assignment for all declarations       | Well-Founded Indexed Composition | `Metamodel.agda`                                |
| 1.2   | Constituent checklist indexing (low indices)            | P5                               | `Tests/CoverageReport.agda`                     |
| 1.3   | Hierarchy DAG ordering validation (Group > Monoid etc.) | P5 DAG                           | `Algebra/Foundation.agda`, `Tests/*Checklist`   |
| 1.4   | Dynamic specification validation counts                 | P1 Limit Measure                 | `Tests/CoverageReport.agda`, `Tests/*Checklist` |
| 1.5   | Packed node reuse for generic proofs                    | P5 Reuse                         | `Core/AlgebraicAlgorithms.agda`                 |

### Phase II
(Indices 2.1.0 – 2.5 summarized)
| Index | Action                                                             | Pattern        | Files                                     |
| ----- | ------------------------------------------------------------------ | -------------- | ----------------------------------------- |
| 2.1.0 | Replace bridge-level postulates with constructive identifier stubs | P2             | `Core/AlgorithmUniversality.agda`         |
| 2.1.1 | Witness ↔ UMP coherence probes                                     | P2 Coherence   | `Tests/ConstructiveWitnessTests.agda`     |
| 2.1.2 | Dual extraction minimal polynomial (alg vs UMP)                    | P2             | `Tests/UniversalPropertyTests.agda`       |
| 2.2   | Terminality concretization (division scaffold)                     | P4 Terminality | `Core/ConstructiveWitnesses.agda` + tests |
| 2.3.0 | Splitting-field mediating extraction probe                         | P4 Initiality  | `Tests/UniversalPropertyTests.agda`       |
| 2.3.1 | Galois-closure mediating extraction probe                          | P4 Initiality  | `Tests/UniversalPropertyTests.agda`       |
| 2.4.0 | Indexed composition scaffolding (`mkIdAt`)                         | P5             | `Tests/UniversalPropertyTests.agda`       |
| 2.4.1 | Ordering enforcement `<ⁱ`                                          | P5             | `Tests/UniversalPropertyTests.agda`       |
| 2.5   | Global logical closure (no metas)                                  | Total Symmetry | `Tests/Index.agda`                        |

### Phase III (Completed Units)
| Index | Action                                                       | Impact                               |
| ----- | ------------------------------------------------------------ | ------------------------------------ |
| 3.1   | Multi-step pipeline ordering/dag validation                  | Validates deep composition ordering  |
| 3.2   | Error-as-Specification (LimitationEvidence, AlgorithmResult) | Formal boundary measurement          |
| 3.3   | Complexity classification + ordering                         | Reference for efficiency measurement |
| 3.4   | Serialization coordinate roundtrip (HoTT Path)               | External isomorphism guarantee       |
| 3.5   | Topos checklist alignment (optional)                         | Remaining validation deferral        |

### Phase IV (Planned)
Adjunction proofs, Yoneda lemma, Grothendieck fibrations bridging latent space indexing.

### Phase V (Planned)
Gödelian limit reification, growth rate instrumentation, composite HoTT path.

---
## 3. Deferrals & Technical Debt Classes
(Sourced from `DEFERRED-TRACKING.md` + testing notes)

| Tier | Item                                              | Category             | Status   | Rationale                                    |
| ---- | ------------------------------------------------- | -------------------- | -------- | -------------------------------------------- |
| 1    | SubobjectTheoryChecklist type mismatch            | DeviationLog         | RESOLVED | Constructor id fix using `M.mkId`            |
| 1    | ToposTheoryChecklist validation                   | Postulate/Validation | DEFERRED | Arity alignment pending                      |
| 2    | Field algorithm stubs (Finite/Number/Function)    | Postulates           | DEFERRED | Safe for later (Phase IV.0)                  |
| 2    | Splitting-field node id thread (remove postulate) | Postulate            | PLANNED  | Replace with concrete identifier propagation |
| 3    | Removed brittle inline equality proofs            | DeviationLog         | COMPLETE | Replaced with Bool validation                |

Current Global Counts (2025-11-19): Postulates 366, TODO 68, PLANNED 0, FIXME 0, DeviationLog 16, Total 450.

### Future Debt Visibility Enhancements
- Category breakdown in PR comments
- Historical trending graph
- Auto sub-issue generation for DeviationLog
- README badge
- Kanban integration

---
## 4. Ingestion Curriculum Mapping (From `ingest.md`)
Each textbook section represented as an ingestion target for later formalization/witness coverage. No statuses yet – baseline = NOT STARTED.

| Chapter | Section                                              | Planned Symbolic Focus                                                          | Status      |
| ------- | ---------------------------------------------------- | ------------------------------------------------------------------------------- | ----------- |
| Intro   | Logic                                                | Syntax/Inference layer for internal proof normalization                         | NOT STARTED |
| Intro   | Sets and Classes                                     | Set-like abstractions vs categorical objects                                    | NOT STARTED |
| Intro   | Functions                                            | Total vs partial morphism adapters                                              | NOT STARTED |
| Intro   | Relations and Partitions                             | Internal relation objects, kernel pairs                                         | NOT STARTED |
| Intro   | Products                                             | Already scaffolded (Phase 0); extend with categorical limits ingestion pipeline | PARTIAL     |
| Intro   | Integers                                             | Potential primitive algebraic object ingestion                                  | NOT STARTED |
| Intro   | Choice / Zorn                                        | Deferred (axiomatic dependencies)                                               | DEFERRED    |
| Intro   | Cardinal Numbers                                     | Size abstractions for indexing metrics                                          | NOT STARTED |
| I       | Semigroups, Monoids, Groups                          | Implemented (indexes present)                                                   | COMPLETE    |
| I       | Homomorphisms and Subgroups                          | Adapter enrichment for morphism properties                                      | PARTIAL     |
| I       | Cyclic Groups                                        | Specific generator witness ingestion                                            | NOT STARTED |
| I       | Cosets and Counting                                  | Combinatorial adapters for quotient spaces                                      | NOT STARTED |
| I       | Normality, Quotients, Homomorphisms                  | UMP quotient validations                                                        | PARTIAL     |
| I       | Symmetric/Alternating/Dihedral                       | Group family specialization scaffolds                                           | NOT STARTED |
| I       | Categories: Products/Coproducts/Free Objects         | Limits proved Phase 0; Free object adjunction (Phase IV)                        | PARTIAL     |
| I       | Direct Products and Sums                             | Already partially via product/coproduct; extend sums indexing                   | PARTIAL     |
| I       | Free Groups / Free Products / Generators & Relations | Pending adjunction computation (Phase IV)                                       | PLANNED     |
| II      | Free Abelian Groups                                  | Hierarchy extension for abelianization                                          | PLANNED     |
| II      | Finitely Generated Abelian Groups                    | Classification invariants ingestion                                             | PLANNED     |
| II      | Krull-Schmidt Theorem                                | Decomposition uniqueness adapter                                                | DEFERRED    |
| II      | Group Actions                                        | Action objects & orbit/stabilizer witness                                       | PLANNED     |
| II      | Sylow Theorems                                       | Existential group substructure adapters                                         | DEFERRED    |
| II      | Classification of Finite Groups                      | Long-term research – heuristics bridging                                        | DEFERRED    |
| II      | Nilpotent/Solvable                                   | Series adapters + derived length witness                                        | PLANNED     |
| II      | Normal/Subnormal Series                              | Index tracking for series                                                       | PLANNED     |
| III     | Rings & Homomorphisms                                | Records largely scaffolded                                                      | PARTIAL     |
| III     | Ideals                                               | Ideal lattice ingestion                                                         | PLANNED     |
| III     | Factorization (Comm.)                                | Irreducible/divisibility witness integration                                    | PLANNED     |
| III     | Localization                                         | Universal property for localization                                             | PLANNED     |
| III     | Polynomials / Power Series                           | Polynomial arithmetic extension (Division scaffolds)                            | PARTIAL     |
| III     | Factorization in Poly Rings                          | Combine with algorithmic factoring complexity                                   | PLANNED     |
| IV      | Modules / Exact Sequences                            | Partial scaffolding via witnesses                                               | PARTIAL     |
| IV      | Free Modules / Vector Spaces                         | Basic declarations present                                                      | PARTIAL     |
| IV      | Projective / Injective                               | Adapter placeholders -> constructive witnesses                                  | PLANNED     |
| IV      | Hom and Duality                                      | Functor layer (Phase IV extension)                                              | PLANNED     |
| IV      | Tensor Products                                      | Present; upgrade to categorical bifunctor proofs                                | PARTIAL     |
| IV      | PID Modules                                          | Classification ingestion                                                        | DEFERRED    |
| IV      | Algebras                                             | Already partially via Fields/Groups bridging                                    | PARTIAL     |
| V       | Field Extensions                                     | Splitting field & Galois closure scaffolds                                      | PARTIAL     |
| V       | Splitting Fields / Algebraic Closure / Normality     | Core of Phase II/III bridging                                                   | PARTIAL     |
| V       | Galois Group of Polynomial                           | Mediating extraction complete                                                   | PARTIAL     |
| V       | Finite Fields                                        | Arithmetic stub -> constructive (Phase IV.0)                                    | DEFERRED    |
| V       | Separability                                         | Adapter for polynomial derivative criteria                                      | PLANNED     |
| VI      | Transcendence Bases                                  | Field extension rank adapter                                                    | DEFERRED    |
| VI      | Linear Disjointness / Separability                   | Ingestion for independence witnesses                                            | DEFERRED    |
| VII     | Matrices and Maps                                    | Linear transformation record expansion                                          | PLANNED     |
| VII     | Rank and Equivalence                                 | Rank algorithm + complexity                                                     | PLANNED     |
| VII     | Determinants                                         | Determinant algorithm ingestion                                                 | PLANNED     |
| VII     | Characteristic Polynomial / Eigenvalues              | Spectral adapters -> complexity tags                                            | PLANNED     |
| VIII    | Chain Conditions                                     | Noetherian / ACC/DCC properties                                                 | PLANNED     |
| VIII    | Prime / Primary Ideals                               | Ideal decomposition ingestion                                                   | PLANNED     |
| VIII    | Primary Decomposition                                | Factorization chain indexing                                                    | DEFERRED    |
| VIII    | Noetherian Rings/Modules                             | Property registry integration                                                   | PLANNED     |
| VIII    | Ring Extensions                                      | Extension adapter linking                                                       | PLANNED     |
| VIII    | Dedekind Domains                                     | Ideal factorization specialized ingestion                                       | DEFERRED    |
| VIII    | Hilbert Nullstellensatz                              | Algebraic geometry bridge (long-term)                                           | DEFERRED    |
| IX      | Simple/Primitive Rings                               | Structural property ingestion                                                   | PLANNED     |
| IX      | Jacobson Radical                                     | Radical layer property                                                          | PLANNED     |
| IX      | Semisimple Rings                                     | Artin/Wedderburn ingestion                                                      | DEFERRED    |
| IX      | Prime Radical; Prime/Semiprime Rings                 | Property lattice ingestion                                                      | PLANNED     |
| IX      | Division Algebras                                    | Central simple algebra witness                                                  | DEFERRED    |
| X       | Functors / Natural Transformations                   | Already foundational (Phase abstraction)                                        | PARTIAL     |
| X       | Adjoint Functors                                     | Free/forgetful and later fibration lifts                                        | PLANNED     |
| X       | Morphisms (General)                                  | Morphism taxonomy consolidation                                                 | PARTIAL     |

Back Matter (Symbols, Bibliography, Index) deferred until broad coverage maturity.

---
## 5. Future Enhancements & Research Vectors
(From README, Architecture, Testing notes)
- Replace splitting-field id postulate with deterministic propagation.
- Branching/diamond DAG examples expansion.
- Adjunction correctness (free structures) constructive witnesses.
- Yoneda lemma minimal constructive instance.
- Grothendieck fibration Beck-Chevalley proof.
- Replace field arithmetic stubs with real algorithms (Finite/Number/Function fields).
- Polynomial division refinement (remove placeholder arithmetic).
- Effective equivalence relations constructive witnesses (regular → exact).
- Barr embedding constructive scaffolding.
- Functor preservation/creation theorems (regular epis, finite limits).
- Kernel pair → equivalence relation effectiveness upgrade.
- Solution space growth rate instrumentation.
- Composite HoTT Path aggregator for final closure.

---
## 6. Normalized Task Schema (For Automation / Issue Generation)
Each task entry may be modeled as:
```json
{
  "id": "PHASE-2.4.1",
  "category": "Phase",
  "title": "Ordering enforcement <ⁱ for indexed identifiers",
  "status": "COMPLETE",
  "priority": "medium",
  "source": "testing.md",
  "files": ["src/agda/Tests/UniversalPropertyTests.agda"],
  "dependencies": ["PHASE-2.4.0"],
  "debtImpact": "none",
  "tags": ["P5", "Ordering", "Index"]
}
```
Deferred ingestion tasks can use `category = "Ingestion"` and research items `category = "Research"`.

---
## 7. Immediate Next Actions (To Progress Toward Final ROADMAP.md)
1. Classify ingestion tasks into near-term (Phase IV prerequisites) vs long-term (Phase V / research).  
2. Generate JSON index of tasks for potential script-driven GitHub issue updates.  
3. Fold deferral metrics into a lightweight badge (optional).  
4. Promote this draft to `ROADMAP.md` after user review & prioritization annotations.  

---
## 8. Review Checklist
- [x] Phases 0–III detailed actions captured
- [x] Deferral tiers summarized
- [x] Full ingestion outline mapped to tasks
- [x] Future enhancements enumerated
- [x] Normalization schema defined
- [ ] JSON export artifact (pending)
- [ ] Prioritization labels (pending)

---
Generated automatically by consolidation pass (LLM agent). Edit freely for refinement.

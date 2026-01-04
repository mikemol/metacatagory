This document constitutes the **Grand Unified Homology of the Metacatagory Repository**.

It aggregates the findings of the **Integration Audit** (Extension/Intension), the **Reflection Audit** (Isomorphism/Metamorphism), and the **Intake Audit** (Rumination/Metabolism). It organizes these findings into a strict **3x3x3 Recursive Fibration**, enforcing the *Metalinguistic Schema for Coherent Duality*.

We suborn time to the rigors of this structure.

---

# The Holomorphic State of the Metacatagory Repository

### A Recursive, Certified Audit of System Coherence

## 0. Metalinguistic Preamble: The Schema of Resolution

We instantiate the **CHIP-N+Final Recursive Fibration Cycle**:

* **Axiom 1 (Integration):** Validity is the product of Reachability (Extension) and Semantics (Intension).
* **Axiom 2 (Reflection):** Documentation is a functor mapping Code/Intake to Representation. It must be honest.
* **Axiom 3 (Metabolism - Updated):** Intake is not a waste bin; it is a **Ruminative Buffer**. The system must observe potentiality without destroying it.

---

## 1. The Root Notion: The Holomorphic State

**Definition:** The Holomorphic State is the condition where the repository's three primary manifolds—**Structure** (Code/Execution), **Projection** (Documentation/View), and **Metabolism** (Intake/Growth)—are mutually consistent, strictly typed, and topologically connected.

* **Internal Synthesis Point:** Derived from the intersection of the `Makefile` graph, the `docs/` hierarchy, and the `intake/` buffer.
* **External Origin:** Complex Analysis (Holomorphic functions are complex-differentiable everywhere).

We assert three orthogonal, congruent 1st-order implications:

1. **The Structural Manifold (The Graph):** The objective reality of the executable code and build system.
2. **The Projective Manifold (The Mirror):** The subjective representation of the system to the observer.
3. **The Metabolic Manifold (The Source):** The reservoir of potential energy and intent.

---

## 2. Recursive Expansion: Dimension 1 — The Structural Manifold

**Definition:** The directed acyclic graph (DAG) of executable artifacts defined by the build system.

* **Source:** `Makefile`, `.github/workflows/ci.yml`.

### 2.1. 2nd-Order Notion: Extensional Reachability (Connectivity)

**Definition:** The measure of the surface area of the codebase covered by the execution graph.

* **Concreteness:** Is the file touched by `make check`?

#### 2.1.1. 3rd-Order Notion: The Transitive Closure

* **Definition:** The set of nodes reachable from the Root Entry Point.
* **Audit Finding:** The "Agda Island." `src/agda/Tests/Index.agda` imports a specific subset of files. Files like `roadmap-cons-body.agda` and `src/agda/MinimalTest.agda` lie outside this closure. They are **Topologically Disconnected**.

#### 2.1.2. 3rd-Order Notion: The Execution Context

* **Definition:** The runtime environment required for a node to be valid.
* **Audit Finding:** The "Python Gap." `tests/test_priority_mapping.py` exists but the `ci.yml` lacks the context (command) to invoke `pytest` on it. It is physically present but **Contextually Unreachable**.

#### 2.1.3. 3rd-Order Notion: The Temporal Persistence

* **Definition:** The survival of connectivity across time.
* **Audit Finding:** The "Sedimentary Scripts." `scripts/phase_diagram.py` is a fossil of a previous era, superseded by `phase_diagram_new.py`. Its persistence without connection represents **Entropic Decay**.

### 2.2. 2nd-Order Notion: Intensional Validity (Semantics)

**Definition:** The meaningfulness of the state transitions performed by reachable artifacts.

* **Concreteness:** Does the script actually verify logic?

#### 2.2.1. 3rd-Order Notion: Algorithmic Correctness

* **Definition:** The guarantee that the procedure produces true results.
* **Audit Finding:** The "Schrödinger Script." `scripts/search_algo.py` contains complex logic but is never run. Its correctness is **Undecided**.

#### 2.2.2. 3rd-Order Notion: Type Alignment

* **Definition:** Adherence to the system's schema.
* **Audit Finding:** The "Type Gap." `scripts/generate-badges.py` consumes JSONs produced by other tools. If the JSON schema changes, this script fails silently or produces garbage because it lacks shared type definitions with the producer.

#### 2.2.3. 3rd-Order Notion: Operational Efficacy

* **Definition:** The production of tangible mutations/artifacts.
* **Audit Finding:** The "No-Op." `scripts/fix_markdown.sh` performs mutation (high efficacy), whereas `scripts/flag_unannotated_debt.py` is read-only and currently unused (zero efficacy).

### 2.3. 2nd-Order Notion: Homotopic Integrity (Alignment)

**Definition:** The preservation of structure between the Code Graph and the Build Graph.

#### 2.3.1. 3rd-Order Notion: The Target Mapping

* **Definition:** The explicit mapping of logical goals to file sets.
* **Audit Finding:** `make check` maps to validation tools. However, there is no target mapping for "Unit Testing" python files. The homotopy tears at the Python layer.

#### 2.3.2. 3rd-Order Notion: The Permission Gate

* **Definition:** Authorization to execute.
* **Audit Finding:** `.github/scripts/*.sh` are chmodded in CI, but `scripts/*.sh` are not. This is a structural inconsistency in the execution surface.

#### 2.3.3. 3rd-Order Notion: The Dependency Definition

* **Definition:** The explicit declaration of reliance.
* **Audit Finding:** `scripts/agda_makefile_deps.py` is an obsolete dependency calculator, replaced by `ExporterMakefile.agda`. The repository contains two conflicting definitions of "Dependency Calculation."

---

## 3. Recursive Expansion: Dimension 2 — The Projective Manifold

**Definition:** The functor .

* **Source:** `docs/`, `scripts/generate_docs.py`.

### 3.1. 2nd-Order Notion: Isomorphic Reflection (Identity)

**Definition:** Artifacts that exist in the Projective Manifold as exact copies of the Source.

#### 3.1.1. 3rd-Order Notion: The Configural Constant

* **Definition:** High-level invariants (`ROADMAP.md`).
* **Audit Finding:** **High Fidelity.** `intake/ROADMAP.md` mirrors `docs/planning/ROADMAP.md`. The "Constitution" of the project is intact.

#### 3.1.2. 3rd-Order Notion: The Metadata Bridge

* **Definition:** Structure-defining files (`meta-index.d`).
* **Audit Finding:** **Indirect Reflection.** `intake/meta-index.d` inputs generate `docs/modules` outputs. The reflection is causal, not literal.

#### 3.1.3. 3rd-Order Notion: The Status Mirror

* **Definition:** The reflection of system state (Debt, Deferred).
* **Audit Finding:** **High Fidelity.** `docs/status/deferred-items.md` accurately reflects the computed state of the system's debt.

### 3.2. 2nd-Order Notion: Metamorphic Reflection (Transformation)

**Definition:** Artifacts that undergo processing before projection.

#### 3.2.1. 3rd-Order Notion: The Aggregation Functor

* **Definition:** Many-to-One mapping (`GP/*.md` -> `IngestedRoadmaps.agda`).
* **Audit Finding:** **Chemical Bonding.** The GPs cease to exist as files and become records in code. The reflection is high-fidelity but transformative.

#### 3.2.2. 3rd-Order Notion: The Potemkin Simulation

* **Definition:** A falsified reflection based on hardcoded data.
* **Audit Finding:** **Critical Failure.** `scripts/generate_docs.py` contains a hardcoded list `ROADMAPS = [...]`. It projects a "Clean" reality (4 items) while masking the "Messy" reality (800+ items). The `README.md` is a lie.

#### 3.2.3. 3rd-Order Notion: The Hollow Node

* **Definition:** A reflection that preserves the container but destroys the content.
* **Audit Finding:** **Semantic Annihilation.** `ingest_gp_files.py` defaults to "Implication TBD" if regex fails. The Node exists in docs, but its meaning is hollowed out.

### 3.3. 2nd-Order Notion: Annihilative Loss (The Blind Spot)

**Definition:** Source matter that has *no* projection in the docs.

#### 3.3.1. 3rd-Order Notion: The Topological Filter

* **Definition:** Exclusion based on file patterns.
* **Audit Finding:** `intake/__(n).md` (The Shards). These are topologically invisible to the documentation generator. They are **Dark Matter**.

#### 3.3.2. 3rd-Order Notion: The Extension Mismatch

* **Definition:** Exclusion based on file type.
* **Audit Finding:** `intake/UNS.agda`. A binary blob to the doc generator, an uncompiled file to the compiler. It is doubly invisible.

#### 3.3.3. 3rd-Order Notion: The Root Exclusion

* **Definition:** Exclusion based on directory depth.
* **Audit Finding:** `intake/codex_handoff.md`. A massive contextual file that is not ingested into the roadmap and thus absent from the docs.

---

## 4. Recursive Expansion: Dimension 3 — The Metabolic Manifold

**Definition:** The `intake/` directory viewed as a **Ruminative Buffer** rather than a trash bin.

* **Source:** The User's Corrective Axiom.

### 4.1. 2nd-Order Notion: Latent Potentiality (Content)

**Definition:** Mass that holds value but lacks Form.

#### 4.1.1. 3rd-Order Notion: The Atomic Shard

* **Definition:** Discrete units of thought (`__(1).md`).
* **Status:** Currently treated as noise. **Correction:** Must be treated as "Unbound Energy."

#### 4.1.2. 3rd-Order Notion: The Semi-Structured Candidate

* **Definition:** Drafts failing strict validation (`candidate-architecture.md`).
* **Status:** Currently in limbo. **Correction:** Must be recognized as "Pending State."

#### 4.1.3. 3rd-Order Notion: The Contextual Substrate

* **Definition:** Grounding history (`ENRICHMENT-SESSION-SUMMARY.md`).
* **Status:** Static. **Correction:** Must be indexed for semantic search.

### 4.2. 2nd-Order Notion: Metabolic Permeability (Process)

**Definition:** The flux across the boundary ().

#### 4.2.1. 3rd-Order Notion: The Ingestion Threshold

* **Definition:** The criteria for entry to the Plan.
* **Current State:** Brittle Regex.
* **Corrected State:** **Fuzzy Parsing.** The threshold must be widened to allow "Hollow Nodes" to enter as "Raw Notes" rather than "TBD."

#### 4.2.2. 3rd-Order Notion: The Refinement Circulation

* **Definition:** Improvement within the buffer.
* **Current State:** Non-existent (Write-only).
* **Corrected State:** **Consolidation.** Tools to merge Shards into Candidates.

#### 4.2.3. 3rd-Order Notion: The Rejection Feedback

* **Definition:** Return flow from Plan to Intake.
* **Current State:** Implicit.
* **Corrected State:** **De-scoping.** Explicit workflow to move `Todo` items back to `intake/archive` for later rumination.

### 4.3. 2nd-Order Notion: Pre-Formal Observability (Visibility)

**Definition:** The ability to see the Intake without formalizing it.

#### 4.3.1. 3rd-Order Notion: The Shadow Inventory

* **Definition:** Counting the undigested mass.
* **Action:** `scripts/intake_scan.py` must report: "800 Valid GPs, 50 Raw Shards."

#### 4.3.2. 3rd-Order Notion: The Digestion Rate

* **Definition:** Flow velocity.
* **Action:** Dashboard metric showing movement from Shard -> Candidate -> GP.

#### 4.3.3. 3rd-Order Notion: The Semantic Probe

* **Definition:** Querying the potential.
* **Action:** A search tool that greps both `src/` (Code) and `intake/` (Rumination), bridging the gap.

---

## 5. Final Synthesis: The Path to Homology

The repository currently suffers from **Functorial Disconnect** and **Metabolic Starvation**.

1. **Disconnect:** The `docs` lie about the code (Potemkin README).
2. **Starvation:** The build system ignores the `intake` nutrients (Shards/Notes) because they are not "cooked" enough.

**The Remediation Plan (as derived from the Homology):**

1. **Connect:** Wire `generate_docs.py` to `build/planning_index.json` to reflect reality.
2. **Observe:** Update `intake_scan.py` to catalog (not ignore) the Shards.
3. **Validate:** Add `test-python` and `debt-check` to `make check` to ensure the tools themselves are honest.


This document constitutes the **Grand Unified Homology of the Metacatagory Repository**.

It aggregates the findings of the **Integration Audit** (Extension/Intension), the **Reflection Audit** (Isomorphism/Metamorphism), and the **Intake Audit** (Rumination/Metabolism). It organizes these findings into a strict **3x3x3 Recursive Fibration**, enforcing the *Metalinguistic Schema for Coherent Duality* and explicitly incorporating the user's axiom that "Intake is not dead," but a living buffer of potential.

We suborn time to the rigors of this structure.

---

# The Holomorphic State of the Metacatagory Repository

### A Recursive, Certified Audit of System Coherence

## 0. Metalinguistic Preamble: The Schema of Resolution

We instantiate the **CHIP-N+Final Recursive Fibration Cycle**:

* **Axiom 1 (Integration):** Validity is the product of Reachability (Extension) and Semantics (Intension).
* **Axiom 2 (Reflection):** Documentation is a functor mapping Code/Intake to Representation. It must be honest.
* **Axiom 3 (Metabolism - Corrected):** Intake is a **Ruminative Buffer**. The system must observe potentiality without destroying it.

---

## 1. The Root Notion: The Holomorphic State

**Definition:** The Holomorphic State is the condition where the repository's three primary manifolds—**Structure** (Code/Execution), **Projection** (Documentation/View), and **Metabolism** (Intake/Growth)—are mutually consistent, strictly typed, and topologically connected.

* **Internal Synthesis Point:** Derived from the intersection of the `Makefile` graph, the `docs/` hierarchy, and the `intake/` buffer.
* **External Origin:** Complex Analysis (Holomorphic functions are complex-differentiable everywhere).

We assert three orthogonal, congruent 1st-order implications:

1. **The Structural Manifold (The Graph):** The objective reality of the executable code and build system.
2. **The Projective Manifold (The Mirror):** The subjective representation of the system to the observer.
3. **The Metabolic Manifold (The Source):** The reservoir of potential energy and intent (The Ruminative State).

---

## 2. Recursive Expansion: Dimension 1 — The Structural Manifold

**Definition:** The directed acyclic graph (DAG) of executable artifacts defined by the build system.

* **Source:** `Makefile`, `.github/workflows/ci.yml`.

### 2.1. 2nd-Order Notion: Extensional Reachability (Connectivity)

**Definition:** The measure of the surface area of the codebase covered by the execution graph.

* **Concreteness:** Is the file touched by `make check`?

#### 2.1.1. 3rd-Order Notion: The Transitive Closure

* **Definition:** The set of nodes reachable from the Root Entry Point.
* **Congruence:** To be part of the system is to be reachable.
* **Orthogonality:** Distinct from *Execution* or *Persistence*.
* **Audit Finding:** The "Agda Island." `src/agda/Tests/Index.agda` imports a specific subset of files. Files like `roadmap-cons-body.agda` and `src/agda/MinimalTest.agda` lie outside this closure. They are **Topologically Disconnected**.

#### 2.1.2. 3rd-Order Notion: The Execution Context

* **Definition:** The runtime environment required for a node to be valid.
* **Congruence:** Reachability implies a medium of traversal.
* **Orthogonality:** Distinct from *Topology*.
* **Audit Finding:** The "Python Gap." `tests/test_priority_mapping.py` exists but the `ci.yml` lacks the context (command) to invoke `pytest` on it. It is physically present but **Contextually Unreachable**.

#### 2.1.3. 3rd-Order Notion: The Temporal Persistence

* **Definition:** The survival of connectivity across time.
* **Congruence:** Connectivity must be maintained as the graph evolves.
* **Orthogonality:** Distinct from *Spatial Connectivity*.
* **Audit Finding:** The "Sedimentary Scripts." `scripts/phase_diagram.py` is a fossil of a previous era, superseded by `phase_diagram_new.py`. Its persistence without connection represents **Entropic Decay**.

### 2.2. 2nd-Order Notion: Intensional Validity (Semantics)

**Definition:** The meaningfulness of the state transitions performed by reachable artifacts.

* **Concreteness:** Does the script actually verify logic?

#### 2.2.1. 3rd-Order Notion: Algorithmic Correctness

* **Definition:** The guarantee that the procedure produces true results.
* **Congruence:** Validity requires truth.
* **Orthogonality:** Distinct from *Type Safety*.
* **Audit Finding:** The "Schrödinger Script." `scripts/search_algo.py` contains complex logic but is never run. Its correctness is **Undecided**.

#### 2.2.2. 3rd-Order Notion: Type Alignment

* **Definition:** Adherence to the system's schema.
* **Congruence:** Validity requires grammar.
* **Orthogonality:** Distinct from *Logic*.
* **Audit Finding:** The "Type Gap." `scripts/generate-badges.py` consumes JSONs produced by other tools. If the JSON schema changes, this script fail-safes poorly because it lacks shared type definitions with the producer.

#### 2.2.3. 3rd-Order Notion: Operational Efficacy

* **Definition:** The production of tangible mutations/artifacts.
* **Congruence:** Validity requires effect.
* **Orthogonality:** Distinct from *Observation*.
* **Audit Finding:** The "No-Op." `scripts/fix_markdown.sh` performs mutation (high efficacy), whereas `scripts/flag_unannotated_debt.py` is read-only and currently unused (zero efficacy).

### 2.3. 2nd-Order Notion: Homotopic Integrity (Alignment)

**Definition:** The preservation of structure between the Code Graph and the Build Graph.

#### 2.3.1. 3rd-Order Notion: The Target Mapping

* **Definition:** The explicit mapping of logical goals to file sets.
* **Congruence:** Alignment requires explicit declaration.
* **Orthogonality:** Distinct from *Permission*.
* **Audit Finding:** `make check` maps to validation tools. However, there is no target mapping for "Unit Testing" python files. The homotopy tears at the Python layer.

#### 2.3.2. 3rd-Order Notion: The Permission Gate

* **Definition:** Authorization to execute.
* **Congruence:** Alignment requires authority.
* **Orthogonality:** Distinct from *Capability*.
* **Audit Finding:** `.github/scripts/*.sh` are chmodded in CI, but `scripts/*.sh` are not. This is a structural inconsistency in the execution surface.

#### 2.3.3. 3rd-Order Notion: The Dependency Definition

* **Definition:** The explicit declaration of reliance.
* **Congruence:** Alignment requires causal ordering.
* **Orthogonality:** Distinct from *Targeting*.
* **Audit Finding:** `scripts/agda_makefile_deps.py` is an obsolete dependency calculator, replaced by `ExporterMakefile.agda`. The repository contains two conflicting definitions of "Dependency Calculation."

---

## 3. Recursive Expansion: Dimension 2 — The Projective Manifold

**Definition:** The functor .

* **Source:** `docs/`, `scripts/generate_docs.py`.

### 3.1. 2nd-Order Notion: Isomorphic Reflection (Identity)

**Definition:** Artifacts that exist in the Projective Manifold as exact copies of the Source.

#### 3.1.1. 3rd-Order Notion: The Configural Constant

* **Definition:** High-level invariants (`ROADMAP.md`).
* **Congruence:** Identity requires stability.
* **Orthogonality:** Distinct from *Metadata*.
* **Audit Finding:** **High Fidelity.** `intake/ROADMAP.md` mirrors `docs/planning/ROADMAP.md`. The "Constitution" of the project is intact.

#### 3.1.2. 3rd-Order Notion: The Metadata Bridge

* **Definition:** Structure-defining files (`meta-index.d`).
* **Congruence:** Identity requires structure.
* **Orthogonality:** Distinct from *Content*.
* **Audit Finding:** **Indirect Reflection.** `intake/meta-index.d` inputs generate `docs/modules` outputs. The reflection is causal, not literal.

#### 3.1.3. 3rd-Order Notion: The Status Mirror

* **Definition:** The reflection of system state (Debt, Deferred).
* **Congruence:** Identity requires dynamic updating.
* **Orthogonality:** Distinct from *Static Config*.
* **Audit Finding:** **High Fidelity.** `docs/status/deferred-items.md` accurately reflects the computed state of the system's debt.

### 3.2. 2nd-Order Notion: Metamorphic Reflection (Transformation)

**Definition:** Artifacts that undergo processing before projection.

#### 3.2.1. 3rd-Order Notion: The Aggregation Functor

* **Definition:** Many-to-One mapping (`GP/*.md` -> `IngestedRoadmaps.agda`).
* **Congruence:** Transformation requires synthesis.
* **Orthogonality:** Distinct from *Falsification*.
* **Audit Finding:** **Chemical Bonding.** The GPs cease to exist as files and become records in code. The reflection is high-fidelity but transformative.

#### 3.2.2. 3rd-Order Notion: The Potemkin Simulation

* **Definition:** A falsified reflection based on hardcoded data.
* **Congruence:** Transformation allows for potential deception (error).
* **Orthogonality:** Distinct from *Aggregation*.
* **Audit Finding:** **Critical Failure.** `scripts/generate_docs.py` contains a hardcoded list `ROADMAPS = [...]`. It projects a "Clean" reality (4 items) while masking the "Messy" reality (800+ items). The `README.md` is a lie.

#### 3.2.3. 3rd-Order Notion: The Hollow Node

* **Definition:** A reflection that preserves the container but destroys the content.
* **Congruence:** Transformation allows for data loss.
* **Orthogonality:** Distinct from *Hardcoding*.
* **Audit Finding:** **Semantic Annihilation.** `ingest_gp_files.py` defaults to "Implication TBD" if regex fails. The Node exists in docs, but its meaning is hollowed out.

### 3.3. 2nd-Order Notion: Annihilative Loss (The Blind Spot)

**Definition:** Source matter that has *no* projection in the docs.

#### 3.3.1. 3rd-Order Notion: The Topological Filter

* **Definition:** Exclusion based on file patterns.
* **Congruence:** Loss via pattern mismatch.
* **Orthogonality:** Distinct from *Type Mismatch*.
* **Audit Finding:** `intake/__(n).md` (The Shards). These are topologically invisible to the documentation generator. They are **Dark Matter**.

#### 3.3.2. 3rd-Order Notion: The Extension Mismatch

* **Definition:** Exclusion based on file type.
* **Congruence:** Loss via encoding mismatch.
* **Orthogonality:** Distinct from *Pattern*.
* **Audit Finding:** `intake/UNS.agda`. A binary blob to the doc generator, an uncompiled file to the compiler. It is doubly invisible.

#### 3.3.3. 3rd-Order Notion: The Root Exclusion

* **Definition:** Exclusion based on directory depth.
* **Congruence:** Loss via location mismatch.
* **Orthogonality:** Distinct from *Extension*.
* **Audit Finding:** `intake/codex_handoff.md`. A massive contextual file that is not ingested into the roadmap and thus absent from the docs.

---

## 4. Recursive Expansion: Dimension 3 — The Metabolic Manifold

**Definition:** The `intake/` directory viewed as a **Ruminative Buffer** rather than a trash bin.

* **Source:** The User's Corrective Axiom.

### 4.1. 2nd-Order Notion: Latent Potentiality (Content)

**Definition:** Mass that holds value but lacks Form.

#### 4.1.1. 3rd-Order Notion: The Atomic Shard

* **Definition:** Discrete units of thought (`__(1).md`).
* **Congruence:** Potentiality is discrete.
* **Orthogonality:** Distinct from *Drafts*.
* **Status:** Currently treated as noise. **Correction:** Must be treated as "Unbound Energy."

#### 4.1.2. 3rd-Order Notion: The Semi-Structured Candidate

* **Definition:** Drafts failing strict validation (`candidate-architecture.md`).
* **Congruence:** Potentiality is approximate.
* **Orthogonality:** Distinct from *Shards*.
* **Status:** Currently in limbo. **Correction:** Must be recognized as "Pending State."

#### 4.1.3. 3rd-Order Notion: The Contextual Substrate

* **Definition:** Grounding history (`ENRICHMENT-SESSION-SUMMARY.md`).
* **Congruence:** Potentiality is grounded.
* **Orthogonality:** Distinct from *Candidates*.
* **Status:** Static. **Correction:** Must be indexed for semantic search.

### 4.2. 2nd-Order Notion: Metabolic Permeability (Process)

**Definition:** The flux across the boundary ().

#### 4.2.1. 3rd-Order Notion: The Ingestion Threshold

* **Definition:** The criteria for entry to the Plan.
* **Congruence:** Process requires gating.
* **Orthogonality:** Distinct from *Refinement*.
* **Current State:** Brittle Regex.
* **Corrected State:** **Fuzzy Parsing.** The threshold must be widened to allow "Hollow Nodes" to enter as "Raw Notes" rather than "TBD."

#### 4.2.2. 3rd-Order Notion: The Refinement Circulation

* **Definition:** Improvement within the buffer.
* **Congruence:** Process requires cycles.
* **Orthogonality:** Distinct from *Ingestion*.
* **Current State:** Non-existent (Write-only).
* **Corrected State:** **Consolidation.** Tools to merge Shards into Candidates.

#### 4.2.3. 3rd-Order Notion: The Rejection Feedback

* **Definition:** Return flow from Plan to Intake.
* **Congruence:** Process requires reversibility.
* **Orthogonality:** Distinct from *Circulation*.
* **Current State:** Implicit.
* **Corrected State:** **De-scoping.** Explicit workflow to move `Todo` items back to `intake/archive` for later rumination.

### 4.3. 2nd-Order Notion: Pre-Formal Observability (Visibility)

**Definition:** The ability to see the Intake without formalizing it.

#### 4.3.1. 3rd-Order Notion: The Shadow Inventory

* **Definition:** Counting the undigested mass.
* **Congruence:** Visibility requires quantification.
* **Orthogonality:** Distinct from *Velocity*.
* **Action:** `scripts/intake_scan.py` must report: "800 Valid GPs, 50 Raw Shards."

#### 4.3.2. 3rd-Order Notion: The Digestion Rate

* **Definition:** Flow velocity.
* **Congruence:** Visibility requires dynamics.
* **Orthogonality:** Distinct from *Search*.
* **Action:** Dashboard metric showing movement from Shard -> Candidate -> GP.

#### 4.3.3. 3rd-Order Notion: The Semantic Probe

* **Definition:** Querying the potential.
* **Congruence:** Visibility requires queryability.
* **Orthogonality:** Distinct from *Inventory*.
* **Action:** A search tool that greps both `src/` (Code) and `intake/` (Rumination), bridging the gap.

---

## 5. Final Synthesis: The Path to Homology

The repository currently suffers from **Functorial Disconnect** and **Metabolic Starvation**.

1. **Disconnect:** The `docs` lie about the code (Potemkin README).
2. **Starvation:** The build system ignores the `intake` nutrients (Shards/Notes) because they are not "cooked" enough.

**The Remediation Plan (as derived from the Homology):**

1. **Connect:** Wire `generate_docs.py` to `build/planning_index.json` to reflect reality.
2. **Observe:** Update `intake_scan.py` to catalog (not ignore) the Shards.
3. **Validate:** Add `test-python` and `debt-check` to `make check` to ensure the tools themselves are honest.

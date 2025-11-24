# The Meta-Process Engine: Automated Observability

> **Coordinate System:** Meta-Layer (Process as Object)
> **Parent Index:** $(8, 2)$
> **Context:** The Metric Functor & Coherence Witnesses.

This directory contains the **Autonomous Agents** (Scripts) responsible for observing the "Solution Space" and reifying its properties into the "Process Space" (Badges, Reports).

-----

## 1. The Metric Functor (`generate-badges.py`)

* **Role:** Calculates the **Emergent Metric** (Cost/Debt) of the lattice.
* **Input:** The state of `src/agda/`.
* **Output:** JSON State vectors in `.github/badges/`.
* **Theory:** This script acts as the measure $\mu$ on the topological space of the code, assigning a "Weight" to every hole (elision) found.

## 2. The Topology Visualizer (`phase_diagram.py`)

* **Role:** Generates the Phase Diagram of the system's evolution.
* **Function:** Maps the trajectory of the repository through the Phase Space defined in `Core/Phase.agda`.

## 3. The Debt Surveyor (`flag_unannotated_debt.py`)

* **Role:** The **Godelian Boundary Scanner**.
* **Function:** Scans the lattice for `{! !}` (Holes) or `TODO` markers that have not been formally registered in `src/agda/Core/TechnicalDebt.agda`.
* **Constraint:** Unregistered debt is a violation of the **Indexing Axiom**.

## 4. Usage (Manual Invocation)

While these agents typically run via CI (GitHub Actions), they can be invoked locally to verify the metric state:

### Direct Script Invocation
```bash
# Calculate current metrics
python3 scripts/generate-badges.py

# Generate visualization
python3 scripts/phase_diagram.py

# Flag unannotated technical debt
python3 scripts/flag_unannotated_debt.py
```

### Recommended: Makefile Targets

All automation scripts and reporting tools are now available via Makefile targets for consistency and ease of use:

```bash
# Generate badges and top-offenders.md
make badges

# Generate all documentation (HTML + Markdown)
make docs-all

# Auto-format markdown files
make md-fix

# Lint markdown files
make md-lint

# Track deferred items
make deferred-items

# Sync roadmap issues
make roadmap-sync
```

These targets are used by CI workflows and can be run locally for development, reporting, and code hygiene.
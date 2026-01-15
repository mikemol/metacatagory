```text
Build Topology | `make` as Lazy Evaluation
============================================

This project treats `make` as an “eventually consistent” engine whose build graph
encodes all interesting artifacts.  Each target is either:

  * a **high-level gravity well** you invoke directly when you need a full
    validation/build run, or
  * a **documented intermediate** whose only purpose is to describe the state
    that the gravity wells pull in lazily.

Make only rebuilds a target when one of its dependencies changes, so you always
get the natural lazily-evaluated behavior you expect from traditional C/C++
builds.  Running `make check-all` or `make regen-all` (or the other gravity
wells below) is the only day-to-day action you need to remember; everything
else is just a map of intermediate nodes you can inspect or force when something
misbehaves.

Gravity Wells (Entry Points)
----------------------------

| Target | Description | Key Dependencies |
|--------|-------------|------------------|
| `regen-all` | Regenerate all tracked artifacts (exports, docs, roadmap, badges). | `regen-agda`, `regen-exports`, `regen-roadmap`, `regen-docs`, `regen-badges`, `regen-intake` |
| `check-all` | Full validation suite (makefile + docs + roadmap + tests). | `check-infra`, `check-docs`, `check-roadmap`, `check-python`, `check-json`, `check-debt` |
| `validate-constructive` | Regenerate and validate end-to-end. | `regen-all`, `check-all` |
| `priority-refresh` | Re-run the priority pipeline (planning index, docs, badge weights). | `data/planning_index.json`, `.github/roadmap/tasks.json`, `.github/badges/weights.json`, `badges` |
| `regen-docs` | Regenerate documentation exports (markdown only). | `docs-all` |

CI artifacts (scoped per job) land under:
- `build/reports/agda` (agda-exports job)
- `build/reports/docs` (docs-checks job)
- `build/reports/roadmap` (roadmap-json job)
- `build/reports/python` (python/debt job)
- Combined bundle: `build-reports-all` (collect-reports job)

What’s inside (typical):
- agda: makefile coverage/validate logs, recipe scripts, dependency graph exports (planning/dependency JSON).
- docs: markdown lint reports, roadmap AST, doc coverage.
- roadmap: tasks_enriched.md, dependency_graph.{mmd,dot}, roundtrip logs.
- python: pytest/test-report outputs, debt checks.

Intermediate Nodes (Documented)
--------------------------------

These targets correspond to specific artifacts or Agda compilations.  They do
not need to be invoked manually unless you have an Agda subsystem change and
want to re-run just that stage.

  * `build/canonical_roadmap.json` – JSON synthesized from `intake/` files; every roadmap/badge export depends on it.
  * `data/planning_index.json` / `roadmap-export-*` / `roadmap-validate-*` – export the planning kernel and roadmap from Agda, then validate them.
  * `roadmap-index`, `planning-kernel`, `roadmap-sppf(-export)`, `roadmap-sync`, etc. – Agda compilations for each Cosmological Roadmap subsystem. (`roadmap-sync` is a local Make target; CI uses `ci.yml`.)
  * `build/priority_profile.json` and `.github/badges/weights.json` – priority pipeline outputs; chained under `priority-refresh`.
  * `build/reports/roadmap_ast.txt` / `docs-modules` – Roadmap AST + module exporters that generate markdown; `make check-all` or `regen-docs` pull these in automatically.
  * `md-lint` / `md-fix` / `intake-lint` – Markdown linting tools; the linting nodes already respect Make’s lazy timestamps so they run only when touched, and `md-fix`/`intake-lint` exist mainly for manual cleanup.
  * `node-deps` / `deferred-items` / `data/dependency_graph.json` – optional analytic runs that you run explicitly; they rarely belong in a gravity well unless you are debugging that particular output.

  * `.github/deferred-queue.json` – machine-readable ordered queue derived from `.github/badges/deferred-files.json`; refreshed with `priority-refresh`.

Running a gravity well writes the timestamp graph and leaves all intermediates “built” until their inputs change again.  That is your complete topology: remember the few high-level wells, and trust `make` to lazily recompute the nodes below when source files shift.
```

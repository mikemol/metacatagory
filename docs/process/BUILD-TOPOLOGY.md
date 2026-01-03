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
builds.  Running `make check` (or the other gravity wells below) is the only
day-to-day action you need to remember; everything else is just a map of
intermediate nodes you can inspect or force when something misbehaves.

Gravity Wells (Entry Points)
----------------------------

| Target | Description | Key Dependencies |
|--------|-------------|------------------|
| `check` | CI-style gate that validates the makefile, docs, roadmap triangle, and the agda build. | `makefile-validate`, `md-lint`, `roadmap-validate-triangle`, `docs-validate`, `all` |
| `priority-refresh` | Re-run the priority pipeline (planning index, docs, badge weights). | `planning-index-json`, `roadmap-export-json`, `priority-badge-weights`, `badges` |
| `docs-all` | Generate docs exports (markdown only). | `docs-generate`, `docs-modules` |
| `validate-constructive` | Runs every constructive validation and enrichment, including docs normalisation. | `docs-all`, `roadmap-export-*`, `roadmap-enrich`, `roadmap-all-enriched`, `intake-scan`, `md-normalize`, `badges` |

Intermediate Nodes (Documented)
--------------------------------

These targets correspond to specific artifacts or Agda compilations.  They do
not need to be invoked manually unless you have an Agda subsystem change and
want to re-run just that stage.

  * `build/canonical_roadmap.json` – JSON synthesized from `intake/` files; every roadmap/badge export depends on it.
  * `planning-index-json` / `roadmap-export-*` / `roadmap-validate-*` – export the planning kernel and roadmap from Agda, then validate them.
  * `roadmap-index`, `planning-kernel`, `roadmap-sppf(-export)`, `roadmap-sync`, etc. – Agda compilations for each Cosmological Roadmap subsystem.
  * `priority-profile-json` and `priority-badge-weights` – priority pipeline outputs; chained under `priority-refresh`.
  * `docs-generate` / `docs-modules` – Roadmap + module exporters that generate markdown; `make check` or `docs-all` pull these in automatically.
  * `md-lint` / `md-fix` / `intake-lint` – Markdown linting tools; the linting nodes already respect Make’s lazy timestamps so they run only when touched, and `md-fix`/`intake-lint` exist mainly for manual cleanup.
  * `node-deps` / `deferred-items` / `dependency-graph-json` – optional analytic runs that you run explicitly; they rarely belong in a gravity well unless you are debugging that particular output.

Running a gravity well writes the timestamp graph and leaves all intermediates “built” until their inputs change again.  That is your complete topology: remember the few high-level wells, and trust `make` to lazily recompute the nodes below when source files shift.
```

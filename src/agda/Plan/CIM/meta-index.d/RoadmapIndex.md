# Meta-Index: Plan.CIM.RoadmapIndex

## Scope

* Canonical index of roadmap items that drives generated projections (MD/JSON).

## Key elements

* Roadmap nodes, categories, and projection helpers defined in `Plan.CIM.RoadmapIndex.agda`.

## Dependencies

* Document algebra modules and roadmap export pipeline (make targets, JSON/MD generation).

## Update triggers

* Schema or workflow changes for roadmap ingestion/export.
* New roadmap categories or status fields.
* Architecture/ROADMAP.md updates that alter the canonical index.

## Technical debt / status

* Keep deferred-items.md in sync when schema shifts; regen projections (`make roadmap-merge`, `make roadmap-export-md`) after changes.

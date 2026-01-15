#!/usr/bin/env text
# Agda Source Overview

This directory hosts the core Agda implementation of Metacatagory. It is split
into foundational modules, formalized math structures, planning/protocol logic,
and runnable examples/tests.

## Entry Points

- `Core/` — foundational types, witnesses, and categorical primitives.
- `Algebra/` — algebraic hierarchies and theorems.
- `Plan/CIM/` — CIM protocol and roadmap infrastructure.
- `Examples/` — runnable examples and exporter tooling (incl. Makefile export).
- `Tests/` — checklist-style verification suites.

## Getting Oriented

- Start with `Core/README.md` for the core architecture.
- Review `Plan/CIM/Utility.agda` for `RoadmapStep` definitions.
- See `Examples/ExporterMakefile.agda` for build/export conventions.

## Build Notes

Use `make agda-all` to compile all modules and `make check` for the full
validation suite (see `TESTING.md` for details). Mutative targets require
`MUTATE_OK=1`.

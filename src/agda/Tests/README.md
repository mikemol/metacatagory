#!/usr/bin/env text
# Agda Tests Overview

This directory contains checklist-style Agda tests that exercise core laws,
protocol adequacy, and regression boundaries.

## What Lives Here

- Structural checklists for algebra and category theory modules.
- Protocol conformance tests for CIM pipelines.
- Proof obligations and serialization/roundtrip checks.

## How To Run

Use the top-level Makefile targets:

- `MUTATE_OK=1 make agda-all` — compile Agda modules.
- `MUTATE_OK=1 make check` — full validation suite (includes Agda tests).

For testing philosophy and the expanded suite (including Python/markdown),
see `TESTING.md` at the repo root.

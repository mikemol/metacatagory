# Shell configuration for error-safe recipes
SHELL := /bin/bash
.SHELLFLAGS := -euo pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Default for conditional backend skipping (avoid unbound var under set -u)
BUILD_SKIP_GHC_BACKEND ?=
SKIP_GHC_BACKEND ?= $(BUILD_SKIP_GHC_BACKEND)
export SKIP_GHC_BACKEND

# Guard for targets that write outside docs/ or data/ (default deny).
BUILD_MUTATE_OK ?= 0
MUTATE_OK ?= $(BUILD_MUTATE_OK)
define require_mutate
	@if [ "$(MUTATE_OK)" != "1" ]; then echo "Mutative target requires MUTATE_OK=1"; exit 1; fi
endef

# Optional override for decomposed JSON staging (kept empty by default to silence
# warn-undefined when not provided).
BUILD_JSON_DECOMPOSE_FALLBACK_DIR ?=
JSON_DECOMPOSE_FALLBACK_DIR ?= $(BUILD_JSON_DECOMPOSE_FALLBACK_DIR)

# Default parallelism scales with available cores unless user overrides MAKEFLAGS.
BUILD_CORES ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
CORES ?= $(BUILD_CORES)
MAKEFLAGS += -j$(CORES)

# Python virtualenv (default within build/)
BUILD_VENV_DIR ?= $(BUILD_WORKDIR)/build/venv/.venv
VIRTUAL_ENV ?= $(BUILD_VENV_DIR)

# Profiling output (JSONL). New file per make invocation for history.
BUILD_PROFILE_DIR ?= $(BUILD_WORKDIR)/build/profiles.d
PROFILE_DIR ?= $(BUILD_PROFILE_DIR)
ifndef PROFILE_RUN
BUILD_PROFILE_RUN := $(shell sh -c "echo $$(date +%Y%m%dT%H%M%S%z)-$$$$")
PROFILE_RUN := $(BUILD_PROFILE_RUN)
endif
PROFILE_LOG ?= $(PROFILE_DIR)/profile-$(PROFILE_RUN).jsonl

# Target mutability groups (exported from Agda typing)
MUTATIVE_TARGETS := regen-makefile build/graph_parsed_state.txt build/venv/dir.stamp build/venv/requirements-main.stamp build/venv/requirements-dev.stamp build/venv/requirements-inputs.stamp build/venv/venv_created.stamp build/venv/pip_upgraded.stamp build/venv/requirements_installed.stamp build/venv/python_setup.stamp python-test python-verified roadmap-merge build/canonical_enriched.json roadmap-enrich .github/roadmap/tasks.json ROADMAP.md build/reports/tasks_enriched.md roadmap-export-deps roadmap-validate-json roadmap-validate-md roadmap-validate-triangle build/gp_roadmap_sppf.json roadmap-all-enriched build/reports/docs-lint.json md-fix md-normalize build/agda/Plan/CIM/RoadmapExporterMain build/reports/roadmap_ast.txt build/agda/Plan/CIM/ModuleExporter docs-modules docs-normalize docs-all docs-validate json-decompose json-decompose-prebuilt build/dependency_graph_recomposed.json json-roundtrip-validate json-roundtrip-validate-light json-decompose-enriched build/canonical_enriched_recomposed.json json-roundtrip-validate-enriched json-decompose-planning build/planning_index_recomposed.json json-roundtrip-validate-planning build/dir.stamp build/reports/dir.stamp docs/status/dir.stamp build/canonical_roadmap.json intake-scan node-deps build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI deferred-items-dirs deferred-items act-list act-ci act-lint act-makefile-validate act-roadmap-sync act-deferred act-all badges build/agda/PriorityOrchestrationFFI priority-strategy-profiles .github/badges/weights.json build/priority_profile_inputs.stamp build/agda/Plan/CIM/PriorityProfileExport build/priority_profile.json priority-refresh roadmap-index build/agda/Plan/CIM/PlanningExport data/planning_index.json planning-kernel roadmap-sync roadmap-sppf build/diagrams/dir.stamp build/diagrams/agda-deps-full.dot roadmap-deps-graph build/agda/Plan/CIM/DependencyGraphExport data/dependency_graph.json regen-agda regen-exports regen-roadmap regen-docs regen-badges regen-intake regen-all all debt-check check-infra check-docs check-roadmap check-python check-json check-debt check-all check validate-constructive ci-light ci-preflight docker-rootless-status docker-build docker-build-ghcr docker-push-ghcr docker-all
READONLY_TARGETS := graph-status graph-assert-ok md-lint docs-lint intake-lint makefile-validate

# Allow callers (e.g., ACT) to redirect execution to an alternate workspace.
BUILD_WORKDIR ?= .
ifneq ($(origin WORKDIR), undefined)
ifneq ($(strip $(WORKDIR)),)
BUILD_WORKDIR := $(WORKDIR)
endif
endif
ifneq ($(origin ACT_WORKDIR), undefined)
ifneq ($(strip $(ACT_WORKDIR)),)
BUILD_WORKDIR := $(ACT_WORKDIR)
endif
endif
WORKDIR ?= $(BUILD_WORKDIR)

# Keep Agda global cache/data inside the repo to avoid cross-project contamination.
XDG_DATA_HOME ?= $(BUILD_WORKDIR)/build/xdg-data
XDG_CACHE_HOME ?= $(BUILD_WORKDIR)/build/xdg-cache
AGDA_BIN ?= agda
AGDA_DATA_DIR := $(shell env AGDA_EXEC_OPTIONS= $(AGDA_BIN) --library-file=/dev/null --no-libraries --no-default-libraries --print-agda-data-dir 2>/dev/null)
AGDA_PRIM_DIR := $(AGDA_DATA_DIR)/lib/prim
AGDA_ENV := env XDG_DATA_HOME=$(XDG_DATA_HOME) XDG_CACHE_HOME=$(XDG_CACHE_HOME) AGDA_EXEC_OPTIONS= AGDA_DATA_DIR=$(AGDA_DATA_DIR)

# Use local Agda 2.8.0 if available, otherwise system agda.
AGDA := $(AGDA_ENV) $(AGDA_BIN)

# Common Agda compilation flags
AGDA_FLAGS := -i src/agda --include-path=$(AGDA_PRIM_DIR) --no-default-libraries --no-libraries --ghc-flag=-Wno-star-is-type --ghc-flag=-j$(CORES)
# Route compiled outputs (MAlonzo + binaries) into build/ by default.
AGDA_COMPILE_DIR ?= $(BUILD_WORKDIR)/build/agda
AGDA_COMPILE := $(AGDA) $(AGDA_FLAGS) --compile --compile-dir=$(AGDA_COMPILE_DIR)

# Pytest workers (defaults to CORES or METACATAGORY_WORKERS when set)
PYTEST_WORKERS ?= $(if $(METACATAGORY_WORKERS),$(METACATAGORY_WORKERS),$(CORES))

# Dependency decomposition directories (fallback-safe)
DEPS_DIR ?= $(if $(JSON_DECOMPOSE_FALLBACK_DIR),$(JSON_DECOMPOSE_FALLBACK_DIR),data/deps/)
DEPS_METADATA ?= $(DEPS_DIR)_metadata.json
PLANNING_DIR ?= $(if $(JSON_DECOMPOSE_FALLBACK_DIR),$(JSON_DECOMPOSE_FALLBACK_DIR)/planning,$(DEPS_DIR)/planning/)
PLANNING_METADATA ?= $(PLANNING_DIR)_metadata.json
.PHONY: regen-makefile graph-status graph-assert-ok python-test python-verified roadmap-merge roadmap-enrich roadmap-export-deps roadmap-validate-json roadmap-validate-md roadmap-validate-triangle roadmap-all-enriched md-lint docs-lint md-fix md-normalize docs-modules docs-normalize docs-all docs-validate json-decompose json-decompose-prebuilt json-roundtrip-validate json-roundtrip-validate-light json-decompose-enriched json-roundtrip-validate-enriched json-decompose-planning json-roundtrip-validate-planning intake-lint intake-scan makefile-validate node-deps deferred-items-dirs deferred-items act-list act-ci act-lint act-makefile-validate act-roadmap-sync act-deferred act-all badges priority-strategy-profiles priority-refresh roadmap-index planning-kernel roadmap-sync roadmap-sppf roadmap-deps-graph regen-agda regen-exports regen-roadmap regen-docs regen-badges regen-intake regen-all all debt-check check-infra check-docs check-roadmap check-python check-json check-debt check-all check validate-constructive ci-light ci-preflight docker-rootless-status docker-build docker-build-ghcr docker-push-ghcr docker-all agda-all
# Regenerate the Makefile from Agda source (Self-Hosting)
regen-makefile: 
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-makefile' 'scripts/recipes/regen-makefile.sh'
# Graph parse state file (produced alongside Makefile generation)
build/graph_parsed_state.txt: build/diagrams/agda-deps-full.dot
	$(call require_mutate)
	scripts/run_profiled.sh 'build/graph_parsed_state.txt' 'build/recipes/build/graph_parsed_state.txt.sh'
# Print parsed graph status
graph-status: build/graph_parsed_state.txt
	scripts/run_profiled.sh 'graph-status' 'build/recipes/graph-status.sh'
# Assert dependency graph is OK (CI guard)
graph-assert-ok: build/graph_parsed_state.txt
	scripts/run_profiled.sh 'graph-assert-ok' 'build/recipes/graph-assert-ok.sh'
# Ensure build/venv exists
build/venv/dir.stamp: 
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/dir.stamp' 'build/recipes/build/venv/dir.stamp.sh'
# Track requirements.txt changes
build/venv/requirements-main.stamp: build/venv/dir.stamp requirements.txt
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/requirements-main.stamp' 'build/recipes/build/venv/requirements-main.stamp.sh'
# Track requirements-dev.txt changes
build/venv/requirements-dev.stamp: build/venv/dir.stamp requirements-dev.txt
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/requirements-dev.stamp' 'build/recipes/build/venv/requirements-dev.stamp.sh'
# Track Python dependency inputs
build/venv/requirements-inputs.stamp: build/venv/requirements-main.stamp build/venv/requirements-dev.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/requirements-inputs.stamp' 'build/recipes/build/venv/requirements-inputs.stamp.sh'
# Create Python virtualenv
build/venv/venv_created.stamp: build/venv/dir.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/venv_created.stamp' 'build/recipes/build/venv/venv_created.stamp.sh'
# Upgrade pip in virtualenv
build/venv/pip_upgraded.stamp: build/venv/venv_created.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/pip_upgraded.stamp' 'build/recipes/build/venv/pip_upgraded.stamp.sh'
# Install Python dependencies
build/venv/requirements_installed.stamp: build/venv/pip_upgraded.stamp build/venv/requirements-inputs.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/requirements_installed.stamp' 'build/recipes/build/venv/requirements_installed.stamp.sh'
# Create Python venv and install dependencies
build/venv/python_setup.stamp: build/venv/requirements_installed.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/venv/python_setup.stamp' 'build/recipes/build/venv/python_setup.stamp.sh'
# Run Python tests (includes pytest suite)
python-test: build/venv/python_setup.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'python-test' 'build/recipes/python-test.sh'
# Witness target: test suite contracted
python-verified: python-test
	$(call require_mutate)
	scripts/run_profiled.sh 'python-verified' 'build/recipes/python-verified.sh'
# Merge ingestion streams
roadmap-merge: 
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-merge' 'build/recipes/roadmap-merge.sh'
# Enrich canonical roadmap
build/canonical_enriched.json: data/planning_index.json build/diagrams/agda-deps-full.dot
	$(call require_mutate)
	scripts/run_profiled.sh 'build/canonical_enriched.json' 'build/recipes/build/canonical_enriched.json.sh'
# Enrich roadmap with graph data
roadmap-enrich: build/canonical_enriched.json
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-enrich' 'build/recipes/roadmap-enrich.sh'
# Export canonical roadmap to JSON
.github/roadmap/tasks.json: data/planning_index.json
	$(call require_mutate)
	scripts/run_profiled.sh '.github/roadmap/tasks.json' 'build/recipes/.github/roadmap/tasks.json.sh'
# Export canonical roadmap to Markdown
ROADMAP.md: data/planning_index.json
	$(call require_mutate)
	scripts/run_profiled.sh 'ROADMAP.md' 'build/recipes/ROADMAP.md.sh'
# Export enriched roadmap
build/reports/tasks_enriched.md: build/canonical_enriched.json
	$(call require_mutate)
	scripts/run_profiled.sh 'build/reports/tasks_enriched.md' 'build/recipes/build/reports/tasks_enriched.md.sh'
# Export roadmap dependency graph
roadmap-export-deps: build/canonical_enriched.json
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-export-deps' 'build/recipes/roadmap-export-deps.sh'
# Validate canonical JSON
roadmap-validate-json: .github/roadmap/tasks.json
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-validate-json' 'build/recipes/roadmap-validate-json.sh'
# Validate canonical Markdown
roadmap-validate-md: ROADMAP.md
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-validate-md' 'build/recipes/roadmap-validate-md.sh'
# Verify Triangle Identity (Agda <-> JSON <-> MD)
roadmap-validate-triangle: roadmap-validate-json roadmap-validate-md
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-validate-triangle' 'build/recipes/roadmap-validate-triangle.sh'
# Export SPPF structure
build/gp_roadmap_sppf.json: data/planning_index.json
	$(call require_mutate)
	scripts/run_profiled.sh 'build/gp_roadmap_sppf.json' 'build/recipes/build/gp_roadmap_sppf.json.sh'
# Build all enriched artifacts
roadmap-all-enriched: build/reports/tasks_enriched.md roadmap-export-deps
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-all-enriched' 'build/recipes/roadmap-all-enriched.sh'
# Lint all markdown files (fail on error)
md-lint: 
	scripts/run_profiled.sh 'md-lint' 'build/recipes/md-lint.sh'
# Emit Agda docs lint report
build/reports/docs-lint.json: 
	$(call require_mutate)
	scripts/run_profiled.sh 'build/reports/docs-lint.json' 'build/recipes/build/reports/docs-lint.json.sh'
# Lint Agda docs coverage (fail on error)
docs-lint: 
	scripts/run_profiled.sh 'docs-lint' 'build/recipes/docs-lint.sh'
# Auto-fix markdown lint errors
md-fix: 
	$(call require_mutate)
	scripts/run_profiled.sh 'md-fix' 'build/recipes/md-fix.sh'
# Normalize markdown formatting
md-normalize: 
	$(call require_mutate)
	scripts/run_profiled.sh 'md-normalize' 'build/recipes/md-normalize.sh'
# Compile roadmap exporter binary
build/agda/Plan/CIM/RoadmapExporterMain: src/agda/Plan/CIM/RoadmapExporterMain.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/Plan/CIM/RoadmapExporterMain' 'build/recipes/build/agda/Plan/CIM/RoadmapExporterMain.sh'
# Export roadmap AST report
build/reports/roadmap_ast.txt: build/reports/dir.stamp build/agda/Plan/CIM/RoadmapExporterMain
	$(call require_mutate)
	scripts/run_profiled.sh 'build/reports/roadmap_ast.txt' 'build/recipes/build/reports/roadmap_ast.txt.sh'
# Compile module exporter binary
build/agda/Plan/CIM/ModuleExporter: src/agda/Plan/CIM/ModuleExporter.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/Plan/CIM/ModuleExporter' 'build/recipes/build/agda/Plan/CIM/ModuleExporter.sh'
# Generate per-module markdown documentation
docs-modules: build/agda/Plan/CIM/ModuleExporter
	$(call require_mutate)
	scripts/run_profiled.sh 'docs-modules' 'build/recipes/docs-modules.sh'
# Normalize generated markdown after docs export
docs-normalize: build/reports/roadmap_ast.txt docs-modules
	$(call require_mutate)
	scripts/run_profiled.sh 'docs-normalize' 'build/recipes/docs-normalize.sh'
# Generate documentation (markdown only)
docs-all: docs-normalize
	$(call require_mutate)
	scripts/run_profiled.sh 'docs-all' 'build/recipes/docs-all.sh'
# Validate documentation integrity
docs-validate: data/planning_index.json ROADMAP.md
	$(call require_mutate)
	scripts/run_profiled.sh 'docs-validate' 'build/recipes/docs-validate.sh'
# Decompose monolithic JSON to hierarchical structure
json-decompose: data/dependency_graph.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-decompose' 'build/recipes/json-decompose.sh'
# Decompose monolithic JSON using prebuilt inputs
json-decompose-prebuilt: data/dependency_graph.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-decompose-prebuilt' 'build/recipes/json-decompose-prebuilt.sh'
# Recompose hierarchical JSON back to monolithic form
build/dependency_graph_recomposed.json: json-decompose $(DEPS_METADATA)
	$(call require_mutate)
	scripts/run_profiled.sh 'build/dependency_graph_recomposed.json' 'build/recipes/build/dependency_graph_recomposed.json.sh'
# Validate JSON decomposition roundtrip
json-roundtrip-validate: json-decompose build/dependency_graph_recomposed.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-roundtrip-validate' 'build/recipes/json-roundtrip-validate.sh'
# Validate JSON decomposition roundtrip (light)
json-roundtrip-validate-light: json-decompose-prebuilt build/dependency_graph_recomposed.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-roundtrip-validate-light' 'build/recipes/json-roundtrip-validate-light.sh'
# Decompose canonical_enriched.json into item hierarchy
json-decompose-enriched: build/canonical_enriched.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-decompose-enriched' 'build/recipes/json-decompose-enriched.sh'
# Recompose enriched items into canonical_enriched.json
build/canonical_enriched_recomposed.json: data/enriched/
	$(call require_mutate)
	scripts/run_profiled.sh 'build/canonical_enriched_recomposed.json' 'build/recipes/build/canonical_enriched_recomposed.json.sh'
# Validate enriched roundtrip
json-roundtrip-validate-enriched: json-decompose-enriched build/canonical_enriched_recomposed.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-roundtrip-validate-enriched' 'build/recipes/json-roundtrip-validate-enriched.sh'
# Decompose planning_index.json into plan hierarchy
json-decompose-planning: data/planning_index.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-decompose-planning' 'build/recipes/json-decompose-planning.sh'
# Recompose planning items into planning_index.json
build/planning_index_recomposed.json: json-decompose-planning $(PLANNING_METADATA)
	$(call require_mutate)
	scripts/run_profiled.sh 'build/planning_index_recomposed.json' 'build/recipes/build/planning_index_recomposed.json.sh'
# Validate planning roundtrip
json-roundtrip-validate-planning: json-decompose-planning build/planning_index_recomposed.json
	$(call require_mutate)
	scripts/run_profiled.sh 'json-roundtrip-validate-planning' 'build/recipes/json-roundtrip-validate-planning.sh'
# Ensure build/ exists
build/dir.stamp: 
	$(call require_mutate)
	scripts/run_profiled.sh 'build/dir.stamp' 'build/recipes/build/dir.stamp.sh'
# Ensure build/reports exists
build/reports/dir.stamp: build/dir.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/reports/dir.stamp' 'build/recipes/build/reports/dir.stamp.sh'
# Ensure docs/status exists
docs/status/dir.stamp: 
	scripts/run_profiled.sh 'docs/status/dir.stamp' 'build/recipes/docs/status/dir.stamp.sh'
# Lint intake files specifically
intake-lint: 
	scripts/run_profiled.sh 'intake-lint' 'build/recipes/intake-lint.sh'
# Generate canonical roadmap JSON from intake
build/canonical_roadmap.json: 
	$(call require_mutate)
	scripts/run_profiled.sh 'build/canonical_roadmap.json' 'build/recipes/build/canonical_roadmap.json.sh'
# Scan intake directory for new files
intake-scan: data/planning_index.json
	$(call require_mutate)
	scripts/run_profiled.sh 'intake-scan' 'build/recipes/intake-scan.sh'
# Validate Makefile consistency
makefile-validate: 
	scripts/run_profiled.sh 'makefile-validate' 'build/recipes/makefile-validate.sh'
# Install Node.js dependencies
node-deps: 
	$(call require_mutate)
	scripts/run_profiled.sh 'node-deps' 'build/recipes/node-deps.sh'
# Compile deferred items scanner (MAlonzo + binary)
build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI: build/dir.stamp src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI' 'build/recipes/build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.sh'
# Ensure deferred items output directories exist
deferred-items-dirs: build/reports/dir.stamp docs/status/dir.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'deferred-items-dirs' 'build/recipes/deferred-items-dirs.sh'
# Scan for TODOs and FIXMEs (Agda FFI binary)
deferred-items: build/agda/TechnicalDebt/DeferredItemsOrchestrationFFI deferred-items-dirs
	$(call require_mutate)
	scripts/run_profiled.sh 'deferred-items' 'build/recipes/deferred-items.sh'
# List available GitHub Actions jobs (act)
act-list: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-list' 'build/recipes/act-list.sh'
# Run CI workflow locally via act
act-ci: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-ci' 'build/recipes/act-ci.sh'
# Run docs checks job locally via act
act-lint: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-lint' 'build/recipes/act-lint.sh'
# Run Agda exports job locally via act
act-makefile-validate: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-makefile-validate' 'build/recipes/act-makefile-validate.sh'
# Run roadmap/JSON job locally via act
act-roadmap-sync: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-roadmap-sync' 'build/recipes/act-roadmap-sync.sh'
# Run Python/debt job locally via act
act-deferred: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-deferred' 'build/recipes/act-deferred.sh'
# Run all workflows locally via act
act-all: 
	$(call require_mutate)
	scripts/run_profiled.sh 'act-all' 'build/recipes/act-all.sh'
# Generate status badges
badges: .github/badges/weights.json
	$(call require_mutate)
	scripts/run_profiled.sh 'badges' 'build/recipes/badges.sh'
# Compile Agda priority orchestration (MAlonzo + binary)
build/agda/PriorityOrchestrationFFI: build/dir.stamp src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/PriorityOrchestrationFFI' 'build/recipes/build/agda/PriorityOrchestrationFFI.sh'
# Compile and run Agda priority orchestration (generate strategy profiles)
priority-strategy-profiles: build/agda/PriorityOrchestrationFFI
	$(call require_mutate)
	scripts/run_profiled.sh 'priority-strategy-profiles' 'build/recipes/priority-strategy-profiles.sh'
# Normalize Agda strategy profiles into badge weights
.github/badges/weights.json: priority-strategy-profiles
	$(call require_mutate)
	scripts/run_profiled.sh '.github/badges/weights.json' 'build/recipes/.github/badges/weights.json.sh'
# Track priority profile inputs
build/priority_profile_inputs.stamp: data/planning_index.json build/reports/docs-lint.json
	$(call require_mutate)
	scripts/run_profiled.sh 'build/priority_profile_inputs.stamp' 'build/recipes/build/priority_profile_inputs.stamp.sh'
# Compile PriorityProfileExport (MAlonzo + binary)
build/agda/Plan/CIM/PriorityProfileExport: build/dir.stamp src/agda/Plan/CIM/PriorityProfileExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/Plan/CIM/PriorityProfileExport' 'build/recipes/build/agda/Plan/CIM/PriorityProfileExport.sh'
# Export structured priority profile (lazy; derived from planning index)
build/priority_profile.json: build/priority_profile_inputs.stamp build/agda/Plan/CIM/PriorityProfileExport
	$(call require_mutate)
	scripts/run_profiled.sh 'build/priority_profile.json' 'build/recipes/build/priority_profile.json.sh'
# Re-run priority pipeline and refresh roadmap/badge outputs
priority-refresh: data/planning_index.json .github/roadmap/tasks.json .github/badges/weights.json badges
	$(call require_mutate)
	scripts/run_profiled.sh 'priority-refresh' 'build/recipes/priority-refresh.sh'
# Compile Roadmap Index
roadmap-index: src/agda/Plan/CIM/RoadmapIndex.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-index' 'build/recipes/roadmap-index.sh'
# Compile PlanningExport (MAlonzo + binary)
build/agda/Plan/CIM/PlanningExport: src/agda/Plan/CIM/PlanningExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/Plan/CIM/PlanningExport' 'build/recipes/build/agda/Plan/CIM/PlanningExport.sh'
# Export planning index to JSON
data/planning_index.json: build/agda/Plan/CIM/PlanningExport
	scripts/run_profiled.sh 'data/planning_index.json' 'build/recipes/data/planning_index.json.sh'
# Compile Planning Kernel
planning-kernel: src/agda/Plan/CIM/PlanningKernel.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'planning-kernel' 'build/recipes/planning-kernel.sh'
# Sync roadmap with external tracker
roadmap-sync: .github/roadmap/tasks.json src/agda/Plan/CIM/RoadmapSync.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-sync' 'build/recipes/roadmap-sync.sh'
# Compile Roadmap SPPF
roadmap-sppf: src/agda/Plan/CIM/RoadmapSPPF.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-sppf' 'build/recipes/roadmap-sppf.sh'
# Ensure build/diagrams exists
build/diagrams/dir.stamp: build/dir.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/diagrams/dir.stamp' 'build/recipes/build/diagrams/dir.stamp.sh'
# Generate dependency graph
build/diagrams/agda-deps-full.dot: build/diagrams/dir.stamp
	$(call require_mutate)
	scripts/run_profiled.sh 'build/diagrams/agda-deps-full.dot' 'build/recipes/build/diagrams/agda-deps-full.dot.sh'
# Generate dependency graph
roadmap-deps-graph: build/diagrams/agda-deps-full.dot
	$(call require_mutate)
	scripts/run_profiled.sh 'roadmap-deps-graph' 'build/recipes/roadmap-deps-graph.sh'
# Compile DependencyGraphExport (MAlonzo + binary)
build/agda/Plan/CIM/DependencyGraphExport: src/agda/Plan/CIM/DependencyGraphExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'build/agda/Plan/CIM/DependencyGraphExport' 'build/recipes/build/agda/Plan/CIM/DependencyGraphExport.sh'
# Export dependency graph JSON via Agda (from agda-deps-full.dot)
data/dependency_graph.json: build/diagrams/agda-deps-full.dot build/agda/Plan/CIM/DependencyGraphExport
	scripts/run_profiled.sh 'data/dependency_graph.json' 'build/recipes/data/dependency_graph.json.sh'
# Regenerate Agda build outputs
regen-agda: agda-all
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-agda' 'build/recipes/regen-agda.sh'
# Regenerate Agda export artifacts
regen-exports: data/planning_index.json build/diagrams/agda-deps-full.dot data/dependency_graph.json build/priority_profile.json
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-exports' 'build/recipes/regen-exports.sh'
# Regenerate roadmap artifacts
regen-roadmap: .github/roadmap/tasks.json ROADMAP.md build/reports/tasks_enriched.md roadmap-export-deps build/gp_roadmap_sppf.json roadmap-all-enriched
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-roadmap' 'build/recipes/regen-roadmap.sh'
# Regenerate documentation exports
regen-docs: docs-all
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-docs' 'build/recipes/regen-docs.sh'
# Regenerate badge artifacts
regen-badges: .github/badges/weights.json badges
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-badges' 'build/recipes/regen-badges.sh'
# Regenerate intake-derived artifacts
regen-intake: build/canonical_roadmap.json roadmap-merge
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-intake' 'build/recipes/regen-intake.sh'
# Regenerate all build artifacts
regen-all: regen-agda regen-exports regen-roadmap regen-docs regen-badges regen-intake
	$(call require_mutate)
	scripts/run_profiled.sh 'regen-all' 'build/recipes/regen-all.sh'
# Build all code and documentation
all: agda-all docs-all
	$(call require_mutate)
	scripts/run_profiled.sh 'all' 'build/recipes/all.sh'
# Run debt tracking validation
debt-check: deferred-items intake-scan
	$(call require_mutate)
	scripts/run_profiled.sh 'debt-check' 'build/recipes/debt-check.sh'
# Validate graph + Makefile
check-infra: graph-assert-ok makefile-validate
	$(call require_mutate)
	scripts/run_profiled.sh 'check-infra' 'build/recipes/check-infra.sh'
# Validate documentation outputs
check-docs: md-lint docs-lint docs-validate
	$(call require_mutate)
	scripts/run_profiled.sh 'check-docs' 'build/recipes/check-docs.sh'
# Validate roadmap exports
check-roadmap: roadmap-validate-triangle
	$(call require_mutate)
	scripts/run_profiled.sh 'check-roadmap' 'build/recipes/check-roadmap.sh'
# Validate Python test suite
check-python: python-verified
	$(call require_mutate)
	scripts/run_profiled.sh 'check-python' 'build/recipes/check-python.sh'
# Validate JSON roundtrip outputs
check-json: json-roundtrip-validate json-roundtrip-validate-enriched json-roundtrip-validate-planning
	$(call require_mutate)
	scripts/run_profiled.sh 'check-json' 'build/recipes/check-json.sh'
# Validate deferred items + intake scan
check-debt: debt-check
	$(call require_mutate)
	scripts/run_profiled.sh 'check-debt' 'build/recipes/check-debt.sh'
# Run full validation suite
check-all: check-infra check-docs check-roadmap check-python check-json check-debt
	$(call require_mutate)
	scripts/run_profiled.sh 'check-all' 'build/recipes/check-all.sh'
# Run all validation checks (alias)
check: check-all
	$(call require_mutate)
	scripts/run_profiled.sh 'check' 'build/recipes/check.sh'
# Regenerate and validate all artifacts
validate-constructive: regen-all check-all
	$(call require_mutate)
	scripts/run_profiled.sh 'validate-constructive' 'build/recipes/validate-constructive.sh'
# Lightweight CI target (no GHC backend)
ci-light: check-infra check-docs json-roundtrip-validate-light json-roundtrip-validate-enriched json-roundtrip-validate-planning
	$(call require_mutate)
	scripts/run_profiled.sh 'ci-light' 'build/recipes/ci-light.sh'
# Fast guard: graph + makefile docs
ci-preflight: check-infra
	$(call require_mutate)
	scripts/run_profiled.sh 'ci-preflight' 'build/recipes/ci-preflight.sh'
# Check rootless Docker daemon status
docker-rootless-status: 
	$(call require_mutate)
	scripts/run_profiled.sh 'docker-rootless-status' 'build/recipes/docker-rootless-status.sh'
# Build Docker image (metacatagory:dev)
docker-build: 
	$(call require_mutate)
	scripts/run_profiled.sh 'docker-build' 'build/recipes/docker-build.sh'
# Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars)
docker-build-ghcr: 
	$(call require_mutate)
	scripts/run_profiled.sh 'docker-build-ghcr' 'build/recipes/docker-build-ghcr.sh'
# Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME)
docker-push-ghcr: docker-build-ghcr
	$(call require_mutate)
	scripts/run_profiled.sh 'docker-push-ghcr' 'build/recipes/docker-push-ghcr.sh'
# Build and push to GHCR (full pipeline)
docker-all: docker-build-ghcr docker-push-ghcr
	$(call require_mutate)
	scripts/run_profiled.sh 'docker-all' 'build/recipes/docker-all.sh'
# Compile src/agda/MetaScan.agda
src/agda/MetaScan.agdai: src/agda/MetaScan.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/MetaScan.agdai' 'build/recipes/src/agda/MetaScan.agdai.sh'
# Compile src/agda/Metamodel.agda
src/agda/Metamodel.agdai: src/agda/Metamodel.agda src/agda/Core/Phase.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Metamodel.agdai' 'build/recipes/src/agda/Metamodel.agdai.sh'
# Compile src/agda/Tests/PropertyRegistryTests.agda
src/agda/Tests/PropertyRegistryTests.agdai: src/agda/Tests/PropertyRegistryTests.agda src/agda/PropertyRegistry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PropertyRegistryTests.agdai' 'build/recipes/src/agda/Tests/PropertyRegistryTests.agdai.sh'
# Compile src/agda/Tests/AbelianCategoriesChecklist.agda
src/agda/Tests/AbelianCategoriesChecklist.agdai: src/agda/Tests/AbelianCategoriesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AbelianCategoriesChecklist.agdai' 'build/recipes/src/agda/Tests/AbelianCategoriesChecklist.agdai.sh'
# Compile src/agda/Tests/DispatchBehaviorTests.agda
src/agda/Tests/DispatchBehaviorTests.agdai: src/agda/Tests/DispatchBehaviorTests.agda src/agda/Core/Algorithms/Registry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/DispatchBehaviorTests.agdai' 'build/recipes/src/agda/Tests/DispatchBehaviorTests.agdai.sh'
# Compile src/agda/Tests/WitnessConstructionTests.agda
src/agda/Tests/WitnessConstructionTests.agdai: src/agda/Tests/WitnessConstructionTests.agda src/agda/Core/Witnesses.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/WitnessConstructionTests.agdai' 'build/recipes/src/agda/Tests/WitnessConstructionTests.agdai.sh'
# Compile src/agda/Tests/VectorSpaceChecklist.agda
src/agda/Tests/VectorSpaceChecklist.agdai: src/agda/Tests/VectorSpaceChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/VectorSpaceChecklist.agdai' 'build/recipes/src/agda/Tests/VectorSpaceChecklist.agdai.sh'
# Compile src/agda/Tests/ProofObligationStatus.agda
src/agda/Tests/ProofObligationStatus.agdai: src/agda/Tests/ProofObligationStatus.agda src/agda/Examples/AlgorithmCorrectnessExamples.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ProofObligationStatus.agdai' 'build/recipes/src/agda/Tests/ProofObligationStatus.agdai.sh'
# Compile src/agda/Tests/SerializationTests.agda
src/agda/Tests/SerializationTests.agdai: src/agda/Tests/SerializationTests.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/TechnicalDebt.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/SerializationTests.agdai' 'build/recipes/src/agda/Tests/SerializationTests.agdai.sh'
# Compile src/agda/Tests/GodelBoundaryTests.agda
src/agda/Tests/GodelBoundaryTests.agdai: src/agda/Tests/GodelBoundaryTests.agda src/agda/Core/GodelBoundary.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/GodelBoundaryTests.agdai' 'build/recipes/src/agda/Tests/GodelBoundaryTests.agdai.sh'
# Compile src/agda/Tests/ModuleStructureChecklist.agda
src/agda/Tests/ModuleStructureChecklist.agdai: src/agda/Tests/ModuleStructureChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ModuleStructureChecklist.agdai' 'build/recipes/src/agda/Tests/ModuleStructureChecklist.agdai.sh'
# Compile src/agda/Tests/ToposTheoryChecklist.agda
src/agda/Tests/ToposTheoryChecklist.agdai: src/agda/Tests/ToposTheoryChecklist.agda src/agda/Tests/ToposObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ToposTheoryChecklist.agdai' 'build/recipes/src/agda/Tests/ToposTheoryChecklist.agdai.sh'
# Compile src/agda/Tests/GrothendieckFibrationsChecklist.agda
src/agda/Tests/GrothendieckFibrationsChecklist.agdai: src/agda/Tests/GrothendieckFibrationsChecklist.agda src/agda/Tests/ObligationAdapters.agdai src/agda/Core/GrothendieckFibrations.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/GrothendieckFibrationsChecklist.agdai' 'build/recipes/src/agda/Tests/GrothendieckFibrationsChecklist.agdai.sh'
# Compile src/agda/Tests/TensorProductChecklist.agda
src/agda/Tests/TensorProductChecklist.agdai: src/agda/Tests/TensorProductChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/TensorProductChecklist.agdai' 'build/recipes/src/agda/Tests/TensorProductChecklist.agdai.sh'
# Compile src/agda/Tests/Index_PhaseII.agda
src/agda/Tests/Index_PhaseII.agdai: src/agda/Tests/Index_PhaseII.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Index_PhaseII.agdai' 'build/recipes/src/agda/Tests/Index_PhaseII.agdai.sh'
# Compile src/agda/Tests/HierarchyValidation.agda
src/agda/Tests/HierarchyValidation.agdai: src/agda/Tests/HierarchyValidation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/HierarchyValidation.agdai' 'build/recipes/src/agda/Tests/HierarchyValidation.agdai.sh'
# Compile src/agda/Tests/PhaseExamples.agda
src/agda/Tests/PhaseExamples.agdai: src/agda/Tests/PhaseExamples.agda src/agda/Core/Algorithms/Registry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PhaseExamples.agdai' 'build/recipes/src/agda/Tests/PhaseExamples.agdai.sh'
# Compile src/agda/Tests/CHIPConformanceChecklist.agda
src/agda/Tests/CHIPConformanceChecklist.agdai: src/agda/Tests/CHIPConformanceChecklist.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/CHIPConformanceChecklist.agdai' 'build/recipes/src/agda/Tests/CHIPConformanceChecklist.agdai.sh'
# Compile src/agda/Tests/RegularCategoriesChecklist.agda
src/agda/Tests/RegularCategoriesChecklist.agdai: src/agda/Tests/RegularCategoriesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/RegularCategoriesChecklist.agdai' 'build/recipes/src/agda/Tests/RegularCategoriesChecklist.agdai.sh'
# Compile src/agda/Tests/AlgorithmCompositionTestsMinimal.agda
src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai: src/agda/Tests/AlgorithmCompositionTestsMinimal.agda src/agda/Core/Algorithms/Registry.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/UniversalProperties.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai' 'build/recipes/src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai.sh'
# Compile src/agda/Tests/MonadAdjunctionChecklist.agda
src/agda/Tests/MonadAdjunctionChecklist.agdai: src/agda/Tests/MonadAdjunctionChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/MonadAdjunctionChecklist.agdai' 'build/recipes/src/agda/Tests/MonadAdjunctionChecklist.agdai.sh'
# Compile src/agda/Tests/UniversalPropertyTests.agda
src/agda/Tests/UniversalPropertyTests.agdai: src/agda/Tests/UniversalPropertyTests.agda src/agda/Core/AlgorithmUniversality.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/UniversalPropertyTests.agdai' 'build/recipes/src/agda/Tests/UniversalPropertyTests.agdai.sh'
# Compile src/agda/Tests/EnrichmentChecklist.agda
src/agda/Tests/EnrichmentChecklist.agdai: src/agda/Tests/EnrichmentChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/EnrichmentChecklist.agdai' 'build/recipes/src/agda/Tests/EnrichmentChecklist.agdai.sh'
# Compile src/agda/Tests/AdvancedPhaseExamples.agda
src/agda/Tests/AdvancedPhaseExamples.agdai: src/agda/Tests/AdvancedPhaseExamples.agda src/agda/Core/Algorithms/Registry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AdvancedPhaseExamples.agdai' 'build/recipes/src/agda/Tests/AdvancedPhaseExamples.agdai.sh'
# Compile src/agda/Tests/ErrorAsSpecificationTests.agda
src/agda/Tests/ErrorAsSpecificationTests.agdai: src/agda/Tests/ErrorAsSpecificationTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ErrorAsSpecificationTests.agdai' 'build/recipes/src/agda/Tests/ErrorAsSpecificationTests.agdai.sh'
# Compile src/agda/Tests/RealWorldAlgorithmsTests.agda
src/agda/Tests/RealWorldAlgorithmsTests.agdai: src/agda/Tests/RealWorldAlgorithmsTests.agda src/agda/Examples/RealWorldAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/RealWorldAlgorithmsTests.agdai' 'build/recipes/src/agda/Tests/RealWorldAlgorithmsTests.agdai.sh'
# Compile src/agda/Tests/PolynomialExtensionsChecklist.agda
src/agda/Tests/PolynomialExtensionsChecklist.agdai: src/agda/Tests/PolynomialExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PolynomialExtensionsChecklist.agdai' 'build/recipes/src/agda/Tests/PolynomialExtensionsChecklist.agdai.sh'
# Compile src/agda/Tests/ModuleTheoryChecklist.agda
src/agda/Tests/ModuleTheoryChecklist.agdai: src/agda/Tests/ModuleTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ModuleTheoryChecklist.agdai' 'build/recipes/src/agda/Tests/ModuleTheoryChecklist.agdai.sh'
# Compile src/agda/Tests/PhaseCategoryExamplesRunner.agda
src/agda/Tests/PhaseCategoryExamplesRunner.agdai: src/agda/Tests/PhaseCategoryExamplesRunner.agda src/agda/Examples/PhaseCategoryExamples.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PhaseCategoryExamplesRunner.agdai' 'build/recipes/src/agda/Tests/PhaseCategoryExamplesRunner.agdai.sh'
# Compile src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai: src/agda/Tests/PolynomialFieldExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai' 'build/recipes/src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai.sh'
# Compile src/agda/Tests/AlgorithmCompositionTests.agda
src/agda/Tests/AlgorithmCompositionTests.agdai: src/agda/Tests/AlgorithmCompositionTests.agda src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AlgorithmCompositionTests.agdai' 'build/recipes/src/agda/Tests/AlgorithmCompositionTests.agdai.sh'
# Compile src/agda/Tests/FieldsBasicChecklist.agda
src/agda/Tests/FieldsBasicChecklist.agdai: src/agda/Tests/FieldsBasicChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/FieldsBasicChecklist.agdai' 'build/recipes/src/agda/Tests/FieldsBasicChecklist.agdai.sh'
# Compile src/agda/Tests/SpecificationValidation.agda
src/agda/Tests/SpecificationValidation.agdai: src/agda/Tests/SpecificationValidation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/SpecificationValidation.agdai' 'build/recipes/src/agda/Tests/SpecificationValidation.agdai.sh'
# Compile src/agda/Tests/AlgorithmSmokeTests.agda
src/agda/Tests/AlgorithmSmokeTests.agdai: src/agda/Tests/AlgorithmSmokeTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AlgorithmSmokeTests.agdai' 'build/recipes/src/agda/Tests/AlgorithmSmokeTests.agdai.sh'
# Compile src/agda/Tests/CoverageReport.agda
src/agda/Tests/CoverageReport.agdai: src/agda/Tests/CoverageReport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/CoverageReport.agdai' 'build/recipes/src/agda/Tests/CoverageReport.agdai.sh'
# Compile src/agda/Tests/GroupsFreeChecklist.agda
src/agda/Tests/GroupsFreeChecklist.agdai: src/agda/Tests/GroupsFreeChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/GroupsFreeChecklist.agdai' 'build/recipes/src/agda/Tests/GroupsFreeChecklist.agdai.sh'
# Compile src/agda/Tests/RingsBasicChecklist.agda
src/agda/Tests/RingsBasicChecklist.agdai: src/agda/Tests/RingsBasicChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/RingsBasicChecklist.agdai' 'build/recipes/src/agda/Tests/RingsBasicChecklist.agdai.sh'
# Compile src/agda/Tests/Chapter2Checklist.agda
src/agda/Tests/Chapter2Checklist.agdai: src/agda/Tests/Chapter2Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Chapter2Checklist.agdai' 'build/recipes/src/agda/Tests/Chapter2Checklist.agdai.sh'
# Compile src/agda/Tests/KanExtensionsChecklist.agda
src/agda/Tests/KanExtensionsChecklist.agdai: src/agda/Tests/KanExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/KanExtensionsChecklist.agdai' 'build/recipes/src/agda/Tests/KanExtensionsChecklist.agdai.sh'
# Compile src/agda/Tests/LimitsColimitsChecklist.agda
src/agda/Tests/LimitsColimitsChecklist.agdai: src/agda/Tests/LimitsColimitsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/LimitsColimitsChecklist.agdai' 'build/recipes/src/agda/Tests/LimitsColimitsChecklist.agdai.sh'
# Compile src/agda/Tests/AdvancedMonadTheoryChecklist.agda
src/agda/Tests/AdvancedMonadTheoryChecklist.agdai: src/agda/Tests/AdvancedMonadTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AdvancedMonadTheoryChecklist.agdai' 'build/recipes/src/agda/Tests/AdvancedMonadTheoryChecklist.agdai.sh'
# Compile src/agda/Tests/CoreUniversalPropertiesChecklist.agda
src/agda/Tests/CoreUniversalPropertiesChecklist.agdai: src/agda/Tests/CoreUniversalPropertiesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/CoreUniversalPropertiesChecklist.agdai' 'build/recipes/src/agda/Tests/CoreUniversalPropertiesChecklist.agdai.sh'
# Compile src/agda/Tests/ChapterObligationsSmoke.agda
src/agda/Tests/ChapterObligationsSmoke.agdai: src/agda/Tests/ChapterObligationsSmoke.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub2.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ChapterObligationsSmoke.agdai' 'build/recipes/src/agda/Tests/ChapterObligationsSmoke.agdai.sh'
# Compile src/agda/Tests/Examples/CrossDomainCompositionTests.agda
src/agda/Tests/Examples/CrossDomainCompositionTests.agdai: src/agda/Tests/Examples/CrossDomainCompositionTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Examples/CrossDomainCompositionTests.agdai' 'build/recipes/src/agda/Tests/Examples/CrossDomainCompositionTests.agdai.sh'
# Compile src/agda/Tests/ConstructiveWitnessTests.agda
src/agda/Tests/ConstructiveWitnessTests.agdai: src/agda/Tests/ConstructiveWitnessTests.agda src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/AlgorithmUniversality.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ConstructiveWitnessTests.agdai' 'build/recipes/src/agda/Tests/ConstructiveWitnessTests.agdai.sh'
# Compile src/agda/Tests/PerformanceBoundaryTests.agda
src/agda/Tests/PerformanceBoundaryTests.agdai: src/agda/Tests/PerformanceBoundaryTests.agda src/agda/GrowthAnalysis.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/Algorithms/Registry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PerformanceBoundaryTests.agdai' 'build/recipes/src/agda/Tests/PerformanceBoundaryTests.agdai.sh'
# Compile src/agda/Tests/PathAggregatorTests.agda
src/agda/Tests/PathAggregatorTests.agdai: src/agda/Tests/PathAggregatorTests.agda src/agda/Core/PathAggregator.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/PathAggregatorTests.agdai' 'build/recipes/src/agda/Tests/PathAggregatorTests.agdai.sh'
# Compile src/agda/Tests/GroupsAbelianChecklist.agda
src/agda/Tests/GroupsAbelianChecklist.agdai: src/agda/Tests/GroupsAbelianChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/GroupsAbelianChecklist.agdai' 'build/recipes/src/agda/Tests/GroupsAbelianChecklist.agdai.sh'
# Compile src/agda/Tests/Chapter1Checklist.agda
src/agda/Tests/Chapter1Checklist.agdai: src/agda/Tests/Chapter1Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Chapter1Checklist.agdai' 'build/recipes/src/agda/Tests/Chapter1Checklist.agdai.sh'
# Compile src/agda/Tests/AdvancedFieldsChecklist.agda
src/agda/Tests/AdvancedFieldsChecklist.agdai: src/agda/Tests/AdvancedFieldsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AdvancedFieldsChecklist.agdai' 'build/recipes/src/agda/Tests/AdvancedFieldsChecklist.agdai.sh'
# Compile src/agda/Tests/WarningAggregatorsTest.agda
src/agda/Tests/WarningAggregatorsTest.agdai: src/agda/Tests/WarningAggregatorsTest.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/WarningAggregatorsTest.agdai' 'build/recipes/src/agda/Tests/WarningAggregatorsTest.agdai.sh'
# Compile src/agda/Tests/Index.agda
src/agda/Tests/Index.agdai: src/agda/Tests/Index.agda src/agda/Tests/PhaseExamples.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/GroupsAbelianChecklist.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Index.agdai' 'build/recipes/src/agda/Tests/Index.agdai.sh'
# Compile src/agda/Tests/ObligationAdapters.agda
src/agda/Tests/ObligationAdapters.agdai: src/agda/Tests/ObligationAdapters.agda src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Core/CategoricalAdapter.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Core/UniversalProperties.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Chapter2/Level2sub2.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ObligationAdapters.agdai' 'build/recipes/src/agda/Tests/ObligationAdapters.agdai.sh'
# Compile src/agda/Tests/ToposObligationAdapters.agda
src/agda/Tests/ToposObligationAdapters.agdai: src/agda/Tests/ToposObligationAdapters.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Core/CategoricalAdapter.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ToposObligationAdapters.agdai' 'build/recipes/src/agda/Tests/ToposObligationAdapters.agdai.sh'
# Compile src/agda/Tests/Chapters.agda
src/agda/Tests/Chapters.agdai: src/agda/Tests/Chapters.agda src/agda/Chapter3/Level3Index.agdai src/agda/Chapter2/Level2Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Chapters.agdai' 'build/recipes/src/agda/Tests/Chapters.agdai.sh'
# Compile src/agda/Tests/YonedaChecklist.agda
src/agda/Tests/YonedaChecklist.agdai: src/agda/Tests/YonedaChecklist.agda src/agda/Tests/ObligationAdapters.agdai src/agda/Core/Yoneda.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/YonedaChecklist.agdai' 'build/recipes/src/agda/Tests/YonedaChecklist.agdai.sh'
# Compile src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agda
src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agdai: src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agdai' 'build/recipes/src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agdai.sh'
# Compile src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agda
src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agdai: src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agdai' 'build/recipes/src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agdai.sh'
# Compile src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agda
src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agdai: src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agdai' 'build/recipes/src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agdai.sh'
# Compile src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agda
src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agdai: src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agdai' 'build/recipes/src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agdai.sh'
# Compile src/agda/Tests/AlgebraicCompletionChecklist.agda
src/agda/Tests/AlgebraicCompletionChecklist.agdai: src/agda/Tests/AlgebraicCompletionChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AlgebraicCompletionChecklist.agdai' 'build/recipes/src/agda/Tests/AlgebraicCompletionChecklist.agdai.sh'
# Compile src/agda/Tests/FunctorPropertiesChecklist.agda
src/agda/Tests/FunctorPropertiesChecklist.agdai: src/agda/Tests/FunctorPropertiesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/FunctorPropertiesChecklist.agdai' 'build/recipes/src/agda/Tests/FunctorPropertiesChecklist.agdai.sh'
# Compile src/agda/Tests/AlgebraChecklist.agda
src/agda/Tests/AlgebraChecklist.agdai: src/agda/Tests/AlgebraChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/AlgebraChecklist.agdai' 'build/recipes/src/agda/Tests/AlgebraChecklist.agdai.sh'
# Compile src/agda/Tests/ErrorHandlingTests.agda
src/agda/Tests/ErrorHandlingTests.agdai: src/agda/Tests/ErrorHandlingTests.agda src/agda/Core/Algorithms/Registry.agdai src/agda/Core/UniversalProperties.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ErrorHandlingTests.agdai' 'build/recipes/src/agda/Tests/ErrorHandlingTests.agdai.sh'
# Compile src/agda/Tests/GroupsStructureChecklist.agda
src/agda/Tests/GroupsStructureChecklist.agdai: src/agda/Tests/GroupsStructureChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/GroupsStructureChecklist.agdai' 'build/recipes/src/agda/Tests/GroupsStructureChecklist.agdai.sh'
# Compile src/agda/Tests/ModulesChecklist.agda
src/agda/Tests/ModulesChecklist.agdai: src/agda/Tests/ModulesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/ModulesChecklist.agdai' 'build/recipes/src/agda/Tests/ModulesChecklist.agdai.sh'
# Compile src/agda/Tests/Chapter3Checklist.agda
src/agda/Tests/Chapter3Checklist.agdai: src/agda/Tests/Chapter3Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Chapter3Checklist.agdai' 'build/recipes/src/agda/Tests/Chapter3Checklist.agdai.sh'
# Compile src/agda/Tests/Core/PhaseCategory/DualityTests.agda
src/agda/Tests/Core/PhaseCategory/DualityTests.agdai: src/agda/Tests/Core/PhaseCategory/DualityTests.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/Core/PhaseCategory/DualityTests.agdai' 'build/recipes/src/agda/Tests/Core/PhaseCategory/DualityTests.agdai.sh'
# Compile src/agda/Tests/SubobjectTheoryChecklist.agda
src/agda/Tests/SubobjectTheoryChecklist.agdai: src/agda/Tests/SubobjectTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Tests/SubobjectTheoryChecklist.agdai' 'build/recipes/src/agda/Tests/SubobjectTheoryChecklist.agdai.sh'
# Compile src/agda/Chapter2/Level2sub3.agda
src/agda/Chapter2/Level2sub3.agdai: src/agda/Chapter2/Level2sub3.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub3.agdai' 'build/recipes/src/agda/Chapter2/Level2sub3.agdai.sh'
# Compile src/agda/Chapter2/Level2sub6.agda
src/agda/Chapter2/Level2sub6.agdai: src/agda/Chapter2/Level2sub6.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub6.agdai' 'build/recipes/src/agda/Chapter2/Level2sub6.agdai.sh'
# Compile src/agda/Chapter2/Level2sub5.agda
src/agda/Chapter2/Level2sub5.agdai: src/agda/Chapter2/Level2sub5.agda src/agda/Chapter1/Level1Index.agdai src/agda/Chapter2/Level2sub3.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub5.agdai' 'build/recipes/src/agda/Chapter2/Level2sub5.agdai.sh'
# Compile src/agda/Chapter2/Level2sub8.agda
src/agda/Chapter2/Level2sub8.agdai: src/agda/Chapter2/Level2sub8.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub8.agdai' 'build/recipes/src/agda/Chapter2/Level2sub8.agdai.sh'
# Compile src/agda/Chapter2/Level2sub7.agda
src/agda/Chapter2/Level2sub7.agdai: src/agda/Chapter2/Level2sub7.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub7.agdai' 'build/recipes/src/agda/Chapter2/Level2sub7.agdai.sh'
# Compile src/agda/Chapter2/Level2sub2.agda
src/agda/Chapter2/Level2sub2.agdai: src/agda/Chapter2/Level2sub2.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub2.agdai' 'build/recipes/src/agda/Chapter2/Level2sub2.agdai.sh'
# Compile src/agda/Chapter2/Level2sub1.agda
src/agda/Chapter2/Level2sub1.agdai: src/agda/Chapter2/Level2sub1.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub1.agdai' 'build/recipes/src/agda/Chapter2/Level2sub1.agdai.sh'
# Compile src/agda/Chapter2/Level2sub4.agda
src/agda/Chapter2/Level2sub4.agdai: src/agda/Chapter2/Level2sub4.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2sub4.agdai' 'build/recipes/src/agda/Chapter2/Level2sub4.agdai.sh'
# Compile src/agda/Chapter2/Level2Index.agda
src/agda/Chapter2/Level2Index.agdai: src/agda/Chapter2/Level2Index.agda src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub2.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter2/Level2Index.agdai' 'build/recipes/src/agda/Chapter2/Level2Index.agdai.sh'
# Compile src/agda/Chapter3/Level3sub1.agda
src/agda/Chapter3/Level3sub1.agdai: src/agda/Chapter3/Level3sub1.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter3/Level3sub1.agdai' 'build/recipes/src/agda/Chapter3/Level3sub1.agdai.sh'
# Compile src/agda/Chapter3/Level3sub2.agda
src/agda/Chapter3/Level3sub2.agdai: src/agda/Chapter3/Level3sub2.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter3/Level3sub2.agdai' 'build/recipes/src/agda/Chapter3/Level3sub2.agdai.sh'
# Compile src/agda/Chapter3/Level3Index.agda
src/agda/Chapter3/Level3Index.agdai: src/agda/Chapter3/Level3Index.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3sub1.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter3/Level3Index.agdai' 'build/recipes/src/agda/Chapter3/Level3Index.agdai.sh'
# Compile src/agda/Infrastructure/Arity/BinTree.agda
src/agda/Infrastructure/Arity/BinTree.agdai: src/agda/Infrastructure/Arity/BinTree.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Arity/BinTree.agdai' 'build/recipes/src/agda/Infrastructure/Arity/BinTree.agdai.sh'
# Compile src/agda/Infrastructure/Universe.agda
src/agda/Infrastructure/Universe.agdai: src/agda/Infrastructure/Universe.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Universe.agdai' 'build/recipes/src/agda/Infrastructure/Universe.agdai.sh'
# Compile src/agda/Infrastructure/Iso/Structural.agda
src/agda/Infrastructure/Iso/Structural.agdai: src/agda/Infrastructure/Iso/Structural.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Iso/Structural.agdai' 'build/recipes/src/agda/Infrastructure/Iso/Structural.agdai.sh'
# Compile src/agda/Infrastructure/Adequacy.agda
src/agda/Infrastructure/Adequacy.agdai: src/agda/Infrastructure/Adequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Adequacy.agdai' 'build/recipes/src/agda/Infrastructure/Adequacy.agdai.sh'
# Compile src/agda/Infrastructure/Adequacy/Polymorphic.agda
src/agda/Infrastructure/Adequacy/Polymorphic.agdai: src/agda/Infrastructure/Adequacy/Polymorphic.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Adequacy/Polymorphic.agdai' 'build/recipes/src/agda/Infrastructure/Adequacy/Polymorphic.agdai.sh'
# Compile src/agda/Infrastructure/Adequacy/CrossDomain.agda
src/agda/Infrastructure/Adequacy/CrossDomain.agdai: src/agda/Infrastructure/Adequacy/CrossDomain.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Adequacy/CrossDomain.agdai' 'build/recipes/src/agda/Infrastructure/Adequacy/CrossDomain.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Compose.agda
src/agda/Infrastructure/Functor/Compose.agdai: src/agda/Infrastructure/Functor/Compose.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Compose.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Compose.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Interface.agda
src/agda/Infrastructure/Functor/Interface.agdai: src/agda/Infrastructure/Functor/Interface.agda src/agda/Infrastructure/Equality.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Interface.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Interface.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Adapters/Funext.agda
src/agda/Infrastructure/Functor/Adapters/Funext.agdai: src/agda/Infrastructure/Functor/Adapters/Funext.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Adapters/Funext.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Adapters/Funext.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/TransformationSystem.agda
src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai: src/agda/Infrastructure/Functor/Instances/TransformationSystem.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/Trivial.agda
src/agda/Infrastructure/Functor/Instances/Trivial.agdai: src/agda/Infrastructure/Functor/Instances/Trivial.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/Trivial.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/Trivial.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda
src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai: src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda
src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai: src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda
src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai: src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/Ambiguity.agda
src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai: src/agda/Infrastructure/Functor/Instances/Ambiguity.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai.sh'
# Compile src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda
src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai: src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai' 'build/recipes/src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai.sh'
# Compile src/agda/Infrastructure/Coherence/Path2.agda
src/agda/Infrastructure/Coherence/Path2.agdai: src/agda/Infrastructure/Coherence/Path2.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Coherence/Path2.agdai' 'build/recipes/src/agda/Infrastructure/Coherence/Path2.agdai.sh'
# Compile src/agda/Infrastructure/Equality.agda
src/agda/Infrastructure/Equality.agdai: src/agda/Infrastructure/Equality.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Equality.agdai' 'build/recipes/src/agda/Infrastructure/Equality.agdai.sh'
# Compile src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agda
src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai: src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai' 'build/recipes/src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai.sh'
# Compile src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agda
src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai: src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai' 'build/recipes/src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai.sh'
# Compile src/agda/Infrastructure/Polytopes/Associahedron.agda
src/agda/Infrastructure/Polytopes/Associahedron.agdai: src/agda/Infrastructure/Polytopes/Associahedron.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Polytopes/Associahedron.agdai' 'build/recipes/src/agda/Infrastructure/Polytopes/Associahedron.agdai.sh'
# Compile src/agda/Infrastructure/Definitions/Dictionary.agda
src/agda/Infrastructure/Definitions/Dictionary.agdai: src/agda/Infrastructure/Definitions/Dictionary.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Definitions/Dictionary.agdai' 'build/recipes/src/agda/Infrastructure/Definitions/Dictionary.agdai.sh'
# Compile src/agda/Infrastructure/Product/Bundle4.agda
src/agda/Infrastructure/Product/Bundle4.agdai: src/agda/Infrastructure/Product/Bundle4.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Product/Bundle4.agdai' 'build/recipes/src/agda/Infrastructure/Product/Bundle4.agdai.sh'
# Compile src/agda/Infrastructure/Index.agda
src/agda/Infrastructure/Index.agdai: src/agda/Infrastructure/Index.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Index.agdai' 'build/recipes/src/agda/Infrastructure/Index.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Movie.agda
src/agda/Infrastructure/Axiom/Movie.agdai: src/agda/Infrastructure/Axiom/Movie.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Movie.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Movie.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/PentagonFromTriangles.agda
src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai: src/agda/Infrastructure/Axiom/PentagonFromTriangles.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Adequacy.agda
src/agda/Infrastructure/Axiom/Adequacy.agdai: src/agda/Infrastructure/Axiom/Adequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Adequacy.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Adequacy.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Solver.agda
src/agda/Infrastructure/Axiom/Solver.agdai: src/agda/Infrastructure/Axiom/Solver.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Solver.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Solver.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Face.agda
src/agda/Infrastructure/Axiom/Face.agdai: src/agda/Infrastructure/Axiom/Face.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Face.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Face.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/SolvableInterface.agda
src/agda/Infrastructure/Axiom/SolvableInterface.agdai: src/agda/Infrastructure/Axiom/SolvableInterface.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/SolvableInterface.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/SolvableInterface.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agda
src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai: src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda
src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai: src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai.sh'
# Compile src/agda/Infrastructure/Axiom/Instance.agda
src/agda/Infrastructure/Axiom/Instance.agdai: src/agda/Infrastructure/Axiom/Instance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Axiom/Instance.agdai' 'build/recipes/src/agda/Infrastructure/Axiom/Instance.agdai.sh'
# Compile src/agda/Infrastructure/Wrapper/With.agda
src/agda/Infrastructure/Wrapper/With.agdai: src/agda/Infrastructure/Wrapper/With.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Infrastructure/Wrapper/With.agdai' 'build/recipes/src/agda/Infrastructure/Wrapper/With.agdai.sh'
# Compile src/agda/Algorithms/Instrumented.agda
src/agda/Algorithms/Instrumented.agdai: src/agda/Algorithms/Instrumented.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algorithms/Instrumented.agdai' 'build/recipes/src/agda/Algorithms/Instrumented.agdai.sh'
# Compile src/agda/Algorithms/Basic.agda
src/agda/Algorithms/Basic.agdai: src/agda/Algorithms/Basic.agda src/agda/Core/AlgebraicAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algorithms/Basic.agdai' 'build/recipes/src/agda/Algorithms/Basic.agdai.sh'
# Compile src/agda/Algorithms/Adapters/BundleAdapter.agda
src/agda/Algorithms/Adapters/BundleAdapter.agdai: src/agda/Algorithms/Adapters/BundleAdapter.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Algorithms/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algorithms/Adapters/BundleAdapter.agdai' 'build/recipes/src/agda/Algorithms/Adapters/BundleAdapter.agdai.sh'
# Compile src/agda/Algorithms/TestInstances.agda
src/agda/Algorithms/TestInstances.agdai: src/agda/Algorithms/TestInstances.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algorithms/TestInstances.agdai' 'build/recipes/src/agda/Algorithms/TestInstances.agdai.sh'
# Compile src/agda/MinimalAUDAXInlineTest.agda
src/agda/MinimalAUDAXInlineTest.agdai: src/agda/MinimalAUDAXInlineTest.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/MinimalAUDAXInlineTest.agdai' 'build/recipes/src/agda/MinimalAUDAXInlineTest.agdai.sh'
# Compile src/agda/Core.agda
src/agda/Core.agdai: src/agda/Core.agda src/agda/Chapter1/Level1.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core.agdai' 'build/recipes/src/agda/Core.agdai.sh'
# Compile src/agda/TechnicalDebt/PriorityMapping.agda
src/agda/TechnicalDebt/PriorityMapping.agdai: src/agda/TechnicalDebt/PriorityMapping.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/PriorityMapping.agdai' 'build/recipes/src/agda/TechnicalDebt/PriorityMapping.agdai.sh'
# Compile src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai: src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai' 'build/recipes/src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai.sh'
# Compile src/agda/TechnicalDebt/PriorityFormatting.agda
src/agda/TechnicalDebt/PriorityFormatting.agdai: src/agda/TechnicalDebt/PriorityFormatting.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/PriorityFormatting.agdai' 'build/recipes/src/agda/TechnicalDebt/PriorityFormatting.agdai.sh'
# Compile src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai: src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai' 'build/recipes/src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai.sh'
# Compile src/agda/TechnicalDebt/DeferredItemsDetection.agda
src/agda/TechnicalDebt/DeferredItemsDetection.agdai: src/agda/TechnicalDebt/DeferredItemsDetection.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/DeferredItemsDetection.agdai' 'build/recipes/src/agda/TechnicalDebt/DeferredItemsDetection.agdai.sh'
# Compile src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agda
src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai: src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai' 'build/recipes/src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai.sh'
# Compile src/agda/TechnicalDebt/PriorityOrchestration.agda
src/agda/TechnicalDebt/PriorityOrchestration.agdai: src/agda/TechnicalDebt/PriorityOrchestration.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/PriorityOrchestration.agdai' 'build/recipes/src/agda/TechnicalDebt/PriorityOrchestration.agdai.sh'
# Compile src/agda/TechnicalDebt/DeferredItemsFormatting.agda
src/agda/TechnicalDebt/DeferredItemsFormatting.agdai: src/agda/TechnicalDebt/DeferredItemsFormatting.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/DeferredItemsFormatting.agdai' 'build/recipes/src/agda/TechnicalDebt/DeferredItemsFormatting.agdai.sh'
# Compile src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda
src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai: src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai' 'build/recipes/src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai.sh'
# Compile src/agda/TechnicalDebt/Priorities.agda
src/agda/TechnicalDebt/Priorities.agdai: src/agda/TechnicalDebt/Priorities.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/TechnicalDebt/Priorities.agdai' 'build/recipes/src/agda/TechnicalDebt/Priorities.agdai.sh'
# Compile src/agda/Examples/RealWorldAlgorithms.agda
src/agda/Examples/RealWorldAlgorithms.agdai: src/agda/Examples/RealWorldAlgorithms.agda src/agda/Core/AlgorithmCorrectness.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/RealWorldAlgorithms.agdai' 'build/recipes/src/agda/Examples/RealWorldAlgorithms.agdai.sh'
# Compile src/agda/Examples/TechnicalDebtExample.agda
src/agda/Examples/TechnicalDebtExample.agdai: src/agda/Examples/TechnicalDebtExample.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/TechnicalDebtExample.agdai' 'build/recipes/src/agda/Examples/TechnicalDebtExample.agdai.sh'
# Compile src/agda/Examples/TransformationPathAdequacy.agda
src/agda/Examples/TransformationPathAdequacy.agdai: src/agda/Examples/TransformationPathAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/TransformationPathAdequacy.agdai' 'build/recipes/src/agda/Examples/TransformationPathAdequacy.agdai.sh'
# Compile src/agda/Examples/AmbiguityExamples.agda
src/agda/Examples/AmbiguityExamples.agdai: src/agda/Examples/AmbiguityExamples.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AmbiguityExamples.agdai' 'build/recipes/src/agda/Examples/AmbiguityExamples.agdai.sh'
# Compile src/agda/Examples/FunctionField/F2x.agda
src/agda/Examples/FunctionField/F2x.agdai: src/agda/Examples/FunctionField/F2x.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/FunctionField/F2x.agdai' 'build/recipes/src/agda/Examples/FunctionField/F2x.agdai.sh'
# Compile src/agda/Examples/AlgorithmCorrectnessExamples.agda
src/agda/Examples/AlgorithmCorrectnessExamples.agdai: src/agda/Examples/AlgorithmCorrectnessExamples.agda src/agda/Core/AlgorithmCorrectness.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AlgorithmCorrectnessExamples.agdai' 'build/recipes/src/agda/Examples/AlgorithmCorrectnessExamples.agdai.sh'
# Compile src/agda/Examples/FunctionPathAdequacy.agda
src/agda/Examples/FunctionPathAdequacy.agdai: src/agda/Examples/FunctionPathAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/FunctionPathAdequacy.agdai' 'build/recipes/src/agda/Examples/FunctionPathAdequacy.agdai.sh'
# Compile src/agda/Examples/FunctionCategoryAdequacy.agda
src/agda/Examples/FunctionCategoryAdequacy.agdai: src/agda/Examples/FunctionCategoryAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/FunctionCategoryAdequacy.agdai' 'build/recipes/src/agda/Examples/FunctionCategoryAdequacy.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Docs.agda
src/agda/Examples/Makefile/Targets/Docs.agdai: src/agda/Examples/Makefile/Targets/Docs.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Docs.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Docs.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Infra.agda
src/agda/Examples/Makefile/Targets/Infra.agdai: src/agda/Examples/Makefile/Targets/Infra.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Infra.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Infra.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Priority.agda
src/agda/Examples/Makefile/Targets/Priority.agdai: src/agda/Examples/Makefile/Targets/Priority.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Priority.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Priority.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Docker.agda
src/agda/Examples/Makefile/Targets/Docker.agdai: src/agda/Examples/Makefile/Targets/Docker.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Docker.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Docker.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/JsonRoundtrip.agda
src/agda/Examples/Makefile/Targets/JsonRoundtrip.agdai: src/agda/Examples/Makefile/Targets/JsonRoundtrip.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/JsonRoundtrip.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/JsonRoundtrip.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/RoadmapExports.agda
src/agda/Examples/Makefile/Targets/RoadmapExports.agdai: src/agda/Examples/Makefile/Targets/RoadmapExports.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/RoadmapExports.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/RoadmapExports.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Python.agda
src/agda/Examples/Makefile/Targets/Python.agdai: src/agda/Examples/Makefile/Targets/Python.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Python.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Python.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/AgdaBuild.agda
src/agda/Examples/Makefile/Targets/AgdaBuild.agdai: src/agda/Examples/Makefile/Targets/AgdaBuild.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/AgdaBuild.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/AgdaBuild.agdai.sh'
# Compile src/agda/Examples/Makefile/Targets/Composite.agda
src/agda/Examples/Makefile/Targets/Composite.agdai: src/agda/Examples/Makefile/Targets/Composite.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/Makefile/Targets/Composite.agdai' 'build/recipes/src/agda/Examples/Makefile/Targets/Composite.agdai.sh'
# Compile src/agda/Examples/NumberField/Sqrt2.agda
src/agda/Examples/NumberField/Sqrt2.agdai: src/agda/Examples/NumberField/Sqrt2.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/NumberField/Sqrt2.agdai' 'build/recipes/src/agda/Examples/NumberField/Sqrt2.agdai.sh'
# Compile src/agda/Examples/FiniteField/GF8.agda
src/agda/Examples/FiniteField/GF8.agdai: src/agda/Examples/FiniteField/GF8.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/FiniteField/GF8.agdai' 'build/recipes/src/agda/Examples/FiniteField/GF8.agdai.sh'
# Compile src/agda/Examples/InstrumentedAlgorithmDemo.agda
src/agda/Examples/InstrumentedAlgorithmDemo.agdai: src/agda/Examples/InstrumentedAlgorithmDemo.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/InstrumentedAlgorithmDemo.agdai' 'build/recipes/src/agda/Examples/InstrumentedAlgorithmDemo.agdai.sh'
# Compile src/agda/Examples/DeferredItemsScanner.agda
src/agda/Examples/DeferredItemsScanner.agdai: src/agda/Examples/DeferredItemsScanner.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/DeferredItemsScanner.agdai' 'build/recipes/src/agda/Examples/DeferredItemsScanner.agdai.sh'
# Compile src/agda/Examples/DefinitionDictionaryAdequacy.agda
src/agda/Examples/DefinitionDictionaryAdequacy.agdai: src/agda/Examples/DefinitionDictionaryAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/DefinitionDictionaryAdequacy.agdai' 'build/recipes/src/agda/Examples/DefinitionDictionaryAdequacy.agdai.sh'
# Compile src/agda/Examples/TransformationSystemExamples.agda
src/agda/Examples/TransformationSystemExamples.agdai: src/agda/Examples/TransformationSystemExamples.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/TransformationSystemExamples.agdai' 'build/recipes/src/agda/Examples/TransformationSystemExamples.agdai.sh'
# Compile src/agda/Examples/AutomaticEvidenceDemo.agda
src/agda/Examples/AutomaticEvidenceDemo.agdai: src/agda/Examples/AutomaticEvidenceDemo.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AutomaticEvidenceDemo.agdai' 'build/recipes/src/agda/Examples/AutomaticEvidenceDemo.agdai.sh'
# Compile src/agda/Examples/ConstructiveWitnessExamples.agda
src/agda/Examples/ConstructiveWitnessExamples.agdai: src/agda/Examples/ConstructiveWitnessExamples.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/ConstructiveWitnessExamples.agdai' 'build/recipes/src/agda/Examples/ConstructiveWitnessExamples.agdai.sh'
# Compile src/agda/Examples/CrossDomainComposition.agda
src/agda/Examples/CrossDomainComposition.agdai: src/agda/Examples/CrossDomainComposition.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/CrossDomainComposition.agdai' 'build/recipes/src/agda/Examples/CrossDomainComposition.agdai.sh'
# Compile src/agda/Examples/AgdaMakefileDeps.agda
src/agda/Examples/AgdaMakefileDeps.agdai: src/agda/Examples/AgdaMakefileDeps.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AgdaMakefileDeps.agdai' 'build/recipes/src/agda/Examples/AgdaMakefileDeps.agdai.sh'
# Compile src/agda/Examples/LazyHybridDemo.agda
src/agda/Examples/LazyHybridDemo.agdai: src/agda/Examples/LazyHybridDemo.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/LazyHybridDemo.agdai' 'build/recipes/src/agda/Examples/LazyHybridDemo.agdai.sh'
# Compile src/agda/Examples/TechnicalDebtRegistry.agda
src/agda/Examples/TechnicalDebtRegistry.agdai: src/agda/Examples/TechnicalDebtRegistry.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/TechnicalDebtRegistry.agdai' 'build/recipes/src/agda/Examples/TechnicalDebtRegistry.agdai.sh'
# Compile src/agda/Examples/RoadmapIssueSync.agda
src/agda/Examples/RoadmapIssueSync.agdai: src/agda/Examples/RoadmapIssueSync.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/RoadmapIssueSync.agdai' 'build/recipes/src/agda/Examples/RoadmapIssueSync.agdai.sh'
# Compile src/agda/Examples/MakefileTargets.agda
src/agda/Examples/MakefileTargets.agdai: src/agda/Examples/MakefileTargets.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/MakefileTargets.agdai' 'build/recipes/src/agda/Examples/MakefileTargets.agdai.sh'
# Compile src/agda/Examples/FunctorComposition.agda
src/agda/Examples/FunctorComposition.agdai: src/agda/Examples/FunctorComposition.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/FunctorComposition.agdai' 'build/recipes/src/agda/Examples/FunctorComposition.agdai.sh'
# Compile src/agda/Examples/PhaseCategoryExamples.agda
src/agda/Examples/PhaseCategoryExamples.agdai: src/agda/Examples/PhaseCategoryExamples.agda src/agda/Core/PhaseCategory.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/PhaseCategoryExamples.agdai' 'build/recipes/src/agda/Examples/PhaseCategoryExamples.agdai.sh'
# Compile src/agda/Examples/EqualityExamples.agda
src/agda/Examples/EqualityExamples.agdai: src/agda/Examples/EqualityExamples.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/EqualityExamples.agdai' 'build/recipes/src/agda/Examples/EqualityExamples.agdai.sh'
# Compile src/agda/Examples/TechnicalDebtChecklist.agda
src/agda/Examples/TechnicalDebtChecklist.agdai: src/agda/Examples/TechnicalDebtChecklist.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/TechnicalDebtChecklist.agdai' 'build/recipes/src/agda/Examples/TechnicalDebtChecklist.agdai.sh'
# Compile src/agda/Examples/AmbiguityAdequacy.agda
src/agda/Examples/AmbiguityAdequacy.agdai: src/agda/Examples/AmbiguityAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AmbiguityAdequacy.agdai' 'build/recipes/src/agda/Examples/AmbiguityAdequacy.agdai.sh'
# Compile src/agda/Examples/AgdaFileScanFFI.agda
src/agda/Examples/AgdaFileScanFFI.agdai: src/agda/Examples/AgdaFileScanFFI.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/AgdaFileScanFFI.agdai' 'build/recipes/src/agda/Examples/AgdaFileScanFFI.agdai.sh'
# Compile src/agda/Examples/ExporterMakefile.agda
src/agda/Examples/ExporterMakefile.agdai: src/agda/Examples/ExporterMakefile.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Examples/ExporterMakefile.agdai' 'build/recipes/src/agda/Examples/ExporterMakefile.agdai.sh'
# Compile src/agda/Markdown/ExportProof.agda
src/agda/Markdown/ExportProof.agdai: src/agda/Markdown/ExportProof.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Markdown/ExportProof.agdai' 'build/recipes/src/agda/Markdown/ExportProof.agdai.sh'
# Compile src/agda/Markdown/Normalization.agda
src/agda/Markdown/Normalization.agdai: src/agda/Markdown/Normalization.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Markdown/Normalization.agdai' 'build/recipes/src/agda/Markdown/Normalization.agdai.sh'
# Compile src/agda/Docs/MetaIndex.agda
src/agda/Docs/MetaIndex.agdai: src/agda/Docs/MetaIndex.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Docs/MetaIndex.agdai' 'build/recipes/src/agda/Docs/MetaIndex.agdai.sh'
# Compile src/agda/GrowthAnalysis.agda
src/agda/GrowthAnalysis.agdai: src/agda/GrowthAnalysis.agda src/agda/Core/GrowthMetrics.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/GrowthAnalysis.agdai' 'build/recipes/src/agda/GrowthAnalysis.agdai.sh'
# Compile src/agda/Plan/CIM/TransformationSystem.agda
src/agda/Plan/CIM/TransformationSystem.agdai: src/agda/Plan/CIM/TransformationSystem.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/TransformationSystem.agdai' 'build/recipes/src/agda/Plan/CIM/TransformationSystem.agdai.sh'
# Compile src/agda/Plan/CIM/PolytopeExpansion.agda
src/agda/Plan/CIM/PolytopeExpansion.agdai: src/agda/Plan/CIM/PolytopeExpansion.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PolytopeExpansion.agdai' 'build/recipes/src/agda/Plan/CIM/PolytopeExpansion.agdai.sh'
# Compile src/agda/Plan/CIM/PlanningKernel.agda
src/agda/Plan/CIM/PlanningKernel.agdai: src/agda/Plan/CIM/PlanningKernel.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PlanningKernel.agdai' 'build/recipes/src/agda/Plan/CIM/PlanningKernel.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationExtraction.agda
src/agda/Plan/CIM/JSONTransformationExtraction.agdai: src/agda/Plan/CIM/JSONTransformationExtraction.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationExtraction.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationExtraction.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationTesting.agda
src/agda/Plan/CIM/JSONTransformationTesting.agdai: src/agda/Plan/CIM/JSONTransformationTesting.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationTesting.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationTesting.agdai.sh'
# Compile src/agda/Plan/CIM/PandocProofExample.agda
src/agda/Plan/CIM/PandocProofExample.agdai: src/agda/Plan/CIM/PandocProofExample.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocProofExample.agdai' 'build/recipes/src/agda/Plan/CIM/PandocProofExample.agdai.sh'
# Compile src/agda/Plan/CIM/ProofTraceGeneric.agda
src/agda/Plan/CIM/ProofTraceGeneric.agdai: src/agda/Plan/CIM/ProofTraceGeneric.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/ProofTraceGeneric.agdai' 'build/recipes/src/agda/Plan/CIM/ProofTraceGeneric.agdai.sh'
# Compile src/agda/Plan/CIM/PandocProtocols.agda
src/agda/Plan/CIM/PandocProtocols.agdai: src/agda/Plan/CIM/PandocProtocols.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocProtocols.agdai' 'build/recipes/src/agda/Plan/CIM/PandocProtocols.agdai.sh'
# Compile src/agda/Plan/CIM/PandocShowBlock.agda
src/agda/Plan/CIM/PandocShowBlock.agdai: src/agda/Plan/CIM/PandocShowBlock.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocShowBlock.agdai' 'build/recipes/src/agda/Plan/CIM/PandocShowBlock.agdai.sh'
# Compile src/agda/Plan/CIM/CHIPCoreRecompose.agda
src/agda/Plan/CIM/CHIPCoreRecompose.agdai: src/agda/Plan/CIM/CHIPCoreRecompose.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/CHIPCoreRecompose.agdai' 'build/recipes/src/agda/Plan/CIM/CHIPCoreRecompose.agdai.sh'
# Compile src/agda/Plan/CIM/PandocProofExport.agda
src/agda/Plan/CIM/PandocProofExport.agdai: src/agda/Plan/CIM/PandocProofExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocProofExport.agdai' 'build/recipes/src/agda/Plan/CIM/PandocProofExport.agdai.sh'
# Compile src/agda/Plan/CIM/Utility.agda
src/agda/Plan/CIM/Utility.agdai: src/agda/Plan/CIM/Utility.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/Utility.agdai' 'build/recipes/src/agda/Plan/CIM/Utility.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps.agda
src/agda/Plan/CIM/IngestedRoadmaps.agdai: src/agda/Plan/CIM/IngestedRoadmaps.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps.agdai.sh'
# Compile src/agda/Plan/CIM/Elasticity.agda
src/agda/Plan/CIM/Elasticity.agdai: src/agda/Plan/CIM/Elasticity.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/Elasticity.agdai' 'build/recipes/src/agda/Plan/CIM/Elasticity.agdai.sh'
# Compile src/agda/Plan/CIM/Structure.agda
src/agda/Plan/CIM/Structure.agdai: src/agda/Plan/CIM/Structure.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/Structure.agdai' 'build/recipes/src/agda/Plan/CIM/Structure.agdai.sh'
# Compile src/agda/Plan/CIM/DocumentSynthesis.agda
src/agda/Plan/CIM/DocumentSynthesis.agdai: src/agda/Plan/CIM/DocumentSynthesis.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/DocumentSynthesis.agdai' 'build/recipes/src/agda/Plan/CIM/DocumentSynthesis.agdai.sh'
# Compile src/agda/Plan/CIM/Metricization.agda
src/agda/Plan/CIM/Metricization.agdai: src/agda/Plan/CIM/Metricization.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/Metricization.agdai' 'build/recipes/src/agda/Plan/CIM/Metricization.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationBackends.agda
src/agda/Plan/CIM/JSONTransformationBackends.agdai: src/agda/Plan/CIM/JSONTransformationBackends.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationBackends.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationBackends.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformation.agda
src/agda/Plan/CIM/JSONTransformation.agdai: src/agda/Plan/CIM/JSONTransformation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformation.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformation.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapExporterMain.agda
src/agda/Plan/CIM/RoadmapExporterMain.agdai: src/agda/Plan/CIM/RoadmapExporterMain.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapExporterMain.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapExporterMain.agdai.sh'
# Compile src/agda/Plan/CIM/CHIPRecomposed.agda
src/agda/Plan/CIM/CHIPRecomposed.agdai: src/agda/Plan/CIM/CHIPRecomposed.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/CHIPRecomposed.agdai' 'build/recipes/src/agda/Plan/CIM/CHIPRecomposed.agdai.sh'
# Compile src/agda/Plan/CIM/PandocShowMdBlock.agda
src/agda/Plan/CIM/PandocShowMdBlock.agdai: src/agda/Plan/CIM/PandocShowMdBlock.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocShowMdBlock.agdai' 'build/recipes/src/agda/Plan/CIM/PandocShowMdBlock.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai.sh'
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai' 'build/recipes/src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai.sh'
# Compile src/agda/Plan/CIM/PriorityProfileExport.agda
src/agda/Plan/CIM/PriorityProfileExport.agdai: src/agda/Plan/CIM/PriorityProfileExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PriorityProfileExport.agdai' 'build/recipes/src/agda/Plan/CIM/PriorityProfileExport.agdai.sh'
# Compile src/agda/Plan/CIM/SchemaValidationGeneric.agda
src/agda/Plan/CIM/SchemaValidationGeneric.agdai: src/agda/Plan/CIM/SchemaValidationGeneric.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/SchemaValidationGeneric.agdai' 'build/recipes/src/agda/Plan/CIM/SchemaValidationGeneric.agdai.sh'
# Compile src/agda/Plan/CIM/PlanningExport.agda
src/agda/Plan/CIM/PlanningExport.agdai: src/agda/Plan/CIM/PlanningExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PlanningExport.agdai' 'build/recipes/src/agda/Plan/CIM/PlanningExport.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapExporter.agda
src/agda/Plan/CIM/RoadmapExporter.agdai: src/agda/Plan/CIM/RoadmapExporter.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapExporter.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapExporter.agdai.sh'
# Compile src/agda/Plan/CIM/CanonicalRoadmap.agda
src/agda/Plan/CIM/CanonicalRoadmap.agdai: src/agda/Plan/CIM/CanonicalRoadmap.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/CanonicalRoadmap.agdai' 'build/recipes/src/agda/Plan/CIM/CanonicalRoadmap.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationAdequacy.agda
src/agda/Plan/CIM/JSONTransformationAdequacy.agdai: src/agda/Plan/CIM/JSONTransformationAdequacy.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationAdequacy.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationAdequacy.agdai.sh'
# Compile src/agda/Plan/CIM/CHIPConformance.agda
src/agda/Plan/CIM/CHIPConformance.agdai: src/agda/Plan/CIM/CHIPConformance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/CHIPConformance.agdai' 'build/recipes/src/agda/Plan/CIM/CHIPConformance.agdai.sh'
# Compile src/agda/Plan/CIM/FrameworkMetadata.agda
src/agda/Plan/CIM/FrameworkMetadata.agdai: src/agda/Plan/CIM/FrameworkMetadata.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/FrameworkMetadata.agdai' 'build/recipes/src/agda/Plan/CIM/FrameworkMetadata.agdai.sh'
# Compile src/agda/Plan/CIM/JSONConcrete.agda
src/agda/Plan/CIM/JSONConcrete.agdai: src/agda/Plan/CIM/JSONConcrete.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONConcrete.agdai' 'build/recipes/src/agda/Plan/CIM/JSONConcrete.agdai.sh'
# Compile src/agda/Plan/CIM/ABNFParserGeneric.agda
src/agda/Plan/CIM/ABNFParserGeneric.agdai: src/agda/Plan/CIM/ABNFParserGeneric.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/ABNFParserGeneric.agdai' 'build/recipes/src/agda/Plan/CIM/ABNFParserGeneric.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationGeneric.agda
src/agda/Plan/CIM/JSONTransformationGeneric.agdai: src/agda/Plan/CIM/JSONTransformationGeneric.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationGeneric.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationGeneric.agdai.sh'
# Compile src/agda/Plan/CIM/DependencyGraphExport.agda
src/agda/Plan/CIM/DependencyGraphExport.agdai: src/agda/Plan/CIM/DependencyGraphExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/DependencyGraphExport.agdai' 'build/recipes/src/agda/Plan/CIM/DependencyGraphExport.agdai.sh'
# Compile src/agda/Plan/CIM/ModuleExporter.agda
src/agda/Plan/CIM/ModuleExporter.agdai: src/agda/Plan/CIM/ModuleExporter.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/ModuleExporter.agdai' 'build/recipes/src/agda/Plan/CIM/ModuleExporter.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapIndex.agda
src/agda/Plan/CIM/RoadmapIndex.agdai: src/agda/Plan/CIM/RoadmapIndex.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapIndex.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapIndex.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapSync.agda
src/agda/Plan/CIM/RoadmapSync.agdai: src/agda/Plan/CIM/RoadmapSync.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapSync.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapSync.agdai.sh'
# Compile src/agda/Plan/CIM/PandocToMarkdown.agda
src/agda/Plan/CIM/PandocToMarkdown.agdai: src/agda/Plan/CIM/PandocToMarkdown.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocToMarkdown.agdai' 'build/recipes/src/agda/Plan/CIM/PandocToMarkdown.agdai.sh'
# Compile src/agda/Plan/CIM/DocumentationContent.agda
src/agda/Plan/CIM/DocumentationContent.agdai: src/agda/Plan/CIM/DocumentationContent.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/DocumentationContent.agdai' 'build/recipes/src/agda/Plan/CIM/DocumentationContent.agdai.sh'
# Compile src/agda/Plan/CIM/YonedaProfiler.agda
src/agda/Plan/CIM/YonedaProfiler.agdai: src/agda/Plan/CIM/YonedaProfiler.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/YonedaProfiler.agdai' 'build/recipes/src/agda/Plan/CIM/YonedaProfiler.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapSPPF.agda
src/agda/Plan/CIM/RoadmapSPPF.agdai: src/agda/Plan/CIM/RoadmapSPPF.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapSPPF.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapSPPF.agdai.sh'
# Compile src/agda/Plan/CIM/Ambiguity.agda
src/agda/Plan/CIM/Ambiguity.agdai: src/agda/Plan/CIM/Ambiguity.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/Ambiguity.agdai' 'build/recipes/src/agda/Plan/CIM/Ambiguity.agdai.sh'
# Compile src/agda/Plan/CIM/PandocShowInline.agda
src/agda/Plan/CIM/PandocShowInline.agdai: src/agda/Plan/CIM/PandocShowInline.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocShowInline.agdai' 'build/recipes/src/agda/Plan/CIM/PandocShowInline.agdai.sh'
# Compile src/agda/Plan/CIM/TypeCheckingGeneric.agda
src/agda/Plan/CIM/TypeCheckingGeneric.agdai: src/agda/Plan/CIM/TypeCheckingGeneric.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/TypeCheckingGeneric.agdai' 'build/recipes/src/agda/Plan/CIM/TypeCheckingGeneric.agdai.sh'
# Compile src/agda/Plan/CIM/RoadmapSPPFExport.agda
src/agda/Plan/CIM/RoadmapSPPFExport.agdai: src/agda/Plan/CIM/RoadmapSPPFExport.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/RoadmapSPPFExport.agdai' 'build/recipes/src/agda/Plan/CIM/RoadmapSPPFExport.agdai.sh'
# Compile src/agda/Plan/CIM/FunctorialConstructs.agda
src/agda/Plan/CIM/FunctorialConstructs.agdai: src/agda/Plan/CIM/FunctorialConstructs.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/FunctorialConstructs.agdai' 'build/recipes/src/agda/Plan/CIM/FunctorialConstructs.agdai.sh'
# Compile src/agda/Plan/CIM/JSONTransformationContract.agda
src/agda/Plan/CIM/JSONTransformationContract.agdai: src/agda/Plan/CIM/JSONTransformationContract.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/JSONTransformationContract.agdai' 'build/recipes/src/agda/Plan/CIM/JSONTransformationContract.agdai.sh'
# Compile src/agda/Plan/CIM/GPNarrativeDAG.agda
src/agda/Plan/CIM/GPNarrativeDAG.agdai: src/agda/Plan/CIM/GPNarrativeDAG.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/GPNarrativeDAG.agdai' 'build/recipes/src/agda/Plan/CIM/GPNarrativeDAG.agdai.sh'
# Compile src/agda/Plan/CIM/MarkdownNormalize.agda
src/agda/Plan/CIM/MarkdownNormalize.agdai: src/agda/Plan/CIM/MarkdownNormalize.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/MarkdownNormalize.agdai' 'build/recipes/src/agda/Plan/CIM/MarkdownNormalize.agdai.sh'
# Compile src/agda/Plan/CIM/PandocAST.agda
src/agda/Plan/CIM/PandocAST.agdai: src/agda/Plan/CIM/PandocAST.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/PandocAST.agdai' 'build/recipes/src/agda/Plan/CIM/PandocAST.agdai.sh'
# Compile src/agda/Plan/CIM/GrammarBridge.agda
src/agda/Plan/CIM/GrammarBridge.agdai: src/agda/Plan/CIM/GrammarBridge.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/GrammarBridge.agdai' 'build/recipes/src/agda/Plan/CIM/GrammarBridge.agdai.sh'
# Compile src/agda/Plan/CIM/MarkdownParse.agda
src/agda/Plan/CIM/MarkdownParse.agdai: src/agda/Plan/CIM/MarkdownParse.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Plan/CIM/MarkdownParse.agdai' 'build/recipes/src/agda/Plan/CIM/MarkdownParse.agdai.sh'
# Compile src/agda/Algebra/Groups/Basic.agda
src/agda/Algebra/Groups/Basic.agdai: src/agda/Algebra/Groups/Basic.agda src/agda/Algebra/Groups/Types.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Basic.agdai' 'build/recipes/src/agda/Algebra/Groups/Basic.agdai.sh'
# Compile src/agda/Algebra/Groups/Theorems/Classical.agda
src/agda/Algebra/Groups/Theorems/Classical.agdai: src/agda/Algebra/Groups/Theorems/Classical.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Theorems/Classical.agdai' 'build/recipes/src/agda/Algebra/Groups/Theorems/Classical.agdai.sh'
# Compile src/agda/Algebra/Groups/ClassicalInstance.agda
src/agda/Algebra/Groups/ClassicalInstance.agdai: src/agda/Algebra/Groups/ClassicalInstance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/ClassicalInstance.agdai' 'build/recipes/src/agda/Algebra/Groups/ClassicalInstance.agdai.sh'
# Compile src/agda/Algebra/Groups/Structure.agda
src/agda/Algebra/Groups/Structure.agdai: src/agda/Algebra/Groups/Structure.agda src/agda/Algebra/Groups/Free.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Structure.agdai' 'build/recipes/src/agda/Algebra/Groups/Structure.agdai.sh'
# Compile src/agda/Algebra/Groups/BasicWithTheorems.agda
src/agda/Algebra/Groups/BasicWithTheorems.agdai: src/agda/Algebra/Groups/BasicWithTheorems.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/BasicWithTheorems.agdai' 'build/recipes/src/agda/Algebra/Groups/BasicWithTheorems.agdai.sh'
# Compile src/agda/Algebra/Groups/BasicParameterized.agda
src/agda/Algebra/Groups/BasicParameterized.agdai: src/agda/Algebra/Groups/BasicParameterized.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/BasicParameterized.agdai' 'build/recipes/src/agda/Algebra/Groups/BasicParameterized.agdai.sh'
# Compile src/agda/Algebra/Groups/Abelian.agda
src/agda/Algebra/Groups/Abelian.agdai: src/agda/Algebra/Groups/Abelian.agda src/agda/Chapter2/Level2sub1.agdai src/agda/Algebra/Enrichment.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Abelian.agdai' 'build/recipes/src/agda/Algebra/Groups/Abelian.agdai.sh'
# Compile src/agda/Algebra/Groups/Types.agda
src/agda/Algebra/Groups/Types.agdai: src/agda/Algebra/Groups/Types.agda src/agda/Algebra/Foundation.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Types.agdai' 'build/recipes/src/agda/Algebra/Groups/Types.agdai.sh'
# Compile src/agda/Algebra/Groups/Free.agda
src/agda/Algebra/Groups/Free.agdai: src/agda/Algebra/Groups/Free.agda src/agda/Algebra/Groups/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Groups/Free.agdai' 'build/recipes/src/agda/Algebra/Groups/Free.agdai.sh'
# Compile src/agda/Algebra/Enrichment.agda
src/agda/Algebra/Enrichment.agdai: src/agda/Algebra/Enrichment.agda src/agda/Algebra/Foundation.agdai src/agda/Chapter2/Level2sub6.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Enrichment.agdai' 'build/recipes/src/agda/Algebra/Enrichment.agdai.sh'
# Compile src/agda/Algebra/Rings/Basic.agda
src/agda/Algebra/Rings/Basic.agdai: src/agda/Algebra/Rings/Basic.agda src/agda/Algebra/Rings/Types.agdai src/agda/Chapter2/Level2sub3.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Rings/Basic.agdai' 'build/recipes/src/agda/Algebra/Rings/Basic.agdai.sh'
# Compile src/agda/Algebra/Rings/Theorems/Classical.agda
src/agda/Algebra/Rings/Theorems/Classical.agdai: src/agda/Algebra/Rings/Theorems/Classical.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Rings/Theorems/Classical.agdai' 'build/recipes/src/agda/Algebra/Rings/Theorems/Classical.agdai.sh'
# Compile src/agda/Algebra/Rings/ClassicalInstance.agda
src/agda/Algebra/Rings/ClassicalInstance.agdai: src/agda/Algebra/Rings/ClassicalInstance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Rings/ClassicalInstance.agdai' 'build/recipes/src/agda/Algebra/Rings/ClassicalInstance.agdai.sh'
# Compile src/agda/Algebra/Rings/BasicWithTheorems.agda
src/agda/Algebra/Rings/BasicWithTheorems.agdai: src/agda/Algebra/Rings/BasicWithTheorems.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Rings/BasicWithTheorems.agdai' 'build/recipes/src/agda/Algebra/Rings/BasicWithTheorems.agdai.sh'
# Compile src/agda/Algebra/Rings/Types.agda
src/agda/Algebra/Rings/Types.agdai: src/agda/Algebra/Rings/Types.agda src/agda/Algebra/Groups/Types.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Rings/Types.agdai' 'build/recipes/src/agda/Algebra/Rings/Types.agdai.sh'
# Compile src/agda/Algebra/Modules/Basic.agda
src/agda/Algebra/Modules/Basic.agdai: src/agda/Algebra/Modules/Basic.agda src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Modules/Types.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Modules/Basic.agdai' 'build/recipes/src/agda/Algebra/Modules/Basic.agdai.sh'
# Compile src/agda/Algebra/Modules/Theorems/Classical.agda
src/agda/Algebra/Modules/Theorems/Classical.agdai: src/agda/Algebra/Modules/Theorems/Classical.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Modules/Theorems/Classical.agdai' 'build/recipes/src/agda/Algebra/Modules/Theorems/Classical.agdai.sh'
# Compile src/agda/Algebra/Modules/ClassicalInstance.agda
src/agda/Algebra/Modules/ClassicalInstance.agdai: src/agda/Algebra/Modules/ClassicalInstance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Modules/ClassicalInstance.agdai' 'build/recipes/src/agda/Algebra/Modules/ClassicalInstance.agdai.sh'
# Compile src/agda/Algebra/Modules/BasicWithTheorems.agda
src/agda/Algebra/Modules/BasicWithTheorems.agdai: src/agda/Algebra/Modules/BasicWithTheorems.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Modules/BasicWithTheorems.agdai' 'build/recipes/src/agda/Algebra/Modules/BasicWithTheorems.agdai.sh'
# Compile src/agda/Algebra/Modules/Types.agda
src/agda/Algebra/Modules/Types.agdai: src/agda/Algebra/Modules/Types.agda src/agda/Algebra/Fields/Types.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Modules/Types.agdai' 'build/recipes/src/agda/Algebra/Modules/Types.agdai.sh'
# Compile src/agda/Algebra/Foundation.agda
src/agda/Algebra/Foundation.agdai: src/agda/Algebra/Foundation.agda src/agda/Chapter1/Level1Index.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Foundation.agdai' 'build/recipes/src/agda/Algebra/Foundation.agdai.sh'
# Compile src/agda/Algebra/Fields/Basic.agda
src/agda/Algebra/Fields/Basic.agdai: src/agda/Algebra/Fields/Basic.agda src/agda/Algebra/Modules/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/Basic.agdai' 'build/recipes/src/agda/Algebra/Fields/Basic.agdai.sh'
# Compile src/agda/Algebra/Fields/Advanced.agda
src/agda/Algebra/Fields/Advanced.agdai: src/agda/Algebra/Fields/Advanced.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/Advanced.agdai' 'build/recipes/src/agda/Algebra/Fields/Advanced.agdai.sh'
# Compile src/agda/Algebra/Fields/Theorems/Classical.agda
src/agda/Algebra/Fields/Theorems/Classical.agdai: src/agda/Algebra/Fields/Theorems/Classical.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/Theorems/Classical.agdai' 'build/recipes/src/agda/Algebra/Fields/Theorems/Classical.agdai.sh'
# Compile src/agda/Algebra/Fields/ClassicalInstance.agda
src/agda/Algebra/Fields/ClassicalInstance.agdai: src/agda/Algebra/Fields/ClassicalInstance.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/ClassicalInstance.agdai' 'build/recipes/src/agda/Algebra/Fields/ClassicalInstance.agdai.sh'
# Compile src/agda/Algebra/Fields/BasicWithTheorems.agda
src/agda/Algebra/Fields/BasicWithTheorems.agdai: src/agda/Algebra/Fields/BasicWithTheorems.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/BasicWithTheorems.agdai' 'build/recipes/src/agda/Algebra/Fields/BasicWithTheorems.agdai.sh'
# Compile src/agda/Algebra/Fields/Types.agda
src/agda/Algebra/Fields/Types.agdai: src/agda/Algebra/Fields/Types.agda src/agda/Algebra/Rings/Types.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Fields/Types.agdai' 'build/recipes/src/agda/Algebra/Fields/Types.agdai.sh'
# Compile src/agda/Algebra/Index.agda
src/agda/Algebra/Index.agdai: src/agda/Algebra/Index.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Index.agdai' 'build/recipes/src/agda/Algebra/Index.agdai.sh'
# Compile src/agda/Algebra/Bridge/ParserAction.agda
src/agda/Algebra/Bridge/ParserAction.agdai: src/agda/Algebra/Bridge/ParserAction.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Algebra/Bridge/ParserAction.agdai' 'build/recipes/src/agda/Algebra/Bridge/ParserAction.agdai.sh'
# Compile src/agda/Chapter1/Level1.agda
src/agda/Chapter1/Level1.agdai: src/agda/Chapter1/Level1.agda src/agda/PropertyRegistry.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1.agdai' 'build/recipes/src/agda/Chapter1/Level1.agdai.sh'
# Compile src/agda/Chapter1/Level1sub8.agda
src/agda/Chapter1/Level1sub8.agdai: src/agda/Chapter1/Level1sub8.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub8.agdai' 'build/recipes/src/agda/Chapter1/Level1sub8.agdai.sh'
# Compile src/agda/Chapter1/Level1sub7.agda
src/agda/Chapter1/Level1sub7.agdai: src/agda/Chapter1/Level1sub7.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub7.agdai' 'build/recipes/src/agda/Chapter1/Level1sub7.agdai.sh'
# Compile src/agda/Chapter1/Level1sub3.agda
src/agda/Chapter1/Level1sub3.agdai: src/agda/Chapter1/Level1sub3.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub3.agdai' 'build/recipes/src/agda/Chapter1/Level1sub3.agdai.sh'
# Compile src/agda/Chapter1/Level1Index.agda
src/agda/Chapter1/Level1Index.agdai: src/agda/Chapter1/Level1Index.agda src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1sub8.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1Index.agdai' 'build/recipes/src/agda/Chapter1/Level1Index.agdai.sh'
# Compile src/agda/Chapter1/Level1sub2.agda
src/agda/Chapter1/Level1sub2.agdai: src/agda/Chapter1/Level1sub2.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub2.agdai' 'build/recipes/src/agda/Chapter1/Level1sub2.agdai.sh'
# Compile src/agda/Chapter1/Level1sub4.agda
src/agda/Chapter1/Level1sub4.agdai: src/agda/Chapter1/Level1sub4.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub4.agdai' 'build/recipes/src/agda/Chapter1/Level1sub4.agdai.sh'
# Compile src/agda/Chapter1/Level1sub5.agda
src/agda/Chapter1/Level1sub5.agdai: src/agda/Chapter1/Level1sub5.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub5.agdai' 'build/recipes/src/agda/Chapter1/Level1sub5.agdai.sh'
# Compile src/agda/Chapter1/Level1sub6.agda
src/agda/Chapter1/Level1sub6.agdai: src/agda/Chapter1/Level1sub6.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Chapter1/Level1sub6.agdai' 'build/recipes/src/agda/Chapter1/Level1sub6.agdai.sh'
# Compile src/agda/PropertyRegistry.agda
src/agda/PropertyRegistry.agdai: src/agda/PropertyRegistry.agda src/agda/Metamodel.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/PropertyRegistry.agdai' 'build/recipes/src/agda/PropertyRegistry.agdai.sh'
# Compile src/agda/ExporterMakefile.agda
src/agda/ExporterMakefile.agdai: src/agda/ExporterMakefile.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/ExporterMakefile.agdai' 'build/recipes/src/agda/ExporterMakefile.agdai.sh'
# Compile src/agda/Core/AlgorithmComplexity.agda
src/agda/Core/AlgorithmComplexity.agdai: src/agda/Core/AlgorithmComplexity.agda src/agda/Metamodel.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AlgorithmComplexity.agdai' 'build/recipes/src/agda/Core/AlgorithmComplexity.agdai.sh'
# Compile src/agda/Core/ABNF.agda
src/agda/Core/ABNF.agdai: src/agda/Core/ABNF.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/ABNF.agdai' 'build/recipes/src/agda/Core/ABNF.agdai.sh'
# Compile src/agda/Core/TechnicalDebt.agda
src/agda/Core/TechnicalDebt.agdai: src/agda/Core/TechnicalDebt.agda src/agda/Core/Utils.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/TechnicalDebt.agdai' 'build/recipes/src/agda/Core/TechnicalDebt.agdai.sh'
# Compile src/agda/Core/Witnesses.agda
src/agda/Core/Witnesses.agdai: src/agda/Core/Witnesses.agda src/agda/Algebra/Fields/Advanced.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Witnesses.agdai' 'build/recipes/src/agda/Core/Witnesses.agdai.sh'
# Compile src/agda/Core/BraidTree.agda
src/agda/Core/BraidTree.agdai: src/agda/Core/BraidTree.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/BraidTree.agdai' 'build/recipes/src/agda/Core/BraidTree.agdai.sh'
# Compile src/agda/Core/Limitations.agda
src/agda/Core/Limitations.agdai: src/agda/Core/Limitations.agda src/agda/Metamodel.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Limitations.agdai' 'build/recipes/src/agda/Core/Limitations.agdai.sh'
# Compile src/agda/Core/AlgorithmCorrectness.agda
src/agda/Core/AlgorithmCorrectness.agdai: src/agda/Core/AlgorithmCorrectness.agda src/agda/Core/ConstructiveWitnesses.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AlgorithmCorrectness.agdai' 'build/recipes/src/agda/Core/AlgorithmCorrectness.agdai.sh'
# Compile src/agda/Core/GrothendieckFibrations.agda
src/agda/Core/GrothendieckFibrations.agdai: src/agda/Core/GrothendieckFibrations.agda src/agda/Algebra/Foundation.agdai src/agda/Chapter2/Level2sub8.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/GrothendieckFibrations.agdai' 'build/recipes/src/agda/Core/GrothendieckFibrations.agdai.sh'
# Compile src/agda/Core/AlgorithmUniversality.agda
src/agda/Core/AlgorithmUniversality.agdai: src/agda/Core/AlgorithmUniversality.agda src/agda/Core/UniversalProperties.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AlgorithmUniversality.agdai' 'build/recipes/src/agda/Core/AlgorithmUniversality.agdai.sh'
# Compile src/agda/Core/AdapterReflection.agda
src/agda/Core/AdapterReflection.agdai: src/agda/Core/AdapterReflection.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AdapterReflection.agdai' 'build/recipes/src/agda/Core/AdapterReflection.agdai.sh'
# Compile src/agda/Core/Phase.agda
src/agda/Core/Phase.agdai: src/agda/Core/Phase.agda src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Coherence/Path2.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Phase.agdai' 'build/recipes/src/agda/Core/Phase.agdai.sh'
# Compile src/agda/Core/AdapterAutomation.agda
src/agda/Core/AdapterAutomation.agdai: src/agda/Core/AdapterAutomation.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AdapterAutomation.agdai' 'build/recipes/src/agda/Core/AdapterAutomation.agdai.sh'
# Compile src/agda/Core/Utils.agda
src/agda/Core/Utils.agdai: src/agda/Core/Utils.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Utils.agdai' 'build/recipes/src/agda/Core/Utils.agdai.sh'
# Compile src/agda/Core/PhaseCategoryWrapper.agda
src/agda/Core/PhaseCategoryWrapper.agdai: src/agda/Core/PhaseCategoryWrapper.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/PhaseCategoryWrapper.agdai' 'build/recipes/src/agda/Core/PhaseCategoryWrapper.agdai.sh'
# Compile src/agda/Core/Algorithms/Registry.agda
src/agda/Core/Algorithms/Registry.agdai: src/agda/Core/Algorithms/Registry.agda src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Algorithms/Adapters/BundleAdapter.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/Registry.agdai' 'build/recipes/src/agda/Core/Algorithms/Registry.agdai.sh'
# Compile src/agda/Core/Algorithms/FunctionFields.agda
src/agda/Core/Algorithms/FunctionFields.agdai: src/agda/Core/Algorithms/FunctionFields.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/FunctionFields.agdai' 'build/recipes/src/agda/Core/Algorithms/FunctionFields.agdai.sh'
# Compile src/agda/Core/Algorithms/External.agda
src/agda/Core/Algorithms/External.agdai: src/agda/Core/Algorithms/External.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/External.agdai' 'build/recipes/src/agda/Core/Algorithms/External.agdai.sh'
# Compile src/agda/Core/Algorithms/FiniteFields.agda
src/agda/Core/Algorithms/FiniteFields.agdai: src/agda/Core/Algorithms/FiniteFields.agda src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/FiniteFields.agdai' 'build/recipes/src/agda/Core/Algorithms/FiniteFields.agdai.sh'
# Compile src/agda/Core/Algorithms/AutomaticEvidence.agda
src/agda/Core/Algorithms/AutomaticEvidence.agdai: src/agda/Core/Algorithms/AutomaticEvidence.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/AutomaticEvidence.agdai' 'build/recipes/src/agda/Core/Algorithms/AutomaticEvidence.agdai.sh'
# Compile src/agda/Core/Algorithms/NumberFields.agda
src/agda/Core/Algorithms/NumberFields.agdai: src/agda/Core/Algorithms/NumberFields.agda src/agda/Core/Algorithms/External.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/NumberFields.agdai' 'build/recipes/src/agda/Core/Algorithms/NumberFields.agdai.sh'
# Compile src/agda/Core/Algorithms/Bundle.agda
src/agda/Core/Algorithms/Bundle.agdai: src/agda/Core/Algorithms/Bundle.agda src/agda/Core/AlgebraicAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/Bundle.agdai' 'build/recipes/src/agda/Core/Algorithms/Bundle.agdai.sh'
# Compile src/agda/Core/Algorithms/InductiveClassification.agda
src/agda/Core/Algorithms/InductiveClassification.agdai: src/agda/Core/Algorithms/InductiveClassification.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Algorithms/InductiveClassification.agdai' 'build/recipes/src/agda/Core/Algorithms/InductiveClassification.agdai.sh'
# Compile src/agda/Core/PhaseCategory.agda
src/agda/Core/PhaseCategory.agdai: src/agda/Core/PhaseCategory.agda src/agda/Core/Phase.agdai src/agda/Infrastructure/Functor/Interface.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/PhaseCategory.agdai' 'build/recipes/src/agda/Core/PhaseCategory.agdai.sh'
# Compile src/agda/Core/Rendering.agda
src/agda/Core/Rendering.agdai: src/agda/Core/Rendering.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Rendering.agdai' 'build/recipes/src/agda/Core/Rendering.agdai.sh'
# Compile src/agda/Core/ConstructiveWitnesses.agda
src/agda/Core/ConstructiveWitnesses.agdai: src/agda/Core/ConstructiveWitnesses.agda src/agda/Core/PolynomialsF2.agdai src/agda/Core/Witnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/ConstructiveWitnesses.agdai' 'build/recipes/src/agda/Core/ConstructiveWitnesses.agdai.sh'
# Compile src/agda/Core/UniversalProperties.agda
src/agda/Core/UniversalProperties.agdai: src/agda/Core/UniversalProperties.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/UniversalProperties.agdai' 'build/recipes/src/agda/Core/UniversalProperties.agdai.sh'
# Compile src/agda/Core/IO.agda
src/agda/Core/IO.agdai: src/agda/Core/IO.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/IO.agdai' 'build/recipes/src/agda/Core/IO.agdai.sh'
# Compile src/agda/Core/GrowthMetrics.agda
src/agda/Core/GrowthMetrics.agdai: src/agda/Core/GrowthMetrics.agda src/agda/Core/Utils.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/GrowthMetrics.agdai' 'build/recipes/src/agda/Core/GrowthMetrics.agdai.sh'
# Compile src/agda/Core/AlgebraicAlgorithms.agda
src/agda/Core/AlgebraicAlgorithms.agdai: src/agda/Core/AlgebraicAlgorithms.agda src/agda/Algebra/Groups/Basic.agdai src/agda/Core/Limitations.agdai src/agda/Algebra/Fields/Advanced.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/AlgebraicAlgorithms.agdai' 'build/recipes/src/agda/Core/AlgebraicAlgorithms.agdai.sh'
# Compile src/agda/Core/PhaseCategory/Duality.agda
src/agda/Core/PhaseCategory/Duality.agdai: src/agda/Core/PhaseCategory/Duality.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/PhaseCategory/Duality.agdai' 'build/recipes/src/agda/Core/PhaseCategory/Duality.agdai.sh'
# Compile src/agda/Core/Yoneda.agda
src/agda/Core/Yoneda.agdai: src/agda/Core/Yoneda.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Yoneda.agdai' 'build/recipes/src/agda/Core/Yoneda.agdai.sh'
# Compile src/agda/Core/GodelBoundary.agda
src/agda/Core/GodelBoundary.agdai: src/agda/Core/GodelBoundary.agda src/agda/Core.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/GodelBoundary.agdai' 'build/recipes/src/agda/Core/GodelBoundary.agdai.sh'
# Compile src/agda/Core/Strings.agda
src/agda/Core/Strings.agdai: src/agda/Core/Strings.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/Strings.agdai' 'build/recipes/src/agda/Core/Strings.agdai.sh'
# Compile src/agda/Core/PathAggregator.agda
src/agda/Core/PathAggregator.agdai: src/agda/Core/PathAggregator.agda src/agda/Core/GrowthMetrics.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/PathAggregator.agdai' 'build/recipes/src/agda/Core/PathAggregator.agdai.sh'
# Compile src/agda/Core/PolynomialsF2.agda
src/agda/Core/PolynomialsF2.agdai: src/agda/Core/PolynomialsF2.agda src/agda/Core/Phase.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/PolynomialsF2.agdai' 'build/recipes/src/agda/Core/PolynomialsF2.agdai.sh'
# Compile src/agda/Core/CategoricalAdapter.agda
src/agda/Core/CategoricalAdapter.agdai: src/agda/Core/CategoricalAdapter.agda src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Coherence/Path2.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/Core/CategoricalAdapter.agdai' 'build/recipes/src/agda/Core/CategoricalAdapter.agdai.sh'
# Compile src/agda/MinimalTest.agda
src/agda/MinimalTest.agdai: src/agda/MinimalTest.agda
	$(call require_mutate)
	scripts/run_profiled.sh 'src/agda/MinimalTest.agdai' 'build/recipes/src/agda/MinimalTest.agdai.sh'
# Compile all Agda modules
agda-all: src/agda/MetaScan.agdai src/agda/Metamodel.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Index_PhaseII.agdai src/agda/Tests/HierarchyValidation.agdai src/agda/Tests/PhaseExamples.agdai src/agda/Tests/CHIPConformanceChecklist.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/ErrorAsSpecificationTests.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/SpecificationValidation.agdai src/agda/Tests/AlgorithmSmokeTests.agdai src/agda/Tests/CoverageReport.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/Examples/CrossDomainCompositionTests.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/GroupsAbelianChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/WarningAggregatorsTest.agdai src/agda/Tests/Index.agdai src/agda/Tests/ObligationAdapters.agdai src/agda/Tests/ToposObligationAdapters.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/Plan/CIM/SchemaValidationGenericTests.agdai src/agda/Tests/Plan/CIM/TypeCheckingGenericTests.agdai src/agda/Tests/Plan/CIM/ABNFParserGenericTests.agdai src/agda/Tests/Plan/CIM/ProofTraceGenericTests.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/Core/PhaseCategory/DualityTests.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Chapter2/Level2sub3.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2Index.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3Index.agdai src/agda/Infrastructure/Arity/BinTree.agdai src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Iso/Structural.agdai src/agda/Infrastructure/Adequacy.agdai src/agda/Infrastructure/Adequacy/Polymorphic.agdai src/agda/Infrastructure/Adequacy/CrossDomain.agdai src/agda/Infrastructure/Functor/Compose.agdai src/agda/Infrastructure/Functor/Interface.agdai src/agda/Infrastructure/Functor/Adapters/Funext.agdai src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai src/agda/Infrastructure/Functor/Instances/Trivial.agdai src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai src/agda/Infrastructure/Coherence/Path2.agdai src/agda/Infrastructure/Equality.agdai src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai src/agda/Infrastructure/Polytopes/Associahedron.agdai src/agda/Infrastructure/Definitions/Dictionary.agdai src/agda/Infrastructure/Product/Bundle4.agdai src/agda/Infrastructure/Index.agdai src/agda/Infrastructure/Axiom/Movie.agdai src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai src/agda/Infrastructure/Axiom/Adequacy.agdai src/agda/Infrastructure/Axiom/Solver.agdai src/agda/Infrastructure/Axiom/Face.agdai src/agda/Infrastructure/Axiom/SolvableInterface.agdai src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai src/agda/Infrastructure/Axiom/Instance.agdai src/agda/Infrastructure/Wrapper/With.agdai src/agda/Algorithms/Instrumented.agdai src/agda/Algorithms/Basic.agdai src/agda/Algorithms/Adapters/BundleAdapter.agdai src/agda/Algorithms/TestInstances.agdai src/agda/MinimalAUDAXInlineTest.agdai src/agda/Core.agdai src/agda/TechnicalDebt/PriorityMapping.agdai src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai src/agda/TechnicalDebt/PriorityFormatting.agdai src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai src/agda/TechnicalDebt/DeferredItemsDetection.agdai src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai src/agda/TechnicalDebt/PriorityOrchestration.agdai src/agda/TechnicalDebt/DeferredItemsFormatting.agdai src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai src/agda/TechnicalDebt/Priorities.agdai src/agda/Examples/RealWorldAlgorithms.agdai src/agda/Examples/TechnicalDebtExample.agdai src/agda/Examples/TransformationPathAdequacy.agdai src/agda/Examples/AmbiguityExamples.agdai src/agda/Examples/FunctionField/F2x.agdai src/agda/Examples/AlgorithmCorrectnessExamples.agdai src/agda/Examples/FunctionPathAdequacy.agdai src/agda/Examples/FunctionCategoryAdequacy.agdai src/agda/Examples/Makefile/Targets/Docs.agdai src/agda/Examples/Makefile/Targets/Infra.agdai src/agda/Examples/Makefile/Targets/Priority.agdai src/agda/Examples/Makefile/Targets/Docker.agdai src/agda/Examples/Makefile/Targets/JsonRoundtrip.agdai src/agda/Examples/Makefile/Targets/RoadmapExports.agdai src/agda/Examples/Makefile/Targets/Python.agdai src/agda/Examples/Makefile/Targets/AgdaBuild.agdai src/agda/Examples/Makefile/Targets/Composite.agdai src/agda/Examples/NumberField/Sqrt2.agdai src/agda/Examples/FiniteField/GF8.agdai src/agda/Examples/InstrumentedAlgorithmDemo.agdai src/agda/Examples/DeferredItemsScanner.agdai src/agda/Examples/DefinitionDictionaryAdequacy.agdai src/agda/Examples/TransformationSystemExamples.agdai src/agda/Examples/AutomaticEvidenceDemo.agdai src/agda/Examples/ConstructiveWitnessExamples.agdai src/agda/Examples/CrossDomainComposition.agdai src/agda/Examples/AgdaMakefileDeps.agdai src/agda/Examples/LazyHybridDemo.agdai src/agda/Examples/TechnicalDebtRegistry.agdai src/agda/Examples/RoadmapIssueSync.agdai src/agda/Examples/MakefileTargets.agdai src/agda/Examples/FunctorComposition.agdai src/agda/Examples/PhaseCategoryExamples.agdai src/agda/Examples/EqualityExamples.agdai src/agda/Examples/TechnicalDebtChecklist.agdai src/agda/Examples/AmbiguityAdequacy.agdai src/agda/Examples/AgdaFileScanFFI.agdai src/agda/Examples/ExporterMakefile.agdai src/agda/Markdown/ExportProof.agdai src/agda/Markdown/Normalization.agdai src/agda/Docs/MetaIndex.agdai src/agda/GrowthAnalysis.agdai src/agda/Plan/CIM/TransformationSystem.agdai src/agda/Plan/CIM/PolytopeExpansion.agdai src/agda/Plan/CIM/PlanningKernel.agdai src/agda/Plan/CIM/JSONTransformationExtraction.agdai src/agda/Plan/CIM/JSONTransformationTesting.agdai src/agda/Plan/CIM/PandocProofExample.agdai src/agda/Plan/CIM/ProofTraceGeneric.agdai src/agda/Plan/CIM/PandocProtocols.agdai src/agda/Plan/CIM/PandocShowBlock.agdai src/agda/Plan/CIM/CHIPCoreRecompose.agdai src/agda/Plan/CIM/PandocProofExport.agdai src/agda/Plan/CIM/Utility.agdai src/agda/Plan/CIM/IngestedRoadmaps.agdai src/agda/Plan/CIM/Elasticity.agdai src/agda/Plan/CIM/Structure.agdai src/agda/Plan/CIM/DocumentSynthesis.agdai src/agda/Plan/CIM/Metricization.agdai src/agda/Plan/CIM/JSONTransformationBackends.agdai src/agda/Plan/CIM/JSONTransformation.agdai src/agda/Plan/CIM/RoadmapExporterMain.agdai src/agda/Plan/CIM/CHIPRecomposed.agdai src/agda/Plan/CIM/PandocShowMdBlock.agdai src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai src/agda/Plan/CIM/PriorityProfileExport.agdai src/agda/Plan/CIM/SchemaValidationGeneric.agdai src/agda/Plan/CIM/PlanningExport.agdai src/agda/Plan/CIM/RoadmapExporter.agdai src/agda/Plan/CIM/CanonicalRoadmap.agdai src/agda/Plan/CIM/JSONTransformationAdequacy.agdai src/agda/Plan/CIM/CHIPConformance.agdai src/agda/Plan/CIM/FrameworkMetadata.agdai src/agda/Plan/CIM/JSONConcrete.agdai src/agda/Plan/CIM/ABNFParserGeneric.agdai src/agda/Plan/CIM/JSONTransformationGeneric.agdai src/agda/Plan/CIM/DependencyGraphExport.agdai src/agda/Plan/CIM/ModuleExporter.agdai src/agda/Plan/CIM/RoadmapIndex.agdai src/agda/Plan/CIM/RoadmapSync.agdai src/agda/Plan/CIM/PandocToMarkdown.agdai src/agda/Plan/CIM/DocumentationContent.agdai src/agda/Plan/CIM/YonedaProfiler.agdai src/agda/Plan/CIM/RoadmapSPPF.agdai src/agda/Plan/CIM/Ambiguity.agdai src/agda/Plan/CIM/PandocShowInline.agdai src/agda/Plan/CIM/TypeCheckingGeneric.agdai src/agda/Plan/CIM/RoadmapSPPFExport.agdai src/agda/Plan/CIM/FunctorialConstructs.agdai src/agda/Plan/CIM/JSONTransformationContract.agdai src/agda/Plan/CIM/GPNarrativeDAG.agdai src/agda/Plan/CIM/MarkdownNormalize.agdai src/agda/Plan/CIM/PandocAST.agdai src/agda/Plan/CIM/GrammarBridge.agdai src/agda/Plan/CIM/MarkdownParse.agdai src/agda/Algebra/Groups/Basic.agdai src/agda/Algebra/Groups/Theorems/Classical.agdai src/agda/Algebra/Groups/ClassicalInstance.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Algebra/Groups/BasicWithTheorems.agdai src/agda/Algebra/Groups/BasicParameterized.agdai src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Groups/Types.agdai src/agda/Algebra/Groups/Free.agdai src/agda/Algebra/Enrichment.agdai src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Rings/Theorems/Classical.agdai src/agda/Algebra/Rings/ClassicalInstance.agdai src/agda/Algebra/Rings/BasicWithTheorems.agdai src/agda/Algebra/Rings/Types.agdai src/agda/Algebra/Modules/Basic.agdai src/agda/Algebra/Modules/Theorems/Classical.agdai src/agda/Algebra/Modules/ClassicalInstance.agdai src/agda/Algebra/Modules/BasicWithTheorems.agdai src/agda/Algebra/Modules/Types.agdai src/agda/Algebra/Foundation.agdai src/agda/Algebra/Fields/Basic.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Algebra/Fields/Theorems/Classical.agdai src/agda/Algebra/Fields/ClassicalInstance.agdai src/agda/Algebra/Fields/BasicWithTheorems.agdai src/agda/Algebra/Fields/Types.agdai src/agda/Algebra/Index.agdai src/agda/Algebra/Bridge/ParserAction.agdai src/agda/Chapter1/Level1.agdai src/agda/Chapter1/Level1sub8.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1Index.agdai src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/PropertyRegistry.agdai src/agda/ExporterMakefile.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/ABNF.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/Witnesses.agdai src/agda/Core/BraidTree.agdai src/agda/Core/Limitations.agdai src/agda/Core/AlgorithmCorrectness.agdai src/agda/Core/GrothendieckFibrations.agdai src/agda/Core/AlgorithmUniversality.agdai src/agda/Core/AdapterReflection.agdai src/agda/Core/Phase.agdai src/agda/Core/AdapterAutomation.agdai src/agda/Core/Utils.agdai src/agda/Core/PhaseCategoryWrapper.agdai src/agda/Core/Algorithms/Registry.agdai src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/External.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Core/Algorithms/AutomaticEvidence.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Algorithms/InductiveClassification.agdai src/agda/Core/PhaseCategory.agdai src/agda/Core/Rendering.agdai src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/IO.agdai src/agda/Core/GrowthMetrics.agdai src/agda/Core/AlgebraicAlgorithms.agdai src/agda/Core/PhaseCategory/Duality.agdai src/agda/Core/Yoneda.agdai src/agda/Core/GodelBoundary.agdai src/agda/Core/Strings.agdai src/agda/Core/PathAggregator.agdai src/agda/Core/PolynomialsF2.agdai src/agda/Core/CategoricalAdapter.agdai src/agda/MinimalTest.agdai
	$(call require_mutate)
	scripts/run_profiled.sh 'agda-all' 'build/recipes/agda-all.sh'
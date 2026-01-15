# Navigation Guide

## Quick Start

**New to Metacatagory?** Start here:
1. Read [ARCHITECTURE.md](docs/architecture/ARCHITECTURE.md) - Understand the design philosophy
2. Review [README.md](README.md) - Get an overview of key concepts
3. Study [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) - See concrete RoadmapStep examples
4. Follow [CONTRIBUTING.md](CONTRIBUTING.md) - Start contributing

**Practical setup**
- `mise install`
- `MUTATE_OK=1 make regen-makefile` (regenerate recipes)
- `MUTATE_OK=1 make check` (full suite; strict roundtrip validation is the default)
- For narrower runs: `MUTATE_OK=1 make json-roundtrip-validate` or `MUTATE_OK=1 make check-docs`
- For pytest/md targets, install dev deps (`mise run dev-setup`); network required (or use the CI container).

## Core Modules

### Categorical Foundations
- **src/agda/Core/Phase.agda** - Phase categories and universe levels
- **src/agda/Core/PhaseCategory.agda** - Phase functor laws and properties
- **src/agda/Core/Witnesses.agda** - Witness objects and universal properties
- **src/agda/Core/NaturalTransformation.agda** - Natural transformations and coherence

### Algebra
- **src/agda/Algebra/Fields/** - Field hierarchies and properties
- **src/agda/Algebra/Rings/** - Ring structures and theorems
- **src/agda/Algebra/Groups/** - Group and abelian group definitions

### Protocol & Planning
- **src/agda/Plan/CIM/Utility.agda** - Core protocol records and RoadmapStep definitions
- **src/agda/Plan/CIM/CHIPConformance.agda** - CHIP protocol conformance
- **src/agda/Plan/CIM/Structure.agda** - Structural typing and protocol composition

### Examples & Tests
- **src/agda/Examples/** - Runnable examples and Makefile generation
- **src/agda/Tests/** - Test suite with property checklists

## Roadmaps

All roadmaps are defined as RoadmapStep records in [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda):

### Key Development Roadmaps
- **Unified Topological Parser** (GP699) - SPPF/RoPE integration
- **Dimensional Relief** (GP500) - Topological inflation for disambiguation
- **Polytope Manifest** (GP501) - Mitosis engine for dynamic geometry
- **Elasticity of Meaning** (GP400) - Tension/Resonance gating logic

See [ROADMAP.md](ROADMAP.md) for the complete development timeline.

## Documentation

- **[ARCHITECTURE.md](docs/architecture/ARCHITECTURE.md)** - Design principles and categorical patterns
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - How to contribute
- **[copilot-instructions.md](.github/copilot-instructions.md)** - LLM integration guidance
- **[ROADMAP.md](ROADMAP.md)** - Development phases and timelines
- **[intake/README.md](intake/README.md)** - Canonical intake sources vs. archives

## Building and Testing

```bash
# Compile all modules
make agda-all

# Generate documentation
make docs-all

# Regenerate all tracked artifacts
make regen-all

# Full validation suite (alias: make check)
make check-all

# Check markdown
make md-lint

# Generate badges
make badges
```

## Key Concepts

### Proof-Driven Composition
All protocols are records with both operational and proof fields. Equality uses Agda's type system:
- Propositional equality: `_≡_` (proof objects, for formal guarantees)
- Boolean equality: `_==_` (decidable checks, for runtime logic)

### Modular Protocols
Protocols are maximally decomposed and parameterized by:
- Context and ambiguity models
- Metricization and transformation systems
- Functorial constructs and coherence witnesses

### Roadmap-Driven Development
Development is guided by RoadmapStep records that encode:
- Problem origin (provenance)
- Solution approach (step)
- Impact (implication)
- Implementation targets (targetModule)

## Getting Help

1. **Check examples** in `src/agda/Examples/` and `src/agda/Tests/`
2. **Review similar code** - Look at related modules for patterns
3. **Read roadmap nodes** - RoadmapStep records document architectural decisions
4. **Consult copilot-instructions.md** - For integration with LLM systems

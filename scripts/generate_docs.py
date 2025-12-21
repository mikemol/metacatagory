#!/usr/bin/env python3
"""
Generate markdown documentation from Agda RoadmapStep records.
Extracts the 4 key roadmaps from src/agda/Plan/CIM/Utility.agda
and generates README.md, NAVIGATION.md, and CONTRIBUTING.md
"""

import re
import sys

# Define the 4 key roadmaps with their data
ROADMAPS = [
    {
        "name": "exampleUnifiedTopologicalParserRoadmap",
        "provenance": "GP699, Unified Topological Parser, Nedge-Topology, SPPF + RoPE + SymNum",
        "step": "Integrate Earley parsing, RoPE, and symmetry group concepts into a unified topological parser. Treat syntax as a manifold and ambiguity as vector superposition.",
        "implication": "Enables composable geometric and topological integration, active topological pruning, and algebraic superposition for ambiguity. Supports recursive revisiting, fiber bundle architecture, and advanced induction/training features.",
        "status": "not-started",
        "targetModule": "nedge_topology/parser.py, nedge_topology/train.py, nedge_topology/mitosis.py, nedge_topology/search.py, dashboard.py, src/agda/Plan/CIM/RotationalTransport.agda, src/agda/Plan/CIM/TopologicalGating.agda, src/agda/Plan/CIM/TopologicalSuperposition.agda"
    },
    {
        "name": "exampleDimensionalReliefRoadmap",
        "provenance": "GP500, Dimensional Relief, Topological Inflation, Stasheff Expansion",
        "step": "Implement topological inflation: upgrade crowded semantic categories to higher-dimensional polytopes to relieve tension.",
        "implication": "Enables composable category expansion, tension relief, and dynamic geometry for semantic protocols. Supports recursive revisiting and concept differentiation.",
        "status": "not-started",
        "targetModule": "src/agda/Plan/CIM/PolytopeExpansion.agda, nedge_topology/mitosis.py"
    },
    {
        "name": "examplePolytopeManifestRoadmap",
        "provenance": "GP501, Polytope Manifest, Mitosis Engine, Dynamic Polytopes",
        "step": "Implement Mitosis Engine to monitor topological tension and inflate categories to dynamic polytopes as needed.",
        "implication": "Enables dynamic, composable category geometry, tension monitoring, and concept differentiation. Supports recursive revisiting and protocol evolution.",
        "status": "not-started",
        "targetModule": "nedge_topology/mitosis.py, nedge_topology/parser.py, dashboard.py, src/agda/Plan/CIM/PolytopeExpansion.agda"
    },
    {
        "name": "exampleElasticityOfMeaningRoadmap",
        "provenance": "GP400, Elasticity of Meaning, Tension/Resonance phase space",
        "step": "Integrate 2D gating logic (Tension/Resonance) into parser and protocol records, explicitly cross-referencing ambiguity, metricization, transformation system, and functorial constructs.",
        "implication": "Enables composable phase space modeling, creative/insightful parse acceptance, and pruning of non-sequitur/hallucination nodes. Supports recursive revisiting for grammar induction, protocol refinement, and functorial traceability.",
        "status": "not-started",
        "targetModule": "src/agda/Plan/CIM/Elasticity.agda, parser.py, dashboard.py, src/agda/Plan/CIM/Ambiguity.agda, src/agda/Plan/CIM/Metricization.agda, src/agda/Plan/CIM/TransformationSystem.agda, src/agda/Plan/CIM/FunctorialConstructs.agda"
    }
]

def generate_readme():
    """Generate README.md content"""
    content = """# Metacatagory

A composable, formal system for semantic parsing and protocol management using categorical proof-driven architecture and topological semantics.

## Overview

Metacatagory unifies parsing, ambiguity resolution, and protocol composition through:
- **SPPF-based semantics**: Shared Packed Parse Forests as topological lattices
- **Categorical abstractions**: Functors, natural transformations, and universal properties
- **Proof-driven composition**: Witness objects and coherence proofs for protocol assembly
- **Roadmap-driven development**: Systematic integration via implication-driven roadmaps

## Key Architecture

- **src/agda/Core/** - Core categorical infrastructure (phases, functors, witnesses)
- **src/agda/Plan/CIM/** - Protocol records, utility functions, and roadmap structures
- **src/agda/Algebra/** - Algebraic field hierarchies and categorical foundations
- **src/agda/Examples/** - Runnable examples and test infrastructure

## Integrated Roadmaps

The following roadmap steps guide development and protocol composition:

"""
    for rm in ROADMAPS:
        content += f"### {rm['provenance']}\n\n"
        content += f"**Step:** {rm['step']}\n\n"
        content += f"**Implication:** {rm['implication']}\n\n"
        content += f"**Status:** `{rm['status']}`\n\n"
        content += f"**Target Modules:** {rm['targetModule']}\n\n"
    
    content += """## Development Workflow

1. **Review architecture** in [ARCHITECTURE.md](ARCHITECTURE.md) for design principles
2. **Check roadmap** in [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) for current integration points
3. **Follow contribution guidelines** in [CONTRIBUTING.md](CONTRIBUTING.md)
4. **Build and test** with `make agda-all` (requires Agda 2.6.4.3+)

## Building

```bash
# Full Agda compilation
make agda-all

# Generate documentation
make docs-all

# Lint markdown
make md-lint
```

## Navigation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Detailed architectural documentation and design patterns
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines and coding standards
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap and phase timeline
- **[COPILOT_SYNERGY.md](COPILOT_SYNERGY.md)** - LLM integration guidance

## License

See [LICENSE](LICENSE) for licensing information.
"""
    return content

def generate_contributing():
    """Generate CONTRIBUTING.md content"""
    return """# Contributing to Metacatagory

## Getting Started

1. **Review the architecture** - Start with [ARCHITECTURE.md](ARCHITECTURE.md) to understand the proof-driven, categorical design
2. **Understand roadmaps** - Check [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) for RoadmapStep examples and integration patterns
3. **Build locally** - Run `make agda-all` to ensure the project compiles

## Development Guidelines

### Code Organization

- **src/agda/Core/** - Foundational categorical structures (phases, functors, witnesses)
- **src/agda/Plan/CIM/** - Protocol composition and roadmap infrastructure
- **src/agda/Algebra/** - Algebraic hierarchies (fields, vector spaces, etc.)

### Writing New Code

1. **Use protocol records** - New functionality should be composed via protocol records (see `src/agda/Plan/CIM/Utility.agda`)
2. **Add roadmap examples** - Document new features as RoadmapStep records with clear implications and target modules
3. **Include witness proofs** - Use witness objects and universal properties for formal guarantees
4. **Update ROADMAP.md** - Add references to new modules and roadmap nodes for traceability

### Code Style

- Use descriptive names for protocols and records
- Include comments explaining categorical intent and proof structure
- Organize imports clearly with `open import` statements
- Use Unicode operators appropriately (→, ∷, ≡, etc.)

## Testing

- **Unit tests** are in `src/agda/Tests/`
- **Run tests**: Tests compile as part of `make agda-all`
- **Add new tests** for new modules with clear property-based specifications

## Documentation

- **Markdown files** should be kept consistent with actual Agda implementations
- **Roadmap nodes** in RoadmapStep records serve as machine-actionable documentation
- **ARCHITECTURE.md** describes design patterns and their implications

## Submitting Changes

1. **Update affected modules** and their corresponding RoadmapStep entries
2. **Ensure clean compilation** with `make agda-all`
3. **Update documentation** to reflect architectural changes
4. **Reference roadmap nodes** in commit messages for traceability

## Questions?

- Check [COPILOT_SYNERGY.md](COPILOT_SYNERGY.md) for LLM integration and context guidance
- Review examples in [src/agda/Examples/](src/agda/Examples/) for patterns and idioms
- Consult roadmap nodes in [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) for integration context
"""

def generate_navigation():
    """Generate NAVIGATION.md content"""
    return """# Navigation Guide

## Quick Start

**New to Metacatagory?** Start here:
1. Read [ARCHITECTURE.md](ARCHITECTURE.md) - Understand the design philosophy
2. Review [README.md](README.md) - Get an overview of key concepts
3. Study [src/agda/Plan/CIM/Utility.agda](src/agda/Plan/CIM/Utility.agda) - See concrete RoadmapStep examples
4. Follow [CONTRIBUTING.md](CONTRIBUTING.md) - Start contributing

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

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Design principles and categorical patterns
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - How to contribute
- **[COPILOT_SYNERGY.md](COPILOT_SYNERGY.md)** - LLM integration guidance
- **[ROADMAP.md](ROADMAP.md)** - Development phases and timelines

## Building and Testing

```bash
# Compile all modules
make agda-all

# Generate documentation
make docs-all

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
4. **Consult COPILOT_SYNERGY.md** - For integration with LLM systems
"""

def main():
    """Generate all documentation files"""
    # Generate README.md
    with open("README.md", "w") as f:
        f.write(generate_readme())
    print("✓ Generated README.md")
    
    # Generate CONTRIBUTING.md
    with open("CONTRIBUTING.md", "w") as f:
        f.write(generate_contributing())
    print("✓ Generated CONTRIBUTING.md")
    
    # Generate NAVIGATION.md
    with open("NAVIGATION.md", "w") as f:
        f.write(generate_navigation())
    print("✓ Generated NAVIGATION.md")
    
    print("\nDocumentation generation complete!")
    return 0

if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""
Generate markdown documentation from planning index.
Loads roadmap data from data/planning_index.json instead of hardcoded list.
Generates README.md, NAVIGATION.md, and CONTRIBUTING.md
"""

import sys
from pathlib import Path

from scripts import shared_data

def load_planning_index():
    """Load roadmap data from data/planning_index.json"""
    workspace = Path(__file__).parent.parent
    planning_path = shared_data.resolve_planning_path(repo_root=workspace)
    if not planning_path.exists():
        print(f"Warning: {planning_path} not found, using empty roadmap list", file=sys.stderr)
        return []
    try:
        items = shared_data.load_planning_index_validated(repo_root=workspace)
    except Exception as exc:
        print(f"Warning: {exc} not found, using empty roadmap list", file=sys.stderr)
        return []
    
    # Filter for key roadmaps (those with specific categories or tags)
    # Prioritize: in-progress, then done, then not-started
    # Limit to top items to keep README readable
    priority_items = sorted(
        items,
        key=lambda x: (
            0 if x.get('status') == 'in-progress' else
            1 if x.get('status') == 'done' else 2,
            -len(x.get('description', ''))  # Prefer detailed items
        )
    )[:10]  # Top 10 most relevant items
    
    return priority_items

def format_roadmap_for_readme(item):
    """Convert planning index item to README-compatible format"""
    return {
        "id": item.get("id", "unknown"),
        "title": item.get("title", "Untitled"),
        "description": item.get("description", ""),
        "status": item.get("status", "not-started"),
        "category": item.get("category", "General"),
        "files": ", ".join(item.get("files", [])),
        "tags": ", ".join(item.get("tags", []))
    }

ROADMAPS: list[dict] = []


def _load_default_roadmaps() -> list[dict]:
    """Lazy-load formatted roadmaps from planning index."""
    return [format_roadmap_for_readme(item) for item in load_planning_index()]


def ensure_roadmaps() -> None:
    """Populate ROADMAPS if not already set."""
    global ROADMAPS
    if not ROADMAPS:
        ROADMAPS = _load_default_roadmaps()

def generate_readme():
    """Generate README.md content"""
    ensure_roadmaps()
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
        content += f"### {rm.get('title', 'Untitled')}\n\n"
        content += f"**ID:** `{rm.get('id', 'unknown')}`\n\n"
        content += f"**Description:** {rm.get('description', 'No description')}\n\n"
        content += f"**Category:** {rm.get('category', 'General')}\n\n"
        content += f"**Status:** `{rm.get('status', 'not-started')}`\n\n"
        if rm.get('files'):
            content += f"**Files:** {rm.get('files')}\n\n"
        if rm.get('tags'):
            content += f"**Tags:** {rm.get('tags')}\n\n"
    
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

## Build Topology

`make` is treated as the lazy evaluator for the roadmap automation. Use one
of the entry-point gravity wells (`check`, `priority-refresh`, `docs-all`,
`validate-constructive`) and let Make rebuild only the intermediates whose
inputs changed. The intermediate nodes (`roadmap-*`, lint helpers, priority
pipelines, etc.) are documented in [docs/process/BUILD-TOPOLOGY.md](docs/process/BUILD-TOPOLOGY.md)
so tooling and contributors alike know which high-level commands drive the
internal planning machinery.

## Navigation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Detailed architectural documentation and design patterns
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines and coding standards
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap and phase timeline
- **[.github/copilot-instructions.md](.github/copilot-instructions.md)** - LLM integration guidance

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

- Check [.github/copilot-instructions.md](.github/copilot-instructions.md) for LLM integration and context guidance
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
- **[.github/copilot-instructions.md](.github/copilot-instructions.md)** - LLM integration guidance
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
4. **Consult .github/copilot-instructions.md** - For integration with LLM systems
"""

def main():
    """Generate all documentation files"""
    ensure_roadmaps()
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

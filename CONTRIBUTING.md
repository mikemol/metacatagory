# Contributing to Metacatagory

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

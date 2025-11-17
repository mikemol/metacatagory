# Tooling & Automation - Quick Start Guide

This guide shows you how to use the metacatagory project's automation tools for test coverage analysis, phase diagram generation, and code search.

## Setup

### First Time Setup

Create and populate the Python virtual environment:

```bash
make venv
```

This will:
- Create a `venv/` directory
- Install dependencies: networkx, graphviz, rich, pyyaml
- Activate automatically when using `make` targets

### Manual Setup (Optional)

If you prefer to manage the venv manually:

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

## Available Tools

### 1. Test Coverage Report

Generate comprehensive test coverage statistics from the Agda test suite:

```bash
make report
```

**Output:**
- `build/reports/test-report.json` - Machine-readable JSON format
- `build/reports/test-report.md` - Human-readable Markdown report

**What it analyzes:**
- Adapter declarations per chapter
- Status assertions (filled vs. unfilled)
- Link counts per adapter
- Section coverage breakdown

**Example output:**
```
Total status assertions: 13

Adapter counts by type:
- AdjunctionHomAdapter: 1
- KernelPairAdapter: 1
- StrongMonoAdapter: 1
...
```

### 2. Phase Boundary Diagram

Generate visual diagrams showing exercised phase boundaries:

```bash
make diagram
```

**Output:**
- `build/diagrams/phases.dot` - Graphviz DOT format

**What it shows:**
- Test module structure
- Record type relationships
- Adapter type connections

**Rendering to image formats:**

If you have the `graphviz` binary installed:

```bash
# Render to SVG
dot -Tsvg build/diagrams/phases.dot -o build/diagrams/phases.svg

# Render to PNG
dot -Tpng build/diagrams/phases.dot -o build/diagrams/phases.png
```

Install graphviz on Ubuntu/Debian:
```bash
sudo apt-get install graphviz
```

### 3. Algorithm/Property Search

Search the Agda codebase for declarations matching keywords:

```bash
make search QUERY="kernel"
make search QUERY="adjunction"
make search QUERY="monad"
```

**What it searches:**
- Record declarations
- Constructor names
- Postulate declarations
- Module names

**Example output:**
```
src/agda/Chapter2/Level2sub2.agda:74: record KernelPairDeclaration : Set where
src/agda/Chapter2/Level2sub2.agda:75: constructor KernelPair_of
src/agda/Chapter2/Level2sub2.agda:108: record KernelPairIsEquivalenceRelationTheorem : Set where
...
```

### 4. Test All Tools

Verify that all automation scripts work correctly:

```bash
make test-tools
```

This runs a quick smoke test of all three tools with temporary output.

## Direct Script Usage

For more control, you can run the scripts directly:

```bash
# Activate venv first
source venv/bin/activate

# Test report with custom options
python scripts/test_report.py --tests-dir src/agda/Tests --out-dir custom-reports

# Phase diagram generation
python scripts/phase_diagram.py --out-dir custom-diagrams

# Search with custom path
python scripts/search_algo.py --q "regular epi" --path src/agda/Chapter2
```

## Output Directory Structure

After running the tools, your `build/` directory will contain:

```
build/
├── diagrams/
│   └── phases.dot          # Phase boundary graph
└── reports/
    ├── test-report.json    # Coverage data (JSON)
    └── test-report.md      # Coverage report (Markdown)
```

## Quick Demo

Run the included demo script to see all tools in action:

```bash
./demo.sh
```

This will:
1. Set up the virtual environment
2. Generate test coverage report
3. Generate phase diagram
4. Run example searches
5. Display generated artifacts

## Dependencies

The tools require:

- **Python 3.8+**
- **networkx** - Graph algorithms for phase analysis
- **graphviz** (Python package) - DOT file generation
- **rich** - Pretty terminal output
- **pyyaml** - Configuration/data parsing

Optional for rendering diagrams:
- **graphviz** (system binary) - For rendering DOT to SVG/PNG

## Troubleshooting

### "Virtual environment not found"

Run `make venv` to create it.

### "Command not found: dot"

The `dot` command is part of the graphviz system package. Install it:
```bash
sudo apt-get install graphviz  # Ubuntu/Debian
brew install graphviz           # macOS
```

### Permission denied on demo.sh

Make it executable:
```bash
chmod +x demo.sh
```

## Integration with Agda Development

The tooling integrates with the standard Agda workflow:

```bash
# 1. Develop/modify Agda code
vim src/agda/Tests/Chapter1Checklist.agda

# 2. Typecheck
make check

# 3. Generate coverage report
make report

# 4. Review what changed
cat build/reports/test-report.md

# 5. Search for related declarations
make search QUERY="your-concept"
```

## Next Steps

- Add more adapters to increase coverage
- Extend phase diagrams with witness links
- Integrate search into proof mining workflows
- Add CI/CD automation for report generation

See the main README.md for complete project documentation.

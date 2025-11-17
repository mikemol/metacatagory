#!/bin/bash
# Demo script showcasing the metacatagory tooling and automation

set -e

echo "================================================"
echo "Metacatagory Tooling & Automation Demo"
echo "================================================"
echo ""

echo "1. Setting up Python virtual environment..."
make venv
echo ""

echo "2. Generating test coverage report..."
make report
echo ""

echo "3. Viewing coverage summary (first 30 lines)..."
head -30 build/reports/test-report.md
echo ""

echo "4. Generating phase boundary diagram..."
make diagram
echo ""

echo "5. Searching for 'kernel' declarations..."
make search QUERY="kernel" | head -15
echo ""

echo "6. Searching for 'monad' declarations..."
make search QUERY="monad" | head -10
echo ""

echo "7. Listing generated artifacts..."
echo ""
echo "Reports:"
ls -lh build/reports/
echo ""
echo "Diagrams:"
ls -lh build/diagrams/
echo ""

echo "================================================"
echo "Demo complete!"
echo "================================================"
echo ""
echo "Try these commands yourself:"
echo "  make report       # Generate test coverage"
echo "  make diagram      # Generate phase diagrams"
echo "  make search QUERY=\"your-term\""
echo "  make help         # See all targets"
echo ""

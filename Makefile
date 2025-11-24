# Makefile for MetaCategory (Agda)
#

# --- Configuration ---
AGDA ?= agda
AGDA_FLAGS ?= --no-main
AGDA_INCLUDE = -i src/agda

VENV = venv
PYTHON = $(VENV)/bin/python3
PIP = $(VENV)/bin/pip

BUILD_DIR = build
HTML_DIR = $(BUILD_DIR)/html
MD_DIR = $(BUILD_DIR)/md
REPORTS_DIR = $(BUILD_DIR)/reports
DIAGRAMS_DIR = $(BUILD_DIR)/diagrams

AGDA_INDEX_1 = src/agda/Chapter1/Level1Index.agda
AGDA_INDEX_2 = src/agda/Chapter2/Level2Index.agda
AGDA_INDEX_3 = src/agda/Chapter3/Level3Index.agda
AGDA_INDEX_ALGEBRA = src/agda/Algebra/Index.agda
AGDA_INDEXES = $(AGDA_INDEX_1) $(AGDA_INDEX_2) $(AGDA_INDEX_3) $(AGDA_INDEX_ALGEBRA)

AGDA_TESTS = $(filter-out $(AGDA_COVERAGE_METADATA),$(wildcard src/agda/Tests/*.agda))
AGDA_COVERAGE_METADATA = src/agda/Tests/CoverageReport.agda

.PHONY: all check check-tests check-coverage docs docs-md docs1 docs2 docs3 docs-algebra clean help venv report diagram search test-tools automation

all: check check-tests

check:
	@echo "Typechecking Agda chapters..."
	@for file in $(AGDA_INDEXES); do \
		echo "Checking $$file..."; \
		$(AGDA) $(AGDA_FLAGS) $(AGDA_INCLUDE) $$file || exit 1; \
	done
	@echo "Typecheck complete."

check-tests:
	@echo "Typechecking Agda test modules..."
	@for file in $(AGDA_TESTS); do \
		echo "Checking $$file..."; \
		$(AGDA) $(AGDA_FLAGS) $(AGDA_INCLUDE) $$file || exit 1; \
	done
	@echo "Test module typecheck complete."

check-coverage:
	@echo "Validating test coverage metadata..."
	@$(AGDA) $(AGDA_FLAGS) $(AGDA_INCLUDE) $(AGDA_COVERAGE_METADATA)
	@echo "✓ Coverage metadata validated by Agda type system"

docs: $(HTML_DIR)/combined.agda.txt | $(HTML_DIR)
	@echo "Generating HTML docs for all chapters into $(HTML_DIR)..."
	@for file in $(AGDA_INDEXES); do \
		echo "Generating HTML for $$file..."; \
		$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $$file || exit 1; \
	done
	@echo "HTML docs generated in $(HTML_DIR). Open index.html for entry points."

$(HTML_DIR)/combined.agda.txt: scripts/combine_agda.py src/agda/Examples/TechnicalDebtRegistry.agda | $(HTML_DIR)
	python3 scripts/combine_agda.py $@ src/agda/Examples/TechnicalDebtRegistry.agda

$(HTML_DIR):
	@mkdir -p $(HTML_DIR)

$(MD_DIR):
	@mkdir -p $(MD_DIR)

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@echo "Removing Agda interface files (*.agdai)..."
	@find src/agda -name '*.agdai' -delete 2>/dev/null || true

venv:
	@echo "Setting up Python virtual environment..."
	@test -d $(VENV) || python3 -m venv $(VENV)
	@$(PIP) install --upgrade pip
	@$(PIP) install -r requirements.txt
	@echo "Virtual environment ready at $(VENV)/"

report: venv check-coverage | $(REPORTS_DIR)
	@echo "Generating test coverage report..."
	@$(PYTHON) scripts/test_report.py --out-dir $(REPORTS_DIR)
	@echo "Report available at $(REPORTS_DIR)/test-report.{json,md}"

diagram: venv | $(DIAGRAMS_DIR)
	@echo "Generating phase diagram..."
	@$(PYTHON) scripts/phase_diagram.py --out-dir $(DIAGRAMS_DIR)
	@echo "Diagram available at $(DIAGRAMS_DIR)/phases.dot"

search: venv
	@if [ -z "$(QUERY)" ]; then \
		echo "Usage: make search QUERY=\"keyword\""; \
		echo "Example: make search QUERY=\"kernel\""; \
		exit 1; \
	fi
	@$(PYTHON) scripts/search_algo.py --q "$(QUERY)"

test-tools: venv
	@echo "Testing tooling scripts..."
	@$(PYTHON) scripts/test_report.py --out-dir /tmp/metacatagory-test
	@$(PYTHON) scripts/phase_diagram.py --out-dir /tmp/metacatagory-test
	@$(PYTHON) scripts/search_algo.py --q "test"
	@echo "All tools tested successfully."

automation: report diagram $(HTML_DIR)/combined.agda.txt
	@echo "All automation scripts and reports generated."

$(REPORTS_DIR):
	@mkdir -p $(REPORTS_DIR)

$(DIAGRAMS_DIR):
	@mkdir -p $(DIAGRAMS_DIR)

help:
	@echo "Available targets:"
	@echo ""
	@echo "  Agda Development:"
	@echo "    all / check  - Typecheck all Agda chapter indexes and algebra"
	@echo "    docs         - Generate HTML docs for all chapters and algebra"
	@echo "    docs1|docs2|docs3 - Generate HTML docs per chapter"
	@echo "    docs-algebra - Generate HTML docs for algebra modules"
	@echo "    docs-md      - Generate Markdown docs (requires pandoc)"
	@echo "    clean        - Remove build artifacts under $(BUILD_DIR)"
	@echo ""
	@echo "  Tooling & Automation:"
	@echo "    venv         - Set up Python virtual environment"
	@echo "    report       - Generate test coverage report (JSON + Markdown)"
	@echo "    diagram      - Generate phase boundary diagram (DOT format)"
	@echo "    search       - Search for algorithms by keyword (use QUERY=\"...\")"
	@echo "    test-tools   - Test all automation scripts"
	@echo "    automation   - Run all automation scripts and generate reports"
	@echo ""
	@echo "Variables: AGDA, AGDA_FLAGS, QUERY"

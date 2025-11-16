# Makefile for MetaCategory (Agda)
#
# This Makefile now focuses on the Agda sources. The previous EBNF
# concatenation pipeline has been retired.

# --- Configuration ---

AGDA ?= agda
# Add any extra flags here (e.g., --safe). We default to --no-main for libraries.
AGDA_FLAGS ?= --no-main

# Root include for Agda modules
AGDA_INCLUDE = -i src/agda

# Chapter index modules (act as entry points for typechecking and docs)
AGDA_INDEX_1 = src/agda/Chapter1/Level1Index.agda
AGDA_INDEX_2 = src/agda/Chapter2/Level2Index.agda
AGDA_INDEX_3 = src/agda/Chapter3/Level3Index.agda
AGDA_INDEX_ALGEBRA = src/agda/Algebra/Index.agda
AGDA_INDEXES = $(AGDA_INDEX_1) $(AGDA_INDEX_2) $(AGDA_INDEX_3) $(AGDA_INDEX_ALGEBRA)

# Build artifacts
BUILD_DIR = build
HTML_DIR = $(BUILD_DIR)/html
MD_DIR = $(BUILD_DIR)/md

# --- Targets ---

.PHONY: all check docs docs-md docs1 docs2 docs3 docs-algebra clean help

# Default: typecheck everything
all: check

# Typecheck all chapter indexes
check:
	@echo "Typechecking Agda chapters..."
	@for file in $(AGDA_INDEXES); do \
		echo "Checking $$file..."; \
		$(AGDA) $(AGDA_FLAGS) $(AGDA_INCLUDE) $$file || exit 1; \
	done
	@echo "Typecheck complete."

# Generate HTML documentation for all chapters
docs: | $(HTML_DIR)
	@echo "Generating HTML docs for all chapters into $(HTML_DIR)..."
	@$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $(AGDA_INDEXES)
	@echo "HTML docs generated in $(HTML_DIR). Open index.html for entry points."

# Generate Markdown documentation by converting the HTML output via pandoc
docs-md: docs | $(MD_DIR)
	@echo "Converting HTML docs to Markdown into $(MD_DIR)..."
	@if ! command -v pandoc >/dev/null 2>&1; then \
	   echo "Error: pandoc not found. Install pandoc to build Markdown docs." 1>&2; \
	   exit 1; \
	 fi
	@find $(HTML_DIR) -name '*.html' -type f | while read -r f; do \
	   rel=$${f#$(HTML_DIR)/}; \
	   out='$(MD_DIR)/'$${rel%.html}.md; \
	   mkdir -p "$$("dirname" "$$out")"; \
	   pandoc "$$f" -f html -t gfm -o "$$out"; \
	 done
	@echo "Markdown docs generated in $(MD_DIR)."

# Per-chapter docs (useful during authoring)
docs1: | $(HTML_DIR)
	@echo "Generating HTML docs for Chapter 1 into $(HTML_DIR)..."
	@$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $(AGDA_INDEX_1)

docs2: | $(HTML_DIR)
	@echo "Generating HTML docs for Chapter 2 into $(HTML_DIR)..."
	@$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $(AGDA_INDEX_2)

docs3: | $(HTML_DIR)
	@echo "Generating HTML docs for Chapter 3 into $(HTML_DIR)..."
	@$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $(AGDA_INDEX_3)

docs-algebra: | $(HTML_DIR)
	@echo "Generating HTML docs for Algebra into $(HTML_DIR)..."
	@$(AGDA) --html --html-dir=$(HTML_DIR) $(AGDA_INCLUDE) $(AGDA_INDEX_ALGEBRA)

# Ensure html output directory exists
$(HTML_DIR):
	@mkdir -p $(HTML_DIR)

$(MD_DIR):
	@mkdir -p $(MD_DIR)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@echo "Removing Agda interface files (*.agdai)..."
	@find src/agda -name '*.agdai' -delete 2>/dev/null || true

# Show available targets
help:
	@echo "Available targets:"
	@echo "  all / check  - Typecheck all Agda chapter indexes and algebra"
	@echo "  docs         - Generate HTML docs for all chapters and algebra"
	@echo "  docs1|docs2|docs3 - Generate HTML docs per chapter"
	@echo "  docs-algebra - Generate HTML docs for algebra modules"
	@echo "  docs-md      - Generate Markdown docs (requires pandoc)"
	@echo "  clean        - Remove build artifacts under $(BUILD_DIR)"
	@echo "Variables: AGDA, AGDA_FLAGS"

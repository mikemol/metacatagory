# Makefile for MetaCategory (Agda)
#
# This Makefile now focuses on the Agda sources. The previous EBNF
# concatenation pipeline has been retired.

# --- Configuration ---

AGDA ?= agda
# Add any extra flags here (e.g., --safe). We default to --no-main for libraries.
AGDA_FLAGS ?= --no-main

# Root include for Agda modules
AGDA_INCLUDE = -i src/ebnf

# Chapter index modules (act as entry points for typechecking and docs)
AGDA_INDEX_1 = src/ebnf/Chapter1/Level1Index.agda
AGDA_INDEX_2 = src/ebnf/Chapter2/Level2Index.agda
AGDA_INDEX_3 = src/ebnf/Chapter3/Level3Index.agda
AGDA_INDEXES = $(AGDA_INDEX_1) $(AGDA_INDEX_2) $(AGDA_INDEX_3)

# Build artifacts
BUILD_DIR = build
HTML_DIR = $(BUILD_DIR)/html

# --- Targets ---

.PHONY: all check docs docs1 docs2 docs3 clean help

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

# Ensure html output directory exists
$(HTML_DIR):
	@mkdir -p $(HTML_DIR)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)

# Show available targets
help:
	@echo "Available targets:"
	@echo "  all / check  - Typecheck all Agda chapter indexes"
	@echo "  docs         - Generate HTML docs for all chapters"
	@echo "  docs1|docs2|docs3 - Generate HTML docs per chapter"
	@echo "  clean        - Remove build artifacts under $(BUILD_DIR)"
	@echo "Variables: AGDA, AGDA_FLAGS"

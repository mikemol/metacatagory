# Use local Agda 2.8.0 if available, otherwise system agda
AGDA := $(if $(wildcard .local/agda),.local/agda,agda)
.PHONY: all check md-fix md-lint intake-lint intake-scan md-normalize makefile-validate badges node-deps regen-makefile agda-all docs-all deferred-items roadmap-index roadmap-sync roadmap-sppf roadmap-merge roadmap-deps-graph roadmap-enrich roadmap-export-json roadmap-export-md roadmap-export-enriched roadmap-export-deps roadmap-validate-json roadmap-validate-md roadmap-validate-triangle roadmap-sppf-export roadmap-all-enriched docs-generate docs-validate docs-modules validate-constructive
# Regenerate the Makefile from Agda source (Self-Hosting)
regen-makefile: 
	$(AGDA) -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile
	cp Makefile.generated Makefile
# Lint all markdown files (fail on error)
md-lint: 
	npx markdownlint-cli2 "**/*.md" "!node_modules" "!build" > build/reports/md-lint.txt 2>&1
# Auto-fix markdown lint errors
md-fix: 
	npx markdownlint-cli2 --fix "**/*.md" "!node_modules" "!build"
# Lint intake files specifically
intake-lint: 
	npx markdownlint-cli2 "intake/**/*.md" > build/reports/intake-md-lint.txt 2>&1
# Generate canonical roadmap JSON from intake
build/canonical_roadmap.json: 
	python3 scripts/intake_scan.py
# Scan intake directory for new files
intake-scan: build/canonical_roadmap.json
	@echo "intake scan complete"
# Normalize markdown formatting
md-normalize: 
	python3 scripts/normalize_generated_markdown.py
# Validate Makefile consistency
makefile-validate: 
	mkdir -p build/reports
	python3 scripts/validate_makefile_docs.py > build/reports/makefile-validate.txt
# Build all code and documentation
all: agda-all docs-all
	@echo "all complete"
# Run all validation checks
check: roadmap-validate-triangle docs-validate makefile-validate
	@echo "check complete"
# Generate status badges
badges: 
	python3 scripts/generate-badges.py
# Generate per-module markdown documentation
docs-modules: src/agda/Plan/CIM/ModuleExporter.agdai
	$(AGDA) -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/ModuleExporter.agda && ./src/agda/Plan/CIM/ModuleExporter
# Generate documentation (markdown only)
docs-all: docs-generate docs-modules
	@echo "docs (markdown) complete"
# Install Node.js dependencies
node-deps: 
	npm install
# Scan for TODOs and FIXMEs
deferred-items: 
	.github/scripts/detect-deferred-items.sh
# Compile Roadmap Index
roadmap-index: src/agda/Plan/CIM/RoadmapIndex.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapIndex.agda
# Sync roadmap with external tracker
roadmap-sync: roadmap-export-json src/agda/Plan/CIM/RoadmapSync.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSync.agda
# Compile Roadmap SPPF
roadmap-sppf: src/agda/Plan/CIM/RoadmapSPPF.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSPPF.agda
# Run all constructive build targets
validate-constructive: docs-all docs-generate docs-modules roadmap-export-json roadmap-export-md roadmap-export-enriched roadmap-export-deps roadmap-deps-graph roadmap-enrich roadmap-all-enriched intake-scan md-normalize badges
	@echo "✓ Constructive validation complete"
# Merge ingestion streams
roadmap-merge: 
	python3 scripts/merge_roadmaps.py
# Generate dependency graph
build/diagrams/agda-deps-full.dot: 
	mkdir -p build/diagrams
	$(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot -i src/agda src/agda/Tests/Index.agda 2>&1 | grep -E "(Checking|Error)" | head -20
# Generate dependency graph
roadmap-deps-graph: build/diagrams/agda-deps-full.dot
	@echo "agda dependency graph generated"
# Enrich canonical roadmap
build/canonical_enriched.json: build/canonical_roadmap.json build/diagrams/agda-deps-full.dot
	python3 scripts/enrich_canonical.py
# Enrich roadmap with graph data
roadmap-enrich: build/canonical_enriched.json
	@echo "roadmap enrichment complete"
# Export canonical roadmap to JSON
roadmap-export-json: build/canonical_roadmap.json
	python3 scripts/export_canonical_json.py
# Export canonical roadmap to Markdown
roadmap-export-md: build/canonical_roadmap.json
	python3 scripts/export_canonical_md.py
# Export enriched roadmap
roadmap-export-enriched: build/canonical_enriched.json
	python3 scripts/export_enriched_md.py
# Export roadmap dependency graph
roadmap-export-deps: build/canonical_enriched.json
	python3 scripts/export_dependency_graph.py
# Validate canonical JSON
roadmap-validate-json: build/canonical_roadmap.json .github/roadmap/tasks.json
	python3 scripts/validate_json.py
# Validate canonical Markdown
roadmap-validate-md: build/canonical_roadmap.json ROADMAP.md
	python3 scripts/validate_md.py
# Verify Triangle Identity (Agda <-> JSON <-> MD)
roadmap-validate-triangle: roadmap-validate-json roadmap-validate-md
	@echo "✓ Triangle validation complete"
# Export SPPF structure
roadmap-sppf-export: build/canonical_roadmap.json
	python3 scripts/export_roadmap_sppf.py
# Build all enriched artifacts
roadmap-all-enriched: roadmap-export-enriched roadmap-export-deps
	@echo "roadmap all enriched complete"
# Compile and run Roadmap Exporter
docs-generate: src/agda/Plan/CIM/RoadmapExporter.agdai
	$(AGDA) -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapExporter.agda && ./src/agda/RoadmapExporter
	python3 scripts/normalize_generated_markdown.py
# Validate documentation integrity
docs-validate: 
	python3 scripts/validate_triangle_identity.py
# Compile src/agda/MetaScan.agda
src/agda/MetaScan.agdai: src/agda/MetaScan.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/MetaScan.agda
# Compile src/agda/Metamodel.agda
src/agda/Metamodel.agdai: src/agda/Metamodel.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Metamodel.agda
# Compile src/agda/Tests/PropertyRegistryTests.agda
src/agda/Tests/PropertyRegistryTests.agdai: src/agda/Tests/PropertyRegistryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PropertyRegistryTests.agda
# Compile src/agda/Tests/AbelianCategoriesChecklist.agda
src/agda/Tests/AbelianCategoriesChecklist.agdai: src/agda/Tests/AbelianCategoriesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AbelianCategoriesChecklist.agda
# Compile src/agda/Tests/DispatchBehaviorTests.agda
src/agda/Tests/DispatchBehaviorTests.agdai: src/agda/Tests/DispatchBehaviorTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/DispatchBehaviorTests.agda
# Compile src/agda/Tests/WitnessConstructionTests.agda
src/agda/Tests/WitnessConstructionTests.agdai: src/agda/Tests/WitnessConstructionTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/WitnessConstructionTests.agda
# Compile src/agda/Tests/VectorSpaceChecklist.agda
src/agda/Tests/VectorSpaceChecklist.agdai: src/agda/Tests/VectorSpaceChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/VectorSpaceChecklist.agda
# Compile src/agda/Tests/ProofObligationStatus.agda
src/agda/Tests/ProofObligationStatus.agdai: src/agda/Tests/ProofObligationStatus.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ProofObligationStatus.agda
# Compile src/agda/Tests/SerializationTests.agda
src/agda/Tests/SerializationTests.agdai: src/agda/Tests/SerializationTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SerializationTests.agda
# Compile src/agda/Tests/GodelBoundaryTests.agda
src/agda/Tests/GodelBoundaryTests.agdai: src/agda/Tests/GodelBoundaryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GodelBoundaryTests.agda
# Compile src/agda/Tests/ModuleStructureChecklist.agda
src/agda/Tests/ModuleStructureChecklist.agdai: src/agda/Tests/ModuleStructureChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModuleStructureChecklist.agda
# Compile src/agda/Tests/ToposTheoryChecklist.agda
src/agda/Tests/ToposTheoryChecklist.agdai: src/agda/Tests/ToposTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ToposTheoryChecklist.agda
# Compile src/agda/Tests/GrothendieckFibrationsChecklist.agda
src/agda/Tests/GrothendieckFibrationsChecklist.agdai: src/agda/Tests/GrothendieckFibrationsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GrothendieckFibrationsChecklist.agda
# Compile src/agda/Tests/TensorProductChecklist.agda
src/agda/Tests/TensorProductChecklist.agdai: src/agda/Tests/TensorProductChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/TensorProductChecklist.agda
# Compile src/agda/Tests/Index_PhaseII.agda
src/agda/Tests/Index_PhaseII.agdai: src/agda/Tests/Index_PhaseII.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Index_PhaseII.agda
# Compile src/agda/Tests/HierarchyValidation.agda
src/agda/Tests/HierarchyValidation.agdai: src/agda/Tests/HierarchyValidation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/HierarchyValidation.agda
# Compile src/agda/Tests/PhaseExamples.agda
src/agda/Tests/PhaseExamples.agdai: src/agda/Tests/PhaseExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PhaseExamples.agda
# Compile src/agda/Tests/RegularCategoriesChecklist.agda
src/agda/Tests/RegularCategoriesChecklist.agdai: src/agda/Tests/RegularCategoriesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RegularCategoriesChecklist.agda
# Compile src/agda/Tests/MonadAdjunctionChecklist.agda
src/agda/Tests/MonadAdjunctionChecklist.agdai: src/agda/Tests/MonadAdjunctionChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/MonadAdjunctionChecklist.agda
# Compile src/agda/Tests/UniversalPropertyTests.agda
src/agda/Tests/UniversalPropertyTests.agdai: src/agda/Tests/UniversalPropertyTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/UniversalPropertyTests.agda
# Compile src/agda/Tests/EnrichmentChecklist.agda
src/agda/Tests/EnrichmentChecklist.agdai: src/agda/Tests/EnrichmentChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/EnrichmentChecklist.agda
# Compile src/agda/Tests/AdvancedPhaseExamples.agda
src/agda/Tests/AdvancedPhaseExamples.agdai: src/agda/Tests/AdvancedPhaseExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedPhaseExamples.agda
# Compile src/agda/Tests/ErrorAsSpecificationTests.agda
src/agda/Tests/ErrorAsSpecificationTests.agdai: src/agda/Tests/ErrorAsSpecificationTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ErrorAsSpecificationTests.agda
# Compile src/agda/Tests/RealWorldAlgorithmsTests.agda
src/agda/Tests/RealWorldAlgorithmsTests.agdai: src/agda/Tests/RealWorldAlgorithmsTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RealWorldAlgorithmsTests.agda
# Compile src/agda/Tests/PolynomialExtensionsChecklist.agda
src/agda/Tests/PolynomialExtensionsChecklist.agdai: src/agda/Tests/PolynomialExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PolynomialExtensionsChecklist.agda
# Compile src/agda/Tests/ModuleTheoryChecklist.agda
src/agda/Tests/ModuleTheoryChecklist.agdai: src/agda/Tests/ModuleTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModuleTheoryChecklist.agda
# Compile src/agda/Tests/PhaseCategoryExamplesRunner.agda
src/agda/Tests/PhaseCategoryExamplesRunner.agdai: src/agda/Tests/PhaseCategoryExamplesRunner.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PhaseCategoryExamplesRunner.agda
# Compile src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai: src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
# Compile src/agda/Tests/AlgorithmCompositionTests.agda
src/agda/Tests/AlgorithmCompositionTests.agdai: src/agda/Tests/AlgorithmCompositionTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgorithmCompositionTests.agda
# Compile src/agda/Tests/FieldsBasicChecklist.agda
src/agda/Tests/FieldsBasicChecklist.agdai: src/agda/Tests/FieldsBasicChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/FieldsBasicChecklist.agda
# Compile src/agda/Tests/SpecificationValidation.agda
src/agda/Tests/SpecificationValidation.agdai: src/agda/Tests/SpecificationValidation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SpecificationValidation.agda
# Compile src/agda/Tests/AlgorithmSmokeTests.agda
src/agda/Tests/AlgorithmSmokeTests.agdai: src/agda/Tests/AlgorithmSmokeTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgorithmSmokeTests.agda
# Compile src/agda/Tests/CoverageReport.agda
src/agda/Tests/CoverageReport.agdai: src/agda/Tests/CoverageReport.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/CoverageReport.agda
# Compile src/agda/Tests/GroupsFreeChecklist.agda
src/agda/Tests/GroupsFreeChecklist.agdai: src/agda/Tests/GroupsFreeChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsFreeChecklist.agda
# Compile src/agda/Tests/RingsBasicChecklist.agda
src/agda/Tests/RingsBasicChecklist.agdai: src/agda/Tests/RingsBasicChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RingsBasicChecklist.agda
# Compile src/agda/Tests/Chapter2Checklist.agda
src/agda/Tests/Chapter2Checklist.agdai: src/agda/Tests/Chapter2Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter2Checklist.agda
# Compile src/agda/Tests/KanExtensionsChecklist.agda
src/agda/Tests/KanExtensionsChecklist.agdai: src/agda/Tests/KanExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/KanExtensionsChecklist.agda
# Compile src/agda/Tests/LimitsColimitsChecklist.agda
src/agda/Tests/LimitsColimitsChecklist.agdai: src/agda/Tests/LimitsColimitsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/LimitsColimitsChecklist.agda
# Compile src/agda/Tests/AdvancedMonadTheoryChecklist.agda
src/agda/Tests/AdvancedMonadTheoryChecklist.agdai: src/agda/Tests/AdvancedMonadTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedMonadTheoryChecklist.agda
# Compile src/agda/Tests/CoreUniversalPropertiesChecklist.agda
src/agda/Tests/CoreUniversalPropertiesChecklist.agdai: src/agda/Tests/CoreUniversalPropertiesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/CoreUniversalPropertiesChecklist.agda
# Compile src/agda/Tests/ChapterObligationsSmoke.agda
src/agda/Tests/ChapterObligationsSmoke.agdai: src/agda/Tests/ChapterObligationsSmoke.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ChapterObligationsSmoke.agda
# Compile src/agda/Tests/ConstructiveWitnessTests.agda
src/agda/Tests/ConstructiveWitnessTests.agdai: src/agda/Tests/ConstructiveWitnessTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ConstructiveWitnessTests.agda
# Compile src/agda/Tests/PerformanceBoundaryTests.agda
src/agda/Tests/PerformanceBoundaryTests.agdai: src/agda/Tests/PerformanceBoundaryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PerformanceBoundaryTests.agda
# Compile src/agda/Tests/PathAggregatorTests.agda
src/agda/Tests/PathAggregatorTests.agdai: src/agda/Tests/PathAggregatorTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PathAggregatorTests.agda
# Compile src/agda/Tests/GroupsAbelianChecklist.agda
src/agda/Tests/GroupsAbelianChecklist.agdai: src/agda/Tests/GroupsAbelianChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsAbelianChecklist.agda
# Compile src/agda/Tests/Chapter1Checklist.agda
src/agda/Tests/Chapter1Checklist.agdai: src/agda/Tests/Chapter1Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter1Checklist.agda
# Compile src/agda/Tests/AdvancedFieldsChecklist.agda
src/agda/Tests/AdvancedFieldsChecklist.agdai: src/agda/Tests/AdvancedFieldsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedFieldsChecklist.agda
# Compile src/agda/Tests/WarningAggregatorsTest.agda
src/agda/Tests/WarningAggregatorsTest.agdai: src/agda/Tests/WarningAggregatorsTest.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/WarningAggregatorsTest.agda
# Compile src/agda/Tests/Index.agda
src/agda/Tests/Index.agdai: src/agda/Tests/Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Index.agda
# Compile src/agda/Tests/ObligationAdapters.agda
src/agda/Tests/ObligationAdapters.agdai: src/agda/Tests/ObligationAdapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ObligationAdapters.agda
# Compile src/agda/Tests/ToposObligationAdapters.agda
src/agda/Tests/ToposObligationAdapters.agdai: src/agda/Tests/ToposObligationAdapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ToposObligationAdapters.agda
# Compile src/agda/Tests/Chapters.agda
src/agda/Tests/Chapters.agdai: src/agda/Tests/Chapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapters.agda
# Compile src/agda/Tests/YonedaChecklist.agda
src/agda/Tests/YonedaChecklist.agdai: src/agda/Tests/YonedaChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/YonedaChecklist.agda
# Compile src/agda/Tests/AlgebraicCompletionChecklist.agda
src/agda/Tests/AlgebraicCompletionChecklist.agdai: src/agda/Tests/AlgebraicCompletionChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgebraicCompletionChecklist.agda
# Compile src/agda/Tests/FunctorPropertiesChecklist.agda
src/agda/Tests/FunctorPropertiesChecklist.agdai: src/agda/Tests/FunctorPropertiesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/FunctorPropertiesChecklist.agda
# Compile src/agda/Tests/AlgebraChecklist.agda
src/agda/Tests/AlgebraChecklist.agdai: src/agda/Tests/AlgebraChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgebraChecklist.agda
# Compile src/agda/Tests/ErrorHandlingTests.agda
src/agda/Tests/ErrorHandlingTests.agdai: src/agda/Tests/ErrorHandlingTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ErrorHandlingTests.agda
# Compile src/agda/Tests/GroupsStructureChecklist.agda
src/agda/Tests/GroupsStructureChecklist.agdai: src/agda/Tests/GroupsStructureChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsStructureChecklist.agda
# Compile src/agda/Tests/ModulesChecklist.agda
src/agda/Tests/ModulesChecklist.agdai: src/agda/Tests/ModulesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModulesChecklist.agda
# Compile src/agda/Tests/Chapter3Checklist.agda
src/agda/Tests/Chapter3Checklist.agdai: src/agda/Tests/Chapter3Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter3Checklist.agda
# Compile src/agda/Tests/SubobjectTheoryChecklist.agda
src/agda/Tests/SubobjectTheoryChecklist.agdai: src/agda/Tests/SubobjectTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SubobjectTheoryChecklist.agda
# Compile src/agda/Chapter2/Level2sub3.agda
src/agda/Chapter2/Level2sub3.agdai: src/agda/Chapter2/Level2sub3.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub3.agda
# Compile src/agda/Chapter2/Level2sub6.agda
src/agda/Chapter2/Level2sub6.agdai: src/agda/Chapter2/Level2sub6.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub6.agda
# Compile src/agda/Chapter2/Level2sub5.agda
src/agda/Chapter2/Level2sub5.agdai: src/agda/Chapter2/Level2sub5.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub5.agda
# Compile src/agda/Chapter2/Level2sub8.agda
src/agda/Chapter2/Level2sub8.agdai: src/agda/Chapter2/Level2sub8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub8.agda
# Compile src/agda/Chapter2/Level2sub7.agda
src/agda/Chapter2/Level2sub7.agdai: src/agda/Chapter2/Level2sub7.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub7.agda
# Compile src/agda/Chapter2/Level2sub2.agda
src/agda/Chapter2/Level2sub2.agdai: src/agda/Chapter2/Level2sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub2.agda
# Compile src/agda/Chapter2/Level2sub1.agda
src/agda/Chapter2/Level2sub1.agdai: src/agda/Chapter2/Level2sub1.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub1.agda
# Compile src/agda/Chapter2/Level2sub4.agda
src/agda/Chapter2/Level2sub4.agdai: src/agda/Chapter2/Level2sub4.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub4.agda
# Compile src/agda/Chapter2/Level2Index.agda
src/agda/Chapter2/Level2Index.agdai: src/agda/Chapter2/Level2Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2Index.agda
# Compile src/agda/Chapter3/Level3sub1.agda
src/agda/Chapter3/Level3sub1.agdai: src/agda/Chapter3/Level3sub1.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3sub1.agda
# Compile src/agda/Chapter3/Level3sub2.agda
src/agda/Chapter3/Level3sub2.agdai: src/agda/Chapter3/Level3sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3sub2.agda
# Compile src/agda/Chapter3/Level3Index.agda
src/agda/Chapter3/Level3Index.agdai: src/agda/Chapter3/Level3Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3Index.agda
# Compile src/agda/Core.agda
src/agda/Core.agdai: src/agda/Core.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core.agda
# Compile src/agda/Examples/RealWorldAlgorithms.agda
src/agda/Examples/RealWorldAlgorithms.agdai: src/agda/Examples/RealWorldAlgorithms.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/RealWorldAlgorithms.agda
# Compile src/agda/Examples/TechnicalDebtExample.agda
src/agda/Examples/TechnicalDebtExample.agdai: src/agda/Examples/TechnicalDebtExample.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtExample.agda
# Compile src/agda/Examples/FunctionField/F2x.agda
src/agda/Examples/FunctionField/F2x.agdai: src/agda/Examples/FunctionField/F2x.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/FunctionField/F2x.agda
# Compile src/agda/Examples/AlgorithmCorrectnessExamples.agda
src/agda/Examples/AlgorithmCorrectnessExamples.agdai: src/agda/Examples/AlgorithmCorrectnessExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AlgorithmCorrectnessExamples.agda
# Compile src/agda/Examples/NumberField/Sqrt2.agda
src/agda/Examples/NumberField/Sqrt2.agdai: src/agda/Examples/NumberField/Sqrt2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/NumberField/Sqrt2.agda
# Compile src/agda/Examples/FiniteField/GF8.agda
src/agda/Examples/FiniteField/GF8.agdai: src/agda/Examples/FiniteField/GF8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/FiniteField/GF8.agda
# Compile src/agda/Examples/DeferredItemsScanner.agda
src/agda/Examples/DeferredItemsScanner.agdai: src/agda/Examples/DeferredItemsScanner.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/DeferredItemsScanner.agda
# Compile src/agda/Examples/AutomaticEvidenceDemo.agda
src/agda/Examples/AutomaticEvidenceDemo.agdai: src/agda/Examples/AutomaticEvidenceDemo.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AutomaticEvidenceDemo.agda
# Compile src/agda/Examples/ConstructiveWitnessExamples.agda
src/agda/Examples/ConstructiveWitnessExamples.agdai: src/agda/Examples/ConstructiveWitnessExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/ConstructiveWitnessExamples.agda
# Compile src/agda/Examples/AgdaMakefileDeps.agda
src/agda/Examples/AgdaMakefileDeps.agdai: src/agda/Examples/AgdaMakefileDeps.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AgdaMakefileDeps.agda
# Compile src/agda/Examples/LazyHybridDemo.agda
src/agda/Examples/LazyHybridDemo.agdai: src/agda/Examples/LazyHybridDemo.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/LazyHybridDemo.agda
# Compile src/agda/Examples/TechnicalDebtRegistry.agda
src/agda/Examples/TechnicalDebtRegistry.agdai: src/agda/Examples/TechnicalDebtRegistry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtRegistry.agda
# Compile src/agda/Examples/RoadmapIssueSync.agda
src/agda/Examples/RoadmapIssueSync.agdai: src/agda/Examples/RoadmapIssueSync.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/RoadmapIssueSync.agda
# Compile src/agda/Examples/MakefileTargets.agda
src/agda/Examples/MakefileTargets.agdai: src/agda/Examples/MakefileTargets.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/MakefileTargets.agda
# Compile src/agda/Examples/PhaseCategoryExamples.agda
src/agda/Examples/PhaseCategoryExamples.agdai: src/agda/Examples/PhaseCategoryExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/PhaseCategoryExamples.agda
# Compile src/agda/Examples/TechnicalDebtChecklist.agda
src/agda/Examples/TechnicalDebtChecklist.agdai: src/agda/Examples/TechnicalDebtChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtChecklist.agda
# Compile src/agda/Examples/AgdaFileScanFFI.agda
src/agda/Examples/AgdaFileScanFFI.agdai: src/agda/Examples/AgdaFileScanFFI.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AgdaFileScanFFI.agda
# Compile src/agda/Examples/ExporterMakefile.agda
src/agda/Examples/ExporterMakefile.agdai: src/agda/Examples/ExporterMakefile.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda
# Compile src/agda/Markdown/ExportProof.agda
src/agda/Markdown/ExportProof.agdai: src/agda/Markdown/ExportProof.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Markdown/ExportProof.agda
# Compile src/agda/Markdown/Normalization.agda
src/agda/Markdown/Normalization.agdai: src/agda/Markdown/Normalization.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Markdown/Normalization.agda
# Compile src/agda/Plan/CIM/PolytopeExpansion.agda
src/agda/Plan/CIM/PolytopeExpansion.agdai: src/agda/Plan/CIM/PolytopeExpansion.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PolytopeExpansion.agda
# Compile src/agda/Plan/CIM/PandocProofExample.agda
src/agda/Plan/CIM/PandocProofExample.agdai: src/agda/Plan/CIM/PandocProofExample.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProofExample.agda
# Compile src/agda/Plan/CIM/PandocProtocols.agda
src/agda/Plan/CIM/PandocProtocols.agdai: src/agda/Plan/CIM/PandocProtocols.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProtocols.agda
# Compile src/agda/Plan/CIM/CHIPCoreRecompose.agda
src/agda/Plan/CIM/CHIPCoreRecompose.agdai: src/agda/Plan/CIM/CHIPCoreRecompose.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPCoreRecompose.agda
# Compile src/agda/Plan/CIM/PandocProofExport.agda
src/agda/Plan/CIM/PandocProofExport.agdai: src/agda/Plan/CIM/PandocProofExport.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProofExport.agda
# Compile src/agda/Plan/CIM/Utility.agda
src/agda/Plan/CIM/Utility.agdai: src/agda/Plan/CIM/Utility.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/Utility.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps.agda
src/agda/Plan/CIM/IngestedRoadmaps.agdai: src/agda/Plan/CIM/IngestedRoadmaps.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps.agda
# Compile src/agda/Plan/CIM/Structure.agda
src/agda/Plan/CIM/Structure.agdai: src/agda/Plan/CIM/Structure.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/Structure.agda
# Compile src/agda/Plan/CIM/DocumentSynthesis.agda
src/agda/Plan/CIM/DocumentSynthesis.agdai: src/agda/Plan/CIM/DocumentSynthesis.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/DocumentSynthesis.agda
# Compile src/agda/Plan/CIM/CHIPRecomposed.agda
src/agda/Plan/CIM/CHIPRecomposed.agdai: src/agda/Plan/CIM/CHIPRecomposed.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPRecomposed.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
# Compile src/agda/Plan/CIM/RoadmapExporter.agda
src/agda/Plan/CIM/RoadmapExporter.agdai: src/agda/Plan/CIM/RoadmapExporter.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapExporter.agda
# Compile src/agda/Plan/CIM/CanonicalRoadmap.agda
src/agda/Plan/CIM/CanonicalRoadmap.agdai: src/agda/Plan/CIM/CanonicalRoadmap.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CanonicalRoadmap.agda
# Compile src/agda/Plan/CIM/CHIPConformance.agda
src/agda/Plan/CIM/CHIPConformance.agdai: src/agda/Plan/CIM/CHIPConformance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPConformance.agda
# Compile src/agda/Plan/CIM/FrameworkMetadata.agda
src/agda/Plan/CIM/FrameworkMetadata.agdai: src/agda/Plan/CIM/FrameworkMetadata.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/FrameworkMetadata.agda
# Compile src/agda/Plan/CIM/ModuleExporter.agda
src/agda/Plan/CIM/ModuleExporter.agdai: src/agda/Plan/CIM/ModuleExporter.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/ModuleExporter.agda
# Compile src/agda/Plan/CIM/RoadmapIndex.agda
src/agda/Plan/CIM/RoadmapIndex.agdai: src/agda/Plan/CIM/RoadmapIndex.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapIndex.agda
# Compile src/agda/Plan/CIM/RoadmapSync.agda
src/agda/Plan/CIM/RoadmapSync.agdai: src/agda/Plan/CIM/RoadmapSync.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSync.agda
# Compile src/agda/Plan/CIM/PandocToMarkdown.agda
src/agda/Plan/CIM/PandocToMarkdown.agdai: src/agda/Plan/CIM/PandocToMarkdown.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocToMarkdown.agda
# Compile src/agda/Plan/CIM/DocumentationContent.agda
src/agda/Plan/CIM/DocumentationContent.agdai: src/agda/Plan/CIM/DocumentationContent.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/DocumentationContent.agda
# Compile src/agda/Plan/CIM/YonedaProfiler.agda
src/agda/Plan/CIM/YonedaProfiler.agdai: src/agda/Plan/CIM/YonedaProfiler.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/YonedaProfiler.agda
# Compile src/agda/Plan/CIM/RoadmapSPPF.agda
src/agda/Plan/CIM/RoadmapSPPF.agdai: src/agda/Plan/CIM/RoadmapSPPF.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSPPF.agda
# Compile src/agda/Plan/CIM/Ambiguity.agda
src/agda/Plan/CIM/Ambiguity.agdai: src/agda/Plan/CIM/Ambiguity.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/Ambiguity.agda
# Compile src/agda/Plan/CIM/RoadmapSPPFExport.agda
src/agda/Plan/CIM/RoadmapSPPFExport.agdai: src/agda/Plan/CIM/RoadmapSPPFExport.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/RoadmapSPPFExport.agda
# Compile src/agda/Plan/CIM/GPNarrativeDAG.agda
src/agda/Plan/CIM/GPNarrativeDAG.agdai: src/agda/Plan/CIM/GPNarrativeDAG.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/GPNarrativeDAG.agda
# Compile src/agda/Plan/CIM/MarkdownNormalize.agda
src/agda/Plan/CIM/MarkdownNormalize.agdai: src/agda/Plan/CIM/MarkdownNormalize.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/MarkdownNormalize.agda
# Compile src/agda/Plan/CIM/PandocAST.agda
src/agda/Plan/CIM/PandocAST.agdai: src/agda/Plan/CIM/PandocAST.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocAST.agda
# Compile src/agda/Plan/CIM/GrammarBridge.agda
src/agda/Plan/CIM/GrammarBridge.agdai: src/agda/Plan/CIM/GrammarBridge.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/GrammarBridge.agda
# Compile src/agda/Plan/CIM/MarkdownParse.agda
src/agda/Plan/CIM/MarkdownParse.agdai: src/agda/Plan/CIM/MarkdownParse.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/MarkdownParse.agda
# Compile src/agda/Algebra/Groups/Basic.agda
src/agda/Algebra/Groups/Basic.agdai: src/agda/Algebra/Groups/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Basic.agda
# Compile src/agda/Algebra/Groups/Theorems/Classical.agda
src/agda/Algebra/Groups/Theorems/Classical.agdai: src/agda/Algebra/Groups/Theorems/Classical.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Theorems/Classical.agda
# Compile src/agda/Algebra/Groups/ClassicalInstance.agda
src/agda/Algebra/Groups/ClassicalInstance.agdai: src/agda/Algebra/Groups/ClassicalInstance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/ClassicalInstance.agda
# Compile src/agda/Algebra/Groups/Structure.agda
src/agda/Algebra/Groups/Structure.agdai: src/agda/Algebra/Groups/Structure.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Structure.agda
# Compile src/agda/Algebra/Groups/BasicWithTheorems.agda
src/agda/Algebra/Groups/BasicWithTheorems.agdai: src/agda/Algebra/Groups/BasicWithTheorems.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/BasicWithTheorems.agda
# Compile src/agda/Algebra/Groups/BasicParameterized.agda
src/agda/Algebra/Groups/BasicParameterized.agdai: src/agda/Algebra/Groups/BasicParameterized.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/BasicParameterized.agda
# Compile src/agda/Algebra/Groups/Abelian.agda
src/agda/Algebra/Groups/Abelian.agdai: src/agda/Algebra/Groups/Abelian.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Abelian.agda
# Compile src/agda/Algebra/Groups/Types.agda
src/agda/Algebra/Groups/Types.agdai: src/agda/Algebra/Groups/Types.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Types.agda
# Compile src/agda/Algebra/Groups/Free.agda
src/agda/Algebra/Groups/Free.agdai: src/agda/Algebra/Groups/Free.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Free.agda
# Compile src/agda/Algebra/Enrichment.agda
src/agda/Algebra/Enrichment.agdai: src/agda/Algebra/Enrichment.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Enrichment.agda
# Compile src/agda/Algebra/Rings/Basic.agda
src/agda/Algebra/Rings/Basic.agdai: src/agda/Algebra/Rings/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/Basic.agda
# Compile src/agda/Algebra/Rings/Theorems/Classical.agda
src/agda/Algebra/Rings/Theorems/Classical.agdai: src/agda/Algebra/Rings/Theorems/Classical.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/Theorems/Classical.agda
# Compile src/agda/Algebra/Rings/ClassicalInstance.agda
src/agda/Algebra/Rings/ClassicalInstance.agdai: src/agda/Algebra/Rings/ClassicalInstance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/ClassicalInstance.agda
# Compile src/agda/Algebra/Rings/BasicWithTheorems.agda
src/agda/Algebra/Rings/BasicWithTheorems.agdai: src/agda/Algebra/Rings/BasicWithTheorems.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/BasicWithTheorems.agda
# Compile src/agda/Algebra/Rings/Types.agda
src/agda/Algebra/Rings/Types.agdai: src/agda/Algebra/Rings/Types.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/Types.agda
# Compile src/agda/Algebra/Modules/Basic.agda
src/agda/Algebra/Modules/Basic.agdai: src/agda/Algebra/Modules/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/Basic.agda
# Compile src/agda/Algebra/Modules/Theorems/Classical.agda
src/agda/Algebra/Modules/Theorems/Classical.agdai: src/agda/Algebra/Modules/Theorems/Classical.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/Theorems/Classical.agda
# Compile src/agda/Algebra/Modules/ClassicalInstance.agda
src/agda/Algebra/Modules/ClassicalInstance.agdai: src/agda/Algebra/Modules/ClassicalInstance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/ClassicalInstance.agda
# Compile src/agda/Algebra/Modules/BasicWithTheorems.agda
src/agda/Algebra/Modules/BasicWithTheorems.agdai: src/agda/Algebra/Modules/BasicWithTheorems.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/BasicWithTheorems.agda
# Compile src/agda/Algebra/Modules/Types.agda
src/agda/Algebra/Modules/Types.agdai: src/agda/Algebra/Modules/Types.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/Types.agda
# Compile src/agda/Algebra/Foundation.agda
src/agda/Algebra/Foundation.agdai: src/agda/Algebra/Foundation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Foundation.agda
# Compile src/agda/Algebra/Fields/Basic.agda
src/agda/Algebra/Fields/Basic.agdai: src/agda/Algebra/Fields/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Basic.agda
# Compile src/agda/Algebra/Fields/Advanced.agda
src/agda/Algebra/Fields/Advanced.agdai: src/agda/Algebra/Fields/Advanced.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Advanced.agda
# Compile src/agda/Algebra/Fields/Theorems/Classical.agda
src/agda/Algebra/Fields/Theorems/Classical.agdai: src/agda/Algebra/Fields/Theorems/Classical.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Theorems/Classical.agda
# Compile src/agda/Algebra/Fields/ClassicalInstance.agda
src/agda/Algebra/Fields/ClassicalInstance.agdai: src/agda/Algebra/Fields/ClassicalInstance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/ClassicalInstance.agda
# Compile src/agda/Algebra/Fields/BasicWithTheorems.agda
src/agda/Algebra/Fields/BasicWithTheorems.agdai: src/agda/Algebra/Fields/BasicWithTheorems.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/BasicWithTheorems.agda
# Compile src/agda/Algebra/Fields/Types.agda
src/agda/Algebra/Fields/Types.agdai: src/agda/Algebra/Fields/Types.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Types.agda
# Compile src/agda/Algebra/Index.agda
src/agda/Algebra/Index.agdai: src/agda/Algebra/Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Index.agda
# Compile src/agda/Chapter1/Level1.agda
src/agda/Chapter1/Level1.agdai: src/agda/Chapter1/Level1.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1.agda
# Compile src/agda/Chapter1/Level1sub8.agda
src/agda/Chapter1/Level1sub8.agdai: src/agda/Chapter1/Level1sub8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub8.agda
# Compile src/agda/Chapter1/Level1sub7.agda
src/agda/Chapter1/Level1sub7.agdai: src/agda/Chapter1/Level1sub7.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub7.agda
# Compile src/agda/Chapter1/Level1sub3.agda
src/agda/Chapter1/Level1sub3.agdai: src/agda/Chapter1/Level1sub3.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub3.agda
# Compile src/agda/Chapter1/Level1Index.agda
src/agda/Chapter1/Level1Index.agdai: src/agda/Chapter1/Level1Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1Index.agda
# Compile src/agda/Chapter1/Level1sub2.agda
src/agda/Chapter1/Level1sub2.agdai: src/agda/Chapter1/Level1sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub2.agda
# Compile src/agda/Chapter1/Level1sub4.agda
src/agda/Chapter1/Level1sub4.agdai: src/agda/Chapter1/Level1sub4.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub4.agda
# Compile src/agda/Chapter1/Level1sub5.agda
src/agda/Chapter1/Level1sub5.agdai: src/agda/Chapter1/Level1sub5.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub5.agda
# Compile src/agda/Chapter1/Level1sub6.agda
src/agda/Chapter1/Level1sub6.agdai: src/agda/Chapter1/Level1sub6.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub6.agda
# Compile src/agda/PropertyRegistry.agda
src/agda/PropertyRegistry.agdai: src/agda/PropertyRegistry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/PropertyRegistry.agda
# Compile src/agda/ExporterMakefile.agda
src/agda/ExporterMakefile.agdai: src/agda/ExporterMakefile.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/ExporterMakefile.agda
# Compile src/agda/Core/AlgorithmComplexity.agda
src/agda/Core/AlgorithmComplexity.agdai: src/agda/Core/AlgorithmComplexity.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmComplexity.agda
# Compile src/agda/Core/ABNF.agda
src/agda/Core/ABNF.agdai: src/agda/Core/ABNF.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/ABNF.agda
# Compile src/agda/Core/TechnicalDebt.agda
src/agda/Core/TechnicalDebt.agdai: src/agda/Core/TechnicalDebt.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/TechnicalDebt.agda
# Compile src/agda/Core/Witnesses.agda
src/agda/Core/Witnesses.agdai: src/agda/Core/Witnesses.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Witnesses.agda
# Compile src/agda/Core/BraidTree.agda
src/agda/Core/BraidTree.agdai: src/agda/Core/BraidTree.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/BraidTree.agda
# Compile src/agda/Core/Limitations.agda
src/agda/Core/Limitations.agdai: src/agda/Core/Limitations.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Limitations.agda
# Compile src/agda/Core/AlgorithmCorrectness.agda
src/agda/Core/AlgorithmCorrectness.agdai: src/agda/Core/AlgorithmCorrectness.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmCorrectness.agda
# Compile src/agda/Core/GrothendieckFibrations.agda
src/agda/Core/GrothendieckFibrations.agdai: src/agda/Core/GrothendieckFibrations.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GrothendieckFibrations.agda
# Compile src/agda/Core/AlgorithmUniversality.agda
src/agda/Core/AlgorithmUniversality.agdai: src/agda/Core/AlgorithmUniversality.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmUniversality.agda
# Compile src/agda/Core/AdapterReflection.agda
src/agda/Core/AdapterReflection.agdai: src/agda/Core/AdapterReflection.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AdapterReflection.agda
# Compile src/agda/Core/Phase.agda
src/agda/Core/Phase.agdai: src/agda/Core/Phase.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Phase.agda
# Compile src/agda/Core/AdapterAutomation.agda
src/agda/Core/AdapterAutomation.agdai: src/agda/Core/AdapterAutomation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AdapterAutomation.agda
# Compile src/agda/Core/Utils.agda
src/agda/Core/Utils.agdai: src/agda/Core/Utils.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Utils.agda
# Compile src/agda/Core/Algorithms/Registry.agda
src/agda/Core/Algorithms/Registry.agdai: src/agda/Core/Algorithms/Registry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/Registry.agda
# Compile src/agda/Core/Algorithms/FunctionFields.agda
src/agda/Core/Algorithms/FunctionFields.agdai: src/agda/Core/Algorithms/FunctionFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/FunctionFields.agda
# Compile src/agda/Core/Algorithms/External.agda
src/agda/Core/Algorithms/External.agdai: src/agda/Core/Algorithms/External.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/External.agda
# Compile src/agda/Core/Algorithms/FiniteFields.agda
src/agda/Core/Algorithms/FiniteFields.agdai: src/agda/Core/Algorithms/FiniteFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/FiniteFields.agda
# Compile src/agda/Core/Algorithms/AutomaticEvidence.agda
src/agda/Core/Algorithms/AutomaticEvidence.agdai: src/agda/Core/Algorithms/AutomaticEvidence.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/AutomaticEvidence.agda
# Compile src/agda/Core/Algorithms/NumberFields.agda
src/agda/Core/Algorithms/NumberFields.agdai: src/agda/Core/Algorithms/NumberFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/NumberFields.agda
# Compile src/agda/Core/Algorithms/Bundle.agda
src/agda/Core/Algorithms/Bundle.agdai: src/agda/Core/Algorithms/Bundle.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/Bundle.agda
# Compile src/agda/Core/Algorithms/InductiveClassification.agda
src/agda/Core/Algorithms/InductiveClassification.agdai: src/agda/Core/Algorithms/InductiveClassification.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/InductiveClassification.agda
# Compile src/agda/Core/PhaseCategory.agda
src/agda/Core/PhaseCategory.agdai: src/agda/Core/PhaseCategory.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PhaseCategory.agda
# Compile src/agda/Core/Rendering.agda
src/agda/Core/Rendering.agdai: src/agda/Core/Rendering.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Rendering.agda
# Compile src/agda/Core/ConstructiveWitnesses.agda
src/agda/Core/ConstructiveWitnesses.agdai: src/agda/Core/ConstructiveWitnesses.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/ConstructiveWitnesses.agda
# Compile src/agda/Core/UniversalProperties.agda
src/agda/Core/UniversalProperties.agdai: src/agda/Core/UniversalProperties.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/UniversalProperties.agda
# Compile src/agda/Core/IO.agda
src/agda/Core/IO.agdai: src/agda/Core/IO.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/IO.agda
# Compile src/agda/Core/GrowthMetrics.agda
src/agda/Core/GrowthMetrics.agdai: src/agda/Core/GrowthMetrics.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GrowthMetrics.agda
# Compile src/agda/Core/AlgebraicAlgorithms.agda
src/agda/Core/AlgebraicAlgorithms.agdai: src/agda/Core/AlgebraicAlgorithms.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgebraicAlgorithms.agda
# Compile src/agda/Core/Yoneda.agda
src/agda/Core/Yoneda.agdai: src/agda/Core/Yoneda.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Yoneda.agda
# Compile src/agda/Core/GodelBoundary.agda
src/agda/Core/GodelBoundary.agdai: src/agda/Core/GodelBoundary.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GodelBoundary.agda
# Compile src/agda/Core/Strings.agda
src/agda/Core/Strings.agdai: src/agda/Core/Strings.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Strings.agda
# Compile src/agda/Core/PathAggregator.agda
src/agda/Core/PathAggregator.agdai: src/agda/Core/PathAggregator.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PathAggregator.agda
# Compile src/agda/Core/PolynomialsF2.agda
src/agda/Core/PolynomialsF2.agdai: src/agda/Core/PolynomialsF2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PolynomialsF2.agda
# Compile src/agda/Core/CategoricalAdapter.agda
src/agda/Core/CategoricalAdapter.agdai: src/agda/Core/CategoricalAdapter.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/CategoricalAdapter.agda
# Compile all Agda modules
agda-all: src/agda/MetaScan.agdai src/agda/Metamodel.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Index_PhaseII.agdai src/agda/Tests/HierarchyValidation.agdai src/agda/Tests/PhaseExamples.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/ErrorAsSpecificationTests.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/SpecificationValidation.agdai src/agda/Tests/AlgorithmSmokeTests.agdai src/agda/Tests/CoverageReport.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/GroupsAbelianChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/WarningAggregatorsTest.agdai src/agda/Tests/Index.agdai src/agda/Tests/ObligationAdapters.agdai src/agda/Tests/ToposObligationAdapters.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Chapter2/Level2sub3.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2Index.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3Index.agdai src/agda/Core.agdai src/agda/Examples/RealWorldAlgorithms.agdai src/agda/Examples/TechnicalDebtExample.agdai src/agda/Examples/FunctionField/F2x.agdai src/agda/Examples/AlgorithmCorrectnessExamples.agdai src/agda/Examples/NumberField/Sqrt2.agdai src/agda/Examples/FiniteField/GF8.agdai src/agda/Examples/DeferredItemsScanner.agdai src/agda/Examples/AutomaticEvidenceDemo.agdai src/agda/Examples/ConstructiveWitnessExamples.agdai src/agda/Examples/AgdaMakefileDeps.agdai src/agda/Examples/LazyHybridDemo.agdai src/agda/Examples/TechnicalDebtRegistry.agdai src/agda/Examples/RoadmapIssueSync.agdai src/agda/Examples/MakefileTargets.agdai src/agda/Examples/PhaseCategoryExamples.agdai src/agda/Examples/TechnicalDebtChecklist.agdai src/agda/Examples/AgdaFileScanFFI.agdai src/agda/Examples/ExporterMakefile.agdai src/agda/Markdown/ExportProof.agdai src/agda/Markdown/Normalization.agdai src/agda/Plan/CIM/PolytopeExpansion.agdai src/agda/Plan/CIM/PandocProofExample.agdai src/agda/Plan/CIM/PandocProtocols.agdai src/agda/Plan/CIM/CHIPCoreRecompose.agdai src/agda/Plan/CIM/PandocProofExport.agdai src/agda/Plan/CIM/Utility.agdai src/agda/Plan/CIM/IngestedRoadmaps.agdai src/agda/Plan/CIM/Structure.agdai src/agda/Plan/CIM/DocumentSynthesis.agdai src/agda/Plan/CIM/CHIPRecomposed.agdai src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai src/agda/Plan/CIM/RoadmapExporter.agdai src/agda/Plan/CIM/CanonicalRoadmap.agdai src/agda/Plan/CIM/CHIPConformance.agdai src/agda/Plan/CIM/FrameworkMetadata.agdai src/agda/Plan/CIM/ModuleExporter.agdai src/agda/Plan/CIM/RoadmapIndex.agdai src/agda/Plan/CIM/RoadmapSync.agdai src/agda/Plan/CIM/PandocToMarkdown.agdai src/agda/Plan/CIM/DocumentationContent.agdai src/agda/Plan/CIM/YonedaProfiler.agdai src/agda/Plan/CIM/RoadmapSPPF.agdai src/agda/Plan/CIM/Ambiguity.agdai src/agda/Plan/CIM/RoadmapSPPFExport.agdai src/agda/Plan/CIM/GPNarrativeDAG.agdai src/agda/Plan/CIM/MarkdownNormalize.agdai src/agda/Plan/CIM/PandocAST.agdai src/agda/Plan/CIM/GrammarBridge.agdai src/agda/Plan/CIM/MarkdownParse.agdai src/agda/Algebra/Groups/Basic.agdai src/agda/Algebra/Groups/Theorems/Classical.agdai src/agda/Algebra/Groups/ClassicalInstance.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Algebra/Groups/BasicWithTheorems.agdai src/agda/Algebra/Groups/BasicParameterized.agdai src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Groups/Types.agdai src/agda/Algebra/Groups/Free.agdai src/agda/Algebra/Enrichment.agdai src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Rings/Theorems/Classical.agdai src/agda/Algebra/Rings/ClassicalInstance.agdai src/agda/Algebra/Rings/BasicWithTheorems.agdai src/agda/Algebra/Rings/Types.agdai src/agda/Algebra/Modules/Basic.agdai src/agda/Algebra/Modules/Theorems/Classical.agdai src/agda/Algebra/Modules/ClassicalInstance.agdai src/agda/Algebra/Modules/BasicWithTheorems.agdai src/agda/Algebra/Modules/Types.agdai src/agda/Algebra/Foundation.agdai src/agda/Algebra/Fields/Basic.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Algebra/Fields/Theorems/Classical.agdai src/agda/Algebra/Fields/ClassicalInstance.agdai src/agda/Algebra/Fields/BasicWithTheorems.agdai src/agda/Algebra/Fields/Types.agdai src/agda/Algebra/Index.agdai src/agda/Chapter1/Level1.agdai src/agda/Chapter1/Level1sub8.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1Index.agdai src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/PropertyRegistry.agdai src/agda/ExporterMakefile.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/ABNF.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/Witnesses.agdai src/agda/Core/BraidTree.agdai src/agda/Core/Limitations.agdai src/agda/Core/AlgorithmCorrectness.agdai src/agda/Core/GrothendieckFibrations.agdai src/agda/Core/AlgorithmUniversality.agdai src/agda/Core/AdapterReflection.agdai src/agda/Core/Phase.agdai src/agda/Core/AdapterAutomation.agdai src/agda/Core/Utils.agdai src/agda/Core/Algorithms/Registry.agdai src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/External.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Core/Algorithms/AutomaticEvidence.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Algorithms/InductiveClassification.agdai src/agda/Core/PhaseCategory.agdai src/agda/Core/Rendering.agdai src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/IO.agdai src/agda/Core/GrowthMetrics.agdai src/agda/Core/AlgebraicAlgorithms.agdai src/agda/Core/Yoneda.agdai src/agda/Core/GodelBoundary.agdai src/agda/Core/Strings.agdai src/agda/Core/PathAggregator.agdai src/agda/Core/PolynomialsF2.agdai src/agda/Core/CategoricalAdapter.agdai
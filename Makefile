# Use local Agda 2.8.0 if available, otherwise system agda
AGDA := $(if $(wildcard .local/agda),.local/agda,agda)

# Common Agda compilation flags
AGDA_FLAGS := -i src/agda --ghc-flag=-Wno-star-is-type
.PHONY: regen-makefile md-lint md-fix intake-lint build/canonical_roadmap.json intake-scan md-normalize makefile-validate all check badges priority-strategy-profiles priority-badge-weights priority-profile-json dependency-graph-json priority-refresh docs-modules docs-all node-deps deferred-items roadmap-index planning-index-json planning-kernel roadmap-sync roadmap-sppf validate-constructive roadmap-merge build/diagrams/agda-deps-full.dot roadmap-deps-graph build/canonical_enriched.json roadmap-enrich roadmap-export-json roadmap-export-md roadmap-export-enriched roadmap-export-deps roadmap-validate-json roadmap-validate-md roadmap-validate-triangle roadmap-sppf-export roadmap-all-enriched docs-generate docs-validate agda-all
# Regenerate the Makefile from Agda source (Self-Hosting)
regen-makefile: build/diagrams/agda-deps-full.dot
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile
	cp Makefile.generated Makefile
# Lint all markdown files (fail on error)
md-lint: 
	mkdir -p build/reports
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
intake-scan: planning-index-json
	@echo "intake scan complete"
# Normalize markdown formatting
md-normalize: 
	python3 scripts/normalize_generated_markdown.py
# Validate Makefile consistency
makefile-validate: 
	mkdir -p build/reports
	python3 scripts/validate_makefile_docs.py > build/reports/makefile-validate.txt || (cat build/reports/makefile-validate.txt; exit 1)
# Build all code and documentation
all: agda-all docs-all
	@echo "all complete"
# Run all validation checks
check: makefile-validate md-lint roadmap-validate-triangle docs-validate all
	@echo "check complete"
# Generate status badges
badges: priority-badge-weights
	python3 scripts/generate-badges.py
# Compile and run Agda priority orchestration (generate strategy profiles)
priority-strategy-profiles: 
	mkdir -p build
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
	./src/agda/PriorityOrchestrationFFI
# Normalize Agda strategy profiles into badge weights
priority-badge-weights: priority-strategy-profiles
	python3 scripts/adopt_priority_strategies.py --input build/priority_strategy_profiles.json --output .github/badges/weights.json
# Export structured priority profile (lazy; derived from planning index)
priority-profile-json: planning-index-json
	mkdir -p build
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PriorityProfileExport.agda && ./src/agda/PriorityProfileExport
# Export dependency graph JSON via Agda (from agda-deps-full.dot)
dependency-graph-json: build/diagrams/agda-deps-full.dot
	mkdir -p build
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/DependencyGraphExport.agda && ./src/agda/DependencyGraphExport
# Re-run priority pipeline and refresh roadmap/badge outputs
priority-refresh: planning-index-json roadmap-export-json priority-badge-weights badges
	@echo "priority pipeline refreshed (planning index, tasks, badge weights, roadmap badges)"
# Generate per-module markdown documentation
docs-modules: src/agda/Plan/CIM/ModuleExporter.agdai
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/ModuleExporter.agda && ./src/agda/Plan/CIM/ModuleExporter
# Generate documentation (markdown only)
docs-all: docs-generate docs-modules
	@echo "docs (markdown) complete"
# Install Node.js dependencies
node-deps: 
	npm install
# Scan for TODOs and FIXMEs (Agda FFI binary)
deferred-items: 
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
	./src/agda/DeferredItemsOrchestrationFFI
# Compile Roadmap Index
roadmap-index: src/agda/Plan/CIM/RoadmapIndex.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda
# Export planning index to JSON
planning-index-json: src/agda/Plan/CIM/PlanningExport.agdai
	mkdir -p build
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/PlanningExport.agda && ./src/agda/PlanningExport
# Compile Planning Kernel
planning-kernel: src/agda/Plan/CIM/PlanningKernel.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda
# Sync roadmap with external tracker
roadmap-sync: roadmap-export-json src/agda/Plan/CIM/RoadmapSync.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda
# Compile Roadmap SPPF
roadmap-sppf: src/agda/Plan/CIM/RoadmapSPPF.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda
# Run all constructive build targets
validate-constructive: docs-all docs-generate docs-modules roadmap-export-json roadmap-export-md roadmap-export-enriched roadmap-export-deps roadmap-deps-graph roadmap-enrich roadmap-all-enriched intake-scan md-normalize badges
	@echo "✓ Constructive validation complete"
# Merge ingestion streams
roadmap-merge: 
	python3 scripts/merge_roadmaps.py
# Generate dependency graph
build/diagrams/agda-deps-full.dot: 
	mkdir -p build/diagrams
	$(AGDA) --dependency-graph=build/diagrams/agda-deps-full.dot $(AGDA_FLAGS) src/agda/Tests/Index.agda 2>&1 | grep -E "(Checking|Error)" | head -20
# Generate dependency graph
roadmap-deps-graph: build/diagrams/agda-deps-full.dot
	@echo "agda dependency graph generated"
# Enrich canonical roadmap
build/canonical_enriched.json: planning-index-json build/diagrams/agda-deps-full.dot
	python3 scripts/enrich_canonical.py
# Enrich roadmap with graph data
roadmap-enrich: build/canonical_enriched.json
	@echo "roadmap enrichment complete"
# Export canonical roadmap to JSON
roadmap-export-json: planning-index-json
	python3 scripts/export_canonical_json.py
# Export canonical roadmap to Markdown
roadmap-export-md: planning-index-json
	python3 scripts/export_canonical_md.py
# Export enriched roadmap
roadmap-export-enriched: build/canonical_enriched.json
	python3 scripts/export_enriched_md.py
# Export roadmap dependency graph
roadmap-export-deps: build/canonical_enriched.json
	python3 scripts/export_dependency_graph.py
# Validate canonical JSON
roadmap-validate-json: roadmap-export-json
	python3 scripts/validate_json.py
# Validate canonical Markdown
roadmap-validate-md: roadmap-export-md
	python3 scripts/validate_md.py
# Verify Triangle Identity (Agda <-> JSON <-> MD)
roadmap-validate-triangle: roadmap-validate-json roadmap-validate-md
	@echo "✓ Triangle validation complete"
# Export SPPF structure
roadmap-sppf-export: planning-index-json
	python3 scripts/export_roadmap_sppf.py
# Build all enriched artifacts
roadmap-all-enriched: roadmap-export-enriched roadmap-export-deps
	@echo "roadmap all enriched complete"
# Compile and run Roadmap Exporter
docs-generate: src/agda/Plan/CIM/RoadmapExporter.agdai
	$(AGDA) $(AGDA_FLAGS) --compile src/agda/Plan/CIM/RoadmapExporter.agda && ./src/agda/RoadmapExporter
	python3 scripts/normalize_generated_markdown.py
# Validate documentation integrity
docs-validate: 
	python3 scripts/validate_triangle_identity.py
# Compile src/agda/MetaScan.agda
src/agda/MetaScan.agdai: src/agda/MetaScan.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/MetaScan.agda
# Compile src/agda/Metamodel.agda
src/agda/Metamodel.agdai: src/agda/Metamodel.agda src/agda/Core/Phase.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Metamodel.agda
# Compile src/agda/Tests/PropertyRegistryTests.agda
src/agda/Tests/PropertyRegistryTests.agdai: src/agda/Tests/PropertyRegistryTests.agda src/agda/PropertyRegistry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PropertyRegistryTests.agda
# Compile src/agda/Tests/AbelianCategoriesChecklist.agda
src/agda/Tests/AbelianCategoriesChecklist.agdai: src/agda/Tests/AbelianCategoriesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AbelianCategoriesChecklist.agda
# Compile src/agda/Tests/DispatchBehaviorTests.agda
src/agda/Tests/DispatchBehaviorTests.agdai: src/agda/Tests/DispatchBehaviorTests.agda src/agda/Core/Algorithms/Registry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/DispatchBehaviorTests.agda
# Compile src/agda/Tests/WitnessConstructionTests.agda
src/agda/Tests/WitnessConstructionTests.agdai: src/agda/Tests/WitnessConstructionTests.agda src/agda/Core/Witnesses.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/WitnessConstructionTests.agda
# Compile src/agda/Tests/VectorSpaceChecklist.agda
src/agda/Tests/VectorSpaceChecklist.agdai: src/agda/Tests/VectorSpaceChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/VectorSpaceChecklist.agda
# Compile src/agda/Tests/ProofObligationStatus.agda
src/agda/Tests/ProofObligationStatus.agdai: src/agda/Tests/ProofObligationStatus.agda src/agda/Examples/AlgorithmCorrectnessExamples.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ProofObligationStatus.agda
# Compile src/agda/Tests/SerializationTests.agda
src/agda/Tests/SerializationTests.agdai: src/agda/Tests/SerializationTests.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/TechnicalDebt.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/SerializationTests.agda
# Compile src/agda/Tests/GodelBoundaryTests.agda
src/agda/Tests/GodelBoundaryTests.agdai: src/agda/Tests/GodelBoundaryTests.agda src/agda/Core/GodelBoundary.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/GodelBoundaryTests.agda
# Compile src/agda/Tests/ModuleStructureChecklist.agda
src/agda/Tests/ModuleStructureChecklist.agdai: src/agda/Tests/ModuleStructureChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ModuleStructureChecklist.agda
# Compile src/agda/Tests/ToposTheoryChecklist.agda
src/agda/Tests/ToposTheoryChecklist.agdai: src/agda/Tests/ToposTheoryChecklist.agda src/agda/Tests/ToposObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ToposTheoryChecklist.agda
# Compile src/agda/Tests/GrothendieckFibrationsChecklist.agda
src/agda/Tests/GrothendieckFibrationsChecklist.agdai: src/agda/Tests/GrothendieckFibrationsChecklist.agda src/agda/Tests/ObligationAdapters.agdai src/agda/Core/GrothendieckFibrations.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/GrothendieckFibrationsChecklist.agda
# Compile src/agda/Tests/TensorProductChecklist.agda
src/agda/Tests/TensorProductChecklist.agdai: src/agda/Tests/TensorProductChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/TensorProductChecklist.agda
# Compile src/agda/Tests/Index_PhaseII.agda
src/agda/Tests/Index_PhaseII.agdai: src/agda/Tests/Index_PhaseII.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Index_PhaseII.agda
# Compile src/agda/Tests/HierarchyValidation.agda
src/agda/Tests/HierarchyValidation.agdai: src/agda/Tests/HierarchyValidation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/HierarchyValidation.agda
# Compile src/agda/Tests/PhaseExamples.agda
src/agda/Tests/PhaseExamples.agdai: src/agda/Tests/PhaseExamples.agda src/agda/Core/Algorithms/Registry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PhaseExamples.agda
# Compile src/agda/Tests/RegularCategoriesChecklist.agda
src/agda/Tests/RegularCategoriesChecklist.agdai: src/agda/Tests/RegularCategoriesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/RegularCategoriesChecklist.agda
# Compile src/agda/Tests/AlgorithmCompositionTestsMinimal.agda
src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai: src/agda/Tests/AlgorithmCompositionTestsMinimal.agda src/agda/Core/Algorithms/Registry.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/UniversalProperties.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AlgorithmCompositionTestsMinimal.agda
# Compile src/agda/Tests/MonadAdjunctionChecklist.agda
src/agda/Tests/MonadAdjunctionChecklist.agdai: src/agda/Tests/MonadAdjunctionChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/MonadAdjunctionChecklist.agda
# Compile src/agda/Tests/UniversalPropertyTests.agda
src/agda/Tests/UniversalPropertyTests.agdai: src/agda/Tests/UniversalPropertyTests.agda src/agda/Core/AlgorithmUniversality.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/UniversalPropertyTests.agda
# Compile src/agda/Tests/EnrichmentChecklist.agda
src/agda/Tests/EnrichmentChecklist.agdai: src/agda/Tests/EnrichmentChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/EnrichmentChecklist.agda
# Compile src/agda/Tests/AdvancedPhaseExamples.agda
src/agda/Tests/AdvancedPhaseExamples.agdai: src/agda/Tests/AdvancedPhaseExamples.agda src/agda/Core/Algorithms/Registry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AdvancedPhaseExamples.agda
# Compile src/agda/Tests/ErrorAsSpecificationTests.agda
src/agda/Tests/ErrorAsSpecificationTests.agdai: src/agda/Tests/ErrorAsSpecificationTests.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ErrorAsSpecificationTests.agda
# Compile src/agda/Tests/RealWorldAlgorithmsTests.agda
src/agda/Tests/RealWorldAlgorithmsTests.agdai: src/agda/Tests/RealWorldAlgorithmsTests.agda src/agda/Examples/RealWorldAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/RealWorldAlgorithmsTests.agda
# Compile src/agda/Tests/PolynomialExtensionsChecklist.agda
src/agda/Tests/PolynomialExtensionsChecklist.agdai: src/agda/Tests/PolynomialExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PolynomialExtensionsChecklist.agda
# Compile src/agda/Tests/ModuleTheoryChecklist.agda
src/agda/Tests/ModuleTheoryChecklist.agdai: src/agda/Tests/ModuleTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ModuleTheoryChecklist.agda
# Compile src/agda/Tests/PhaseCategoryExamplesRunner.agda
src/agda/Tests/PhaseCategoryExamplesRunner.agdai: src/agda/Tests/PhaseCategoryExamplesRunner.agda src/agda/Examples/PhaseCategoryExamples.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PhaseCategoryExamplesRunner.agda
# Compile src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai: src/agda/Tests/PolynomialFieldExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
# Compile src/agda/Tests/AlgorithmCompositionTests.agda
src/agda/Tests/AlgorithmCompositionTests.agdai: src/agda/Tests/AlgorithmCompositionTests.agda src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AlgorithmCompositionTests.agda
# Compile src/agda/Tests/FieldsBasicChecklist.agda
src/agda/Tests/FieldsBasicChecklist.agdai: src/agda/Tests/FieldsBasicChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/FieldsBasicChecklist.agda
# Compile src/agda/Tests/SpecificationValidation.agda
src/agda/Tests/SpecificationValidation.agdai: src/agda/Tests/SpecificationValidation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/SpecificationValidation.agda
# Compile src/agda/Tests/AlgorithmSmokeTests.agda
src/agda/Tests/AlgorithmSmokeTests.agdai: src/agda/Tests/AlgorithmSmokeTests.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AlgorithmSmokeTests.agda
# Compile src/agda/Tests/CoverageReport.agda
src/agda/Tests/CoverageReport.agdai: src/agda/Tests/CoverageReport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/CoverageReport.agda
# Compile src/agda/Tests/GroupsFreeChecklist.agda
src/agda/Tests/GroupsFreeChecklist.agdai: src/agda/Tests/GroupsFreeChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/GroupsFreeChecklist.agda
# Compile src/agda/Tests/RingsBasicChecklist.agda
src/agda/Tests/RingsBasicChecklist.agdai: src/agda/Tests/RingsBasicChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/RingsBasicChecklist.agda
# Compile src/agda/Tests/Chapter2Checklist.agda
src/agda/Tests/Chapter2Checklist.agdai: src/agda/Tests/Chapter2Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Chapter2Checklist.agda
# Compile src/agda/Tests/KanExtensionsChecklist.agda
src/agda/Tests/KanExtensionsChecklist.agdai: src/agda/Tests/KanExtensionsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/KanExtensionsChecklist.agda
# Compile src/agda/Tests/LimitsColimitsChecklist.agda
src/agda/Tests/LimitsColimitsChecklist.agdai: src/agda/Tests/LimitsColimitsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/LimitsColimitsChecklist.agda
# Compile src/agda/Tests/AdvancedMonadTheoryChecklist.agda
src/agda/Tests/AdvancedMonadTheoryChecklist.agdai: src/agda/Tests/AdvancedMonadTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AdvancedMonadTheoryChecklist.agda
# Compile src/agda/Tests/CoreUniversalPropertiesChecklist.agda
src/agda/Tests/CoreUniversalPropertiesChecklist.agdai: src/agda/Tests/CoreUniversalPropertiesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/CoreUniversalPropertiesChecklist.agda
# Compile src/agda/Tests/ChapterObligationsSmoke.agda
src/agda/Tests/ChapterObligationsSmoke.agdai: src/agda/Tests/ChapterObligationsSmoke.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub2.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ChapterObligationsSmoke.agda
# Compile src/agda/Tests/ConstructiveWitnessTests.agda
src/agda/Tests/ConstructiveWitnessTests.agdai: src/agda/Tests/ConstructiveWitnessTests.agda src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/AlgorithmUniversality.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ConstructiveWitnessTests.agda
# Compile src/agda/Tests/PerformanceBoundaryTests.agda
src/agda/Tests/PerformanceBoundaryTests.agdai: src/agda/Tests/PerformanceBoundaryTests.agda src/agda/GrowthAnalysis.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/Algorithms/Registry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PerformanceBoundaryTests.agda
# Compile src/agda/Tests/PathAggregatorTests.agda
src/agda/Tests/PathAggregatorTests.agdai: src/agda/Tests/PathAggregatorTests.agda src/agda/Core/PathAggregator.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/PathAggregatorTests.agda
# Compile src/agda/Tests/GroupsAbelianChecklist.agda
src/agda/Tests/GroupsAbelianChecklist.agdai: src/agda/Tests/GroupsAbelianChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/GroupsAbelianChecklist.agda
# Compile src/agda/Tests/Chapter1Checklist.agda
src/agda/Tests/Chapter1Checklist.agdai: src/agda/Tests/Chapter1Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Chapter1Checklist.agda
# Compile src/agda/Tests/AdvancedFieldsChecklist.agda
src/agda/Tests/AdvancedFieldsChecklist.agdai: src/agda/Tests/AdvancedFieldsChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AdvancedFieldsChecklist.agda
# Compile src/agda/Tests/WarningAggregatorsTest.agda
src/agda/Tests/WarningAggregatorsTest.agdai: src/agda/Tests/WarningAggregatorsTest.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/WarningAggregatorsTest.agda
# Compile src/agda/Tests/Index.agda
src/agda/Tests/Index.agdai: src/agda/Tests/Index.agda src/agda/Tests/PhaseExamples.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/GroupsAbelianChecklist.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Index.agda
# Compile src/agda/Tests/ObligationAdapters.agda
src/agda/Tests/ObligationAdapters.agdai: src/agda/Tests/ObligationAdapters.agda src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Core/CategoricalAdapter.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Core/UniversalProperties.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Chapter2/Level2sub2.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ObligationAdapters.agda
# Compile src/agda/Tests/ToposObligationAdapters.agda
src/agda/Tests/ToposObligationAdapters.agdai: src/agda/Tests/ToposObligationAdapters.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Core/CategoricalAdapter.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ToposObligationAdapters.agda
# Compile src/agda/Tests/Chapters.agda
src/agda/Tests/Chapters.agdai: src/agda/Tests/Chapters.agda src/agda/Chapter3/Level3Index.agdai src/agda/Chapter2/Level2Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Chapters.agda
# Compile src/agda/Tests/YonedaChecklist.agda
src/agda/Tests/YonedaChecklist.agdai: src/agda/Tests/YonedaChecklist.agda src/agda/Tests/ObligationAdapters.agdai src/agda/Core/Yoneda.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/YonedaChecklist.agda
# Compile src/agda/Tests/AlgebraicCompletionChecklist.agda
src/agda/Tests/AlgebraicCompletionChecklist.agdai: src/agda/Tests/AlgebraicCompletionChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AlgebraicCompletionChecklist.agda
# Compile src/agda/Tests/FunctorPropertiesChecklist.agda
src/agda/Tests/FunctorPropertiesChecklist.agdai: src/agda/Tests/FunctorPropertiesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/FunctorPropertiesChecklist.agda
# Compile src/agda/Tests/AlgebraChecklist.agda
src/agda/Tests/AlgebraChecklist.agdai: src/agda/Tests/AlgebraChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/AlgebraChecklist.agda
# Compile src/agda/Tests/ErrorHandlingTests.agda
src/agda/Tests/ErrorHandlingTests.agdai: src/agda/Tests/ErrorHandlingTests.agda src/agda/Core/Algorithms/Registry.agdai src/agda/Core/UniversalProperties.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ErrorHandlingTests.agda
# Compile src/agda/Tests/GroupsStructureChecklist.agda
src/agda/Tests/GroupsStructureChecklist.agdai: src/agda/Tests/GroupsStructureChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/GroupsStructureChecklist.agda
# Compile src/agda/Tests/ModulesChecklist.agda
src/agda/Tests/ModulesChecklist.agdai: src/agda/Tests/ModulesChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/ModulesChecklist.agda
# Compile src/agda/Tests/Chapter3Checklist.agda
src/agda/Tests/Chapter3Checklist.agdai: src/agda/Tests/Chapter3Checklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/Chapter3Checklist.agda
# Compile src/agda/Tests/SubobjectTheoryChecklist.agda
src/agda/Tests/SubobjectTheoryChecklist.agdai: src/agda/Tests/SubobjectTheoryChecklist.agda src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Tests/SubobjectTheoryChecklist.agda
# Compile src/agda/Chapter2/Level2sub3.agda
src/agda/Chapter2/Level2sub3.agdai: src/agda/Chapter2/Level2sub3.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub3.agda
# Compile src/agda/Chapter2/Level2sub6.agda
src/agda/Chapter2/Level2sub6.agdai: src/agda/Chapter2/Level2sub6.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub6.agda
# Compile src/agda/Chapter2/Level2sub5.agda
src/agda/Chapter2/Level2sub5.agdai: src/agda/Chapter2/Level2sub5.agda src/agda/Chapter1/Level1Index.agdai src/agda/Chapter2/Level2sub3.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub5.agda
# Compile src/agda/Chapter2/Level2sub8.agda
src/agda/Chapter2/Level2sub8.agdai: src/agda/Chapter2/Level2sub8.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub8.agda
# Compile src/agda/Chapter2/Level2sub7.agda
src/agda/Chapter2/Level2sub7.agdai: src/agda/Chapter2/Level2sub7.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub7.agda
# Compile src/agda/Chapter2/Level2sub2.agda
src/agda/Chapter2/Level2sub2.agdai: src/agda/Chapter2/Level2sub2.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub2.agda
# Compile src/agda/Chapter2/Level2sub1.agda
src/agda/Chapter2/Level2sub1.agdai: src/agda/Chapter2/Level2sub1.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub1.agda
# Compile src/agda/Chapter2/Level2sub4.agda
src/agda/Chapter2/Level2sub4.agdai: src/agda/Chapter2/Level2sub4.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2sub4.agda
# Compile src/agda/Chapter2/Level2Index.agda
src/agda/Chapter2/Level2Index.agdai: src/agda/Chapter2/Level2Index.agda src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub2.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter2/Level2Index.agda
# Compile src/agda/Chapter3/Level3sub1.agda
src/agda/Chapter3/Level3sub1.agdai: src/agda/Chapter3/Level3sub1.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter3/Level3sub1.agda
# Compile src/agda/Chapter3/Level3sub2.agda
src/agda/Chapter3/Level3sub2.agdai: src/agda/Chapter3/Level3sub2.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter3/Level3sub2.agda
# Compile src/agda/Chapter3/Level3Index.agda
src/agda/Chapter3/Level3Index.agdai: src/agda/Chapter3/Level3Index.agda src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3sub1.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter3/Level3Index.agda
# Compile src/agda/Infrastructure/Arity/BinTree.agda
src/agda/Infrastructure/Arity/BinTree.agdai: src/agda/Infrastructure/Arity/BinTree.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Arity/BinTree.agda
# Compile src/agda/Infrastructure/Universe.agda
src/agda/Infrastructure/Universe.agdai: src/agda/Infrastructure/Universe.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Universe.agda
# Compile src/agda/Infrastructure/Iso/Structural.agda
src/agda/Infrastructure/Iso/Structural.agdai: src/agda/Infrastructure/Iso/Structural.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Iso/Structural.agda
# Compile src/agda/Infrastructure/Functor/Compose.agda
src/agda/Infrastructure/Functor/Compose.agdai: src/agda/Infrastructure/Functor/Compose.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Compose.agda
# Compile src/agda/Infrastructure/Functor/Interface.agda
src/agda/Infrastructure/Functor/Interface.agdai: src/agda/Infrastructure/Functor/Interface.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Interface.agda
# Compile src/agda/Infrastructure/Functor/Adapters/Funext.agda
src/agda/Infrastructure/Functor/Adapters/Funext.agdai: src/agda/Infrastructure/Functor/Adapters/Funext.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Adapters/Funext.agda
# Compile src/agda/Infrastructure/Functor/Instances/TransformationSystem.agda
src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai: src/agda/Infrastructure/Functor/Instances/TransformationSystem.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/TransformationSystem.agda
# Compile src/agda/Infrastructure/Functor/Instances/Trivial.agda
src/agda/Infrastructure/Functor/Instances/Trivial.agdai: src/agda/Infrastructure/Functor/Instances/Trivial.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/Trivial.agda
# Compile src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda
src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai: src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/PhaseCategory.agda
# Compile src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda
src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai: src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/PathAlgebra.agda
# Compile src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda
src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai: src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/FunctionCategory.agda
# Compile src/agda/Infrastructure/Functor/Instances/Ambiguity.agda
src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai: src/agda/Infrastructure/Functor/Instances/Ambiguity.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/Ambiguity.agda
# Compile src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda
src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai: src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agda
# Compile src/agda/Infrastructure/Coherence/Path2.agda
src/agda/Infrastructure/Coherence/Path2.agdai: src/agda/Infrastructure/Coherence/Path2.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Coherence/Path2.agda
# Compile src/agda/Infrastructure/Equality.agda
src/agda/Infrastructure/Equality.agdai: src/agda/Infrastructure/Equality.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Equality.agda
# Compile src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agda
src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai: src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agda
# Compile src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agda
src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai: src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agda
# Compile src/agda/Infrastructure/Polytopes/Associahedron.agda
src/agda/Infrastructure/Polytopes/Associahedron.agdai: src/agda/Infrastructure/Polytopes/Associahedron.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Polytopes/Associahedron.agda
# Compile src/agda/Infrastructure/Definitions/Dictionary.agda
src/agda/Infrastructure/Definitions/Dictionary.agdai: src/agda/Infrastructure/Definitions/Dictionary.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Definitions/Dictionary.agda
# Compile src/agda/Infrastructure/Product/Bundle4.agda
src/agda/Infrastructure/Product/Bundle4.agdai: src/agda/Infrastructure/Product/Bundle4.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Product/Bundle4.agda
# Compile src/agda/Infrastructure/Index.agda
src/agda/Infrastructure/Index.agdai: src/agda/Infrastructure/Index.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Index.agda
# Compile src/agda/Infrastructure/Axiom/Movie.agda
src/agda/Infrastructure/Axiom/Movie.agdai: src/agda/Infrastructure/Axiom/Movie.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Movie.agda
# Compile src/agda/Infrastructure/Axiom/PentagonFromTriangles.agda
src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai: src/agda/Infrastructure/Axiom/PentagonFromTriangles.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/PentagonFromTriangles.agda
# Compile src/agda/Infrastructure/Axiom/Adequacy.agda
src/agda/Infrastructure/Axiom/Adequacy.agdai: src/agda/Infrastructure/Axiom/Adequacy.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Adequacy.agda
# Compile src/agda/Infrastructure/Axiom/Solver.agda
src/agda/Infrastructure/Axiom/Solver.agdai: src/agda/Infrastructure/Axiom/Solver.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Solver.agda
# Compile src/agda/Infrastructure/Axiom/Face.agda
src/agda/Infrastructure/Axiom/Face.agdai: src/agda/Infrastructure/Axiom/Face.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Face.agda
# Compile src/agda/Infrastructure/Axiom/SolvableInterface.agda
src/agda/Infrastructure/Axiom/SolvableInterface.agdai: src/agda/Infrastructure/Axiom/SolvableInterface.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/SolvableInterface.agda
# Compile src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agda
src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai: src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agda
# Compile src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda
src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai: src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agda
# Compile src/agda/Infrastructure/Axiom/Instance.agda
src/agda/Infrastructure/Axiom/Instance.agdai: src/agda/Infrastructure/Axiom/Instance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Axiom/Instance.agda
# Compile src/agda/Infrastructure/Wrapper/With.agda
src/agda/Infrastructure/Wrapper/With.agdai: src/agda/Infrastructure/Wrapper/With.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Infrastructure/Wrapper/With.agda
# Compile src/agda/Algorithms/Instrumented.agda
src/agda/Algorithms/Instrumented.agdai: src/agda/Algorithms/Instrumented.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algorithms/Instrumented.agda
# Compile src/agda/Algorithms/Basic.agda
src/agda/Algorithms/Basic.agdai: src/agda/Algorithms/Basic.agda src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algorithms/Basic.agda
# Compile src/agda/Algorithms/Adapters/BundleAdapter.agda
src/agda/Algorithms/Adapters/BundleAdapter.agdai: src/agda/Algorithms/Adapters/BundleAdapter.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Algorithms/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algorithms/Adapters/BundleAdapter.agda
# Compile src/agda/Algorithms/TestInstances.agda
src/agda/Algorithms/TestInstances.agdai: src/agda/Algorithms/TestInstances.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algorithms/TestInstances.agda
# Compile src/agda/MinimalAUDAXInlineTest.agda
src/agda/MinimalAUDAXInlineTest.agdai: src/agda/MinimalAUDAXInlineTest.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/MinimalAUDAXInlineTest.agda
# Compile src/agda/Core.agda
src/agda/Core.agdai: src/agda/Core.agda src/agda/Chapter1/Level1.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core.agda
# Compile src/agda/TechnicalDebt/PriorityMapping.agda
src/agda/TechnicalDebt/PriorityMapping.agdai: src/agda/TechnicalDebt/PriorityMapping.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/PriorityMapping.agda
# Compile src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai: src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/PriorityOrchestrationFFI.agda
# Compile src/agda/TechnicalDebt/PriorityFormatting.agda
src/agda/TechnicalDebt/PriorityFormatting.agdai: src/agda/TechnicalDebt/PriorityFormatting.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/PriorityFormatting.agda
# Compile src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai: src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agda
# Compile src/agda/TechnicalDebt/DeferredItemsDetection.agda
src/agda/TechnicalDebt/DeferredItemsDetection.agdai: src/agda/TechnicalDebt/DeferredItemsDetection.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/DeferredItemsDetection.agda
# Compile src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agda
src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai: src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agda
# Compile src/agda/TechnicalDebt/PriorityOrchestration.agda
src/agda/TechnicalDebt/PriorityOrchestration.agdai: src/agda/TechnicalDebt/PriorityOrchestration.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/PriorityOrchestration.agda
# Compile src/agda/TechnicalDebt/DeferredItemsFormatting.agda
src/agda/TechnicalDebt/DeferredItemsFormatting.agdai: src/agda/TechnicalDebt/DeferredItemsFormatting.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/DeferredItemsFormatting.agda
# Compile src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda
src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai: src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agda
# Compile src/agda/TechnicalDebt/Priorities.agda
src/agda/TechnicalDebt/Priorities.agdai: src/agda/TechnicalDebt/Priorities.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/TechnicalDebt/Priorities.agda
# Compile src/agda/Examples/RealWorldAlgorithms.agda
src/agda/Examples/RealWorldAlgorithms.agdai: src/agda/Examples/RealWorldAlgorithms.agda src/agda/Core/AlgorithmCorrectness.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/RealWorldAlgorithms.agda
# Compile src/agda/Examples/TechnicalDebtExample.agda
src/agda/Examples/TechnicalDebtExample.agdai: src/agda/Examples/TechnicalDebtExample.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/TechnicalDebtExample.agda
# Compile src/agda/Examples/AmbiguityExamples.agda
src/agda/Examples/AmbiguityExamples.agdai: src/agda/Examples/AmbiguityExamples.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/AmbiguityExamples.agda
# Compile src/agda/Examples/FunctionField/F2x.agda
src/agda/Examples/FunctionField/F2x.agdai: src/agda/Examples/FunctionField/F2x.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/FunctionField/F2x.agda
# Compile src/agda/Examples/AlgorithmCorrectnessExamples.agda
src/agda/Examples/AlgorithmCorrectnessExamples.agdai: src/agda/Examples/AlgorithmCorrectnessExamples.agda src/agda/Core/AlgorithmCorrectness.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/AlgorithmCorrectnessExamples.agda
# Compile src/agda/Examples/NumberField/Sqrt2.agda
src/agda/Examples/NumberField/Sqrt2.agdai: src/agda/Examples/NumberField/Sqrt2.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/NumberField/Sqrt2.agda
# Compile src/agda/Examples/FiniteField/GF8.agda
src/agda/Examples/FiniteField/GF8.agdai: src/agda/Examples/FiniteField/GF8.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/FiniteField/GF8.agda
# Compile src/agda/Examples/InstrumentedAlgorithmDemo.agda
src/agda/Examples/InstrumentedAlgorithmDemo.agdai: src/agda/Examples/InstrumentedAlgorithmDemo.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/InstrumentedAlgorithmDemo.agda
# Compile src/agda/Examples/DeferredItemsScanner.agda
src/agda/Examples/DeferredItemsScanner.agdai: src/agda/Examples/DeferredItemsScanner.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/DeferredItemsScanner.agda
# Compile src/agda/Examples/TransformationSystemExamples.agda
src/agda/Examples/TransformationSystemExamples.agdai: src/agda/Examples/TransformationSystemExamples.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/TransformationSystemExamples.agda
# Compile src/agda/Examples/AutomaticEvidenceDemo.agda
src/agda/Examples/AutomaticEvidenceDemo.agdai: src/agda/Examples/AutomaticEvidenceDemo.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/AutomaticEvidenceDemo.agda
# Compile src/agda/Examples/ConstructiveWitnessExamples.agda
src/agda/Examples/ConstructiveWitnessExamples.agdai: src/agda/Examples/ConstructiveWitnessExamples.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/ConstructiveWitnessExamples.agda
# Compile src/agda/Examples/AgdaMakefileDeps.agda
src/agda/Examples/AgdaMakefileDeps.agdai: src/agda/Examples/AgdaMakefileDeps.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/AgdaMakefileDeps.agda
# Compile src/agda/Examples/LazyHybridDemo.agda
src/agda/Examples/LazyHybridDemo.agdai: src/agda/Examples/LazyHybridDemo.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/LazyHybridDemo.agda
# Compile src/agda/Examples/TechnicalDebtRegistry.agda
src/agda/Examples/TechnicalDebtRegistry.agdai: src/agda/Examples/TechnicalDebtRegistry.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/TechnicalDebtRegistry.agda
# Compile src/agda/Examples/RoadmapIssueSync.agda
src/agda/Examples/RoadmapIssueSync.agdai: src/agda/Examples/RoadmapIssueSync.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/RoadmapIssueSync.agda
# Compile src/agda/Examples/MakefileTargets.agda
src/agda/Examples/MakefileTargets.agdai: src/agda/Examples/MakefileTargets.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/MakefileTargets.agda
# Compile src/agda/Examples/PhaseCategoryExamples.agda
src/agda/Examples/PhaseCategoryExamples.agdai: src/agda/Examples/PhaseCategoryExamples.agda src/agda/Core/PhaseCategory.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/PhaseCategoryExamples.agda
# Compile src/agda/Examples/EqualityExamples.agda
src/agda/Examples/EqualityExamples.agdai: src/agda/Examples/EqualityExamples.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/EqualityExamples.agda
# Compile src/agda/Examples/TechnicalDebtChecklist.agda
src/agda/Examples/TechnicalDebtChecklist.agdai: src/agda/Examples/TechnicalDebtChecklist.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/TechnicalDebtChecklist.agda
# Compile src/agda/Examples/AgdaFileScanFFI.agda
src/agda/Examples/AgdaFileScanFFI.agdai: src/agda/Examples/AgdaFileScanFFI.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/AgdaFileScanFFI.agda
# Compile src/agda/Examples/ExporterMakefile.agda
src/agda/Examples/ExporterMakefile.agdai: src/agda/Examples/ExporterMakefile.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Examples/ExporterMakefile.agda
# Compile src/agda/Markdown/ExportProof.agda
src/agda/Markdown/ExportProof.agdai: src/agda/Markdown/ExportProof.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Markdown/ExportProof.agda
# Compile src/agda/Markdown/Normalization.agda
src/agda/Markdown/Normalization.agdai: src/agda/Markdown/Normalization.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Markdown/Normalization.agda
# Compile src/agda/Docs/MetaIndex.agda
src/agda/Docs/MetaIndex.agdai: src/agda/Docs/MetaIndex.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Docs/MetaIndex.agda
# Compile src/agda/GrowthAnalysis.agda
src/agda/GrowthAnalysis.agdai: src/agda/GrowthAnalysis.agda src/agda/Core/GrowthMetrics.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/GrowthAnalysis.agda
# Compile src/agda/Plan/CIM/PolytopeExpansion.agda
src/agda/Plan/CIM/PolytopeExpansion.agdai: src/agda/Plan/CIM/PolytopeExpansion.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PolytopeExpansion.agda
# Compile src/agda/Plan/CIM/PlanningKernel.agda
src/agda/Plan/CIM/PlanningKernel.agdai: src/agda/Plan/CIM/PlanningKernel.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningKernel.agda
# Compile src/agda/Plan/CIM/PandocProofExample.agda
src/agda/Plan/CIM/PandocProofExample.agdai: src/agda/Plan/CIM/PandocProofExample.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocProofExample.agda
# Compile src/agda/Plan/CIM/PandocProtocols.agda
src/agda/Plan/CIM/PandocProtocols.agdai: src/agda/Plan/CIM/PandocProtocols.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocProtocols.agda
# Compile src/agda/Plan/CIM/PandocShowBlock.agda
src/agda/Plan/CIM/PandocShowBlock.agdai: src/agda/Plan/CIM/PandocShowBlock.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocShowBlock.agda
# Compile src/agda/Plan/CIM/CHIPCoreRecompose.agda
src/agda/Plan/CIM/CHIPCoreRecompose.agdai: src/agda/Plan/CIM/CHIPCoreRecompose.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/CHIPCoreRecompose.agda
# Compile src/agda/Plan/CIM/PandocProofExport.agda
src/agda/Plan/CIM/PandocProofExport.agdai: src/agda/Plan/CIM/PandocProofExport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocProofExport.agda
# Compile src/agda/Plan/CIM/Utility.agda
src/agda/Plan/CIM/Utility.agdai: src/agda/Plan/CIM/Utility.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/Utility.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps.agda
src/agda/Plan/CIM/IngestedRoadmaps.agdai: src/agda/Plan/CIM/IngestedRoadmaps.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps.agda
# Compile src/agda/Plan/CIM/Structure.agda
src/agda/Plan/CIM/Structure.agdai: src/agda/Plan/CIM/Structure.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/Structure.agda
# Compile src/agda/Plan/CIM/DocumentSynthesis.agda
src/agda/Plan/CIM/DocumentSynthesis.agdai: src/agda/Plan/CIM/DocumentSynthesis.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/DocumentSynthesis.agda
# Compile src/agda/Plan/CIM/CHIPRecomposed.agda
src/agda/Plan/CIM/CHIPRecomposed.agdai: src/agda/Plan/CIM/CHIPRecomposed.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/CHIPRecomposed.agda
# Compile src/agda/Plan/CIM/PandocShowMdBlock.agda
src/agda/Plan/CIM/PandocShowMdBlock.agdai: src/agda/Plan/CIM/PandocShowMdBlock.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocShowMdBlock.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agda
# Compile src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai: src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agda
# Compile src/agda/Plan/CIM/PriorityProfileExport.agda
src/agda/Plan/CIM/PriorityProfileExport.agdai: src/agda/Plan/CIM/PriorityProfileExport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PriorityProfileExport.agda
# Compile src/agda/Plan/CIM/PlanningExport.agda
src/agda/Plan/CIM/PlanningExport.agdai: src/agda/Plan/CIM/PlanningExport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PlanningExport.agda
# Compile src/agda/Plan/CIM/RoadmapExporter.agda
src/agda/Plan/CIM/RoadmapExporter.agdai: src/agda/Plan/CIM/RoadmapExporter.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapExporter.agda
# Compile src/agda/Plan/CIM/CanonicalRoadmap.agda
src/agda/Plan/CIM/CanonicalRoadmap.agdai: src/agda/Plan/CIM/CanonicalRoadmap.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/CanonicalRoadmap.agda
# Compile src/agda/Plan/CIM/CHIPConformance.agda
src/agda/Plan/CIM/CHIPConformance.agdai: src/agda/Plan/CIM/CHIPConformance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/CHIPConformance.agda
# Compile src/agda/Plan/CIM/FrameworkMetadata.agda
src/agda/Plan/CIM/FrameworkMetadata.agdai: src/agda/Plan/CIM/FrameworkMetadata.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/FrameworkMetadata.agda
# Compile src/agda/Plan/CIM/DependencyGraphExport.agda
src/agda/Plan/CIM/DependencyGraphExport.agdai: src/agda/Plan/CIM/DependencyGraphExport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/DependencyGraphExport.agda
# Compile src/agda/Plan/CIM/ModuleExporter.agda
src/agda/Plan/CIM/ModuleExporter.agdai: src/agda/Plan/CIM/ModuleExporter.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/ModuleExporter.agda
# Compile src/agda/Plan/CIM/RoadmapIndex.agda
src/agda/Plan/CIM/RoadmapIndex.agdai: src/agda/Plan/CIM/RoadmapIndex.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapIndex.agda
# Compile src/agda/Plan/CIM/RoadmapSync.agda
src/agda/Plan/CIM/RoadmapSync.agdai: src/agda/Plan/CIM/RoadmapSync.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSync.agda
# Compile src/agda/Plan/CIM/PandocToMarkdown.agda
src/agda/Plan/CIM/PandocToMarkdown.agdai: src/agda/Plan/CIM/PandocToMarkdown.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocToMarkdown.agda
# Compile src/agda/Plan/CIM/DocumentationContent.agda
src/agda/Plan/CIM/DocumentationContent.agdai: src/agda/Plan/CIM/DocumentationContent.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/DocumentationContent.agda
# Compile src/agda/Plan/CIM/YonedaProfiler.agda
src/agda/Plan/CIM/YonedaProfiler.agdai: src/agda/Plan/CIM/YonedaProfiler.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/YonedaProfiler.agda
# Compile src/agda/Plan/CIM/RoadmapSPPF.agda
src/agda/Plan/CIM/RoadmapSPPF.agdai: src/agda/Plan/CIM/RoadmapSPPF.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPF.agda
# Compile src/agda/Plan/CIM/Ambiguity.agda
src/agda/Plan/CIM/Ambiguity.agdai: src/agda/Plan/CIM/Ambiguity.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/Ambiguity.agda
# Compile src/agda/Plan/CIM/PandocShowInline.agda
src/agda/Plan/CIM/PandocShowInline.agdai: src/agda/Plan/CIM/PandocShowInline.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocShowInline.agda
# Compile src/agda/Plan/CIM/RoadmapSPPFExport.agda
src/agda/Plan/CIM/RoadmapSPPFExport.agdai: src/agda/Plan/CIM/RoadmapSPPFExport.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/RoadmapSPPFExport.agda
# Compile src/agda/Plan/CIM/GPNarrativeDAG.agda
src/agda/Plan/CIM/GPNarrativeDAG.agdai: src/agda/Plan/CIM/GPNarrativeDAG.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/GPNarrativeDAG.agda
# Compile src/agda/Plan/CIM/MarkdownNormalize.agda
src/agda/Plan/CIM/MarkdownNormalize.agdai: src/agda/Plan/CIM/MarkdownNormalize.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/MarkdownNormalize.agda
# Compile src/agda/Plan/CIM/PandocAST.agda
src/agda/Plan/CIM/PandocAST.agdai: src/agda/Plan/CIM/PandocAST.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/PandocAST.agda
# Compile src/agda/Plan/CIM/GrammarBridge.agda
src/agda/Plan/CIM/GrammarBridge.agdai: src/agda/Plan/CIM/GrammarBridge.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/GrammarBridge.agda
# Compile src/agda/Plan/CIM/MarkdownParse.agda
src/agda/Plan/CIM/MarkdownParse.agdai: src/agda/Plan/CIM/MarkdownParse.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Plan/CIM/MarkdownParse.agda
# Compile src/agda/Algebra/Groups/Basic.agda
src/agda/Algebra/Groups/Basic.agdai: src/agda/Algebra/Groups/Basic.agda src/agda/Algebra/Groups/Types.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Basic.agda
# Compile src/agda/Algebra/Groups/Theorems/Classical.agda
src/agda/Algebra/Groups/Theorems/Classical.agdai: src/agda/Algebra/Groups/Theorems/Classical.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Theorems/Classical.agda
# Compile src/agda/Algebra/Groups/ClassicalInstance.agda
src/agda/Algebra/Groups/ClassicalInstance.agdai: src/agda/Algebra/Groups/ClassicalInstance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/ClassicalInstance.agda
# Compile src/agda/Algebra/Groups/Structure.agda
src/agda/Algebra/Groups/Structure.agdai: src/agda/Algebra/Groups/Structure.agda src/agda/Algebra/Groups/Free.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Structure.agda
# Compile src/agda/Algebra/Groups/BasicWithTheorems.agda
src/agda/Algebra/Groups/BasicWithTheorems.agdai: src/agda/Algebra/Groups/BasicWithTheorems.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/BasicWithTheorems.agda
# Compile src/agda/Algebra/Groups/BasicParameterized.agda
src/agda/Algebra/Groups/BasicParameterized.agdai: src/agda/Algebra/Groups/BasicParameterized.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/BasicParameterized.agda
# Compile src/agda/Algebra/Groups/Abelian.agda
src/agda/Algebra/Groups/Abelian.agdai: src/agda/Algebra/Groups/Abelian.agda src/agda/Chapter2/Level2sub1.agdai src/agda/Algebra/Enrichment.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Abelian.agda
# Compile src/agda/Algebra/Groups/Types.agda
src/agda/Algebra/Groups/Types.agdai: src/agda/Algebra/Groups/Types.agda src/agda/Algebra/Foundation.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Types.agda
# Compile src/agda/Algebra/Groups/Free.agda
src/agda/Algebra/Groups/Free.agdai: src/agda/Algebra/Groups/Free.agda src/agda/Algebra/Groups/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Groups/Free.agda
# Compile src/agda/Algebra/Enrichment.agda
src/agda/Algebra/Enrichment.agdai: src/agda/Algebra/Enrichment.agda src/agda/Algebra/Foundation.agdai src/agda/Chapter2/Level2sub6.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Enrichment.agda
# Compile src/agda/Algebra/Rings/Basic.agda
src/agda/Algebra/Rings/Basic.agdai: src/agda/Algebra/Rings/Basic.agda src/agda/Algebra/Rings/Types.agdai src/agda/Chapter2/Level2sub3.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Rings/Basic.agda
# Compile src/agda/Algebra/Rings/Theorems/Classical.agda
src/agda/Algebra/Rings/Theorems/Classical.agdai: src/agda/Algebra/Rings/Theorems/Classical.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Rings/Theorems/Classical.agda
# Compile src/agda/Algebra/Rings/ClassicalInstance.agda
src/agda/Algebra/Rings/ClassicalInstance.agdai: src/agda/Algebra/Rings/ClassicalInstance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Rings/ClassicalInstance.agda
# Compile src/agda/Algebra/Rings/BasicWithTheorems.agda
src/agda/Algebra/Rings/BasicWithTheorems.agdai: src/agda/Algebra/Rings/BasicWithTheorems.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Rings/BasicWithTheorems.agda
# Compile src/agda/Algebra/Rings/Types.agda
src/agda/Algebra/Rings/Types.agdai: src/agda/Algebra/Rings/Types.agda src/agda/Algebra/Groups/Types.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Rings/Types.agda
# Compile src/agda/Algebra/Modules/Basic.agda
src/agda/Algebra/Modules/Basic.agdai: src/agda/Algebra/Modules/Basic.agda src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Modules/Types.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Modules/Basic.agda
# Compile src/agda/Algebra/Modules/Theorems/Classical.agda
src/agda/Algebra/Modules/Theorems/Classical.agdai: src/agda/Algebra/Modules/Theorems/Classical.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Modules/Theorems/Classical.agda
# Compile src/agda/Algebra/Modules/ClassicalInstance.agda
src/agda/Algebra/Modules/ClassicalInstance.agdai: src/agda/Algebra/Modules/ClassicalInstance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Modules/ClassicalInstance.agda
# Compile src/agda/Algebra/Modules/BasicWithTheorems.agda
src/agda/Algebra/Modules/BasicWithTheorems.agdai: src/agda/Algebra/Modules/BasicWithTheorems.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Modules/BasicWithTheorems.agda
# Compile src/agda/Algebra/Modules/Types.agda
src/agda/Algebra/Modules/Types.agdai: src/agda/Algebra/Modules/Types.agda src/agda/Algebra/Fields/Types.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Modules/Types.agda
# Compile src/agda/Algebra/Foundation.agda
src/agda/Algebra/Foundation.agdai: src/agda/Algebra/Foundation.agda src/agda/Chapter1/Level1Index.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Foundation.agda
# Compile src/agda/Algebra/Fields/Basic.agda
src/agda/Algebra/Fields/Basic.agdai: src/agda/Algebra/Fields/Basic.agda src/agda/Algebra/Modules/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/Basic.agda
# Compile src/agda/Algebra/Fields/Advanced.agda
src/agda/Algebra/Fields/Advanced.agdai: src/agda/Algebra/Fields/Advanced.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/Advanced.agda
# Compile src/agda/Algebra/Fields/Theorems/Classical.agda
src/agda/Algebra/Fields/Theorems/Classical.agdai: src/agda/Algebra/Fields/Theorems/Classical.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/Theorems/Classical.agda
# Compile src/agda/Algebra/Fields/ClassicalInstance.agda
src/agda/Algebra/Fields/ClassicalInstance.agdai: src/agda/Algebra/Fields/ClassicalInstance.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/ClassicalInstance.agda
# Compile src/agda/Algebra/Fields/BasicWithTheorems.agda
src/agda/Algebra/Fields/BasicWithTheorems.agdai: src/agda/Algebra/Fields/BasicWithTheorems.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/BasicWithTheorems.agda
# Compile src/agda/Algebra/Fields/Types.agda
src/agda/Algebra/Fields/Types.agdai: src/agda/Algebra/Fields/Types.agda src/agda/Algebra/Rings/Types.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Fields/Types.agda
# Compile src/agda/Algebra/Index.agda
src/agda/Algebra/Index.agdai: src/agda/Algebra/Index.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Algebra/Index.agda
# Compile src/agda/Chapter1/Level1.agda
src/agda/Chapter1/Level1.agdai: src/agda/Chapter1/Level1.agda src/agda/PropertyRegistry.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1.agda
# Compile src/agda/Chapter1/Level1sub8.agda
src/agda/Chapter1/Level1sub8.agdai: src/agda/Chapter1/Level1sub8.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub8.agda
# Compile src/agda/Chapter1/Level1sub7.agda
src/agda/Chapter1/Level1sub7.agdai: src/agda/Chapter1/Level1sub7.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub7.agda
# Compile src/agda/Chapter1/Level1sub3.agda
src/agda/Chapter1/Level1sub3.agdai: src/agda/Chapter1/Level1sub3.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub3.agda
# Compile src/agda/Chapter1/Level1Index.agda
src/agda/Chapter1/Level1Index.agdai: src/agda/Chapter1/Level1Index.agda src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1sub8.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1Index.agda
# Compile src/agda/Chapter1/Level1sub2.agda
src/agda/Chapter1/Level1sub2.agdai: src/agda/Chapter1/Level1sub2.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub2.agda
# Compile src/agda/Chapter1/Level1sub4.agda
src/agda/Chapter1/Level1sub4.agdai: src/agda/Chapter1/Level1sub4.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub4.agda
# Compile src/agda/Chapter1/Level1sub5.agda
src/agda/Chapter1/Level1sub5.agdai: src/agda/Chapter1/Level1sub5.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub5.agda
# Compile src/agda/Chapter1/Level1sub6.agda
src/agda/Chapter1/Level1sub6.agdai: src/agda/Chapter1/Level1sub6.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Chapter1/Level1sub6.agda
# Compile src/agda/PropertyRegistry.agda
src/agda/PropertyRegistry.agdai: src/agda/PropertyRegistry.agda src/agda/Metamodel.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/PropertyRegistry.agda
# Compile src/agda/ExporterMakefile.agda
src/agda/ExporterMakefile.agdai: src/agda/ExporterMakefile.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/ExporterMakefile.agda
# Compile src/agda/Core/AlgorithmComplexity.agda
src/agda/Core/AlgorithmComplexity.agdai: src/agda/Core/AlgorithmComplexity.agda src/agda/Metamodel.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AlgorithmComplexity.agda
# Compile src/agda/Core/ABNF.agda
src/agda/Core/ABNF.agdai: src/agda/Core/ABNF.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/ABNF.agda
# Compile src/agda/Core/TechnicalDebt.agda
src/agda/Core/TechnicalDebt.agdai: src/agda/Core/TechnicalDebt.agda src/agda/Core/Utils.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/TechnicalDebt.agda
# Compile src/agda/Core/Witnesses.agda
src/agda/Core/Witnesses.agdai: src/agda/Core/Witnesses.agda src/agda/Algebra/Fields/Advanced.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Witnesses.agda
# Compile src/agda/Core/BraidTree.agda
src/agda/Core/BraidTree.agdai: src/agda/Core/BraidTree.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/BraidTree.agda
# Compile src/agda/Core/Limitations.agda
src/agda/Core/Limitations.agdai: src/agda/Core/Limitations.agda src/agda/Metamodel.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Limitations.agda
# Compile src/agda/Core/AlgorithmCorrectness.agda
src/agda/Core/AlgorithmCorrectness.agdai: src/agda/Core/AlgorithmCorrectness.agda src/agda/Core/ConstructiveWitnesses.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AlgorithmCorrectness.agda
# Compile src/agda/Core/GrothendieckFibrations.agda
src/agda/Core/GrothendieckFibrations.agdai: src/agda/Core/GrothendieckFibrations.agda src/agda/Algebra/Foundation.agdai src/agda/Chapter2/Level2sub8.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/GrothendieckFibrations.agda
# Compile src/agda/Core/AlgorithmUniversality.agda
src/agda/Core/AlgorithmUniversality.agdai: src/agda/Core/AlgorithmUniversality.agda src/agda/Core/UniversalProperties.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AlgorithmUniversality.agda
# Compile src/agda/Core/AdapterReflection.agda
src/agda/Core/AdapterReflection.agdai: src/agda/Core/AdapterReflection.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AdapterReflection.agda
# Compile src/agda/Core/Phase.agda
src/agda/Core/Phase.agdai: src/agda/Core/Phase.agda src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Coherence/Path2.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Phase.agda
# Compile src/agda/Core/AdapterAutomation.agda
src/agda/Core/AdapterAutomation.agdai: src/agda/Core/AdapterAutomation.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AdapterAutomation.agda
# Compile src/agda/Core/Utils.agda
src/agda/Core/Utils.agdai: src/agda/Core/Utils.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Utils.agda
# Compile src/agda/Core/PhaseCategoryWrapper.agda
src/agda/Core/PhaseCategoryWrapper.agdai: src/agda/Core/PhaseCategoryWrapper.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/PhaseCategoryWrapper.agda
# Compile src/agda/Core/Algorithms/Registry.agda
src/agda/Core/Algorithms/Registry.agdai: src/agda/Core/Algorithms/Registry.agda src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Algorithms/Adapters/BundleAdapter.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/Registry.agda
# Compile src/agda/Core/Algorithms/FunctionFields.agda
src/agda/Core/Algorithms/FunctionFields.agdai: src/agda/Core/Algorithms/FunctionFields.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/FunctionFields.agda
# Compile src/agda/Core/Algorithms/External.agda
src/agda/Core/Algorithms/External.agdai: src/agda/Core/Algorithms/External.agda src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/External.agda
# Compile src/agda/Core/Algorithms/FiniteFields.agda
src/agda/Core/Algorithms/FiniteFields.agdai: src/agda/Core/Algorithms/FiniteFields.agda src/agda/Core/Witnesses.agdai src/agda/Algorithms/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/FiniteFields.agda
# Compile src/agda/Core/Algorithms/AutomaticEvidence.agda
src/agda/Core/Algorithms/AutomaticEvidence.agdai: src/agda/Core/Algorithms/AutomaticEvidence.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/AutomaticEvidence.agda
# Compile src/agda/Core/Algorithms/NumberFields.agda
src/agda/Core/Algorithms/NumberFields.agdai: src/agda/Core/Algorithms/NumberFields.agda src/agda/Core/Algorithms/External.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/NumberFields.agda
# Compile src/agda/Core/Algorithms/Bundle.agda
src/agda/Core/Algorithms/Bundle.agdai: src/agda/Core/Algorithms/Bundle.agda src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/Bundle.agda
# Compile src/agda/Core/Algorithms/InductiveClassification.agda
src/agda/Core/Algorithms/InductiveClassification.agdai: src/agda/Core/Algorithms/InductiveClassification.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Algorithms/InductiveClassification.agda
# Compile src/agda/Core/PhaseCategory.agda
src/agda/Core/PhaseCategory.agdai: src/agda/Core/PhaseCategory.agda src/agda/Core/Phase.agdai src/agda/Infrastructure/Functor/Interface.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/PhaseCategory.agda
# Compile src/agda/Core/Rendering.agda
src/agda/Core/Rendering.agdai: src/agda/Core/Rendering.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Rendering.agda
# Compile src/agda/Core/ConstructiveWitnesses.agda
src/agda/Core/ConstructiveWitnesses.agdai: src/agda/Core/ConstructiveWitnesses.agda src/agda/Core/PolynomialsF2.agdai src/agda/Core/Witnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/ConstructiveWitnesses.agda
# Compile src/agda/Core/UniversalProperties.agda
src/agda/Core/UniversalProperties.agdai: src/agda/Core/UniversalProperties.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/UniversalProperties.agda
# Compile src/agda/Core/IO.agda
src/agda/Core/IO.agdai: src/agda/Core/IO.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/IO.agda
# Compile src/agda/Core/GrowthMetrics.agda
src/agda/Core/GrowthMetrics.agdai: src/agda/Core/GrowthMetrics.agda src/agda/Core/Utils.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/GrowthMetrics.agda
# Compile src/agda/Core/AlgebraicAlgorithms.agda
src/agda/Core/AlgebraicAlgorithms.agdai: src/agda/Core/AlgebraicAlgorithms.agda src/agda/Algebra/Groups/Basic.agdai src/agda/Core/Limitations.agdai src/agda/Algebra/Fields/Advanced.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/AlgebraicAlgorithms.agda
# Compile src/agda/Core/Yoneda.agda
src/agda/Core/Yoneda.agdai: src/agda/Core/Yoneda.agda src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Fields/Basic.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Yoneda.agda
# Compile src/agda/Core/GodelBoundary.agda
src/agda/Core/GodelBoundary.agdai: src/agda/Core/GodelBoundary.agda src/agda/Core.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/GodelBoundary.agda
# Compile src/agda/Core/Strings.agda
src/agda/Core/Strings.agdai: src/agda/Core/Strings.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/Strings.agda
# Compile src/agda/Core/PathAggregator.agda
src/agda/Core/PathAggregator.agdai: src/agda/Core/PathAggregator.agda src/agda/Core/GrowthMetrics.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/PathAggregator.agda
# Compile src/agda/Core/PolynomialsF2.agda
src/agda/Core/PolynomialsF2.agdai: src/agda/Core/PolynomialsF2.agda src/agda/Core/Phase.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/PolynomialsF2.agda
# Compile src/agda/Core/CategoricalAdapter.agda
src/agda/Core/CategoricalAdapter.agdai: src/agda/Core/CategoricalAdapter.agda src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Coherence/Path2.agdai
	$(AGDA) $(AGDA_FLAGS) src/agda/Core/CategoricalAdapter.agda
# Compile src/agda/MinimalTest.agda
src/agda/MinimalTest.agdai: src/agda/MinimalTest.agda
	$(AGDA) $(AGDA_FLAGS) src/agda/MinimalTest.agda
# Compile all Agda modules
agda-all: src/agda/MetaScan.agdai src/agda/Metamodel.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Index_PhaseII.agdai src/agda/Tests/HierarchyValidation.agdai src/agda/Tests/PhaseExamples.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/AlgorithmCompositionTestsMinimal.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/ErrorAsSpecificationTests.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/SpecificationValidation.agdai src/agda/Tests/AlgorithmSmokeTests.agdai src/agda/Tests/CoverageReport.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/GroupsAbelianChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/WarningAggregatorsTest.agdai src/agda/Tests/Index.agdai src/agda/Tests/ObligationAdapters.agdai src/agda/Tests/ToposObligationAdapters.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Chapter2/Level2sub3.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2Index.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3Index.agdai src/agda/Infrastructure/Arity/BinTree.agdai src/agda/Infrastructure/Universe.agdai src/agda/Infrastructure/Iso/Structural.agdai src/agda/Infrastructure/Functor/Compose.agdai src/agda/Infrastructure/Functor/Interface.agdai src/agda/Infrastructure/Functor/Adapters/Funext.agdai src/agda/Infrastructure/Functor/Instances/TransformationSystem.agdai src/agda/Infrastructure/Functor/Instances/Trivial.agdai src/agda/Infrastructure/Functor/Instances/PhaseCategory.agdai src/agda/Infrastructure/Functor/Instances/PathAlgebra.agdai src/agda/Infrastructure/Functor/Instances/FunctionCategory.agdai src/agda/Infrastructure/Functor/Instances/Ambiguity.agdai src/agda/Infrastructure/Functor/Instances/FunctionPathCategory.agdai src/agda/Infrastructure/Coherence/Path2.agdai src/agda/Infrastructure/Equality.agdai src/agda/Infrastructure/Polytopes/Braiding/HexagonTriangulation.agdai src/agda/Infrastructure/Polytopes/Associahedron/Triangulation.agdai src/agda/Infrastructure/Polytopes/Associahedron.agdai src/agda/Infrastructure/Definitions/Dictionary.agdai src/agda/Infrastructure/Product/Bundle4.agdai src/agda/Infrastructure/Index.agdai src/agda/Infrastructure/Axiom/Movie.agdai src/agda/Infrastructure/Axiom/PentagonFromTriangles.agdai src/agda/Infrastructure/Axiom/Adequacy.agdai src/agda/Infrastructure/Axiom/Solver.agdai src/agda/Infrastructure/Axiom/Face.agdai src/agda/Infrastructure/Axiom/SolvableInterface.agdai src/agda/Infrastructure/Axiom/Instances/HexagonBraiding.agdai src/agda/Infrastructure/Axiom/Instances/PentagonAssociahedron.agdai src/agda/Infrastructure/Axiom/Instance.agdai src/agda/Infrastructure/Wrapper/With.agdai src/agda/Algorithms/Instrumented.agdai src/agda/Algorithms/Basic.agdai src/agda/Algorithms/Adapters/BundleAdapter.agdai src/agda/Algorithms/TestInstances.agdai src/agda/MinimalAUDAXInlineTest.agdai src/agda/Core.agdai src/agda/TechnicalDebt/PriorityMapping.agdai src/agda/TechnicalDebt/PriorityOrchestrationFFI.agdai src/agda/TechnicalDebt/PriorityFormatting.agdai src/agda/TechnicalDebt/DeferredItemsOrchestrationFFI.agdai src/agda/TechnicalDebt/DeferredItemsDetection.agdai src/agda/TechnicalDebt/AlgorithmCompositionRegistry.agdai src/agda/TechnicalDebt/PriorityOrchestration.agdai src/agda/TechnicalDebt/DeferredItemsFormatting.agdai src/agda/TechnicalDebt/DeferredItemsFormatting_AUDAX.agdai src/agda/TechnicalDebt/Priorities.agdai src/agda/Examples/RealWorldAlgorithms.agdai src/agda/Examples/TechnicalDebtExample.agdai src/agda/Examples/AmbiguityExamples.agdai src/agda/Examples/FunctionField/F2x.agdai src/agda/Examples/AlgorithmCorrectnessExamples.agdai src/agda/Examples/NumberField/Sqrt2.agdai src/agda/Examples/FiniteField/GF8.agdai src/agda/Examples/InstrumentedAlgorithmDemo.agdai src/agda/Examples/DeferredItemsScanner.agdai src/agda/Examples/TransformationSystemExamples.agdai src/agda/Examples/AutomaticEvidenceDemo.agdai src/agda/Examples/ConstructiveWitnessExamples.agdai src/agda/Examples/AgdaMakefileDeps.agdai src/agda/Examples/LazyHybridDemo.agdai src/agda/Examples/TechnicalDebtRegistry.agdai src/agda/Examples/RoadmapIssueSync.agdai src/agda/Examples/MakefileTargets.agdai src/agda/Examples/PhaseCategoryExamples.agdai src/agda/Examples/EqualityExamples.agdai src/agda/Examples/TechnicalDebtChecklist.agdai src/agda/Examples/AgdaFileScanFFI.agdai src/agda/Examples/ExporterMakefile.agdai src/agda/Markdown/ExportProof.agdai src/agda/Markdown/Normalization.agdai src/agda/Docs/MetaIndex.agdai src/agda/GrowthAnalysis.agdai src/agda/Plan/CIM/PolytopeExpansion.agdai src/agda/Plan/CIM/PlanningKernel.agdai src/agda/Plan/CIM/PandocProofExample.agdai src/agda/Plan/CIM/PandocProtocols.agdai src/agda/Plan/CIM/PandocShowBlock.agdai src/agda/Plan/CIM/CHIPCoreRecompose.agdai src/agda/Plan/CIM/PandocProofExport.agdai src/agda/Plan/CIM/Utility.agdai src/agda/Plan/CIM/IngestedRoadmaps.agdai src/agda/Plan/CIM/Structure.agdai src/agda/Plan/CIM/DocumentSynthesis.agdai src/agda/Plan/CIM/CHIPRecomposed.agdai src/agda/Plan/CIM/PandocShowMdBlock.agdai src/agda/Plan/CIM/IngestedRoadmaps/Corrections.agdai src/agda/Plan/CIM/IngestedRoadmaps/Analysis.agdai src/agda/Plan/CIM/IngestedRoadmaps/Polytopes.agdai src/agda/Plan/CIM/IngestedRoadmaps/Foundation.agdai src/agda/Plan/CIM/IngestedRoadmaps/Geometry.agdai src/agda/Plan/CIM/PriorityProfileExport.agdai src/agda/Plan/CIM/PlanningExport.agdai src/agda/Plan/CIM/RoadmapExporter.agdai src/agda/Plan/CIM/CanonicalRoadmap.agdai src/agda/Plan/CIM/CHIPConformance.agdai src/agda/Plan/CIM/FrameworkMetadata.agdai src/agda/Plan/CIM/DependencyGraphExport.agdai src/agda/Plan/CIM/ModuleExporter.agdai src/agda/Plan/CIM/RoadmapIndex.agdai src/agda/Plan/CIM/RoadmapSync.agdai src/agda/Plan/CIM/PandocToMarkdown.agdai src/agda/Plan/CIM/DocumentationContent.agdai src/agda/Plan/CIM/YonedaProfiler.agdai src/agda/Plan/CIM/RoadmapSPPF.agdai src/agda/Plan/CIM/Ambiguity.agdai src/agda/Plan/CIM/PandocShowInline.agdai src/agda/Plan/CIM/RoadmapSPPFExport.agdai src/agda/Plan/CIM/GPNarrativeDAG.agdai src/agda/Plan/CIM/MarkdownNormalize.agdai src/agda/Plan/CIM/PandocAST.agdai src/agda/Plan/CIM/GrammarBridge.agdai src/agda/Plan/CIM/MarkdownParse.agdai src/agda/Algebra/Groups/Basic.agdai src/agda/Algebra/Groups/Theorems/Classical.agdai src/agda/Algebra/Groups/ClassicalInstance.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Algebra/Groups/BasicWithTheorems.agdai src/agda/Algebra/Groups/BasicParameterized.agdai src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Groups/Types.agdai src/agda/Algebra/Groups/Free.agdai src/agda/Algebra/Enrichment.agdai src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Rings/Theorems/Classical.agdai src/agda/Algebra/Rings/ClassicalInstance.agdai src/agda/Algebra/Rings/BasicWithTheorems.agdai src/agda/Algebra/Rings/Types.agdai src/agda/Algebra/Modules/Basic.agdai src/agda/Algebra/Modules/Theorems/Classical.agdai src/agda/Algebra/Modules/ClassicalInstance.agdai src/agda/Algebra/Modules/BasicWithTheorems.agdai src/agda/Algebra/Modules/Types.agdai src/agda/Algebra/Foundation.agdai src/agda/Algebra/Fields/Basic.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Algebra/Fields/Theorems/Classical.agdai src/agda/Algebra/Fields/ClassicalInstance.agdai src/agda/Algebra/Fields/BasicWithTheorems.agdai src/agda/Algebra/Fields/Types.agdai src/agda/Algebra/Index.agdai src/agda/Chapter1/Level1.agdai src/agda/Chapter1/Level1sub8.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1Index.agdai src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/PropertyRegistry.agdai src/agda/ExporterMakefile.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/ABNF.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/Witnesses.agdai src/agda/Core/BraidTree.agdai src/agda/Core/Limitations.agdai src/agda/Core/AlgorithmCorrectness.agdai src/agda/Core/GrothendieckFibrations.agdai src/agda/Core/AlgorithmUniversality.agdai src/agda/Core/AdapterReflection.agdai src/agda/Core/Phase.agdai src/agda/Core/AdapterAutomation.agdai src/agda/Core/Utils.agdai src/agda/Core/PhaseCategoryWrapper.agdai src/agda/Core/Algorithms/Registry.agdai src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/External.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Core/Algorithms/AutomaticEvidence.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Algorithms/InductiveClassification.agdai src/agda/Core/PhaseCategory.agdai src/agda/Core/Rendering.agdai src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/IO.agdai src/agda/Core/GrowthMetrics.agdai src/agda/Core/AlgebraicAlgorithms.agdai src/agda/Core/Yoneda.agdai src/agda/Core/GodelBoundary.agdai src/agda/Core/Strings.agdai src/agda/Core/PathAggregator.agdai src/agda/Core/PolynomialsF2.agdai src/agda/Core/CategoricalAdapter.agdai src/agda/MinimalTest.agdai
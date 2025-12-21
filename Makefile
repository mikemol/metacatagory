# Use local Agda 2.8.0
AGDA := .local/agda

SHELL := /bin/bash

# Ensure nvm-provisioned node/npm/npx are available inside make recipes
NVM_DIR ?= $(HOME)/.nvm
NODE_BIN ?= $(shell ls -1d $(NVM_DIR)/versions/node/*/bin 2>/dev/null | sort -V | tail -1)
NPM ?= $(NODE_BIN)/npm
NPX ?= $(NODE_BIN)/npx
PATH := $(NODE_BIN):$(PATH)

regen-makefile:
	$(AGDA) -i src/agda --compile --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda && ./src/agda/ExporterMakefile
	cp Makefile.generated Makefile

docs: src/agda/RoadmapExporter
	./src/agda/RoadmapExporter

src/agda/RoadmapExporter: src/agda/Plan/CIM/RoadmapExporter.agda src/agda/Plan/CIM/Utility.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type --compile src/agda/Plan/CIM/RoadmapExporter.agda

src/agda/MarkdownNormalize: src/agda/Plan/CIM/MarkdownNormalize.agda src/agda/Plan/CIM/MarkdownParse.agda src/agda/Plan/CIM/PandocAST.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type --compile src/agda/Plan/CIM/MarkdownNormalize.agda

.PHONY: all check md-fix md-lint badges node-deps regen-makefile agda-all docs-all docs
md-lint: src/agda/MarkdownNormalize
	@mkdir -p build/reports
	find . -name "*.md" -not -path "./node_modules/*" -print0 | xargs -0 ./src/agda/MarkdownNormalize
	"$(NPX)" remark . --quiet --frail > build/reports/md-lint.txt
	"$(NPX)" markdownlint-cli2 "**/*.md" "#node_modules" > build/reports/markdownlint.txt

.PHONY: md-normalize-review
md-normalize-review: src/agda/MarkdownNormalize scripts/md_normalize_review.sh
	chmod +x scripts/md_normalize_review.sh
	./scripts/md_normalize_review.sh
badges: build/reports/test-results.json
	python3 scripts/generate-badges.py
node-deps: 
	"$(NPM)" install
src/agda/MetaScan.agdai: src/agda/MetaScan.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/MetaScan.agda
src/agda/Metamodel.agdai: src/agda/Metamodel.agda src/agda/Core/Phase.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Metamodel.agda
src/agda/Tests/PropertyRegistryTests.agdai: src/agda/Tests/PropertyRegistryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PropertyRegistryTests.agda
src/agda/Tests/AbelianCategoriesChecklist.agdai: src/agda/Tests/AbelianCategoriesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AbelianCategoriesChecklist.agda
src/agda/Tests/DispatchBehaviorTests.agdai: src/agda/Tests/DispatchBehaviorTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/DispatchBehaviorTests.agda
src/agda/Tests/WitnessConstructionTests.agdai: src/agda/Tests/WitnessConstructionTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/WitnessConstructionTests.agda
src/agda/Tests/VectorSpaceChecklist.agdai: src/agda/Tests/VectorSpaceChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/VectorSpaceChecklist.agda
src/agda/Tests/ProofObligationStatus.agdai: src/agda/Tests/ProofObligationStatus.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ProofObligationStatus.agda
src/agda/Tests/SerializationTests.agdai: src/agda/Tests/SerializationTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SerializationTests.agda
src/agda/Tests/GodelBoundaryTests.agdai: src/agda/Tests/GodelBoundaryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GodelBoundaryTests.agda
src/agda/Tests/ModuleStructureChecklist.agdai: src/agda/Tests/ModuleStructureChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModuleStructureChecklist.agda
src/agda/Tests/ToposTheoryChecklist.agdai: src/agda/Tests/ToposTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ToposTheoryChecklist.agda
src/agda/Tests/GrothendieckFibrationsChecklist.agdai: src/agda/Tests/GrothendieckFibrationsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GrothendieckFibrationsChecklist.agda
src/agda/Tests/TensorProductChecklist.agdai: src/agda/Tests/TensorProductChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/TensorProductChecklist.agda
src/agda/Tests/Index_PhaseII.agdai: src/agda/Tests/Index_PhaseII.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Index_PhaseII.agda
src/agda/Tests/HierarchyValidation.agdai: src/agda/Tests/HierarchyValidation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/HierarchyValidation.agda
src/agda/Tests/PhaseExamples.agdai: src/agda/Tests/PhaseExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PhaseExamples.agda
src/agda/Tests/RegularCategoriesChecklist.agdai: src/agda/Tests/RegularCategoriesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RegularCategoriesChecklist.agda
src/agda/Tests/MonadAdjunctionChecklist.agdai: src/agda/Tests/MonadAdjunctionChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/MonadAdjunctionChecklist.agda
src/agda/Tests/UniversalPropertyTests.agdai: src/agda/Tests/UniversalPropertyTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/UniversalPropertyTests.agda
src/agda/Tests/EnrichmentChecklist.agdai: src/agda/Tests/EnrichmentChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/EnrichmentChecklist.agda
src/agda/Tests/AdvancedPhaseExamples.agdai: src/agda/Tests/AdvancedPhaseExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedPhaseExamples.agda
src/agda/Tests/ErrorAsSpecificationTests.agdai: src/agda/Tests/ErrorAsSpecificationTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ErrorAsSpecificationTests.agda
src/agda/Tests/RealWorldAlgorithmsTests.agdai: src/agda/Tests/RealWorldAlgorithmsTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RealWorldAlgorithmsTests.agda
src/agda/Tests/PolynomialExtensionsChecklist.agdai: src/agda/Tests/PolynomialExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PolynomialExtensionsChecklist.agda
src/agda/Tests/ModuleTheoryChecklist.agdai: src/agda/Tests/ModuleTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModuleTheoryChecklist.agda
src/agda/Tests/PhaseCategoryExamplesRunner.agdai: src/agda/Tests/PhaseCategoryExamplesRunner.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PhaseCategoryExamplesRunner.agda
src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai: src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
src/agda/Tests/AlgorithmCompositionTests.agdai: src/agda/Tests/AlgorithmCompositionTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgorithmCompositionTests.agda
src/agda/Tests/FieldsBasicChecklist.agdai: src/agda/Tests/FieldsBasicChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/FieldsBasicChecklist.agda
src/agda/Tests/SpecificationValidation.agdai: src/agda/Tests/SpecificationValidation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SpecificationValidation.agda
src/agda/Tests/AlgorithmSmokeTests.agdai: src/agda/Tests/AlgorithmSmokeTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgorithmSmokeTests.agda
src/agda/Tests/CoverageReport.agdai: src/agda/Tests/CoverageReport.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/CoverageReport.agda
src/agda/Tests/GroupsFreeChecklist.agdai: src/agda/Tests/GroupsFreeChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsFreeChecklist.agda
src/agda/Tests/RingsBasicChecklist.agdai: src/agda/Tests/RingsBasicChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/RingsBasicChecklist.agda
src/agda/Tests/Chapter2Checklist.agdai: src/agda/Tests/Chapter2Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter2Checklist.agda
src/agda/Tests/KanExtensionsChecklist.agdai: src/agda/Tests/KanExtensionsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/KanExtensionsChecklist.agda
src/agda/Tests/LimitsColimitsChecklist.agdai: src/agda/Tests/LimitsColimitsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/LimitsColimitsChecklist.agda
src/agda/Tests/AdvancedMonadTheoryChecklist.agdai: src/agda/Tests/AdvancedMonadTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedMonadTheoryChecklist.agda
src/agda/Tests/CoreUniversalPropertiesChecklist.agdai: src/agda/Tests/CoreUniversalPropertiesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/CoreUniversalPropertiesChecklist.agda
src/agda/Tests/ChapterObligationsSmoke.agdai: src/agda/Tests/ChapterObligationsSmoke.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ChapterObligationsSmoke.agda
src/agda/Tests/ConstructiveWitnessTests.agdai: src/agda/Tests/ConstructiveWitnessTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ConstructiveWitnessTests.agda
src/agda/Tests/PerformanceBoundaryTests.agdai: src/agda/Tests/PerformanceBoundaryTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PerformanceBoundaryTests.agda
src/agda/Tests/PathAggregatorTests.agdai: src/agda/Tests/PathAggregatorTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/PathAggregatorTests.agda
src/agda/Tests/GroupsAbelianChecklist.agdai: src/agda/Tests/GroupsAbelianChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsAbelianChecklist.agda
src/agda/Tests/Chapter1Checklist.agdai: src/agda/Tests/Chapter1Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter1Checklist.agda
src/agda/Tests/AdvancedFieldsChecklist.agdai: src/agda/Tests/AdvancedFieldsChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AdvancedFieldsChecklist.agda
src/agda/Tests/WarningAggregatorsTest.agdai: src/agda/Tests/WarningAggregatorsTest.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/WarningAggregatorsTest.agda
src/agda/Tests/Index.agdai: src/agda/Tests/Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Index.agda
src/agda/Tests/ObligationAdapters.agdai: src/agda/Tests/ObligationAdapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ObligationAdapters.agda
src/agda/Tests/ToposObligationAdapters.agdai: src/agda/Tests/ToposObligationAdapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ToposObligationAdapters.agda
src/agda/Tests/Chapters.agdai: src/agda/Tests/Chapters.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapters.agda
src/agda/Tests/YonedaChecklist.agdai: src/agda/Tests/YonedaChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/YonedaChecklist.agda
src/agda/Tests/AlgebraicCompletionChecklist.agdai: src/agda/Tests/AlgebraicCompletionChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgebraicCompletionChecklist.agda
src/agda/Tests/FunctorPropertiesChecklist.agdai: src/agda/Tests/FunctorPropertiesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/FunctorPropertiesChecklist.agda
src/agda/Tests/AlgebraChecklist.agdai: src/agda/Tests/AlgebraChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/AlgebraChecklist.agda
src/agda/Tests/ErrorHandlingTests.agdai: src/agda/Tests/ErrorHandlingTests.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ErrorHandlingTests.agda
src/agda/Tests/GroupsStructureChecklist.agdai: src/agda/Tests/GroupsStructureChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/GroupsStructureChecklist.agda
src/agda/Tests/ModulesChecklist.agdai: src/agda/Tests/ModulesChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/ModulesChecklist.agda
src/agda/Tests/Chapter3Checklist.agdai: src/agda/Tests/Chapter3Checklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/Chapter3Checklist.agda
src/agda/Tests/SubobjectTheoryChecklist.agdai: src/agda/Tests/SubobjectTheoryChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Tests/SubobjectTheoryChecklist.agda
src/agda/Chapter2/Level2sub3.agdai: src/agda/Chapter2/Level2sub3.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub3.agda
src/agda/Chapter2/Level2sub6.agdai: src/agda/Chapter2/Level2sub6.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub6.agda
src/agda/Chapter2/Level2sub5.agdai: src/agda/Chapter2/Level2sub5.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub5.agda
src/agda/Chapter2/Level2sub8.agdai: src/agda/Chapter2/Level2sub8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub8.agda
src/agda/Chapter2/Level2sub7.agdai: src/agda/Chapter2/Level2sub7.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub7.agda
src/agda/Chapter2/Level2sub2.agdai: src/agda/Chapter2/Level2sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub2.agda
src/agda/Chapter2/Level2sub1.agdai: src/agda/Chapter2/Level2sub1.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub1.agda
src/agda/Chapter2/Level2sub4.agdai: src/agda/Chapter2/Level2sub4.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2sub4.agda
src/agda/Chapter2/Level2Index.agdai: src/agda/Chapter2/Level2Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter2/Level2Index.agda
src/agda/Chapter3/Level3sub1.agdai: src/agda/Chapter3/Level3sub1.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3sub1.agda
src/agda/Chapter3/Level3sub2.agdai: src/agda/Chapter3/Level3sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3sub2.agda
src/agda/Chapter3/Level3Index.agdai: src/agda/Chapter3/Level3Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter3/Level3Index.agda
src/agda/Core.agdai: src/agda/Core.agda src/agda/Chapter1/Level1.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core.agda
src/agda/Examples/RealWorldAlgorithms.agdai: src/agda/Examples/RealWorldAlgorithms.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/RealWorldAlgorithms.agda
src/agda/Examples/TechnicalDebtExample.agdai: src/agda/Examples/TechnicalDebtExample.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtExample.agda
src/agda/Examples/FunctionField/F2x.agdai: src/agda/Examples/FunctionField/F2x.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/FunctionField/F2x.agda
src/agda/Examples/AlgorithmCorrectnessExamples.agdai: src/agda/Examples/AlgorithmCorrectnessExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AlgorithmCorrectnessExamples.agda
src/agda/Examples/NumberField/Sqrt2.agdai: src/agda/Examples/NumberField/Sqrt2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/NumberField/Sqrt2.agda
src/agda/Examples/FiniteField/GF8.agdai: src/agda/Examples/FiniteField/GF8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/FiniteField/GF8.agda
src/agda/Examples/AutomaticEvidenceDemo.agdai: src/agda/Examples/AutomaticEvidenceDemo.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AutomaticEvidenceDemo.agda
src/agda/Examples/ConstructiveWitnessExamples.agdai: src/agda/Examples/ConstructiveWitnessExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/ConstructiveWitnessExamples.agda
src/agda/Examples/AgdaMakefileDeps.agdai: src/agda/Examples/AgdaMakefileDeps.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AgdaMakefileDeps.agda
src/agda/Examples/LazyHybridDemo.agdai: src/agda/Examples/LazyHybridDemo.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/LazyHybridDemo.agda
src/agda/Examples/TechnicalDebtRegistry.agdai: src/agda/Examples/TechnicalDebtRegistry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtRegistry.agda
src/agda/Examples/MakefileTargets.agdai: src/agda/Examples/MakefileTargets.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/MakefileTargets.agda
src/agda/Examples/PhaseCategoryExamples.agdai: src/agda/Examples/PhaseCategoryExamples.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/PhaseCategoryExamples.agda
src/agda/Examples/TechnicalDebtChecklist.agdai: src/agda/Examples/TechnicalDebtChecklist.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/TechnicalDebtChecklist.agda
src/agda/Examples/AgdaFileScanFFI.agdai: src/agda/Examples/AgdaFileScanFFI.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/AgdaFileScanFFI.agda
src/agda/Examples/ExporterMakefile.agdai: src/agda/Examples/ExporterMakefile.agda src/agda/Examples/AgdaMakefileDeps.agdai src/agda/Examples/AgdaFileScanFFI.agdai src/agda/Core/Utils.agdai src/agda/Core/AdapterAutomation.agdai src/agda/Examples/MakefileTargets.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Examples/ExporterMakefile.agda
src/agda/Markdown/ExportProof.agdai: src/agda/Markdown/ExportProof.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Markdown/ExportProof.agda
src/agda/Markdown/Normalization.agdai: src/agda/Markdown/Normalization.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Markdown/Normalization.agda
src/agda/Plan/CIM/PandocProofExample.agdai: src/agda/Plan/CIM/PandocProofExample.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProofExample.agda
src/agda/Plan/CIM/PandocProtocols.agdai: src/agda/Plan/CIM/PandocProtocols.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProtocols.agda
src/agda/Plan/CIM/CHIPCoreRecompose.agdai: src/agda/Plan/CIM/CHIPCoreRecompose.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPCoreRecompose.agda
src/agda/Plan/CIM/PandocProofExport.agdai: src/agda/Plan/CIM/PandocProofExport.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocProofExport.agda
src/agda/Plan/CIM/Utility.agdai: src/agda/Plan/CIM/Utility.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/Utility.agda
src/agda/Plan/CIM/Structure.agdai: src/agda/Plan/CIM/Structure.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/Structure.agda
src/agda/Plan/CIM/CHIPRecomposed.agdai: src/agda/Plan/CIM/CHIPRecomposed.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPRecomposed.agda
src/agda/Plan/CIM/CHIPConformance.agdai: src/agda/Plan/CIM/CHIPConformance.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/CHIPConformance.agda
src/agda/Plan/CIM/PandocToMarkdown.agdai: src/agda/Plan/CIM/PandocToMarkdown.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocToMarkdown.agda
src/agda/Plan/CIM/PandocAST.agdai: src/agda/Plan/CIM/PandocAST.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/PandocAST.agda
src/agda/Plan/CIM/GrammarBridge.agdai: src/agda/Plan/CIM/GrammarBridge.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Plan/CIM/GrammarBridge.agda
src/agda/Algebra/Groups/Basic.agdai: src/agda/Algebra/Groups/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Basic.agda
src/agda/Algebra/Groups/Structure.agdai: src/agda/Algebra/Groups/Structure.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Structure.agda
src/agda/Algebra/Groups/Abelian.agdai: src/agda/Algebra/Groups/Abelian.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Abelian.agda
src/agda/Algebra/Groups/Free.agdai: src/agda/Algebra/Groups/Free.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Groups/Free.agda
src/agda/Algebra/Enrichment.agdai: src/agda/Algebra/Enrichment.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Enrichment.agda
src/agda/Algebra/Rings/Basic.agdai: src/agda/Algebra/Rings/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Rings/Basic.agda
src/agda/Algebra/Modules/Basic.agdai: src/agda/Algebra/Modules/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Modules/Basic.agda
src/agda/Algebra/Foundation.agdai: src/agda/Algebra/Foundation.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Foundation.agda
src/agda/Algebra/Fields/Basic.agdai: src/agda/Algebra/Fields/Basic.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Basic.agda
src/agda/Algebra/Fields/Advanced.agdai: src/agda/Algebra/Fields/Advanced.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Fields/Advanced.agda
src/agda/Algebra/Index.agdai: src/agda/Algebra/Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Algebra/Index.agda
src/agda/Chapter1/Level1.agdai: src/agda/Chapter1/Level1.agda src/agda/PropertyRegistry.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1.agda
src/agda/Chapter1/Level1sub8.agdai: src/agda/Chapter1/Level1sub8.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub8.agda
src/agda/Chapter1/Level1sub7.agdai: src/agda/Chapter1/Level1sub7.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub7.agda
src/agda/Chapter1/Level1sub3.agdai: src/agda/Chapter1/Level1sub3.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub3.agda
src/agda/Chapter1/Level1Index.agdai: src/agda/Chapter1/Level1Index.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1Index.agda
src/agda/Chapter1/Level1sub2.agdai: src/agda/Chapter1/Level1sub2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub2.agda
src/agda/Chapter1/Level1sub4.agdai: src/agda/Chapter1/Level1sub4.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub4.agda
src/agda/Chapter1/Level1sub5.agdai: src/agda/Chapter1/Level1sub5.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub5.agda
src/agda/Chapter1/Level1sub6.agdai: src/agda/Chapter1/Level1sub6.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Chapter1/Level1sub6.agda
src/agda/PropertyRegistry.agdai: src/agda/PropertyRegistry.agda src/agda/Metamodel.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/PropertyRegistry.agda
src/agda/ExporterMakefile.agdai: src/agda/ExporterMakefile.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/ExporterMakefile.agda
src/agda/Core/AlgorithmComplexity.agdai: src/agda/Core/AlgorithmComplexity.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmComplexity.agda
src/agda/Core/ABNF.agdai: src/agda/Core/ABNF.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/ABNF.agda
src/agda/Core/TechnicalDebt.agdai: src/agda/Core/TechnicalDebt.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/TechnicalDebt.agda
src/agda/Core/Witnesses.agdai: src/agda/Core/Witnesses.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Witnesses.agda
src/agda/Core/BraidTree.agdai: src/agda/Core/BraidTree.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/BraidTree.agda
src/agda/Core/Limitations.agdai: src/agda/Core/Limitations.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Limitations.agda
src/agda/Core/AlgorithmCorrectness.agdai: src/agda/Core/AlgorithmCorrectness.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmCorrectness.agda
src/agda/Core/GrothendieckFibrations.agdai: src/agda/Core/GrothendieckFibrations.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GrothendieckFibrations.agda
src/agda/Core/AlgorithmUniversality.agdai: src/agda/Core/AlgorithmUniversality.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgorithmUniversality.agda
src/agda/Core/AdapterReflection.agdai: src/agda/Core/AdapterReflection.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AdapterReflection.agda
src/agda/Core/Phase.agdai: src/agda/Core/Phase.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Phase.agda
src/agda/Core/AdapterAutomation.agdai: src/agda/Core/AdapterAutomation.agda src/agda/Core/CategoricalAdapter.agdai src/agda/Core/Phase.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AdapterAutomation.agda
src/agda/Core/Utils.agdai: src/agda/Core/Utils.agda src/agda/Core.agdai
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Utils.agda
src/agda/Core/Algorithms/Registry.agdai: src/agda/Core/Algorithms/Registry.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/Registry.agda
src/agda/Core/Algorithms/FunctionFields.agdai: src/agda/Core/Algorithms/FunctionFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/FunctionFields.agda
src/agda/Core/Algorithms/External.agdai: src/agda/Core/Algorithms/External.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/External.agda
src/agda/Core/Algorithms/FiniteFields.agdai: src/agda/Core/Algorithms/FiniteFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/FiniteFields.agda
src/agda/Core/Algorithms/AutomaticEvidence.agdai: src/agda/Core/Algorithms/AutomaticEvidence.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/AutomaticEvidence.agda
src/agda/Core/Algorithms/NumberFields.agdai: src/agda/Core/Algorithms/NumberFields.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/NumberFields.agda
src/agda/Core/Algorithms/Bundle.agdai: src/agda/Core/Algorithms/Bundle.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/Bundle.agda
src/agda/Core/Algorithms/InductiveClassification.agdai: src/agda/Core/Algorithms/InductiveClassification.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Algorithms/InductiveClassification.agda
src/agda/Core/PhaseCategory.agdai: src/agda/Core/PhaseCategory.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PhaseCategory.agda
src/agda/Core/ConstructiveWitnesses.agdai: src/agda/Core/ConstructiveWitnesses.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/ConstructiveWitnesses.agda
src/agda/Core/UniversalProperties.agdai: src/agda/Core/UniversalProperties.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/UniversalProperties.agda
src/agda/Core/GrowthMetrics.agdai: src/agda/Core/GrowthMetrics.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GrowthMetrics.agda
src/agda/Core/AlgebraicAlgorithms.agdai: src/agda/Core/AlgebraicAlgorithms.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/AlgebraicAlgorithms.agda
src/agda/Core/Yoneda.agdai: src/agda/Core/Yoneda.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/Yoneda.agda
src/agda/Core/GodelBoundary.agdai: src/agda/Core/GodelBoundary.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/GodelBoundary.agda
src/agda/Core/PathAggregator.agdai: src/agda/Core/PathAggregator.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PathAggregator.agda
src/agda/Core/PolynomialsF2.agdai: src/agda/Core/PolynomialsF2.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/PolynomialsF2.agda
src/agda/Core/CategoricalAdapter.agdai: src/agda/Core/CategoricalAdapter.agda
	$(AGDA) -i src/agda --ghc-flag=-Wno-star-is-type src/agda/Core/CategoricalAdapter.agda
build/html/src/agda/MetaScan.agda.html: src/agda/MetaScan.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/MetaScan.agda
build/html/src/agda/Metamodel.agda.html: src/agda/Metamodel.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Metamodel.agda
build/html/src/agda/Tests/PropertyRegistryTests.agda.html: src/agda/Tests/PropertyRegistryTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PropertyRegistryTests.agda
build/html/src/agda/Tests/AbelianCategoriesChecklist.agda.html: src/agda/Tests/AbelianCategoriesChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AbelianCategoriesChecklist.agda
build/html/src/agda/Tests/DispatchBehaviorTests.agda.html: src/agda/Tests/DispatchBehaviorTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/DispatchBehaviorTests.agda
build/html/src/agda/Tests/WitnessConstructionTests.agda.html: src/agda/Tests/WitnessConstructionTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/WitnessConstructionTests.agda
build/html/src/agda/Tests/VectorSpaceChecklist.agda.html: src/agda/Tests/VectorSpaceChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/VectorSpaceChecklist.agda
build/html/src/agda/Tests/ProofObligationStatus.agda.html: src/agda/Tests/ProofObligationStatus.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ProofObligationStatus.agda
build/html/src/agda/Tests/SerializationTests.agda.html: src/agda/Tests/SerializationTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/SerializationTests.agda
build/html/src/agda/Tests/GodelBoundaryTests.agda.html: src/agda/Tests/GodelBoundaryTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/GodelBoundaryTests.agda
build/html/src/agda/Tests/ModuleStructureChecklist.agda.html: src/agda/Tests/ModuleStructureChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ModuleStructureChecklist.agda
build/html/src/agda/Tests/ToposTheoryChecklist.agda.html: src/agda/Tests/ToposTheoryChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ToposTheoryChecklist.agda
build/html/src/agda/Tests/GrothendieckFibrationsChecklist.agda.html: src/agda/Tests/GrothendieckFibrationsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/GrothendieckFibrationsChecklist.agda
build/html/src/agda/Tests/TensorProductChecklist.agda.html: src/agda/Tests/TensorProductChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/TensorProductChecklist.agda
build/html/src/agda/Tests/Index_PhaseII.agda.html: src/agda/Tests/Index_PhaseII.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Index_PhaseII.agda
build/html/src/agda/Tests/HierarchyValidation.agda.html: src/agda/Tests/HierarchyValidation.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/HierarchyValidation.agda
build/html/src/agda/Tests/PhaseExamples.agda.html: src/agda/Tests/PhaseExamples.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PhaseExamples.agda
build/html/src/agda/Tests/RegularCategoriesChecklist.agda.html: src/agda/Tests/RegularCategoriesChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/RegularCategoriesChecklist.agda
build/html/src/agda/Tests/MonadAdjunctionChecklist.agda.html: src/agda/Tests/MonadAdjunctionChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/MonadAdjunctionChecklist.agda
build/html/src/agda/Tests/UniversalPropertyTests.agda.html: src/agda/Tests/UniversalPropertyTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/UniversalPropertyTests.agda
build/html/src/agda/Tests/EnrichmentChecklist.agda.html: src/agda/Tests/EnrichmentChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/EnrichmentChecklist.agda
build/html/src/agda/Tests/AdvancedPhaseExamples.agda.html: src/agda/Tests/AdvancedPhaseExamples.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AdvancedPhaseExamples.agda
build/html/src/agda/Tests/ErrorAsSpecificationTests.agda.html: src/agda/Tests/ErrorAsSpecificationTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ErrorAsSpecificationTests.agda
build/html/src/agda/Tests/RealWorldAlgorithmsTests.agda.html: src/agda/Tests/RealWorldAlgorithmsTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/RealWorldAlgorithmsTests.agda
build/html/src/agda/Tests/PolynomialExtensionsChecklist.agda.html: src/agda/Tests/PolynomialExtensionsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PolynomialExtensionsChecklist.agda
build/html/src/agda/Tests/ModuleTheoryChecklist.agda.html: src/agda/Tests/ModuleTheoryChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ModuleTheoryChecklist.agda
build/html/src/agda/Tests/PhaseCategoryExamplesRunner.agda.html: src/agda/Tests/PhaseCategoryExamplesRunner.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PhaseCategoryExamplesRunner.agda
build/html/src/agda/Tests/PolynomialFieldExtensionsChecklist.agda.html: src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PolynomialFieldExtensionsChecklist.agda
build/html/src/agda/Tests/AlgorithmCompositionTests.agda.html: src/agda/Tests/AlgorithmCompositionTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AlgorithmCompositionTests.agda
build/html/src/agda/Tests/FieldsBasicChecklist.agda.html: src/agda/Tests/FieldsBasicChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/FieldsBasicChecklist.agda
build/html/src/agda/Tests/SpecificationValidation.agda.html: src/agda/Tests/SpecificationValidation.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/SpecificationValidation.agda
build/html/src/agda/Tests/AlgorithmSmokeTests.agda.html: src/agda/Tests/AlgorithmSmokeTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AlgorithmSmokeTests.agda
build/html/src/agda/Tests/CoverageReport.agda.html: src/agda/Tests/CoverageReport.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/CoverageReport.agda
build/html/src/agda/Tests/GroupsFreeChecklist.agda.html: src/agda/Tests/GroupsFreeChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/GroupsFreeChecklist.agda
build/html/src/agda/Tests/RingsBasicChecklist.agda.html: src/agda/Tests/RingsBasicChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/RingsBasicChecklist.agda
build/html/src/agda/Tests/Chapter2Checklist.agda.html: src/agda/Tests/Chapter2Checklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Chapter2Checklist.agda
build/html/src/agda/Tests/KanExtensionsChecklist.agda.html: src/agda/Tests/KanExtensionsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/KanExtensionsChecklist.agda
build/html/src/agda/Tests/LimitsColimitsChecklist.agda.html: src/agda/Tests/LimitsColimitsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/LimitsColimitsChecklist.agda
build/html/src/agda/Tests/AdvancedMonadTheoryChecklist.agda.html: src/agda/Tests/AdvancedMonadTheoryChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AdvancedMonadTheoryChecklist.agda
build/html/src/agda/Tests/CoreUniversalPropertiesChecklist.agda.html: src/agda/Tests/CoreUniversalPropertiesChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/CoreUniversalPropertiesChecklist.agda
build/html/src/agda/Tests/ChapterObligationsSmoke.agda.html: src/agda/Tests/ChapterObligationsSmoke.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ChapterObligationsSmoke.agda
build/html/src/agda/Tests/ConstructiveWitnessTests.agda.html: src/agda/Tests/ConstructiveWitnessTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ConstructiveWitnessTests.agda
build/html/src/agda/Tests/PerformanceBoundaryTests.agda.html: src/agda/Tests/PerformanceBoundaryTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PerformanceBoundaryTests.agda
build/html/src/agda/Tests/PathAggregatorTests.agda.html: src/agda/Tests/PathAggregatorTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/PathAggregatorTests.agda
build/html/src/agda/Tests/GroupsAbelianChecklist.agda.html: src/agda/Tests/GroupsAbelianChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/GroupsAbelianChecklist.agda
build/html/src/agda/Tests/Chapter1Checklist.agda.html: src/agda/Tests/Chapter1Checklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Chapter1Checklist.agda
build/html/src/agda/Tests/AdvancedFieldsChecklist.agda.html: src/agda/Tests/AdvancedFieldsChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AdvancedFieldsChecklist.agda
build/html/src/agda/Tests/WarningAggregatorsTest.agda.html: src/agda/Tests/WarningAggregatorsTest.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/WarningAggregatorsTest.agda
build/html/src/agda/Tests/Index.agda.html: src/agda/Tests/Index.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Index.agda
build/html/src/agda/Tests/ObligationAdapters.agda.html: src/agda/Tests/ObligationAdapters.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ObligationAdapters.agda
build/html/src/agda/Tests/ToposObligationAdapters.agda.html: src/agda/Tests/ToposObligationAdapters.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ToposObligationAdapters.agda
build/html/src/agda/Tests/Chapters.agda.html: src/agda/Tests/Chapters.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Chapters.agda
build/html/src/agda/Tests/YonedaChecklist.agda.html: src/agda/Tests/YonedaChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/YonedaChecklist.agda
build/html/src/agda/Tests/AlgebraicCompletionChecklist.agda.html: src/agda/Tests/AlgebraicCompletionChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AlgebraicCompletionChecklist.agda
build/html/src/agda/Tests/FunctorPropertiesChecklist.agda.html: src/agda/Tests/FunctorPropertiesChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/FunctorPropertiesChecklist.agda
build/html/src/agda/Tests/AlgebraChecklist.agda.html: src/agda/Tests/AlgebraChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/AlgebraChecklist.agda
build/html/src/agda/Tests/ErrorHandlingTests.agda.html: src/agda/Tests/ErrorHandlingTests.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ErrorHandlingTests.agda
build/html/src/agda/Tests/GroupsStructureChecklist.agda.html: src/agda/Tests/GroupsStructureChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/GroupsStructureChecklist.agda
build/html/src/agda/Tests/ModulesChecklist.agda.html: src/agda/Tests/ModulesChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/ModulesChecklist.agda
build/html/src/agda/Tests/Chapter3Checklist.agda.html: src/agda/Tests/Chapter3Checklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/Chapter3Checklist.agda
build/html/src/agda/Tests/SubobjectTheoryChecklist.agda.html: src/agda/Tests/SubobjectTheoryChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Tests/SubobjectTheoryChecklist.agda
build/html/src/agda/Chapter2/Level2sub3.agda.html: src/agda/Chapter2/Level2sub3.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub3.agda
build/html/src/agda/Chapter2/Level2sub6.agda.html: src/agda/Chapter2/Level2sub6.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub6.agda
build/html/src/agda/Chapter2/Level2sub5.agda.html: src/agda/Chapter2/Level2sub5.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub5.agda
build/html/src/agda/Chapter2/Level2sub8.agda.html: src/agda/Chapter2/Level2sub8.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub8.agda
build/html/src/agda/Chapter2/Level2sub7.agda.html: src/agda/Chapter2/Level2sub7.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub7.agda
build/html/src/agda/Chapter2/Level2sub2.agda.html: src/agda/Chapter2/Level2sub2.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub2.agda
build/html/src/agda/Chapter2/Level2sub1.agda.html: src/agda/Chapter2/Level2sub1.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub1.agda
build/html/src/agda/Chapter2/Level2sub4.agda.html: src/agda/Chapter2/Level2sub4.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2sub4.agda
build/html/src/agda/Chapter2/Level2Index.agda.html: src/agda/Chapter2/Level2Index.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter2/Level2Index.agda
build/html/src/agda/Chapter3/Level3sub1.agda.html: src/agda/Chapter3/Level3sub1.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter3/Level3sub1.agda
build/html/src/agda/Chapter3/Level3sub2.agda.html: src/agda/Chapter3/Level3sub2.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter3/Level3sub2.agda
build/html/src/agda/Chapter3/Level3Index.agda.html: src/agda/Chapter3/Level3Index.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter3/Level3Index.agda
build/html/src/agda/Core.agda.html: src/agda/Core.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core.agda
build/html/src/agda/Examples/RealWorldAlgorithms.agda.html: src/agda/Examples/RealWorldAlgorithms.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/RealWorldAlgorithms.agda
build/html/src/agda/Examples/TechnicalDebtExample.agda.html: src/agda/Examples/TechnicalDebtExample.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/TechnicalDebtExample.agda
build/html/src/agda/Examples/FunctionField/F2x.agda.html: src/agda/Examples/FunctionField/F2x.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/FunctionField/F2x.agda
build/html/src/agda/Examples/AlgorithmCorrectnessExamples.agda.html: src/agda/Examples/AlgorithmCorrectnessExamples.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/AlgorithmCorrectnessExamples.agda
build/html/src/agda/Examples/NumberField/Sqrt2.agda.html: src/agda/Examples/NumberField/Sqrt2.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/NumberField/Sqrt2.agda
build/html/src/agda/Examples/FiniteField/GF8.agda.html: src/agda/Examples/FiniteField/GF8.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/FiniteField/GF8.agda
build/html/src/agda/Examples/AutomaticEvidenceDemo.agda.html: src/agda/Examples/AutomaticEvidenceDemo.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/AutomaticEvidenceDemo.agda
build/html/src/agda/Examples/ConstructiveWitnessExamples.agda.html: src/agda/Examples/ConstructiveWitnessExamples.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/ConstructiveWitnessExamples.agda
build/html/src/agda/Examples/AgdaMakefileDeps.agda.html: src/agda/Examples/AgdaMakefileDeps.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/AgdaMakefileDeps.agda
build/html/src/agda/Examples/LazyHybridDemo.agda.html: src/agda/Examples/LazyHybridDemo.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/LazyHybridDemo.agda
build/html/src/agda/Examples/TechnicalDebtRegistry.agda.html: src/agda/Examples/TechnicalDebtRegistry.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/TechnicalDebtRegistry.agda
build/html/src/agda/Examples/MakefileTargets.agda.html: src/agda/Examples/MakefileTargets.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/MakefileTargets.agda
build/html/src/agda/Examples/PhaseCategoryExamples.agda.html: src/agda/Examples/PhaseCategoryExamples.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/PhaseCategoryExamples.agda
build/html/src/agda/Examples/TechnicalDebtChecklist.agda.html: src/agda/Examples/TechnicalDebtChecklist.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/TechnicalDebtChecklist.agda
build/html/src/agda/Examples/AgdaFileScanFFI.agda.html: src/agda/Examples/AgdaFileScanFFI.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/AgdaFileScanFFI.agda
build/html/src/agda/Examples/ExporterMakefile.agda.html: src/agda/Examples/ExporterMakefile.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Examples/ExporterMakefile.agda
build/html/src/agda/Markdown/ExportProof.agda.html: src/agda/Markdown/ExportProof.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Markdown/ExportProof.agda
build/html/src/agda/Markdown/Normalization.agda.html: src/agda/Markdown/Normalization.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Markdown/Normalization.agda
build/html/src/agda/Plan/CIM/PandocProofExample.agda.html: src/agda/Plan/CIM/PandocProofExample.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/PandocProofExample.agda
build/html/src/agda/Plan/CIM/PandocProtocols.agda.html: src/agda/Plan/CIM/PandocProtocols.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/PandocProtocols.agda
build/html/src/agda/Plan/CIM/CHIPCoreRecompose.agda.html: src/agda/Plan/CIM/CHIPCoreRecompose.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/CHIPCoreRecompose.agda
build/html/src/agda/Plan/CIM/PandocProofExport.agda.html: src/agda/Plan/CIM/PandocProofExport.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/PandocProofExport.agda
build/html/src/agda/Plan/CIM/Utility.agda.html: src/agda/Plan/CIM/Utility.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/Utility.agda
build/html/src/agda/Plan/CIM/Structure.agda.html: src/agda/Plan/CIM/Structure.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/Structure.agda
build/html/src/agda/Plan/CIM/CHIPRecomposed.agda.html: src/agda/Plan/CIM/CHIPRecomposed.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/CHIPRecomposed.agda
build/html/src/agda/Plan/CIM/CHIPConformance.agda.html: src/agda/Plan/CIM/CHIPConformance.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/CHIPConformance.agda
build/html/src/agda/Plan/CIM/PandocToMarkdown.agda.html: src/agda/Plan/CIM/PandocToMarkdown.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/PandocToMarkdown.agda
build/html/src/agda/Plan/CIM/PandocAST.agda.html: src/agda/Plan/CIM/PandocAST.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/PandocAST.agda
build/html/src/agda/Plan/CIM/GrammarBridge.agda.html: src/agda/Plan/CIM/GrammarBridge.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Plan/CIM/GrammarBridge.agda
build/html/src/agda/Algebra/Groups/Basic.agda.html: src/agda/Algebra/Groups/Basic.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Groups/Basic.agda
build/html/src/agda/Algebra/Groups/Structure.agda.html: src/agda/Algebra/Groups/Structure.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Groups/Structure.agda
build/html/src/agda/Algebra/Groups/Abelian.agda.html: src/agda/Algebra/Groups/Abelian.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Groups/Abelian.agda
build/html/src/agda/Algebra/Groups/Free.agda.html: src/agda/Algebra/Groups/Free.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Groups/Free.agda
build/html/src/agda/Algebra/Enrichment.agda.html: src/agda/Algebra/Enrichment.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Enrichment.agda
build/html/src/agda/Algebra/Rings/Basic.agda.html: src/agda/Algebra/Rings/Basic.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Rings/Basic.agda
build/html/src/agda/Algebra/Modules/Basic.agda.html: src/agda/Algebra/Modules/Basic.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Modules/Basic.agda
build/html/src/agda/Algebra/Foundation.agda.html: src/agda/Algebra/Foundation.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Foundation.agda
build/html/src/agda/Algebra/Fields/Basic.agda.html: src/agda/Algebra/Fields/Basic.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Fields/Basic.agda
build/html/src/agda/Algebra/Fields/Advanced.agda.html: src/agda/Algebra/Fields/Advanced.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Fields/Advanced.agda
build/html/src/agda/Algebra/Index.agda.html: src/agda/Algebra/Index.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Algebra/Index.agda
build/html/src/agda/Chapter1/Level1.agda.html: src/agda/Chapter1/Level1.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1.agda
build/html/src/agda/Chapter1/Level1sub8.agda.html: src/agda/Chapter1/Level1sub8.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub8.agda
build/html/src/agda/Chapter1/Level1sub7.agda.html: src/agda/Chapter1/Level1sub7.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub7.agda
build/html/src/agda/Chapter1/Level1sub3.agda.html: src/agda/Chapter1/Level1sub3.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub3.agda
build/html/src/agda/Chapter1/Level1Index.agda.html: src/agda/Chapter1/Level1Index.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1Index.agda
build/html/src/agda/Chapter1/Level1sub2.agda.html: src/agda/Chapter1/Level1sub2.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub2.agda
build/html/src/agda/Chapter1/Level1sub4.agda.html: src/agda/Chapter1/Level1sub4.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub4.agda
build/html/src/agda/Chapter1/Level1sub5.agda.html: src/agda/Chapter1/Level1sub5.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub5.agda
build/html/src/agda/Chapter1/Level1sub6.agda.html: src/agda/Chapter1/Level1sub6.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Chapter1/Level1sub6.agda
build/html/src/agda/PropertyRegistry.agda.html: src/agda/PropertyRegistry.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/PropertyRegistry.agda
build/html/src/agda/ExporterMakefile.agda.html: src/agda/ExporterMakefile.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/ExporterMakefile.agda
build/html/src/agda/Core/AlgorithmComplexity.agda.html: src/agda/Core/AlgorithmComplexity.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AlgorithmComplexity.agda
build/html/src/agda/Core/ABNF.agda.html: src/agda/Core/ABNF.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/ABNF.agda
build/html/src/agda/Core/TechnicalDebt.agda.html: src/agda/Core/TechnicalDebt.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/TechnicalDebt.agda
build/html/src/agda/Core/Witnesses.agda.html: src/agda/Core/Witnesses.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Witnesses.agda
build/html/src/agda/Core/BraidTree.agda.html: src/agda/Core/BraidTree.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/BraidTree.agda
build/html/src/agda/Core/Limitations.agda.html: src/agda/Core/Limitations.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Limitations.agda
build/html/src/agda/Core/AlgorithmCorrectness.agda.html: src/agda/Core/AlgorithmCorrectness.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AlgorithmCorrectness.agda
build/html/src/agda/Core/GrothendieckFibrations.agda.html: src/agda/Core/GrothendieckFibrations.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/GrothendieckFibrations.agda
build/html/src/agda/Core/AlgorithmUniversality.agda.html: src/agda/Core/AlgorithmUniversality.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AlgorithmUniversality.agda
build/html/src/agda/Core/AdapterReflection.agda.html: src/agda/Core/AdapterReflection.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AdapterReflection.agda
build/html/src/agda/Core/Phase.agda.html: src/agda/Core/Phase.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Phase.agda
build/html/src/agda/Core/AdapterAutomation.agda.html: src/agda/Core/AdapterAutomation.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AdapterAutomation.agda
build/html/src/agda/Core/Utils.agda.html: src/agda/Core/Utils.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Utils.agda
build/html/src/agda/Core/Algorithms/Registry.agda.html: src/agda/Core/Algorithms/Registry.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/Registry.agda
build/html/src/agda/Core/Algorithms/FunctionFields.agda.html: src/agda/Core/Algorithms/FunctionFields.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/FunctionFields.agda
build/html/src/agda/Core/Algorithms/External.agda.html: src/agda/Core/Algorithms/External.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/External.agda
build/html/src/agda/Core/Algorithms/FiniteFields.agda.html: src/agda/Core/Algorithms/FiniteFields.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/FiniteFields.agda
build/html/src/agda/Core/Algorithms/AutomaticEvidence.agda.html: src/agda/Core/Algorithms/AutomaticEvidence.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/AutomaticEvidence.agda
build/html/src/agda/Core/Algorithms/NumberFields.agda.html: src/agda/Core/Algorithms/NumberFields.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/NumberFields.agda
build/html/src/agda/Core/Algorithms/Bundle.agda.html: src/agda/Core/Algorithms/Bundle.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/Bundle.agda
build/html/src/agda/Core/Algorithms/InductiveClassification.agda.html: src/agda/Core/Algorithms/InductiveClassification.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Algorithms/InductiveClassification.agda
build/html/src/agda/Core/PhaseCategory.agda.html: src/agda/Core/PhaseCategory.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/PhaseCategory.agda
build/html/src/agda/Core/ConstructiveWitnesses.agda.html: src/agda/Core/ConstructiveWitnesses.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/ConstructiveWitnesses.agda
build/html/src/agda/Core/UniversalProperties.agda.html: src/agda/Core/UniversalProperties.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/UniversalProperties.agda
build/html/src/agda/Core/GrowthMetrics.agda.html: src/agda/Core/GrowthMetrics.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/GrowthMetrics.agda
build/html/src/agda/Core/AlgebraicAlgorithms.agda.html: src/agda/Core/AlgebraicAlgorithms.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/AlgebraicAlgorithms.agda
build/html/src/agda/Core/Yoneda.agda.html: src/agda/Core/Yoneda.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/Yoneda.agda
build/html/src/agda/Core/GodelBoundary.agda.html: src/agda/Core/GodelBoundary.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/GodelBoundary.agda
build/html/src/agda/Core/PathAggregator.agda.html: src/agda/Core/PathAggregator.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/PathAggregator.agda
build/html/src/agda/Core/PolynomialsF2.agda.html: src/agda/Core/PolynomialsF2.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/PolynomialsF2.agda
build/html/src/agda/Core/CategoricalAdapter.agda.html: src/agda/Core/CategoricalAdapter.agdai
	$(AGDA) --html --html-dir=build/html -i src/agda src/agda/Core/CategoricalAdapter.agda
agda-all: src/agda/MetaScan.agdai src/agda/Metamodel.agdai src/agda/Tests/PropertyRegistryTests.agdai src/agda/Tests/AbelianCategoriesChecklist.agdai src/agda/Tests/DispatchBehaviorTests.agdai src/agda/Tests/WitnessConstructionTests.agdai src/agda/Tests/VectorSpaceChecklist.agdai src/agda/Tests/ProofObligationStatus.agdai src/agda/Tests/SerializationTests.agdai src/agda/Tests/GodelBoundaryTests.agdai src/agda/Tests/ModuleStructureChecklist.agdai src/agda/Tests/ToposTheoryChecklist.agdai src/agda/Tests/GrothendieckFibrationsChecklist.agdai src/agda/Tests/TensorProductChecklist.agdai src/agda/Tests/Index_PhaseII.agdai src/agda/Tests/HierarchyValidation.agdai src/agda/Tests/PhaseExamples.agdai src/agda/Tests/RegularCategoriesChecklist.agdai src/agda/Tests/MonadAdjunctionChecklist.agdai src/agda/Tests/UniversalPropertyTests.agdai src/agda/Tests/EnrichmentChecklist.agdai src/agda/Tests/AdvancedPhaseExamples.agdai src/agda/Tests/ErrorAsSpecificationTests.agdai src/agda/Tests/RealWorldAlgorithmsTests.agdai src/agda/Tests/PolynomialExtensionsChecklist.agdai src/agda/Tests/ModuleTheoryChecklist.agdai src/agda/Tests/PhaseCategoryExamplesRunner.agdai src/agda/Tests/PolynomialFieldExtensionsChecklist.agdai src/agda/Tests/AlgorithmCompositionTests.agdai src/agda/Tests/FieldsBasicChecklist.agdai src/agda/Tests/SpecificationValidation.agdai src/agda/Tests/AlgorithmSmokeTests.agdai src/agda/Tests/CoverageReport.agdai src/agda/Tests/GroupsFreeChecklist.agdai src/agda/Tests/RingsBasicChecklist.agdai src/agda/Tests/Chapter2Checklist.agdai src/agda/Tests/KanExtensionsChecklist.agdai src/agda/Tests/LimitsColimitsChecklist.agdai src/agda/Tests/AdvancedMonadTheoryChecklist.agdai src/agda/Tests/CoreUniversalPropertiesChecklist.agdai src/agda/Tests/ChapterObligationsSmoke.agdai src/agda/Tests/ConstructiveWitnessTests.agdai src/agda/Tests/PerformanceBoundaryTests.agdai src/agda/Tests/PathAggregatorTests.agdai src/agda/Tests/GroupsAbelianChecklist.agdai src/agda/Tests/Chapter1Checklist.agdai src/agda/Tests/AdvancedFieldsChecklist.agdai src/agda/Tests/WarningAggregatorsTest.agdai src/agda/Tests/Index.agdai src/agda/Tests/ObligationAdapters.agdai src/agda/Tests/ToposObligationAdapters.agdai src/agda/Tests/Chapters.agdai src/agda/Tests/YonedaChecklist.agdai src/agda/Tests/AlgebraicCompletionChecklist.agdai src/agda/Tests/FunctorPropertiesChecklist.agdai src/agda/Tests/AlgebraChecklist.agdai src/agda/Tests/ErrorHandlingTests.agdai src/agda/Tests/GroupsStructureChecklist.agdai src/agda/Tests/ModulesChecklist.agdai src/agda/Tests/Chapter3Checklist.agdai src/agda/Tests/SubobjectTheoryChecklist.agdai src/agda/Chapter2/Level2sub3.agdai src/agda/Chapter2/Level2sub6.agdai src/agda/Chapter2/Level2sub5.agdai src/agda/Chapter2/Level2sub8.agdai src/agda/Chapter2/Level2sub7.agdai src/agda/Chapter2/Level2sub2.agdai src/agda/Chapter2/Level2sub1.agdai src/agda/Chapter2/Level2sub4.agdai src/agda/Chapter2/Level2Index.agdai src/agda/Chapter3/Level3sub1.agdai src/agda/Chapter3/Level3sub2.agdai src/agda/Chapter3/Level3Index.agdai src/agda/Core.agdai src/agda/Examples/RealWorldAlgorithms.agdai src/agda/Examples/TechnicalDebtExample.agdai src/agda/Examples/FunctionField/F2x.agdai src/agda/Examples/AlgorithmCorrectnessExamples.agdai src/agda/Examples/NumberField/Sqrt2.agdai src/agda/Examples/FiniteField/GF8.agdai src/agda/Examples/AutomaticEvidenceDemo.agdai src/agda/Examples/ConstructiveWitnessExamples.agdai src/agda/Examples/AgdaMakefileDeps.agdai src/agda/Examples/LazyHybridDemo.agdai src/agda/Examples/TechnicalDebtRegistry.agdai src/agda/Examples/MakefileTargets.agdai src/agda/Examples/PhaseCategoryExamples.agdai src/agda/Examples/TechnicalDebtChecklist.agdai src/agda/Examples/AgdaFileScanFFI.agdai src/agda/Examples/ExporterMakefile.agdai src/agda/Markdown/ExportProof.agdai src/agda/Markdown/Normalization.agdai src/agda/Plan/CIM/PandocProofExample.agdai src/agda/Plan/CIM/PandocProtocols.agdai src/agda/Plan/CIM/CHIPCoreRecompose.agdai src/agda/Plan/CIM/PandocProofExport.agdai src/agda/Plan/CIM/Utility.agdai src/agda/Plan/CIM/Structure.agdai src/agda/Plan/CIM/CHIPRecomposed.agdai src/agda/Plan/CIM/CHIPConformance.agdai src/agda/Plan/CIM/PandocToMarkdown.agdai src/agda/Plan/CIM/PandocAST.agdai src/agda/Plan/CIM/GrammarBridge.agdai src/agda/Algebra/Groups/Basic.agdai src/agda/Algebra/Groups/Structure.agdai src/agda/Algebra/Groups/Abelian.agdai src/agda/Algebra/Groups/Free.agdai src/agda/Algebra/Enrichment.agdai src/agda/Algebra/Rings/Basic.agdai src/agda/Algebra/Modules/Basic.agdai src/agda/Algebra/Foundation.agdai src/agda/Algebra/Fields/Basic.agdai src/agda/Algebra/Fields/Advanced.agdai src/agda/Algebra/Index.agdai src/agda/Chapter1/Level1.agdai src/agda/Chapter1/Level1sub8.agdai src/agda/Chapter1/Level1sub7.agdai src/agda/Chapter1/Level1sub3.agdai src/agda/Chapter1/Level1Index.agdai src/agda/Chapter1/Level1sub2.agdai src/agda/Chapter1/Level1sub4.agdai src/agda/Chapter1/Level1sub5.agdai src/agda/Chapter1/Level1sub6.agdai src/agda/PropertyRegistry.agdai src/agda/ExporterMakefile.agdai src/agda/Core/AlgorithmComplexity.agdai src/agda/Core/ABNF.agdai src/agda/Core/TechnicalDebt.agdai src/agda/Core/Witnesses.agdai src/agda/Core/BraidTree.agdai src/agda/Core/Limitations.agdai src/agda/Core/AlgorithmCorrectness.agdai src/agda/Core/GrothendieckFibrations.agdai src/agda/Core/AlgorithmUniversality.agdai src/agda/Core/AdapterReflection.agdai src/agda/Core/Phase.agdai src/agda/Core/AdapterAutomation.agdai src/agda/Core/Utils.agdai src/agda/Core/Algorithms/Registry.agdai src/agda/Core/Algorithms/FunctionFields.agdai src/agda/Core/Algorithms/External.agdai src/agda/Core/Algorithms/FiniteFields.agdai src/agda/Core/Algorithms/AutomaticEvidence.agdai src/agda/Core/Algorithms/NumberFields.agdai src/agda/Core/Algorithms/Bundle.agdai src/agda/Core/Algorithms/InductiveClassification.agdai src/agda/Core/PhaseCategory.agdai src/agda/Core/ConstructiveWitnesses.agdai src/agda/Core/UniversalProperties.agdai src/agda/Core/GrowthMetrics.agdai src/agda/Core/AlgebraicAlgorithms.agdai src/agda/Core/Yoneda.agdai src/agda/Core/GodelBoundary.agdai src/agda/Core/PathAggregator.agdai src/agda/Core/PolynomialsF2.agdai src/agda/Core/CategoricalAdapter.agdai
docs-all: build/html/src/agda/MetaScan.agda.html build/html/src/agda/Metamodel.agda.html build/html/src/agda/Tests/PropertyRegistryTests.agda.html build/html/src/agda/Tests/AbelianCategoriesChecklist.agda.html build/html/src/agda/Tests/DispatchBehaviorTests.agda.html build/html/src/agda/Tests/WitnessConstructionTests.agda.html build/html/src/agda/Tests/VectorSpaceChecklist.agda.html build/html/src/agda/Tests/ProofObligationStatus.agda.html build/html/src/agda/Tests/SerializationTests.agda.html build/html/src/agda/Tests/GodelBoundaryTests.agda.html build/html/src/agda/Tests/ModuleStructureChecklist.agda.html build/html/src/agda/Tests/ToposTheoryChecklist.agda.html build/html/src/agda/Tests/GrothendieckFibrationsChecklist.agda.html build/html/src/agda/Tests/TensorProductChecklist.agda.html build/html/src/agda/Tests/Index_PhaseII.agda.html build/html/src/agda/Tests/HierarchyValidation.agda.html build/html/src/agda/Tests/PhaseExamples.agda.html build/html/src/agda/Tests/RegularCategoriesChecklist.agda.html build/html/src/agda/Tests/MonadAdjunctionChecklist.agda.html build/html/src/agda/Tests/UniversalPropertyTests.agda.html build/html/src/agda/Tests/EnrichmentChecklist.agda.html build/html/src/agda/Tests/AdvancedPhaseExamples.agda.html build/html/src/agda/Tests/ErrorAsSpecificationTests.agda.html build/html/src/agda/Tests/RealWorldAlgorithmsTests.agda.html build/html/src/agda/Tests/PolynomialExtensionsChecklist.agda.html build/html/src/agda/Tests/ModuleTheoryChecklist.agda.html build/html/src/agda/Tests/PhaseCategoryExamplesRunner.agda.html build/html/src/agda/Tests/PolynomialFieldExtensionsChecklist.agda.html build/html/src/agda/Tests/AlgorithmCompositionTests.agda.html build/html/src/agda/Tests/FieldsBasicChecklist.agda.html build/html/src/agda/Tests/SpecificationValidation.agda.html build/html/src/agda/Tests/AlgorithmSmokeTests.agda.html build/html/src/agda/Tests/CoverageReport.agda.html build/html/src/agda/Tests/GroupsFreeChecklist.agda.html build/html/src/agda/Tests/RingsBasicChecklist.agda.html build/html/src/agda/Tests/Chapter2Checklist.agda.html build/html/src/agda/Tests/KanExtensionsChecklist.agda.html build/html/src/agda/Tests/LimitsColimitsChecklist.agda.html build/html/src/agda/Tests/AdvancedMonadTheoryChecklist.agda.html build/html/src/agda/Tests/CoreUniversalPropertiesChecklist.agda.html build/html/src/agda/Tests/ChapterObligationsSmoke.agda.html build/html/src/agda/Tests/ConstructiveWitnessTests.agda.html build/html/src/agda/Tests/PerformanceBoundaryTests.agda.html build/html/src/agda/Tests/PathAggregatorTests.agda.html build/html/src/agda/Tests/GroupsAbelianChecklist.agda.html build/html/src/agda/Tests/Chapter1Checklist.agda.html build/html/src/agda/Tests/AdvancedFieldsChecklist.agda.html build/html/src/agda/Tests/WarningAggregatorsTest.agda.html build/html/src/agda/Tests/Index.agda.html build/html/src/agda/Tests/ObligationAdapters.agda.html build/html/src/agda/Tests/ToposObligationAdapters.agda.html build/html/src/agda/Tests/Chapters.agda.html build/html/src/agda/Tests/YonedaChecklist.agda.html build/html/src/agda/Tests/AlgebraicCompletionChecklist.agda.html build/html/src/agda/Tests/FunctorPropertiesChecklist.agda.html build/html/src/agda/Tests/AlgebraChecklist.agda.html build/html/src/agda/Tests/ErrorHandlingTests.agda.html build/html/src/agda/Tests/GroupsStructureChecklist.agda.html build/html/src/agda/Tests/ModulesChecklist.agda.html build/html/src/agda/Tests/Chapter3Checklist.agda.html build/html/src/agda/Tests/SubobjectTheoryChecklist.agda.html build/html/src/agda/Chapter2/Level2sub3.agda.html build/html/src/agda/Chapter2/Level2sub6.agda.html build/html/src/agda/Chapter2/Level2sub5.agda.html build/html/src/agda/Chapter2/Level2sub8.agda.html build/html/src/agda/Chapter2/Level2sub7.agda.html build/html/src/agda/Chapter2/Level2sub2.agda.html build/html/src/agda/Chapter2/Level2sub1.agda.html build/html/src/agda/Chapter2/Level2sub4.agda.html build/html/src/agda/Chapter2/Level2Index.agda.html build/html/src/agda/Chapter3/Level3sub1.agda.html build/html/src/agda/Chapter3/Level3sub2.agda.html build/html/src/agda/Chapter3/Level3Index.agda.html build/html/src/agda/Core.agda.html build/html/src/agda/Examples/RealWorldAlgorithms.agda.html build/html/src/agda/Examples/TechnicalDebtExample.agda.html build/html/src/agda/Examples/FunctionField/F2x.agda.html build/html/src/agda/Examples/AlgorithmCorrectnessExamples.agda.html build/html/src/agda/Examples/NumberField/Sqrt2.agda.html build/html/src/agda/Examples/FiniteField/GF8.agda.html build/html/src/agda/Examples/AutomaticEvidenceDemo.agda.html build/html/src/agda/Examples/ConstructiveWitnessExamples.agda.html build/html/src/agda/Examples/AgdaMakefileDeps.agda.html build/html/src/agda/Examples/LazyHybridDemo.agda.html build/html/src/agda/Examples/TechnicalDebtRegistry.agda.html build/html/src/agda/Examples/MakefileTargets.agda.html build/html/src/agda/Examples/PhaseCategoryExamples.agda.html build/html/src/agda/Examples/TechnicalDebtChecklist.agda.html build/html/src/agda/Examples/AgdaFileScanFFI.agda.html build/html/src/agda/Examples/ExporterMakefile.agda.html build/html/src/agda/Markdown/ExportProof.agda.html build/html/src/agda/Markdown/Normalization.agda.html build/html/src/agda/Plan/CIM/PandocProofExample.agda.html build/html/src/agda/Plan/CIM/PandocProtocols.agda.html build/html/src/agda/Plan/CIM/CHIPCoreRecompose.agda.html build/html/src/agda/Plan/CIM/PandocProofExport.agda.html build/html/src/agda/Plan/CIM/Utility.agda.html build/html/src/agda/Plan/CIM/Structure.agda.html build/html/src/agda/Plan/CIM/CHIPRecomposed.agda.html build/html/src/agda/Plan/CIM/CHIPConformance.agda.html build/html/src/agda/Plan/CIM/PandocToMarkdown.agda.html build/html/src/agda/Plan/CIM/PandocAST.agda.html build/html/src/agda/Plan/CIM/GrammarBridge.agda.html build/html/src/agda/Algebra/Groups/Basic.agda.html build/html/src/agda/Algebra/Groups/Structure.agda.html build/html/src/agda/Algebra/Groups/Abelian.agda.html build/html/src/agda/Algebra/Groups/Free.agda.html build/html/src/agda/Algebra/Enrichment.agda.html build/html/src/agda/Algebra/Rings/Basic.agda.html build/html/src/agda/Algebra/Modules/Basic.agda.html build/html/src/agda/Algebra/Foundation.agda.html build/html/src/agda/Algebra/Fields/Basic.agda.html build/html/src/agda/Algebra/Fields/Advanced.agda.html build/html/src/agda/Algebra/Index.agda.html build/html/src/agda/Chapter1/Level1.agda.html build/html/src/agda/Chapter1/Level1sub8.agda.html build/html/src/agda/Chapter1/Level1sub7.agda.html build/html/src/agda/Chapter1/Level1sub3.agda.html build/html/src/agda/Chapter1/Level1Index.agda.html build/html/src/agda/Chapter1/Level1sub2.agda.html build/html/src/agda/Chapter1/Level1sub4.agda.html build/html/src/agda/Chapter1/Level1sub5.agda.html build/html/src/agda/Chapter1/Level1sub6.agda.html build/html/src/agda/PropertyRegistry.agda.html build/html/src/agda/ExporterMakefile.agda.html build/html/src/agda/Core/AlgorithmComplexity.agda.html build/html/src/agda/Core/ABNF.agda.html build/html/src/agda/Core/TechnicalDebt.agda.html build/html/src/agda/Core/Witnesses.agda.html build/html/src/agda/Core/BraidTree.agda.html build/html/src/agda/Core/Limitations.agda.html build/html/src/agda/Core/AlgorithmCorrectness.agda.html build/html/src/agda/Core/GrothendieckFibrations.agda.html build/html/src/agda/Core/AlgorithmUniversality.agda.html build/html/src/agda/Core/AdapterReflection.agda.html build/html/src/agda/Core/Phase.agda.html build/html/src/agda/Core/AdapterAutomation.agda.html build/html/src/agda/Core/Utils.agda.html build/html/src/agda/Core/Algorithms/Registry.agda.html build/html/src/agda/Core/Algorithms/FunctionFields.agda.html build/html/src/agda/Core/Algorithms/External.agda.html build/html/src/agda/Core/Algorithms/FiniteFields.agda.html build/html/src/agda/Core/Algorithms/AutomaticEvidence.agda.html build/html/src/agda/Core/Algorithms/NumberFields.agda.html build/html/src/agda/Core/Algorithms/Bundle.agda.html build/html/src/agda/Core/Algorithms/InductiveClassification.agda.html build/html/src/agda/Core/PhaseCategory.agda.html build/html/src/agda/Core/ConstructiveWitnesses.agda.html build/html/src/agda/Core/UniversalProperties.agda.html build/html/src/agda/Core/GrowthMetrics.agda.html build/html/src/agda/Core/AlgebraicAlgorithms.agda.html build/html/src/agda/Core/Yoneda.agda.html build/html/src/agda/Core/GodelBoundary.agda.html build/html/src/agda/Core/PathAggregator.agda.html build/html/src/agda/Core/PolynomialsF2.agda.html build/html/src/agda/Core/CategoricalAdapter.agda.html
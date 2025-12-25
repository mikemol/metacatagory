# Parameterization Buffer (temporary)

## Core.AlgebraicAlgorithms consumers (still open the postulate module)
- src/agda/Core/AlgorithmUniversality.agda
- src/agda/Core/AlgorithmCorrectness.agda
- src/agda/Core/ConstructiveWitnesses.agda
- src/agda/Core/Algorithms/Bundle.agda
- src/agda/Core/Algorithms/External.agda
- src/agda/Core/Algorithms/FiniteFields.agda
- src/agda/Core/Algorithms/FunctionFields.agda
- src/agda/Core/Algorithms/NumberFields.agda
- src/agda/Core/Algorithms/Registry.agda
- src/agda/Algebra/Index.agda
- src/agda/Examples/AlgorithmCorrectnessExamples.agda
- src/agda/Examples/AutomaticEvidenceDemo.agda
- src/agda/Examples/ConstructiveWitnessExamples.agda
- src/agda/Examples/FunctionField/F2x.agda
- src/agda/Examples/LazyHybridDemo.agda
- src/agda/Examples/NumberField/Sqrt2.agda
- src/agda/Tests/DispatchBehaviorTests.agda
- src/agda/Tests/SerializationTests.agda
- src/agda/Tests/PhaseExamples.agda
- src/agda/Tests/UniversalPropertyTests.agda
- src/agda/Tests/ErrorAsSpecificationTests.agda
- src/agda/Tests/AdvancedPhaseExamples.agda
- src/agda/Tests/AlgorithmCompositionTests.agda
- src/agda/Tests/AlgorithmSmokeTests.agda (imports with alias; still consumes Core types)
- src/agda/Tests/PerformanceBoundaryTests.agda (Core import remains for types)
- src/agda/Tests/WarningAggregatorsTest.agda
- src/agda/Tests/ErrorHandlingTests.agda
- src/agda/Tests/WitnessConstructionTests.agda
- src/agda/Tests/ConstructiveWitnessTests.agda
- src/agda/Core/AlgebraicAlgorithms.agda (source of the postulates)
- src/agda/Plan/CIM/FrameworkMetadata.agda (string reference in metadata list)

## GrowthAnalysis usage outside its own module
- src/agda/Tests/PerformanceBoundaryTests.agda (Phase14 imports histories from GrowthAnalysis)

## TechnicalDebt.Priorities usage outside its own module
- None found (only the defining module).

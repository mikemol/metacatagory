# Deferred Items Report

Generated on: 2025-12-23 05:56:27 UTC

## Recent Migrations

* PARAM-ALG-1: Specialized bundles updated to use `Algorithms.Basic.Defaults` constructors where applicable:
  * Finite fields: generic decisions/closures now via `mk*`; finite-specific galois/subgroups preserved.
  * Number fields: external oracles retained for core algorithms; decisions/closures moved to `mk*`.
  * Function fields: generic algorithms replaced with `mk*` constructors for consistency.

## DeviationLog Entries

Found **14** instances:

* `src/agda/Tests/SpecificationValidation.agda:12` — `--  - DeviationLog [2025-11-18]: Expected counts are currently specified`
* `src/agda/Tests/SpecificationValidation.agda:35` — `-- DeviationLog [2025-11-18]: ToposTheoryChecklist has constructor arity/type mismatches`
* `src/agda/Tests/SpecificationValidation.agda:92` — `-- DeviationLog [2025-11-18]: Partial enumeration to keep compile time fast.`
* `src/agda/Tests/CoverageReport.agda:286` — `-- DeviationLog [2025-11-18]: Removed inline equality proof`\_ : totalAssertions ≡ 62\`\`
* `src/agda/Examples/DeferredItemsScanner.agda:123` — `DeviationLogCat : DeferredCategory`
* `src/agda/Examples/DeferredItemsScanner.agda:149` — `categoryPattern DeviationLogCat = "DeviationLog"`
* `src/agda/Examples/DeferredItemsScanner.agda:157` — `categoryLabel DeviationLogCat = "DeviationLog Entries"`
* `src/agda/Examples/DeferredItemsScanner.agda:165` — `allDeferredCategories = DeviationLogCat ∷ PostulateCat ∷ TodoCat ∷ PlannedCat ∷ FixmeCat ∷ []`
* `src/agda/Examples/DeferredItemsScanner.agda:216` — `∷ ("| DeviationLog | " ++ (natToString (DeferredSummary.deviationLog summary) ++ " |"))`
* `src/agda/Examples/DeferredItemsScanner.agda:257` — `{ deviationLog = getCountFor DeviationLogCat scans`
* `src/agda/Examples/DeferredItemsScanner.agda:267` — `eqCategory DeviationLogCat DeviationLogCat = true`
* `src/agda/Algebra/Foundation.agda:110` — `-- DeviationLog [2025-11-18]: Removed inline validation proof that Group index`
* `src/agda/Algebra/Foundation.agda:139` — `-- DeviationLog [2025-11-18]: Removed inline validation proof that`
* `src/agda/Core/Algorithms/FunctionFields.agda:40` — `-- DeviationLog [2025-11-18]: Stubbing function field algorithms to generic defaults`

## Postulates

Found **348** instances:

* `src/agda/Tests/PropertyRegistryTests.agda:92` — `postulate`
* `src/agda/Tests/PropertyRegistryTests.agda:132` — `postulate`
* `src/agda/Tests/DispatchBehaviorTests.agda:31` — `postulate`
* `src/agda/Tests/WitnessConstructionTests.agda:24` — `postulate`
* `src/agda/Tests/WitnessConstructionTests.agda:338` — `postulate`
* `src/agda/Tests/SerializationTests.agda:38` — `postulate TestFixturesPackage : M.Identifier`
* `src/agda/Tests/SerializationTests.agda:73` — `postulate`
* `src/agda/Tests/SerializationTests.agda:77` — `postulate`
* `src/agda/Tests/SerializationTests.agda:98` — `postulate`
* `src/agda/Tests/SerializationTests.agda:111` — `postulate`
* `src/agda/Tests/SerializationTests.agda:147` — `postulate`
* `src/agda/Tests/SerializationTests.agda:158` — `postulate`
* `src/agda/Tests/SerializationTests.agda:162` — `postulate`
* `src/agda/Tests/SerializationTests.agda:176` — `postulate`
* `src/agda/Tests/SerializationTests.agda:187` — `postulate`
* `src/agda/Tests/SerializationTests.agda:191` — `postulate`
* `src/agda/Tests/SerializationTests.agda:205` — `postulate`
* `src/agda/Tests/SerializationTests.agda:217` — `postulate`
* `src/agda/Tests/SerializationTests.agda:221` — `postulate`
* `src/agda/Tests/SerializationTests.agda:225` — `postulate`
* `src/agda/Tests/SerializationTests.agda:245` — `postulate`
* `src/agda/Tests/SerializationTests.agda:258` — `postulate`
* `src/agda/Tests/SerializationTests.agda:266` — `postulate`
* `src/agda/Tests/SerializationTests.agda:399` — `postulate`
* `src/agda/Tests/SerializationTests.agda:410` — `postulate`
* `src/agda/Tests/SerializationTests.agda:430` — `postulate`
* `src/agda/Tests/SerializationTests.agda:434` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:28` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:51` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:71` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:103` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:119` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:134` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:138` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:141` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:153` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:169` — `postulate`
* `src/agda/Tests/PhaseExamples.agda:181` — `postulate`
* `src/agda/Tests/UniversalPropertyTests.agda:24` — `postulate`
* `src/agda/Tests/UniversalPropertyTests.agda:179` — `postulate`
* `src/agda/Tests/UniversalPropertyTests.agda:332` — `postulate`
* `src/agda/Tests/UniversalPropertyTests.agda:345` — `postulate`
* `src/agda/Tests/EnrichmentChecklist.agda:23` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:31` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:38` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:42` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:46` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:61` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:71` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:95` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:99` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:107` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:114` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:133` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:163` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:172` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:186` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:191` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:199` — `postulate`
* `src/agda/Tests/AdvancedPhaseExamples.agda:209` — `postulate`
* `src/agda/Tests/ErrorAsSpecificationTests.agda:30` — `postulate`
* `src/agda/Tests/ErrorAsSpecificationTests.agda:56` — `postulate`
* `src/agda/Tests/ErrorAsSpecificationTests.agda:78` — `postulate`
* `src/agda/Tests/ErrorAsSpecificationTests.agda:104` — `postulate`
* `src/agda/Tests/ErrorAsSpecificationTests.agda:122` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:40` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:60` — `postulate TestFixturesPackage : M.Identifier`
* `src/agda/Tests/AlgorithmCompositionTests.agda:70` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:77` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:84` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:98` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:105` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:124` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:131` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:155` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:182` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:222` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:235` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:248` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:257` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:271` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:303` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:308` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:331` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:338` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:352` — `postulate`
* `src/agda/Tests/AlgorithmCompositionTests.agda:390` — `postulate`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:15` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:33` — `postulate TestFixturesPackage : M.Identifier`
* `src/agda/Tests/ConstructiveWitnessTests.agda:41` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:72` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:104` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:134` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:163` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:195` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:225` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:255` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:291` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:317` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:345` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:375` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:406` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:436` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:453` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:465` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:504` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:582` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:607` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:629` — `postulate`
* `src/agda/Tests/ConstructiveWitnessTests.agda:676` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:56` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:72` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:90` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:95` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:112` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:142` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:147` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:176` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:196` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:216` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:228` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:233` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:250` — `postulate`
* `src/agda/Tests/PerformanceBoundaryTests.agda:280` — `postulate`
* `src/agda/Tests/WarningAggregatorsTest.agda:17` — `postulate`
* `src/agda/Tests/YonedaChecklist.agda:191` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:36` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:42` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:62` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:75` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:90` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:95` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:106` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:126` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:130` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:149` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:170` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:193` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:198` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:206` — `postulate`
* `src/agda/Tests/ErrorHandlingTests.agda:221` — `postulate`
* `src/agda/Chapter2/Level2sub3.agda:449` — `postulate`
* `src/agda/Chapter2/Level2sub2.agda:197` — `postulate`
* `src/agda/Chapter2/Level2sub1.agda:512` — `postulate`
* `src/agda/Examples/RealWorldAlgorithms.agda:173` — `postulate`
* `src/agda/Examples/RealWorldAlgorithms.agda:282` — `postulate`
* `src/agda/Examples/TechnicalDebtExample.agda:12` — `postulate`
* `src/agda/Examples/FunctionField/F2x.agda:19` — `postulate`
* `src/agda/Examples/FunctionField/F2x.agda:24` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:32` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:69` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:109` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:159` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:200` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:248` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:271` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:285` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:290` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:294` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:316` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:338` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:361` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:389` — `postulate`
* `src/agda/Examples/AlgorithmCorrectnessExamples.agda:416` — `postulate`
* `src/agda/Examples/NumberField/Sqrt2.agda:25` — `postulate`
* `src/agda/Examples/NumberField/Sqrt2.agda:30` — `postulate`
* `src/agda/Examples/FiniteField/GF8.agda:18` — `postulate`
* `src/agda/Examples/FiniteField/GF8.agda:23` — `postulate`
* `src/agda/Examples/DeferredItemsScanner.agda:79` — `postulate`
* `src/agda/Examples/DeferredItemsScanner.agda:108` — `postulate`
* `src/agda/Examples/DeferredItemsScanner.agda:140` — `postulates : Nat`
* `src/agda/Examples/AutomaticEvidenceDemo.agda:23` — `postulate`
* `src/agda/Examples/AutomaticEvidenceDemo.agda:30` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:34` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:62` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:93` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:125` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:153` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:184` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:218` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:232` — `postulate minpolyAlg : MinimalPolynomialAlgorithm F E`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:243` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:259` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:273` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:291` — `postulate`
* `src/agda/Examples/ConstructiveWitnessExamples.agda:309` — `postulate`
* `src/agda/Examples/AgdaMakefileDeps.agda:24` — `postulate`
* `src/agda/Examples/AgdaMakefileDeps.agda:95` — `postulate`
* `src/agda/Examples/AgdaMakefileDeps.agda:157` — `postulate`
* `src/agda/Examples/AgdaMakefileDeps.agda:207` — `postulate`
* `src/agda/Examples/LazyHybridDemo.agda:25` — `postulate`
* `src/agda/Examples/LazyHybridDemo.agda:48` — `postulate`
* `src/agda/Examples/LazyHybridDemo.agda:71` — `postulate`
* `src/agda/Examples/LazyHybridDemo.agda:96` — `postulate`
* `src/agda/Examples/TechnicalDebtRegistry.agda:69` — `postulate`
* `src/agda/Examples/RoadmapIssueSync.agda:172` — `postulate`
* `src/agda/Examples/RoadmapIssueSync.agda:205` — `postulate`
* `src/agda/Examples/RoadmapIssueSync.agda:223` — `postulate`
* `src/agda/Examples/RoadmapIssueSync.agda:234` — `postulate`
* `src/agda/Examples/MakefileTargets.agda:185` — `postulate`
* `src/agda/Examples/AgdaFileScanFFI.agda:11` — `postulate`
* `src/agda/Examples/ExporterMakefile.agda:71` — `postulate`
* `src/agda/Examples/ExporterMakefile.agda:84` — `postulate`
* `src/agda/Examples/ExporterMakefile.agda:253` — `postulate`
* `src/agda/Markdown/ExportProof.agda:15` — `postulate`
* `src/agda/Plan/CIM/RoadmapExporter.agda:326` — `postulate`
* `src/agda/Plan/CIM/RoadmapSPPFExport.agda:76` — `postulate`
* `src/agda/Plan/CIM/MarkdownNormalize.agda:135` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:33` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:39` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:45` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:51` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:57` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:63` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:69` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:75` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:82` — `postulate`
* `src/agda/Algebra/Groups/Theorems/Classical.agda:87` — `postulate`
* `src/agda/Algebra/Groups/Structure.agda:30` — `postulate FinitelyGeneratedAbelianStructurePackage : (A : FinitelyGeneratedAbelianGroup) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:47` — `postulate KrullSchmidtPackage : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:48` — `postulate GroupActionCoreTheorems : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:73` — `postulate OrbitStabilizerTheorem : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:91` — `postulate SylowTheoremsPackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:105` — `postulate FiniteSimpleGroupsClassification : M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:106` — `postulate CompositionSeriesPackage : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:138` — `postulate NilpotentSolvablePackage : (G : GroupDeclaration) → (p : M.Identifier) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:159` — `postulate SchreierRefinementTheorem : (G : GroupDeclaration) → (S₁ S₂ : NormalSeries G) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:162` — `postulate JordanHolderTheorem : (G : GroupDeclaration) → (S₁ S₂ : CompositionSeries G) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:163` — `postulate NilpotentImpliesSolvable : (G : GroupDeclaration) → (N : NilpotentGroup G) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:164` — `postulate PGroupsAreNilpotent : (p : M.Identifier) → (G : GroupDeclaration) → (P : PGroup p G) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:165` — `postulate SolvableViaAbelianQuotients : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:168` — `postulate FGAbelianAsLawvereModels : M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:169` — `postulate GroupActionFunctorCorrespondence : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:170` — `postulate SylowCategoricalPerspective : M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:171` — `postulate CompositionSeriesAsFiltration : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:172` — `postulate SolvableAsIteratedExtension : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:173` — `postulate NilpotentAsCentralExtension : (G : GroupDeclaration) → M.Identifier`
* `src/agda/Algebra/Groups/Structure.agda:174` — `postulate GroupExtensionsAndCohomology : M.Identifier`
* `src/agda/Algebra/Groups/Abelian.agda:272` — `postulate`
* `src/agda/Algebra/Groups/Abelian.agda:277` — `postulate`
* `src/agda/Algebra/Groups/Abelian.agda:281` — `postulate`
* `src/agda/Algebra/Groups/Abelian.agda:286` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:36` — `postulate CategoricalProductCoproductPackage : M.Identifier`
* `src/agda/Algebra/Groups/Free.agda:103` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:118` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:169` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:187` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:193` — `postulate`
* `src/agda/Algebra/Groups/Free.agda:208` — `postulate CategoricalPerspectivePackage : M.Identifier`
* `src/agda/Algebra/Groups/Free.agda:221` — `postulate HomologicalAlgebraConnectionsPackage : M.Identifier`
* `src/agda/Algebra/Groups/Free.agda:238` — `postulate ComputabilityTheoremsPackage : M.Identifier`
* `src/agda/Algebra/Enrichment.agda:139` — `postulate`
* `src/agda/Algebra/Enrichment.agda:147` — `postulate`
* `src/agda/Algebra/Enrichment.agda:156` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:23` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:33` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:43` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:49` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:55` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:61` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:67` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:76` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:82` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:88` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:94` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:104` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:109` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:114` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:120` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:126` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:131` — `postulate`
* `src/agda/Algebra/Rings/Theorems/Classical.agda:137` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:22` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:32` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:42` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:48` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:58` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:64` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:74` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:84` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:90` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:100` — `postulate`
* `src/agda/Algebra/Modules/Theorems/Classical.agda:110` — `postulate`
* `src/agda/Algebra/Modules/BasicWithTheorems.agda:29` — `postulate ZnZAsZModule : M.Identifier → M.Identifier`
* `src/agda/Algebra/Modules/BasicWithTheorems.agda:30` — `postulate RPowerN : M.Identifier → M.Identifier → M.Identifier`
* `src/agda/Algebra/Modules/BasicWithTheorems.agda:31` — `postulate FAsFVectorspace : M.Identifier → M.Identifier`
* `src/agda/Algebra/Foundation.agda:219` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:55` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:72` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:78` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:84` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:101` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:108` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:127` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:154` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:180` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:186` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:196` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:201` — `postulate`
* `src/agda/Algebra/Fields/Advanced.agda:207` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:22` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:32` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:38` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:48` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:58` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:64` — `postulate`
* `src/agda/Algebra/Fields/Theorems/Classical.agda:70` — `postulate`
* `src/agda/Chapter1/Level1.agda:177` — `postulate`
* `src/agda/Chapter1/Level1.agda:182` — `postulate`
* `src/agda/Chapter1/Level1.agda:291` — `postulate`
* `src/agda/Chapter1/Level1.agda:613` — `postulate`
* `src/agda/Chapter1/Level1.agda:847` — `postulate`
* `src/agda/Chapter1/Level1sub8.agda:156` — `postulate`
* `src/agda/Chapter1/Level1sub7.agda:362` — `postulate`
* `src/agda/Chapter1/Level1sub3.agda:321` — `postulate`
* `src/agda/Chapter1/Level1sub2.agda:51` — `postulate`
* `src/agda/Chapter1/Level1sub2.agda:100` — `postulate`
* `src/agda/Chapter1/Level1sub2.agda:159` — `postulate`
* `src/agda/Chapter1/Level1sub4.agda:217` — `postulate`
* `src/agda/Chapter1/Level1sub5.agda:311` — `postulate`
* `src/agda/Chapter1/Level1sub6.agda:270` — `postulate`
* `src/agda/Core/TechnicalDebt.agda:56` — `postulate`
* `src/agda/Core/AlgorithmCorrectness.agda:398` — `postulate`
* `src/agda/Core/AlgorithmUniversality.agda:20` — `postulate`
* `src/agda/Core/Phase.agda:205` — `postulate`
* `src/agda/Core/Phase.agda:212` — `postulate`
* `src/agda/Core/Algorithms/FunctionFields.agda:20` — `postulate`
* `src/agda/Core/Algorithms/External.agda:51` — `postulate`
* `src/agda/Core/Algorithms/External.agda:55` — `postulate`
* `src/agda/Core/Algorithms/External.agda:59` — `postulate`
* `src/agda/Core/Algorithms/FiniteFields.agda:20` — `postulate _++_ : String → String → String`
* `src/agda/Core/Algorithms/FiniteFields.agda:24` — `postulate`
* `src/agda/Core/Algorithms/FiniteFields.agda:31` — `postulate showNat : Nat → String`
* `src/agda/Core/Algorithms/FiniteFields.agda:86` — `postulate _%_ : Nat → Nat → Nat`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:70` — `postulate`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:74` — `postulate`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:88` — `postulate`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:93` — `postulate`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:123` — `postulate`
* `src/agda/Core/Algorithms/NumberFields.agda:20` — `postulate`
* `src/agda/Core/Algorithms/InductiveClassification.agda:53` — `postulate`
* `src/agda/Core/PhaseCategory.agda:88` — `postulate`
* `src/agda/Core/PhaseCategory.agda:147` — `postulate`
* `src/agda/Core/ConstructiveWitnesses.agda:159` — `postulate fieldFromId : M.Identifier → FieldDeclaration`
* `src/agda/Core/ConstructiveWitnesses.agda:409` — `postulate`
* `src/agda/Core/AlgebraicAlgorithms.agda:312` — `postulate`
* `src/agda/Core/Yoneda.agda:139` — `postulate proof : ∀ {ℓ} {A : Set ℓ} → A`
* `src/agda/Core/GodelBoundary.agda:298` — `postulateId : M.Identifier`

## TODO Items

Found **83** instances:

* `src/agda/Tests/AbelianCategoriesChecklist.agda:15` — `-- TODO: Use mkHasZeroObjectPropertyAdapter with concrete zero object (e.g. 0 in Ab).`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:22` — `-- TODO: Provide actual kernel-as-equalizer instance via constructor.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:29` — `-- TODO: Replace with actual biproduct (e.g. A⊕B in Ab) via mkBiproductObjectAdapter.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:36` — `-- TODO: Construct additive category declaration with real witnesses.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:43` — `-- TODO: Populate abelian category declaration (e.g., Ab) and switch to constructor.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:50` — `-- TODO: Provide actual morphism and iso witness.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:57` — `-- TODO: Provide normal mono example via constructor.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:64` — `-- TODO: Provide example for Ab category.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:71` — `-- TODO: Provide R-Mod category example with explicit ring.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:78` — `-- TODO: Populate additive functor (e.g., Hom functor) via constructor adapter.`
* `src/agda/Tests/AbelianCategoriesChecklist.agda:85` — `-- TODO: Provide biproduct coincidence theorem witness.`
* `src/agda/Tests/CoverageReport.agda:304` — `checklistToJSON m = "TODO: implement JSON serialization"`
* `src/agda/Tests/Chapter2Checklist.agda:29` — `-- TODO: These are smoke placeholders. Replace each with a constructed witness`
* `src/agda/Tests/Chapter2Checklist.agda:41` — `-- TODO(Ch2 §2.1): Replace with an actual additive-category instance when ready.`
* `src/agda/Tests/Chapter2Checklist.agda:45` — `-- TODO(Ch2 §2.1): Replace with Hom functor built from a concrete category.`
* `src/agda/Tests/Chapter2Checklist.agda:112` — `-- TODO(Ch2 §2.2): Use a concrete coequalizer presentation when examples exist.`
* `src/agda/Tests/Chapter2Checklist.agda:116` — `-- TODO(Ch2 §2.2): Populate from an explicit pullback example later.`
* `src/agda/Tests/Chapter2Checklist.agda:120` — `-- TODO(Ch2 §2.3): Internal equivalence relation induced by (r1,r2) on A.`
* `src/agda/Tests/Chapter2Checklist.agda:125` — `-- TODO(Ch2 §2.3): Regular exact sequence K --(k1,k2)--> A --e--> Q.`
* `src/agda/Tests/Chapter2Checklist.agda:227` — `-- TODO(Ch2 §2.3): Replace with regularity witness derived from Mod(T,Set).`
* `src/agda/Tests/Chapter2Checklist.agda:231` — `-- TODO(Ch2 §2.3): Replace with actual F ⊣ U from a Lawvere theory example.`
* `src/agda/Tests/Chapter2Checklist.agda:272` — `-- TODO(Ch2 §2.4): Replace with (A,h) obtained from a concrete list-algebra.`
* `src/agda/Tests/Chapter2Checklist.agda:276` — `-- TODO(Ch2 §2.4): Replace with monad induced by a concrete adjunction example.`
* `src/agda/Tests/Chapter2Checklist.agda:312` — `-- TODO(Ch2 §2.5): Replace with LP-category declaration built from small generators.`
* `src/agda/Tests/Chapter2Checklist.agda:325` — `-- TODO(Ch2 §2.6): Build from an enriched category example once available.`
* `src/agda/Tests/Chapter2Checklist.agda:346` — `-- TODO(Ch2 §2.6): Replace with identity in the enriching category of the example.`
* `src/agda/Tests/Chapter2Checklist.agda:375` — `-- TODO(Ch2 §2.7): Replace with explicit counterexample space when added.`
* `src/agda/Tests/Chapter2Checklist.agda:400` — `-- TODO(Ch2 §2.8): Replace with Grothendieck construction on a concrete fibration.`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:42` — `-- TODO: Fix malformed categorical adapter tests`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:43` — `-- TODO: Fix malformed test: -- _ : CategoricalAdapter.morphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:44` — `-- TODO: Fix malformed test: --     CategoricalAdapter.object (A.monadWithRankCategorical monadWithRankAdapt) ⊤`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:47` — `-- TODO: Fix malformed test: -- _ : CategoricalAdapter.isomorphism (A.monadWithRankCategorical monadWithRankAdapt) ⊤ ⊤ ≡ refl`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:57` — `-- TODO: Fix malformed test: _ : CategoricalAdapter.morphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:58` — `-- TODO: Fix malformed test:     CategoricalAdapter.object (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:61` — `-- TODO: Fix malformed test: _ : CategoricalAdapter.isomorphism (A.locallyPresentableCategoryCategorical locallyPresentableCategoryAdapt) ⊤ ⊤ ≡ refl`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:71` — `-- TODO: Fix malformed test: _ : CategoricalAdapter.morphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:72` — `-- TODO: Fix malformed test:     CategoricalAdapter.object (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤`
* `src/agda/Tests/AdvancedMonadTheoryChecklist.agda:75` — `-- TODO: Fix malformed test: _ : CategoricalAdapter.isomorphism (A.rankTheoremForMonadicCategoriesTheoremCategorical rankTheoremForMonadicCategoriesTheoremAdapt) ⊤ ⊤ ≡ refl`
* `src/agda/Tests/Chapter1Checklist.agda:25` — `-- TODO: These are smoke placeholders. Replace each with a constructed witness`
* `src/agda/Tests/Chapter1Checklist.agda:36` — `-- TODO(Ch1 §1.2): Replace with proof artifact once completeness machinery lands.`
* `src/agda/Tests/Chapter1Checklist.agda:40` — `-- TODO(Ch1 §1.2): Replace with actual constructor built via products/equalizers example.`
* `src/agda/Tests/Chapter1Checklist.agda:48` — `-- TODO(Ch1 §1.3): Replace with adjunction declared via concrete F ⊣ G once available.`
* `src/agda/Tests/Chapter1Checklist.agda:77` — `-- TODO(Ch1 §1.3): Use Core bridges when RightAdjointsPreserveLimits proof is wired.`
* `src/agda/Tests/Chapter1Checklist.agda:85` — `-- TODO(Ch1 §1.4): Swap for derived completeness witness when available.`
* `src/agda/Tests/Chapter1Checklist.agda:89` — `-- TODO(Ch1 §1.4): Populate from example morphism once strong-mono tests exist.`
* `src/agda/Tests/Chapter1Checklist.agda:128` — `-- TODO(Ch1 §1.5): Replace with reflection/localization instance from Examples/*.`
* `src/agda/Tests/Chapter1Checklist.agda:153` — `-- TODO(Ch1 §1.5): Bind to concrete (E,M) once canonical system example is added.`
* `src/agda/Tests/Chapter1Checklist.agda:161` — `-- TODO(Ch1 §1.6): Use actual Ab-category and functor identifiers later.`
* `src/agda/Tests/Chapter1Checklist.agda:165` — `-- TODO(Ch1 §1.6): Replace with representable constructed in Examples/* when added.`
* `src/agda/Tests/Chapter1Checklist.agda:173` — `-- TODO(Ch1 §1.7): Replace with proof sketch artifact once diagram-chase helpers land.`
* `src/agda/Tests/Chapter1Checklist.agda:177` — `-- TODO(Ch1 §1.7): Use concrete bicategory objects once examples exist.`
* `src/agda/Tests/Chapter1Checklist.agda:192` — `-- TODO(Ch1 §1.8): Replace with internal category built from a monoid example.`
* `src/agda/Tests/Chapter1Checklist.agda:196` — `-- TODO(Ch1 §1.8): Replace with internal presheaf built from internal category example.`
* `src/agda/Tests/Chapter3Checklist.agda:15` — `-- TODO: These are smoke placeholders for Chapter 3. Replace with constructed`
* `src/agda/Tests/Chapter3Checklist.agda:40` — `-- TODO(Ch3 §3.1): Replace with duality built from a concrete locale/frame pair.`
* `src/agda/Tests/Chapter3Checklist.agda:63` — `-- TODO(Ch3 §3.2): Replace with a specific local homeomorphism between spaces.`
* `src/agda/Tests/Chapter3Checklist.agda:82` — `-- TODO(Ch3 §3.2): Replace with an étale space built from a sheaf example.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:14` — `-- TODO: Replace with constructor-based adapter via mkSubobjectLatticeAdapter once a concrete lattice example is added.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:22` — `-- TODO: Provide real well-powered category (e.g., Set) and switch to mkWellPoweredCategoryAdapter.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:30` — `-- TODO: Populate with actual completeness proof of a subobject lattice.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:37` — `-- TODO: Provide concrete strong epi (e.g., quotient map in Set) via mkStrongEpimorphismAdapter.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:44` — `-- TODO: Replace with (E,M) factorization system witness using constructor.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:51` — `-- TODO: Provide actual factorization f = m ∘ e with witness identifiers.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:58` — `-- TODO: Provide generator object for a concrete category (e.g., singleton set for Set).`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:65` — `-- TODO: Use free module or free object example for projective witness.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:72` — `-- TODO: Provide injective object (e.g., divisible group) using constructor adapter.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:79` — `-- TODO: Replace with category having enough projectives proof.`
* `src/agda/Tests/SubobjectTheoryChecklist.agda:86` — `-- TODO: Replace with category having enough injectives proof.`
* `src/agda/Examples/DeferredItemsScanner.agda:151` — `categoryPattern TodoCat = "TODO"`
* `src/agda/Examples/DeferredItemsScanner.agda:159` — `categoryLabel TodoCat = "TODO Items"`
* `src/agda/Examples/DeferredItemsScanner.agda:218` — `∷ ("| TODO | " ++ (natToString (DeferredSummary.todo summary) ++ " |"))`
* `src/agda/Plan/CIM/CHIPConformance.agda:45` — `-- TODO: BraidedSPPF not yet defined in Utility.agda`
* `src/agda/Plan/CIM/CHIPConformance.agda:56` — `-- TODO: If Chapter3 provides a universal property or adjunction record, wrap or alias here`
* `src/agda/Plan/CIM/DocumentationContent.agda:225` — `bullets ("Totals: 567 items" ∷ "Postulates: 351" ∷ "TODOs: 155" ∷ "Other markers (FIXME/HACK/etc.): 61" ∷ []) ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:271` — `codeBlock "Core.disabled/          : ~80 postulates, ~30 TODOs\nAlgebra.disabled/       : ~45 postulates, ~20 TODOs\nExamples.disabled/      : ~25 postulates, ~15 TODOs\nPlan.CIM/               : ~15 TODOs (stubs)\nScripts/                : ~40 TODOs (improvements)\nDocumentation/          : ~50 TODOs (missing sections)\nOther                   : ~106 postulates, ~35 TODOs" ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:274` — `codeBlock "Postulates (351):\n  - Unproven theorems awaiting formal proofs\n  - Placeholder functions for disabled modules\n  - Abstract interface requirements\n\nTODOs (155):\n  - Code improvements and refactoring\n  - Missing implementations\n  - Documentation gaps\n\nOther markers (61):\n  - FIXME: Known bugs or issues\n  - HACK: Temporary workarounds\n  - NOTE: Important context or warnings" ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:279` — `("Add inline marker: -- TODO: [category] description or postulate name : Type" ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:302` — `codeBlock "# TODOs\ngrep -r \"TODO\" src/agda/\n\n# Postulates\ngrep -r \"postulate\" src/agda/\n\n# All markers\ngrep -rE \"TODO|FIXME|HACK|XXX|NOTE\" src/agda/" ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:317` — `p (text "Last updated: 2025-12-20; Next review: after Phase 1 cleanup (ROADMAP.md); Totals: 567 (351 postulates, 155 TODOs, 61 other)" ∷ []) ∷`
* `src/agda/Core/AdapterReflection.agda:49` — `unknown  -- TODO: implement quoteTerm-based field generation`
* `src/agda/Core/Algorithms/AutomaticEvidence.agda:45` — `-- TODO: implement reflection-based detection once field invariants are defined`
* `src/agda/Core/Algorithms/InductiveClassification.agda:32` — `-- TODO: replace placeholder with concrete invariants and proofs`
* `src/agda/Core/Algorithms/InductiveClassification.agda:36` — `-- TODO: replace placeholder with concrete invariants and proofs`

## PLANNED Items

Found **3** instances:

* `src/agda/Examples/DeferredItemsScanner.agda:152` — `categoryPattern PlannedCat = "PLANNED"`
* `src/agda/Examples/DeferredItemsScanner.agda:160` — `categoryLabel PlannedCat = "PLANNED Items"`
* `src/agda/Examples/DeferredItemsScanner.agda:219` — `∷ ("| PLANNED | " ++ (natToString (DeferredSummary.planned summary) ++ " |"))`

## FIXME Items

Found **6** instances:

* `src/agda/Examples/DeferredItemsScanner.agda:153` — `categoryPattern FixmeCat = "FIXME"`
* `src/agda/Examples/DeferredItemsScanner.agda:161` — `categoryLabel FixmeCat = "FIXME Items"`
* `src/agda/Examples/DeferredItemsScanner.agda:220` — `∷ ("| FIXME | " ++ (natToString (DeferredSummary.fixme summary) ++ " |"))`
* `src/agda/Plan/CIM/DocumentationContent.agda:225` — `bullets ("Totals: 567 items" ∷ "Postulates: 351" ∷ "TODOs: 155" ∷ "Other markers (FIXME/HACK/etc.): 61" ∷ []) ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:274` — `codeBlock "Postulates (351):\n  - Unproven theorems awaiting formal proofs\n  - Placeholder functions for disabled modules\n  - Abstract interface requirements\n\nTODOs (155):\n  - Code improvements and refactoring\n  - Missing implementations\n  - Documentation gaps\n\nOther markers (61):\n  - FIXME: Known bugs or issues\n  - HACK: Temporary workarounds\n  - NOTE: Important context or warnings" ∷`
* `src/agda/Plan/CIM/DocumentationContent.agda:302` — `codeBlock "# TODOs\ngrep -r \"TODO\" src/agda/\n\n# Postulates\ngrep -r \"postulate\" src/agda/\n\n# All markers\ngrep -rE \"TODO|FIXME|HACK|XXX|NOTE\" src/agda/" ∷`

## Summary

| Category | Count |
|----------|-------|
| DeviationLog | 14 |
| Postulates | 348 |
| TODO | 83 |
| PLANNED | 3 |
| FIXME | 6 |
| **Total** | **454** |

## Manual notes (2025-12-24)

* Core.AlgebraicAlgorithms consumers still opening postulated interfaces (targets for parameterization): src/agda/Core/AlgorithmUniversality.agda, Core/AlgorithmCorrectness.agda, Core/ConstructiveWitnesses.agda, Core/Algorithms/{Bundle,External,FiniteFields,FunctionFields,NumberFields,Registry}.agda, Algebra/Index.agda, Examples/{AlgorithmCorrectnessExamples,AutomaticEvidenceDemo,ConstructiveWitnessExamples,FunctionField/F2x,LazyHybridDemo,NumberField/Sqrt2}.agda, Tests/{DispatchBehaviorTests,SerializationTests,PhaseExamples,UniversalPropertyTests,ErrorAsSpecificationTests,AdvancedPhaseExamples,AlgorithmCompositionTests,AlgorithmSmokeTests,PerformanceBoundaryTests,WarningAggregatorsTest,ErrorHandlingTests,WitnessConstructionTests,ConstructiveWitnessTests}.agda, Core/AlgebraicAlgorithms.agda (source postulates), Plan/CIM/FrameworkMetadata.agda (string mention).
* GrowthAnalysis usage outside module: Tests/PerformanceBoundaryTests.agda (Phase14 imports histories from GrowthAnalysis).
* TechnicalDebt.Priorities usage outside module: none.

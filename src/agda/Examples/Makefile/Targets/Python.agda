{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Python where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget)

-- Python build/test/witness targets, factoring out of ExporterMakefile.agda
pythonTargets : List MakefileTarget
pythonTargets =
  generatorToTarget "python-build" "Prepare Python artifacts (interpreted; placeholder for future bytecode/vendor steps)" ([])
    ("@echo \"python build: no-op (interpreted artifacts)\"" ∷ [])
  ∷ generatorToTarget "python-test" "Run Python tests (includes pytest suite)" ("python-build" ∷ [])
    ("pytest tests/ -v" ∷ [])
  ∷ generatorToTarget "python-verified" "Witness target: build + test contracted" ("python-build" ∷ "python-test" ∷ [])
    ("@echo \"python verified: build+test complete\"" ∷ [])
  ∷ []

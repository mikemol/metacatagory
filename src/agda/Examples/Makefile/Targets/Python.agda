{-# OPTIONS --without-K #-}
-- | Python environment setup and test targets.
module Examples.Makefile.Targets.Python where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget; generatorToFileTarget; mutateCert)

-- Python build/test/witness targets, factoring out of ExporterMakefile.agda
pythonTargets : List MakefileTarget
pythonTargets =
  generatorToFileTarget mutateCert "build/venv/dir.stamp" "Ensure build/venv exists" ([])
    ("mkdir -p build/venv && touch build/venv/dir.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/requirements-main.stamp" "Track requirements.txt changes" ("build/venv/dir.stamp" ∷ "requirements.txt" ∷ [])
    ("touch build/venv/requirements-main.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/requirements-dev.stamp" "Track requirements-dev.txt changes" ("build/venv/dir.stamp" ∷ "requirements-dev.txt" ∷ [])
    ("touch build/venv/requirements-dev.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/requirements-inputs.stamp" "Track Python dependency inputs" ("build/venv/requirements-main.stamp" ∷ "build/venv/requirements-dev.stamp" ∷ [])
    ("touch build/venv/requirements-inputs.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/venv_created.stamp" "Create Python virtualenv" ("build/venv/dir.stamp" ∷ [])
    ("if [ ! -d \"$(VIRTUAL_ENV)\" ]; then python3 -m venv \"$(VIRTUAL_ENV)\"; fi && touch build/venv/venv_created.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/pip_upgraded.stamp" "Upgrade pip in virtualenv" ("build/venv/venv_created.stamp" ∷ [])
    ("\"$(VIRTUAL_ENV)/bin/python\" -m pip install --upgrade pip && touch build/venv/pip_upgraded.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/requirements_installed.stamp" "Install Python dependencies" ("build/venv/pip_upgraded.stamp" ∷ "build/venv/requirements-inputs.stamp" ∷ [])
    ("\"$(VIRTUAL_ENV)/bin/python\" -m pip install -r requirements.txt -r requirements-dev.txt && touch build/venv/requirements_installed.stamp" ∷ [])
  ∷ generatorToFileTarget mutateCert "build/venv/python_setup.stamp" "Create Python venv and install dependencies" ("build/venv/requirements_installed.stamp" ∷ [])
    ("touch build/venv/python_setup.stamp" ∷ [])
  ∷ generatorToTarget mutateCert "python-test" "Run Python tests (includes pytest suite)" ("build/venv/python_setup.stamp" ∷ [])
    ("\"$(VIRTUAL_ENV)/bin/python\" -m pytest tests/ -v -n \"$(PYTEST_WORKERS)\"" ∷ [])
  ∷ generatorToTarget mutateCert "python-verified" "Witness target: test suite contracted" ("python-test" ∷ [])
    ("@echo \"python verified: tests complete\"" ∷ [])
  ∷ []

{-# OPTIONS --without-K #-}
module Examples.Makefile.Targets.Docker where

open import Agda.Builtin.List using (List; _∷_; [])
open import Examples.MakefileTargets using (MakefileTarget; generatorToTarget)

-- Docker rootless build and GHCR push targets
dockerTargets : List MakefileTarget
dockerTargets =
  generatorToTarget "docker-rootless-status" "Check rootless Docker daemon status" ([])
    ("source scripts/docker-rootless-env.sh && docker info --format 'Rootless: {{join .SecurityOptions \", \"}}'" ∷ [])
  ∷ generatorToTarget "docker-build" "Build Docker image (metacatagory:dev)" ([])
    ("source scripts/docker-rootless-env.sh && docker build -t metacatagory:dev ." ∷ [])
  ∷ generatorToTarget "docker-build-ghcr" "Build and tag image for GHCR (requires GHCR_REGISTRY and GHCR_USERNAME env vars)" ([] )
    ("source scripts/docker-rootless-env.sh && docker build -t $${GHCR_REGISTRY}/$${GHCR_USERNAME}/metacatagory:dev . && docker tag $${GHCR_REGISTRY}/$${GHCR_USERNAME}/metacatagory:dev $${GHCR_REGISTRY}/$${GHCR_USERNAME}/metacatagory:latest" ∷ [])
  ∷ generatorToTarget "docker-push-ghcr" "Push image to GHCR (requires docker login; set GHCR_REGISTRY and GHCR_USERNAME)" ("docker-build-ghcr" ∷ [])
    ("source scripts/docker-rootless-env.sh && docker push $${GHCR_REGISTRY}/$${GHCR_USERNAME}/metacatagory:dev && docker push $${GHCR_REGISTRY}/$${GHCR_USERNAME}/metacatagory:latest" ∷ [])
  ∷ generatorToTarget "docker-all" "Build and push to GHCR (full pipeline)" ("docker-build-ghcr" ∷ "docker-push-ghcr" ∷ [])
    ("@echo \"Docker build and push to GHCR complete\"" ∷ [])
  ∷ []

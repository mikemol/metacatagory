# Request: Include Agda Standard Library in act-ubuntu-agda Image

**Image:** `ghcr.io/mikemol/act-ubuntu-agda:latest`

## Summary
The current image does **not** include the Agda standard library. After a fresh pull (digest `sha256:ff2eef33374f01cbbea9fff550e29280673f69a456a7e0f0a59c689ba30cbcfa`), I verified:

- No `standard-library.agda-lib` under `/usr`, `/opt`, or `/home`.
- `/home/runner/.cabal/store` is absent by default.
- `agda --print-agda-data-dir` points into a non-existent cabal store.

## Impact
Workflows that expect stdlib configuration fail unless they run `agda --setup` (which writes into a cabal store) or explicitly skip stdlib in ACT. The image is intended to include stdlib, so this is a regression or misbuild.

## Requested Fix
Bake stdlib into the image and expose `standard-library.agda-lib` at a stable path, for example:

- `/usr/share/agda-stdlib/standard-library.agda-lib`
- `/usr/share/libghc-agda-dev/standard-library.agda-lib`

## Possible Approaches
1. Install system package `agda-stdlib` and ensure `standard-library.agda-lib` is installed.
2. Cabal install `Agda-stdlib` and copy `standard-library.agda-lib` into `/usr/share/agda-stdlib/`.
3. Vendor stdlib into the image and point `~/.agda/libraries` accordingly.

## Note
We prefer stdlib-free builds in the repo (self-sufficient), but if the image is intended to include stdlib, it should be present and discoverable without `agda --setup`.

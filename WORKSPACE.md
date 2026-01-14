# Metacatagory Data Workspace

This sparse worktree contains data artifacts and minimal scripts to consume/regenerate them.

Contents kept:
- `data/`
- `scripts/` (subset as needed for data export/decomposition/recomposition)
- Root `README.md`, `Makefile`, `.gitignore`, `.github/`

Typical operations (from this worktree):
```bash
MUTATE_OK=1 make regen-makefile
MUTATE_OK=1 make json-decompose json-roundtrip-validate
```

Cross-references:
- Docs workspace: ../docs-worktree (docs only)
- Full repo: ../metacatagory

Keep this worktree on branch `data-worktree` (tracks `next`).
Update from main: `git -C ..../data pull --rebase origin next`.

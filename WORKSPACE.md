# Metacatagory Docs Workspace

This sparse worktree contains only documentation and the minimal scripts needed to regenerate docs.

Contents kept:
- `docs/`
- `scripts/` (subset as needed for doc generation)
- Root `README.md`, `Makefile`, `.gitignore`, `.github/`

To refresh docs from the planning index:
```bash
MUTATE_OK=1 make regen-makefile
MUTATE_OK=1 make docs-all
```

Cross-references:
- Data workspace: ../data-worktree (sparse checkout of `data/` and supporting scripts)
- Full repo: ../metacatagory

Keep this worktree on branch `docs-worktree` (tracks `next`).
Update from main: `git -C ..../docs pull --rebase origin next`.

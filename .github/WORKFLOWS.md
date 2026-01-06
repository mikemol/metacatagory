# Local Workflow Execution Guide

Run GitHub Actions workflows locally using [act](https://github.com/nektos/act).

## Configuration

Your project is configured with:
- **Docker Image**: `catthehacker/ubuntu:act-22.04`
- **Config File**: `.actrc`
- **Environment**: Loaded from `.env.local`

## Available Workflows

| Workflow | Command | Purpose |
|----------|---------|---------|
| CI | `make act-ci` or `act -W .github/workflows/ci.yml` | Test & validation pipeline |
| Markdown Lint | `make act-lint` or `act -W .github/workflows/markdown-lint.yml` | Check markdown formatting |
| Markdown Auto-Fix | `make act-markdown-fix` or `act -W .github/workflows/markdown-auto-fix.yml` | Auto-format markdown |
| Makefile Validate | `make act-makefile-validate` or `act -W .github/workflows/makefile-validate.yml` | Validate Makefile |
| Roadmap Sync | `make act-roadmap-sync` or `act -W .github/workflows/roadmap-sync.yml` | Sync roadmap artifacts |
| Deferred Items | `make act-deferred` or `act -W .github/workflows/deferred-items.yml` | Process deferred items |
| Badge Update | `make act-badges` or `act -W .github/workflows/badge-update.yml` | Update badges |

## Quick Start

### List all available jobs

```bash
# Using Makefile
make act-list

# Using act directly
act -l
```

### Run a specific workflow

```bash
# Using Makefile
make act-ci

# Using act directly
act -W .github/workflows/ci.yml
```

### Run all workflows

```bash
# Using Makefile
make act-all

# Using act directly
act
```

## VS Code Integration

All workflows are available as tasks in VS Code. Access them via:

1. **Command Palette**: `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
2. **Type**: `Tasks: Run Task`
3. **Select**: A workflow task (e.g., "Act: Run CI Workflow")

Or use the Terminal menu → Run Task.

## Troubleshooting

### Docker not found

Ensure Docker is installed and the Docker daemon is running:

```bash
docker ps
```

### Permission denied

Add your user to the docker group:

```bash
sudo usermod -aG docker $USER
newgrp docker
```

### Container issues

Clear act cache and rebuild:

```bash
act -h  # Show help
act -P ubuntu-latest=catthehacker/ubuntu:act-22.04 -W .github/workflows/ci.yml
```

## Environment Variables

Edit `.env.local` to set environment variables for workflows. Common variables:

- `GITHUB_TOKEN`: GitHub API token (for authenticated requests)
- `CI`: Always set to `true` in act
- Workflow-specific variables as documented in individual workflow files

## References

- [act Documentation](https://github.com/nektos/act)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Makefile Targets](./../MAKEFILE-TARGETS.md)

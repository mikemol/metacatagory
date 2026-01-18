# Local Workflow Execution Guide

Run the GitHub Actions CI workflow locally using [act](https://github.com/nektos/act).

## Configuration

Your project is configured with:
- **Docker Image**: `ghcr.io/mikemol/act-ubuntu-agda:latest` (prebuilt)
- **Config File**: `.actrc`
- **Environment**: Loaded from `.env.local`
- **Rootless Docker**: Recommended; see `docs/process/DOCKER-ROOTLESS.md`

Pull the worker image locally (once per machine, or when refreshed):

```bash
docker pull ghcr.io/mikemol/act-ubuntu-agda:latest
```

## Available Workflow

| Workflow | Command | Purpose |
|----------|---------|---------|
| CI | `MUTATE_LEVEL=repo mise exec -- make act-ci` | Full test/validation pipeline |

## Quick Start

### List available jobs

```bash
# Using Makefile
make act-list

# Using act directly
act -l
```

### Run CI locally

```bash
# Using Makefile
MUTATE_LEVEL=repo mise exec -- make act-ci

# Using act directly
act -W .github/workflows/ci.yml
```

## VS Code Integration

CI is available as a VS Code task:

1. **Command Palette**: `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
2. **Type**: `Tasks: Run Task`
3. **Select**: "Act: Run CI Workflow"

Or use the Terminal menu → Run Task.

## Troubleshooting

### Docker not found

Ensure Docker is installed and the Docker daemon is running:

```bash
docker ps
```

### Permission denied

Ensure rootless Docker is running and the user socket is in `DOCKER_HOST`:

```bash
export DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"
```

### Container issues

If the image is stale, force a pull and retry:

```bash
docker pull ghcr.io/mikemol/act-ubuntu-agda:latest
act -W .github/workflows/ci.yml
```

## Environment Variables

Edit `.env.local` to set environment variables for workflows. Common variables:

- `GITHUB_TOKEN`: GitHub API token (for authenticated requests)
- `CI`: Always set to `true` in act
- Workflow-specific variables as documented in `.github/workflows/ci.yml`

## References

- [act Documentation](https://github.com/nektos/act)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Makefile Targets](../docs/automation/MAKEFILE-TARGETS.md)

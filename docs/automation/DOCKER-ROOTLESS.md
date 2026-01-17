# Rootless Docker quick-start

Use the rootless daemon created by `dockerd-rootless-setuptool.sh` and point clients at the user socket.

## Environment

Source once per shell (or add to your shell rc):

```bash
source scripts/docker-rootless-env.sh
```

## Makefile targets

- `make docker-rootless-status` – Check daemon status and rootless mode.
- `make docker-build` – Build `metacatagory:dev` image.
- `make docker-build-ghcr` – Build and tag for GHCR (requires `GHCR_REGISTRY` and `GHCR_USERNAME`).
- `make docker-push-ghcr` – Push to GHCR (requires auth and env vars).
- `make docker-all` – Full pipeline (build and push to GHCR).

## GHCR authentication and env vars

Set your GHCR credentials:

```bash
# (1) Create a GitHub PAT with write:packages scope
# See: https://github.com/settings/tokens/new?scopes=write:packages

# (2) Export env vars (or add to your shell rc)
export GHCR_REGISTRY=ghcr.io
export GHCR_USERNAME=YOUR_GH_USERNAME

# (3) Log in to GHCR (paste PAT when prompted for password)
docker login ghcr.io -u YOUR_GH_USERNAME
```

Then run:

```bash
make docker-build-ghcr
make docker-push-ghcr
```

Or in one step:

```bash
make docker-all
```

## VS Code tasks

- **Docker: Rootless Status** – Check daemon status.
- **Docker: Build Image** – Build `metacatagory:dev`.

## Manual build

```bash
source scripts/docker-rootless-env.sh
docker build -t metacatagory:dev .
```

## Switching engines

Rootless-only: keep `DOCKER_HOST=unix:///run/user/$UID/docker.sock` and leave rootful docker.service masked.

Temporarily use rootful Docker:
```bash
sudo systemctl unmask docker.service docker.socket
sudo systemctl enable --now docker.service docker.socket
unset DOCKER_HOST
```
Return to rootless:
```bash
sudo systemctl disable --now docker.service docker.socket
sudo systemctl mask docker.service docker.socket
source scripts/docker-rootless-env.sh
```

# task-server — multi-stage build.
#
# IMPORTANT — build context layout. The Task workspace path-depends on
# sibling repos (`../Editor`, `../architect`, `../FastTrackStudio/*`),
# so the context for this Dockerfile is NOT the repo root: it is a
# directory that contains Task plus the siblings. Assemble it with:
#
#   deploy/docker/siblings.sh context /tmp/task-build-ctx
#   docker build \
#     -f /tmp/task-build-ctx/Task/deploy/docker/server.Dockerfile \
#     -t codeberg.org/fasttrackstudios/task-server:dev \
#     /tmp/task-build-ctx
#
# The script prefers local sibling checkouts and falls back to cloning
# the pins in deploy/docker/siblings.lock. Cargo additionally fetches
# the [patch] git deps (vox / moire / dynamic-template on Codeberg) at
# build time — network access is required in the builder stage.
#
# Features: the default `task-server` feature set is what production
# wants. Do NOT enable whisper / pdf extras here — they drag native
# toolchain deps (and protoc) into the build for no server benefit.

# ── builder ─────────────────────────────────────────────────────────
# rust:1.94 matches rust-toolchain.toml (channel = "stable").
FROM rust:1.94-bookworm AS builder

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        git pkg-config libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build/Task

# Whole-context copy: Task + sibling repos, preserving the ../ layout
# the workspace manifests expect.
COPY . /build/

# Cache mounts keep the registry + git db + target dir across builds.
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/usr/local/cargo/git \
    --mount=type=cache,target=/build/Task/target \
    cargo build --release -p task-server \
    && cp target/release/task-server /usr/local/bin/task-server

# ── runtime ─────────────────────────────────────────────────────────
FROM debian:bookworm-slim AS runtime

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates libssl3 curl \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --uid 10001 --create-home --home-dir /home/task task \
    && mkdir -p /data && chown task:task /data

COPY --from=builder /usr/local/bin/task-server /usr/local/bin/task-server

# ── runtime env (canonical list: apps/server/src/main.rs + .env.example)
#
# TASK_DATA_ROOT          everything the server owns lives under here:
#                         orgs/<slug>/ (sqlites, vault, crdt, wiki),
#                         blobs/, server-key.ed25519. Back this dir up
#                         and you have backed up the server.
# TASK_SERVER_BIND        listen address.
# TASK_SERVER_ORG         org slug to serve; unset = auto-pick the
#                         single org under orgs/ (bootstraps `default`).
# TASK_SERVER_PUBLIC_URL  public base URL (https://tasks.example.com) —
#                         used to mint signed attachment upload /
#                         download URLs. Set it when behind a proxy.
# TASK_SERVER_SEALING_KEY hex key sealing at-rest webhook secrets +
#                         integration tokens (`openssl rand -hex 32`).
#                         Unset = machine-stable dev key + a warning;
#                         fine locally, never for prod.
# TASK_VOX_URL            native clients / CLI sync URL (not the server).
# Optional integrations (unset = feature off):
#   TASK_FORGEJO_BASE_URL / TASK_FORGEJO_TOKEN / TASK_FORGEJO_BOT_TOKEN
#   TASK_FORGE_POLL_SECS  forge-sync poll interval
#   HERMES_BASE_URL / HERMES_SESSION_TOKEN / HERMES_DEFAULT_BOARD /
#   HERMES_DEFAULT_PROFILE / HERMES_WEBHOOK_SECRET
# Path overrides (rarely needed — default under TASK_DATA_ROOT):
#   TASK_SERVER_VAULT_ROOT / TASK_SERVER_CRDT_ROOT /
#   TASK_SERVER_WIKI_ROOT / TASK_SERVER_MAIL_ROOT
ENV TASK_DATA_ROOT=/data \
    TASK_SERVER_BIND=0.0.0.0:8080 \
    RUST_LOG=info

USER task
VOLUME /data
EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -fsS http://127.0.0.1:8080/.well-known/task-server.json >/dev/null || exit 1

ENTRYPOINT ["/usr/local/bin/task-server"]

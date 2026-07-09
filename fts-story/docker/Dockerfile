# Reproducible-render container for fts-story-snapshots.
#
# Pinning fontconfig + freetype + the system-provided fonts is the
# single biggest factor in cross-machine rendering stability — local
# vs. CI vs. another contributor's box can diverge by tens of pixels
# without something like this. This image deliberately mirrors the
# approach used by `tonybierman/blitz-vrt`'s Docker setup.
#
# Build:
#   docker build -t fts-story-snapshots -f docker/Dockerfile .
#
# Run snapshot tests for a consumer crate:
#   docker run --rm -v "$PWD":/workspace -w /workspace \
#     fts-story-snapshots \
#     cargo test --release -p <crate> --features stories
#
# Refresh baselines:
#   docker run --rm -v "$PWD":/workspace -w /workspace \
#     -e FTS_STORY_UPDATE_SNAPSHOTS=1 \
#     fts-story-snapshots \
#     cargo test --release -p <crate> --features stories

FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    RUST_BACKTRACE=1 \
    CARGO_TERM_COLOR=always

RUN apt-get update && apt-get install --no-install-recommends -y \
    ca-certificates \
    curl \
    pkg-config \
    build-essential \
    git \
    fontconfig \
    libfontconfig1-dev \
    libfreetype6-dev \
    libssl-dev \
    fonts-liberation2 \
    fonts-noto-core \
    fonts-noto-cjk \
    fonts-noto-color-emoji \
    && rm -rf /var/lib/apt/lists/* \
    && fc-cache -fv

# Pin Rust to the toolchain used by the workspace. Update when bumping.
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    RUST_VERSION=1.92.0

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --default-toolchain "$RUST_VERSION" --profile minimal \
    && cargo --version \
    && rustc --version

WORKDIR /workspace

# Default to running the consumer's snapshot tests. Override via
# `docker run … <cmd>`.
CMD ["cargo", "test", "--release", "--features", "stories"]

+++
title = "FastTrackStudio Docs"
description = "Documentation for the FastTrackStudio stack — Keyflow, Signal, Session, DAW, architect, and the shared toolchain."
+++

FastTrackStudio is a suite of music-production tools built as one Rust monorepo. This site collects the documentation for every domain in the stack.

## Domains

- **[Keyflow](/keyflow/)** — A plain-text notation format for lead sheets, chord charts, and rhythm charts. Human-readable `.kf` source parses into a structured chart model and engraves to publication-quality SVG and PDF with GPU-accelerated rendering. Start with the [guide](/keyflow/guide/).

- **[Signal](/signal/)** — The signal-chain and plugin-management domain: live instrument rigs (guitar, keys, drums, strings), a Kontakt-class sampler engine, built-in FX, NAM amp modeling, and CLAP/VST3 plugin output from a single Rust codebase.

- **[Session](/session/)** — The coordination layer: transport state, playback position, setlists, and shared session data across every tool in the ecosystem, locally or over the network.

- **[DAW](/daw/)** — DAW integration: the REAPER extension and bridge, DAW file-format converters (Pro Tools, Ableton, Logic, AAF, DAWproject), and the standalone audio engine.

- **[architect](/architect/)** — The entity/RPC framework that powers the stack: one `#[derive(Entity)]` yields wire types, vox repository traits, SeaORM backends, optimistic stores, and live-event streams.

- **[Guides](/guide/)** — Cross-domain guides for the shared toolchain: Facet reflection, the Styx configuration language, Tracey spec traceability, RPC best practices, and the crate facade pattern.

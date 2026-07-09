+++
title = "Session"
description = "Transport synchronization and session state coordination for FastTrackStudio"
weight = 3
+++

Session is the coordination layer for FastTrackStudio.

It manages transport state, playback position, and shared session data across every tool in the ecosystem — whether running locally or connected over the network.

## Overview

- [Protocol](/session/protocol/) — The session state protocol
- [Architecture](/session/architecture/) — Internal structure and design decisions

## What Session Does

- **Transport synchronization** — Play, stop, seek, and loop state shared across tools
- **Session state management** — Tempo, time signature, markers, and regions
- **Network coordination** — State broadcast for connected peers and devices
- **DAW bridging** — Translates between DAW-specific state and the FTS protocol

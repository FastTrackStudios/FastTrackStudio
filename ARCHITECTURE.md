# Architecture

## System Overview

Task is a local-first, real-time collaborative production workflow management platform. Everything is built in Rust. Clients in any language connect via **Vox** RPC. All data types serialize via **Facet**.

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Clients                                    │
│                                                                      │
│  ┌─────┐ ┌────────────────┐ ┌────────────┐ ┌────────────────────┐ │
│  │ CLI │ │ Obsidian Plugin│ │ Nextcloud  │ │ Invoice Ninja      │ │
│  │     │ │ (WASM)         │ │ Integration│ │ Integration        │ │
│  └──┬──┘ └───────┬────────┘ └─────┬──────┘ └──────────┬─────────┘ │
│     │            │                │                   │           │
│     │       Vox RPC (in-process / WebSocket)           │           │
│     └────────────┼────────────────┼───────────────────┘           │
│                  │                │                               │
└──────────────────┼────────────────┼───────────────────────────────┘
                   │                │
              ┌────▼────────────────▼─────────────┐
              │              task-core             │
              │                                         │
              │  ┌─────────────────────────────────┐   │
              │  │     Domain Types (Facet)         │   │
              │  │  Task · Project · Event          │   │
              │  │  Setlist · StagePlot · InputList  │   │
              │  │  Output · Budget · Deliverable    │   │
              │  └─────────────────────────────────┘   │
              │                                         │
              │  ┌─────────────────────────────────┐   │
              │  │     CRDT Layer                   │   │
              │  │  Loro documents + sync protocol  │   │
              │  └──────────────┬──────────────────┘   │
              │                 │                       │
              │  ┌──────────────▼──────────────────┐   │
              │  │     Sync Engine                  │   │
              │  │  WebSocket (/crdt)               │   │
              │  │  Nextcloud (CalDAV + Deck)       │   │
              │  │  Offline queue + merge           │   │
              │  └──────────────┬──────────────────┘   │
              │                 │                       │
              │  ┌──────────────▼──────────────────┐   │
              │  │     Storage Providers            │   │
              │  │  Local · Nextcloud · S3 · WebDAV │   │
              │  └──────────────┬──────────────────┘   │
              │                 │                       │
              │  ┌──────────────▼──────────────────┐   │
              │  │     Index Cache                  │   │
              │  │  SQLite index                    │   │
              │  │  Disposable — rebuilt from files  │   │
              │  └─────────────────────────────────┘   │
              │                                         │
              └─────────────────┬───────────────────────┘
                                │
                  ┌─────────────▼─────────────┐
                  │   Markdown Files (.md)      │
                  │   YAML properties + body    │
                  │   Source of truth. Always.   │
                  └───────────────────────────┘
```

## Core Technology Stack

### Vox — Cross-Language RPC

**Vox** is the RPC layer that makes vault-core accessible from any language. All service interfaces are defined once in Rust with `#[vox::service]` and automatically generate:

- **Rust** — in-process service calls from the CLI and integrations
- **WebSocket** — remote service calls over `/vox`
- **TypeScript** — generated bindings for integrations that need them

```rust
#[vox::service]
pub trait TaskService {
    async fn list_tasks(&self) -> Vec<Task>;
    async fn create_task(&self, task: Task) -> Result<Task, VaultError>;
    async fn complete_task(&self, title: String) -> Result<Task, VaultError>;
}
```

The codebase now exposes split service traits (`TaskService`, `ProjectService`, `TimeService`, `CalendarService`, etc.) instead of one compatibility trait or REST facade.

### Facet — Universal Serialization

**Facet** replaces serde for all domain types. Every struct derives `Facet` and gets:

- **YAML** serialization (frontmatter in `.md` files) via `facet-yaml`
- **JSON** serialization (Vox RPC, metadata endpoints, WASM bridge) via `facet-json`
- **Reflection** for query engine, validation, schema introspection

```rust
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Task {
    pub title: String,
    pub status: Status,
    pub priority: Priority,
    pub assignee: Option<String>,
    pub due: Option<NaiveDate>,
    #[facet(skip)]    // not in YAML — lives after the --- block
    pub body: String,
    // ...
}
```

Facet is wasm-safe (no serde dependency for wasm32 targets). The Obsidian plugin compiles vault-core to WASM with facet serialization — same types, same logic, in the browser.

### Why Vox + Facet Matter

They enforce a single source of truth for the API surface:

```
vault-core (Rust)
    │
    ├─→ Vox generates TypeScript types
    │     → integrations call service methods over WebSocket
    │
    ├─→ Facet serializes to YAML
    │     → .md files on disk
    │
    ├─→ Facet serializes to JSON
    │     → metadata, RPC payloads, generated bindings
    │
    └─→ WASM + facet-json
          → Obsidian plugin calls parse_task_yaml() in browser
```

Add a field to `Task` in Rust → it appears in TypeScript, YAML, JSON, and WASM automatically. No manual sync of types across layers.

## Data Flow

### Write Path

```
User action (e.g. complete a task)
  │
  ▼
Client updates local CRDT document
  │
  ├─→ Immediate UI update (optimistic)
  │
  ├─→ CRDT op queued for sync
  │     │
  │     ├─→ [Online] WebSocket sends op to server
  │     │     → Server merges → broadcasts to other clients
  │     │     → Server writes .md file (debounced)
  │     │     → Server pushes to Nextcloud CalDAV + Deck
  │     │
  │     └─→ [Offline] Op stored locally
  │           → Sent on reconnect → merge is automatic (CRDT)
  │
  └─→ Local .md file updated (debounced, not per-keystroke)
```

### Read Path

```
Client needs data (e.g. "show me today's tasks")
  │
  ├─→ [Hot path] Read from local CRDT state (in-memory)
  │     → Instant, no I/O
  │
  ├─→ [Warm path] Query SQLite index
  │     → Sub-millisecond, indexed by status/assignee/due/project
  │
  └─→ [Cold path] Scan .md files on disk
        → Only on first load or index rebuild
        → File watcher keeps index in sync
```

### Sync Path

```
.md file changes (local edit, Nextcloud sync, Obsidian edit)
  │
  ▼
File watcher (notify crate) detects change
  │
  ▼
Parse .md → update CRDT document → update SQLite index
  │
  ▼
Broadcast change to connected WebSocket clients
  │
  ▼
Push to Nextcloud (CalDAV VTODO + Deck card + WebDAV file)
```

## CRDT Architecture

Each collaborative `.md` file is represented by a Loro-backed CRDT document.

### Metadata

YAML frontmatter fields are synchronized as structured CRDT state:

```
Loro document {
  "title": "Mix track 1",
  "status": "InProgress",
  "assignee": "codywright",
  "due": "2026-04-15",
  "priority": "High",
  "tags": ["mixing"],
  "projects": ["Montreal Album"]
}
```

Two people can concurrently change `status` and `assignee`; Loro merges independent field edits and emits explicit conflict events for cases the app needs to surface.

### Body

Markdown content after the `---` block is synchronized through the same Loro document model:

```
Loro text {
  "Apply compression chain and finalize mix.\n\n## Subtasks\n- [x] Import stems\n- [ ] Set up reverb sends\n..."
}
```

Two people can edit different checklist lines simultaneously without rewriting the whole file.

### Persistence

```
CRDT state ──debounced write──→ .md file (source of truth)
CRDT state ──on change──→ SQLite index (query cache)
CRDT state ──on change──→ WebSocket broadcast (real-time)
CRDT state ──on sync──→ Nextcloud CalDAV + Deck (integration)
```

The `.md` file is always regenerated from CRDT state. The CRDT is always rebuildable from the `.md` file. Either can be the bootstrap source.

## Module Map

```
vault-core/
├── src/
│   ├── task.rs              # Task struct (Facet)
│   ├── project.rs           # Project struct (Facet)
│   ├── query.rs             # Filter, Sort, Group engine
│   ├── capture.rs           # NLP quick-add parser
│   ├── rrule.rs             # Recurrence (RFC 5545)
│   ├── vault.rs             # File I/O (.md read/write)
│   ├── service.rs           # Split Vox service traits
│   ├── service_impl.rs      # VaultServiceImpl
│   ├── watch.rs             # File system watcher (notify)
│   │
│   ├── workflows/
│   │   ├── event.rs         # Event, Performance, Setlist, StagePlot, InputList
│   │   ├── output.rs        # OutputManifest, versioned deliverables, feedback
│   │   ├── external.rs      # ExternalRef, FireflyRef, InvoiceNinjaRef, GitHubRef
│   │   └── download.rs      # DownloadPortal, DownloadBundle, role-based distribution
│   │
│   ├── provider/
│   │   ├── traits.rs        # ProjectProvider trait
│   │   ├── registry.rs      # ProjectRegistry (aggregates providers)
│   │   ├── local.rs         # Local filesystem provider
│   │   ├── vault.rs         # Obsidian vault provider
│   │   ├── nextcloud.rs     # Nextcloud provider (WebDAV + CalDAV + Deck + OCS)
│   │   ├── nextcloud_sync.rs# Bidirectional sync engine
│   │   ├── s3.rs            # S3 provider (stub)
│   │   └── webdav.rs        # Generic WebDAV provider (stub)
│   │
│   ├── caldav/
│   │   ├── vtodo.rs         # Task ↔ VTODO conversion
│   │   └── sync.rs          # CalDAV client
│   │
│   ├── crdt/
│   │   ├── loro_doc.rs      # Loro document wrapper
│   │   ├── sync.rs          # WebSocket sync protocol
│   │   └── mod.rs           # CRDT domain facade
│   │
│   └── index/               # (Phase 4)
│       ├── sqlite.rs        # SQLite index over frontmatter
│       └── search.rs        # Full-text search (FTS5)
│
├── Cargo.toml               # Features: default, server, caldav
└── ...

apps/
└── server/                   # Axum HTTP metadata + Vox/CRDT WebSocket server

crates/
├── task-core/                # Domain model, vault implementation, Vox services
├── task-cli/                 # CLI tool
└── task-db/                  # Database/auth adapter layer

integrations/
├── obsidian/plugin/          # WASM plugin for Obsidian
├── nextcloud/                # Planned Nextcloud integration
└── invoice-ninja/            # Planned Invoice Ninja integration

nix/                          # NixOS module for deployment
```

## Dependency Stack

### Core (always available, wasm-safe)
| Crate | Purpose |
|---|---|
| `facet` + `facet-yaml` + `facet-json` | Serialization for all domain types |
| `chrono` | Date/time handling |
| `rrule` | Recurrence rules (RFC 5545) |
| `thiserror` + `eyre` | Error handling |

### Server (feature-gated, not in WASM)
| Crate | Purpose |
|---|---|
| `vox` + `vox-core` | RPC service traits + native service dispatch |
| `tokio` | Async runtime |
| `axum` | HTTP server |
| `notify` | File system watching |
| `reqwest` | HTTP client for integrations |
| `serde` + `serde_json` | Deck API transport types |
| `async-trait` | Async trait support |
| `tracing` | Structured logging |
| `uuid` | ID generation |

### CalDAV (optional feature)
| Crate | Purpose |
|---|---|
| `icalendar` | VTODO parsing/generation |
| `libdav` | CalDAV protocol client |

### Collaboration
| Crate | Purpose |
|---|---|
| `loro` | CRDT document engine |
| `rusqlite` | SQLite index cache |
| `rusqlite_migration` | Index schema evolution |

### Integrations
Integration implementations live under `integrations/` and should call the core Vox services instead of introducing compatibility REST layers.

## Client SDK Strategy

Vox + Facet generate client SDKs from the Rust source:

| Platform | Transport | SDK | Generated From |
|---|---|---|---|
| Platform | Transport | SDK | Generated From |
|---|---|---|---|
| **Service clients** | WebSocket | Vox native RPC | `#[vox::service]` traits |
| **Obsidian** | WASM in-process + generated TypeScript | TypeScript + WASM | Vox descriptors and `wasm-bindgen` exports |
| **CLI** | In-process Rust | Direct Rust calls | — |

Adding a new API method:

1. Add it to the relevant split service trait (`TaskService`, `TimeService`, `CalendarService`, etc.).
2. Implement it on `VaultServiceImpl`.
3. Run `cargo run -p xtask -- codegen` for generated TypeScript bindings.
4. Serve it through `/vox` with the matching Vox dispatcher.

## Security Model

- **Authentication:** Nextcloud users + app passwords (existing)
- **Transport:** HTTPS for Nextcloud API, WSS for WebSocket
- **Secrets:** sops-nix for deployment, `LoadCredential` in systemd
- **Permissions:** Nextcloud folder-level sharing (inherited by projects)
- **Client portal:** Nextcloud public share links with password + expiry
- **No telemetry, no phone-home, no cloud dependency**

## Deployment

### NixOS (recommended)

```nix
services.task-server = {
  enable = true;
  package = inputs.task.packages.${system}.task-server;
  vaultRoot = "/mnt/data/Projects";
  nextcloud.enable = true;
  nextcloud.url = "https://cloud.example.com";
  nextcloud.passwordFile = config.sops.secrets."task/password".path;
  openFirewall = true;
};
```

### Docker (planned)

```yaml
services:
  task-server:
    image: ghcr.io/fasttackstudios/task-server
    volumes:
      - ./projects:/data/projects
    environment:
      - VAULT_ROOT=/data/projects
      - NEXTCLOUD_URL=https://cloud.example.com
```

### Standalone binary

```bash
VAULT_ROOT=./projects task-server
```

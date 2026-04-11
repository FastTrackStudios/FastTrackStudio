# Task — Vision

**A local-first, real-time collaborative production workflow management platform — backed by markdown files you own.**

## The Problem

Production work — music, video, events, software, creative projects — involves tasks, schedules, file delivery, team coordination, budgets, and client communication. These are fragmented across dozens of tools that don't talk to each other. Nothing is self-contained, nothing works offline, and you don't own your data.

## The Solution

**Task** is an open platform that manages the full lifecycle of production work:

- **Local-first** — every client has a full copy of the data. Works offline. Syncs when connected. No server dependency for core operations.
- **Real-time collaborative** — multiple people edit the same project simultaneously. Changes propagate instantly via WebSocket. Conflicts are detected and resolved.
- **File-based source of truth** — plain `.md` files with YAML properties and structured markdown body. Readable by any text editor, portable to any system.
- **Self-contained project folders** — a project is a folder. Tasks, schedules, deliverables, session files, media — everything together. Copy the folder and you have everything.
- **Obsidian-compatible** — works as an Obsidian vault. Properties panel, Dataview, checkboxes, wikilinks — all native.
- **Nextcloud-native** — full integration with Nextcloud Tasks (CalDAV), Deck (kanban), WebDAV (files), and user management for team collaboration.
- **Multiple views, same data** — desktop app, web app, mobile app, CLI, Obsidian, Nextcloud Deck, Nextcloud Tasks, Apple Reminders, any CalDAV client.
- **Events as first-class entities** — concerts, services, recording sessions, shoots. Recurring events with templates and per-instance overrides. Not everything is a project.
- **Download portals** — role-based file distribution. One link for the orchestra, each person picks their part.
- **Self-hosted, privacy-first** — your infrastructure, your data, no cloud dependency.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                          Clients                                 │
│   Desktop · Web · Mobile · CLI · Obsidian · Nextcloud Deck       │
│                                                                  │
│   Each client has a FULL LOCAL COPY of project data (.md files)  │
│   All operations work offline. Sync is eventual.                 │
└────────────────────────┬────────────────────────────────────────┘
                         │  WebSocket (real-time ops)
                         │  REST API (CRUD)
                ┌────────▼────────┐
                │   task-server   │
                │  ┌────────────┐ │
                │  │ Real-time  │ │  WebSocket hub — broadcasts field-level
                │  │ sync layer │ │  operations to all connected clients
                │  └─────┬──────┘ │
                │  ┌─────▼──────┐ │
                │  │  In-memory │ │  Merged state from all clients
                │  │   state    │ │  Debounced writes to .md files
                │  └─────┬──────┘ │
                │  ┌─────▼──────┐ │
                │  │   Index    │ │  SQLite (queries) + Redis (hot cache)
                │  │   cache    │ │  Rebuilt from files at any time
                │  └─────┬──────┘ │
                │  ┌─────▼──────┐ │
                │  │ Sync loop  │ │  Nextcloud CalDAV + Deck + WebDAV
                │  └────────────┘ │
                └────────┬────────┘
                         │
          ┌──────────────┼──────────────┐
          │              │              │
 ┌────────▼──────┐ ┌────▼─────┐ ┌──────▼───────┐
 │    Local      │ │ CalDAV   │ │  Nextcloud   │
 │  Filesystem   │ │ (VTODO)  │ │ (Deck/WebDAV)│
 └────────┬──────┘ └────┬─────┘ └──────┬───────┘
          │              │              │
 ┌────────▼──────────────▼──────────────▼───────┐
 │              Storage Providers                │
 │  Local · NFS · Nextcloud · S3 · WebDAV · Git  │
 └──────────────────────┬───────────────────────┘
                        │
          ┌─────────────▼─────────────┐
          │   Projects & Events (.md)  │
          │                            │
          │  project.md · tasks/*.md   │
          │  event.md · setlist.md     │
          │  outputs/ · downloads/     │
          │  sessions/ · audio/ video/ │
          └───────────────────────────┘
              ↑ source of truth
              Files are always portable.
              Delete the server, keep the folders.
```

## Core Principles

### 1. Files Are the Source of Truth

Every task is a `.md` file. Every project is a folder. No proprietary database, no vendor lock-in. Copy a project folder to a USB drive and everything goes with it.

```
Projects/Montreal Album/
├── project.md              ← project metadata (YAML frontmatter)
├── tasks/
│   ├── Mix track 1.md      ← task with subtask checkboxes
│   ├── Record piano.md     ← assignee, due date, priority
│   └── Master all.md
├── sessions/               ← DAW projects, stems
├── audio/                  ← bounces, raw recordings
└── deliverables/           ← final exports
```

### 2. Open Standards

- **YAML frontmatter** — same format as Obsidian, Hugo, Jekyll
- **CalDAV/VTODO** (RFC 5545) — interop with every calendar/task app
- **WebDAV** — standard file access protocol (Nextcloud, ownCloud, any WebDAV server)
- **Markdown** — subtask checkboxes, notes, descriptions — universally readable

### 3. Multiple Views, Same Data

A single task appears consistently across:

| View | Technology | What you see |
|---|---|---|
| **Obsidian** | Markdown file | YAML properties + checkbox subtasks |
| **Desktop/Mobile app** | Dioxus UI | Rich task card with badges, progress |
| **Nextcloud Deck** | Kanban board | Card in status column with assignee |
| **Nextcloud Tasks** | CalDAV client | VTODO with priority, due date |
| **Apple Reminders** | CalDAV sync | Task with due date, completion |
| **CLI** | Terminal | Filtered, sorted task list |

### 4. Storage-Agnostic Providers

The `ProjectProvider` trait abstracts storage backends. The same task management logic works across:

- **Local filesystem** — direct file access, NFS mounts
- **Nextcloud** — WebDAV + CalDAV + Deck + user APIs
- **S3** — cloud-native object storage (AWS, MinIO, Cloudflare R2)
- **WebDAV** — any WebDAV server
- **Git** — version-controlled project repositories

Multiple providers can be active simultaneously. Personal tasks in your Obsidian vault, team projects on Nextcloud, archived projects on S3 — all unified in one view.

### 5. Collaboration Built on Nextcloud

For teams, Nextcloud provides the collaboration layer:

- **Deck boards** — one board per project, stacks map to task statuses
- **Assignments** — assign tasks to team members, visible in all views
- **Sharing** — project boards auto-shared with team members
- **Notifications** — assignment and due date notifications via Nextcloud
- **Comments** — discussion on Deck cards
- **Activity** — audit trail of who changed what

### 6. Self-Hosted, Privacy-First

Everything runs on your infrastructure:

- **task-server** — lightweight Axum HTTP server, packaged as NixOS module
- **Nextcloud** — self-hosted on your server
- **NFS/local storage** — your drives, your data
- **No cloud dependency** — works fully offline, syncs when connected

## Platform Layers

### Layer 1: Core Engine (`vault-core`)

Domain-agnostic task and project model in Rust:

- Tasks: status, priority, assignee, due/scheduled dates, recurrence (RRULE), time estimates, dependencies, subtasks
- Projects: team, area, status lifecycle, repository links
- Query engine: filter, sort, group by any dimension
- NLP capture: `"Buy groceries tomorrow !high #errands"` → structured task
- File I/O: YAML frontmatter round-trip, atomic writes
- CalDAV/VTODO conversion

### Layer 2: Provider System

Storage-agnostic project access:

- `ProjectProvider` trait with list/get/save/delete/watch
- `ProjectRegistry` aggregates multiple providers
- Bidirectional sync with conflict detection
- Reverse sync: changes in Deck/CalDAV flow back to `.md` files

### Layer 3: Server (`task-server`)

HTTP API + sync engine:

- REST API for projects, tasks, sync status
- Automatic sync loop (configurable interval)
- Manual sync trigger endpoint
- NixOS module for deployment
- Environment-based configuration

### Layer 4: Client Apps

- **Desktop** (Dioxus/WebKit) — sidebar navigation, task list, sheet detail, project dashboard
- **Mobile** (Dioxus) — today view, quick capture, offline-first
- **Web** (Dioxus) — runs alongside the server
- **CLI** — list, add, complete, show, project commands
- **Obsidian plugin** — WASM-powered validation, sorting, querying inside Obsidian

### Layer 5: Design System (`fts-ui`)

55 shadcn v4 maia components, fully standalone:

- Buttons, inputs, cards, badges, tables, dialogs, dropdowns, tooltips, etc.
- Sidebar, navigation menu, command palette, breadcrumbs
- Calendar, accordion, tabs, carousel
- `cn()` utility for Tailwind class merging
- Component showcase for visual verification

### Layer 6: Workflow System

Domain-specific project structures that extend the generic model:

- **Event workflow** — concerts, recording sessions, festivals, multi-act events
  - Setlists with Song objects (key, tempo, duration, takes)
  - Stage plots with positions, backline, power requirements
  - Input lists with per-channel mic/DI/phantom details
  - Personnel roles with acceptance status (Accepted/Unconfirmed/Declined)
  - Run of show with timed cues
  - Changeover plans between acts
  - Venue advance (PA, console, load-in, parking, green room)
  - Budget with line items and payment tracking
  - Deliverables with status lifecycle

- **Output system** — versioned deliverables with feedback and approval
  - Semantic versioning (v1 rough mix → v2 with fixes → v3 approved)
  - Approval workflow (Draft → Review → ChangesRequested → Approved)
  - Timestamped feedback with timecode references for audio
  - ProjectLink for referencing outputs across projects (setlist → song output)
  - Release metadata (ISRC, UPC, credits, splits, distributor)

- **Download portal** — role-based file distribution
  - Portal page with role selector (violinist picks "Violin 1", sees their sheet music)
  - Cross-role browsing (peek at woodwind parts from the violin view)
  - Direct role links for one-click download
  - Shared files (schedule, venue info) auto-included in every bundle
  - Recipient tracking (sent, accessed, downloaded)
  - Nextcloud share integration with password and expiry

### Layer 7: External Integrations

Connect projects to external services — we store references, they own the data:

- **Firefly III** — budgets, transactions, accounts → `firefly_transaction_id`
- **Invoice Ninja** — invoices, quotes, payments → `invoice_ninja_id`
- **GitHub** — issues, PRs, releases → `github_issue`
- **REAPER/DAW** — session files, markers, regions → `daw_session` path link
- **CalDAV** — calendar events, scheduling → native VTODO sync
- **Nextcloud** — files, versions, sharing → native WebDAV

### Layer 8: Client Apps

All sharing the same Dioxus components and fts-ui design system:

- **Desktop** (Dioxus/WebKit) — full app with sidebar, project dashboard, task detail sheet
- **Web** (Dioxus SSR) — same app served by task-server, plus download portal routes
- **Mobile** (Dioxus native) — today view, quick capture, offline-first
- **CLI** — list, add, complete, show, project commands
- **Obsidian plugin** — WASM-powered validation, sorting, querying inside Obsidian

## Roadmap

### Phase 1: Foundation ✅
- [x] vault-core with full task/project schema
- [x] File I/O with YAML frontmatter round-trip
- [x] Query engine, NLP capture, recurrence
- [x] Desktop app with Plane-inspired UI
- [x] CLI tool
- [x] fts-ui component library (55 components)
- [x] Obsidian plugin (WASM)

### Phase 2: Collaboration ✅
- [x] Nextcloud provider (WebDAV + CalDAV + Deck + OCS)
- [x] Bidirectional sync with Nextcloud Tasks and Deck
- [x] Assignees, team management via Nextcloud users
- [x] Project → Deck board auto-creation with shared access
- [x] Subtask checkboxes in card descriptions
- [x] Reverse sync (Deck/CalDAV → .md files)
- [x] task-server with sync loop
- [x] NixOS module for deployment

### Phase 3: Workflows & Outputs ✅ (schema)
- [x] Event workflow schema (setlists, stage plots, input lists, personnel, schedule)
- [x] Output system (versioned deliverables, feedback, approval)
- [x] External integration references (Firefly III, Invoice Ninja, GitHub, DAW)
- [x] Download portal schema (role selector, cross-role browsing, recipient tracking)
- [ ] Workflow file I/O (read/write event.md, setlist.md, etc.)
- [ ] Download portal Dioxus page (role selector UI)
- [ ] Bundle generation (collect files into role-specific folders)
- [ ] Nextcloud share creation for bundles

### Phase 4: Local-First & Real-Time
- [ ] Automerge CRDT for metadata (task/event fields — concurrent edits merge automatically)
- [ ] Yrs CRDT for body text (markdown body — collaborative subtask/note editing)
- [ ] WebSocket sync via yrs-axum (field-level ops broadcast to all clients)
- [ ] Offline operation queue (CRDT ops queue locally, merge on reconnect)
- [ ] SQLite index with rusqlite (fast queries over frontmatter, rebuilt from files)
- [ ] cr-sqlite for CRDT-enabled index sync between devices
- [ ] Entity change log (Trilium pattern — track who changed what, when)
- [ ] Conflict UI (show both versions when CRDTs can't auto-merge)

### Phase 5: Web App & Portal
- [ ] Dioxus web app served by task-server (SSR)
- [ ] Download portal route (`/portal/:slug`) with role selector
- [ ] Audio preview/streaming in portal
- [ ] Mobile app (Dioxus native, Swift client via Vox)
- [ ] iOS widgets (WidgetKit + Vox Swift bindings)
- [ ] Push notifications (Nextcloud + APNs)

### Phase 5.5: Scale & Search
- [ ] Full-text search (SQLite FTS5 or Tantivy)
- [ ] S3 provider implementation
- [ ] Subtask progress tracking (parse `- [x]` lines)
- [ ] Comment sync between Deck cards and .md files
- [ ] Activity feed / audit trail

### Phase 6: Views & UX
- [ ] Kanban board view in the app
- [ ] Calendar view (month/week)
- [ ] Gantt / timeline view
- [ ] Command palette (Cmd+K)
- [ ] Keyboard shortcuts
- [ ] Drag-and-drop task reordering

### Phase 7: Finance & Business
- [ ] Firefly III integration (budget sync, transaction linking)
- [ ] Invoice Ninja integration (invoice generation, payment tracking)
- [ ] Client portal (scoped Nextcloud shares for client review/approval)
- [ ] Expense tracking per project
- [ ] Revenue attribution per deliverable

### Phase 8: Ecosystem
- [ ] GitHub Issues bidirectional sync
- [ ] Webhook API for external tools
- [ ] REAPER integration (session markers → tasks, region → song)
- [ ] Custom workflow definitions (user-defined schemas)
- [ ] Community workflow templates
- [ ] "Task Compatible" certification standard

## Design Philosophy

- **Local-first** — every client works fully offline. Sync is eventual, not required. Your laptop has the data, not just a cache of it.
- **Files are forever** — markdown outlasts any app, service, or company. No proprietary format, no database migration, no export needed.
- **Real-time when connected** — field-level operations over WebSocket. See changes as they happen. But never block on connectivity.
- **Events ≠ Projects** — a weekly church service is an event that recurs, not a new project every week. Events have templates with per-instance overrides.
- **Properties are simple, body is rich** — YAML frontmatter for key/value metadata (like Obsidian properties). Structured markdown (tables, checklists) in the body for complex data (setlists, input lists).
- **Generic core, specific edges** — the engine is universal. Domain knowledge (music events, video shoots, software releases) lives in workflow schemas.
- **Open standards** — CalDAV/VTODO, WebDAV, Markdown, YAML. Not "compatible with" — actually IS these standards.
- **Self-hosted, privacy-first** — your infrastructure, your data. No cloud dependency, no telemetry, no lock-in.
- **Multiple views, one truth** — desktop, web, mobile, CLI, Obsidian, Nextcloud Deck, Apple Reminders. Same files underneath.
- **Incremental adoption** — start with Obsidian and markdown files. Add Nextcloud when you need teams. Add the server when you need real-time and portals. Each layer is optional.

## Known Limitations & Trade-offs

- **Rename/move breaks links** — file paths are the identifiers. Renaming a project folder breaks references from other projects. Mitigation: UUID-based linking with path as cached lookup.
- **File-level conflict resolution** — two offline edits to the same `.md` file produce a conflict. Mitigation: field-level CRDT operations in the real-time layer; file sync is the fallback.
- **Performance at scale** — scanning thousands of `.md` files for queries is slow without an index. Mitigation: SQLite + Redis cache layer, rebuilt from files on demand.
- **No atomic multi-file transactions** — completing a task and updating project progress touches two files. Mitigation: write-ahead log or accept eventual consistency.
- **Permissions are folder-level** — Nextcloud sharing is by folder, not by field. Mitigation: structure folders to match permission boundaries.
- **Not Google Docs** — real-time collaboration is field-level operations, not character-level co-editing of prose. The body of a `.md` file is not collaboratively editable in real-time (yet).

These trade-offs are intentional. We optimize for **data ownership, portability, and offline capability** over **consistency guarantees and real-time text co-editing**. For production workflow management — where the work products are files (audio, video, documents) and the metadata is structured — this is the right trade.

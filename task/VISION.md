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
- **Multiple views, same data** — CLI, Obsidian, Nextcloud Deck, Nextcloud Tasks, Apple Reminders, any CalDAV client, and future apps over Vox.
- **Events as first-class entities** — concerts, services, recording sessions, shoots. Recurring events with templates and per-instance overrides. Not everything is a project.
- **Download portals** — role-based file distribution. One link for the orchestra, each person picks their part.
- **Self-hosted, privacy-first** — your infrastructure, your data, no cloud dependency.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Clients and Integrations                       │
│   CLI · Obsidian · Nextcloud · Invoice Ninja · future apps        │
│                                                                  │
│   Each client has a FULL LOCAL COPY of project data (.md files)  │
│   All operations work offline. Sync is eventual.                 │
└────────────────────────┬────────────────────────────────────────┘
                         │  Vox RPC + Loro CRDT WebSockets
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

### 7. AI Agents Are First-Class Team Members

The system is designed from the ground up for a world where AI agents work alongside humans. A fleet of AI workers in your Nextcloud organization can read, write, comment, tag, and manipulate project data just like any human team member. This isn't an afterthought API — it's a core architectural principle.

**Why this matters:**
- AI agents need **structured data** to reason about projects. YAML frontmatter is trivially parseable by any LLM — no scraping, no brittle HTML parsing.
- AI agents need **the same API** as humans. Vox RPC, WebSocket, REST — bots call the same endpoints, get the same responses.
- AI agents need **identity**. Each bot is a Nextcloud user with its own name, avatar, and permissions. When a bot leaves a comment, it shows up as `@mixing-assistant` just like `@cody`.
- AI agents need **context**. The markdown file format means an agent can read an entire project by scanning a folder. Tasks, sessions, comments, approvals — all plain text, all in one place.
- AI agents need **actions**. Create tasks, complete tasks, leave feedback, approve mixes, tag deliverables, trigger syncs — all through typed RPC calls.

**What this looks like in practice:**

| Agent | What it does | How |
|-------|-------------|-----|
| **@mixing-assistant** | Listens for new mix uploads, runs loudness analysis, leaves a comment with LUFS readings at problem spots | Watches file events → analyzes audio → `add_comment` with timecode ranges |
| **@project-manager** | Checks due dates daily, pings assignees on overdue tasks, generates weekly status summaries | Queries tasks → creates notifications → posts summary to project body |
| **@transcription-bot** | Transcribes vocal recordings, adds lyrics to the writing workflow | Reads audio files → transcribes → updates `WritingWorkflow.lyric_versions` |
| **@qc-checker** | Validates deliverables meet spec (sample rate, bit depth, format), flags issues | Reads file metadata → checks against project spec → creates task if non-compliant |
| **@social-media-bot** | When a master is approved, drafts social posts, creates promo tasks | Watches approval events → generates content → creates tasks in marketing project |

**Design implications:**

- **Comments are typed** — not just free text. An AI can leave a comment with a timecode range, and the UI renders it as a waveform marker. Structured data in, structured UI out.
- **Actions are idempotent** — bots can retry safely. Completing an already-complete task is a no-op, not an error.
- **Everything has an API** — if a human can do it in the UI, a bot can do it through RPC. No "admin-only" actions that bypass the API.
- **Audit trail is universal** — bot actions show up in the activity feed alongside human actions. Full accountability.
- **@mentions work for bots** — mention `@mixing-assistant` in a comment and the bot gets a notification, just like a human. The notification system doesn't distinguish.

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

Core service runtime:

- Vox RPC services for tasks, projects, time, calendar, clients, invoices, and activity
- Loro CRDT WebSocket endpoint for realtime document collaboration
- Minimal HTTP metadata endpoints for health, server discovery, auth, and CRDT status
- Better-auth users, sessions, and organizations
- NixOS module for deployment
- Environment-based configuration

### Layer 4: Clients and Integrations

- **CLI** — list, add, complete, show, project commands
- **Obsidian plugin** — WASM-powered validation, sorting, querying inside Obsidian
- **Nextcloud integration** — WebDAV, CalDAV, files, shares, and tasks
- **Invoice Ninja integration** — invoices, quotes, payments, and client references
- **Future apps** — should use the same Vox service boundary rather than compatibility REST

### Layer 5: Workflow System

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

### Layer 6: External Integrations

Connect projects to external services — we store references, they own the data:

- **Firefly III** — budgets, transactions, accounts → `firefly_transaction_id`
- **Invoice Ninja** — invoices, quotes, payments → `invoice_ninja_id`
- **GitHub** — issues, PRs, releases → `github_issue`
- **REAPER/DAW** — session files, markers, regions → `daw_session` path link
- **CalDAV** — calendar events, scheduling → native VTODO sync
- **Nextcloud** — files, versions, sharing → native WebDAV

## Roadmap

### Phase 1: Foundation ✅
- [x] vault-core with full task/project schema
- [x] File I/O with YAML frontmatter round-trip
- [x] Query engine, NLP capture, recurrence
- [x] CLI tool
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
- [x] Workflow file I/O (generic parse_frontmatter/render_frontmatter for all types)
- [ ] Download portal Dioxus page (role selector UI)
- [x] Bundle generation (collect files into role-specific folders)
- [x] Nextcloud share creation for bundles (OCS sharing API)

### Phase 4: Local-First & Real-Time
- [x] Loro CRDT for metadata and markdown body collaboration
- [x] WebSocket sync via `/crdt` with field-level events and conflict notifications
- [x] Offline operation queue (CRDT ops queue locally, merge on reconnect)
- [x] SQLite index with rusqlite (fast queries over frontmatter, rebuilt from files)
- [ ] cr-sqlite for CRDT-enabled index sync between devices
- [x] Entity change log (implemented in SQLite index changes table)
- [ ] Conflict UI (show both versions when CRDTs can't auto-merge)

### Phase 5: Client Experience & Portal
- [ ] Decide first-party app surface after core service architecture stabilizes
- [ ] Download portal route (`/portal/:slug`) with role selector
- [ ] Audio preview/streaming in portal
- [ ] Mobile client strategy
- [ ] iOS widgets
- [ ] Push notifications (Nextcloud + APNs)

### Phase 5.5: Scale & Search
- [x] Full-text search (SQLite FTS5 in TaskIndex)
- [x] S3 provider implementation (MinIO/R2 compatible, full CRUD)
- [x] Subtask progress tracking (parse `- [x]` lines, subtask_progress())
- [x] Comment sync between Deck cards and .md files (## Comments section)
- [x] Activity feed / audit trail (SQLite changes table + /api/activity)

### Phase 6: Views & UX
- [ ] Kanban board view in the app
- [ ] Calendar view (month/week)
- [ ] Gantt / timeline view
- [ ] Command palette (Cmd+K)
- [ ] Keyboard shortcuts
- [ ] Drag-and-drop task reordering

### Phase 7: Finance & Business
- [x] Firefly III integration (budget sync, transaction linking)
- [x] Invoice Ninja integration (invoice generation, payment tracking)
- [ ] Client portal (scoped Nextcloud shares for client review/approval)
- [ ] Expense tracking per project
- [ ] Revenue attribution per deliverable

### Phase 8: Ecosystem
- [x] GitHub Issues bidirectional sync (pull/push/create)
- [x] Webhook API (POST /api/tasks, POST /api/tasks/:title/complete)
- [ ] REAPER integration (session markers → tasks, region → song)
- [ ] Custom workflow definitions (user-defined schemas)
- [ ] Community workflow templates
- [ ] "Task Compatible" certification standard

### Phase 9: AI Agent Platform
- [ ] Bot user accounts in Nextcloud (dedicated identities with avatars)
- [ ] Agent SDK — typed Rust/Python/TS client for bot development
- [ ] Agent event bus — subscribe to file changes, comments, approvals, uploads
- [ ] Agent action permissions — scoped API keys per bot (read-only, comment-only, full)
- [ ] Built-in agents: loudness analysis, transcription, QC validation, status summarizer
- [ ] Agent marketplace — community-built agents installable via config
- [ ] MCP (Model Context Protocol) server — expose project data as MCP resources for LLM tools
- [ ] Claude Code / Cursor integration — `.claude/` hooks that understand project context

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
- **AI-native** — structured markdown is the perfect format for AI agents. Every field is parseable, every action is API-callable, every entity is addressable. Bots are team members, not integrations.

## Known Limitations & Trade-offs

- **Rename/move breaks links** — file paths are the identifiers. Renaming a project folder breaks references from other projects. Mitigation: UUID-based linking with path as cached lookup.
- **File-level conflict resolution** — two offline edits to the same `.md` file produce a conflict. Mitigation: field-level CRDT operations in the real-time layer; file sync is the fallback.
- **Performance at scale** — scanning thousands of `.md` files for queries is slow without an index. Mitigation: SQLite + Redis cache layer, rebuilt from files on demand.
- **No atomic multi-file transactions** — completing a task and updating project progress touches two files. Mitigation: write-ahead log or accept eventual consistency.
- **Permissions are folder-level** — Nextcloud sharing is by folder, not by field. Mitigation: structure folders to match permission boundaries.
- **Not Google Docs** — real-time collaboration is field-level operations, not character-level co-editing of prose. The body of a `.md` file is not collaboratively editable in real-time (yet).

These trade-offs are intentional. We optimize for **data ownership, portability, and offline capability** over **consistency guarantees and real-time text co-editing**. For production workflow management — where the work products are files (audio, video, documents) and the metadata is structured — this is the right trade.

# Task — Vision

## The Problem

Task and project management is fragmented. Your tasks live in one app, your files in another, your kanban boards in a third, and your calendar in a fourth. None of them talk to each other. You lose context switching between tools, and nothing is self-contained.

For teams, it's worse — project files are scattered across drives, cloud storage, and collaboration tools. There's no single source of truth that ties a project's tasks, files, and communication together.

## The Solution

**Task** is an open, file-based task and project management system that works everywhere:

- **Markdown files are the source of truth** — plain `.md` files with YAML frontmatter, readable by any text editor
- **Obsidian-compatible** — works as an Obsidian vault for personal tasks
- **Nextcloud-native** — full integration with Nextcloud Tasks, Deck, WebDAV, and user management for team collaboration
- **Self-contained project folders** — each project is a folder with everything: `project.md`, `tasks/*.md`, session files, media, deliverables
- **Multiple views, same data** — desktop app, CLI, Obsidian, Nextcloud Deck (kanban), Nextcloud Tasks (list), CalDAV clients (Apple Reminders, Thunderbird, GNOME To Do)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Clients                              │
│  Desktop App · Mobile App · Web App · CLI · Obsidian Plugin  │
└────────────────────────────┬────────────────────────────────┘
                             │
                    ┌────────▼────────┐
                    │   task-server   │  HTTP API + sync loop
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
     ┌────────▼──────┐ ┌────▼─────┐ ┌──────▼───────┐
     │ vault-core    │ │ CalDAV   │ │ Nextcloud    │
     │ (Rust engine) │ │ (VTODO)  │ │ (Deck/WebDAV)│
     └────────┬──────┘ └────┬─────┘ └──────┬───────┘
              │              │              │
     ┌────────▼──────────────▼──────────────▼───────┐
     │              Storage Providers                │
     │  Local · NFS · Nextcloud · S3 · WebDAV · Git  │
     └──────────────────────┬───────────────────────┘
                            │
              ┌─────────────▼─────────────┐
              │    Project Folders (.md)    │
              │  project.md + tasks/*.md   │
              │  + sessions/ audio/ video/ │
              └───────────────────────────┘
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

### Phase 4: Polish & Scale
- [ ] Real-time sync (webhooks/push instead of polling)
- [ ] Conflict resolution (three-way merge)
- [ ] Full-text search across all tasks and projects
- [ ] S3 provider implementation
- [ ] Subtask progress tracking (parse `- [x]` lines)
- [ ] Comment sync between Deck cards and .md files
- [ ] Activity feed / audit trail

### Phase 5: Web App & Portal
- [ ] Dioxus web app served by task-server (SSR)
- [ ] Download portal route (`/portal/:slug`)
- [ ] Audio preview/streaming in portal
- [ ] Mobile app (Dioxus native)
- [ ] iOS widgets (WidgetKit)
- [ ] Push notifications (Nextcloud + APNs)
- [ ] Offline-first with sync queue

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

- **Generic core, specific edges** — the engine is universal; domain knowledge lives in workflows and integrations
- **Files are forever** — markdown outlasts any app or service
- **Open by default** — schema, protocols, and standards are open
- **Self-hosted first** — your data on your hardware
- **Multiple views, one truth** — every tool sees the same data
- **Incremental adoption** — start with Obsidian, add Nextcloud when you need teams, add the server when you need APIs

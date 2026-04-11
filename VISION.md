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

### Layer 6: Integrations

Domain-specific workflow integrations that create tasks and projects:

- **Fast Track Studio** — music production lifecycle (tracking → mixing → mastering → release)
- **Fitness** — training plans, workout sessions, progressive overload
- **Music Practice** — practice sessions, repertoire tracking, streaks
- **Learning** — courses, study sessions, spaced repetition
- **GitHub** — issues ↔ tasks, PR status tracking
- **REAPER** — DAW session markers → tasks

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

### Phase 3: Polish & Scale
- [ ] Real-time sync (webhooks/push instead of polling)
- [ ] Conflict resolution (three-way merge)
- [ ] Full-text search across all tasks and projects
- [ ] S3 provider implementation
- [ ] Subtask progress tracking (parse `- [x]` lines)
- [ ] Comment sync between Deck cards and .md files
- [ ] Activity feed / audit trail

### Phase 4: Mobile & Web
- [ ] Mobile app (Dioxus native)
- [ ] Web app deployment
- [ ] iOS widgets (WidgetKit)
- [ ] Push notifications (Nextcloud + APNs)
- [ ] Offline-first with sync queue

### Phase 5: Views & UX
- [ ] Kanban board view in the app
- [ ] Calendar view (month/week)
- [ ] Gantt / timeline view
- [ ] Command palette (Cmd+K)
- [ ] Keyboard shortcuts
- [ ] Drag-and-drop task reordering

### Phase 6: Integrations & Ecosystem
- [ ] GitHub Issues sync
- [ ] Webhook API for external tools
- [ ] REAPER integration (session markers → tasks)
- [ ] Custom workflow definitions
- [ ] Community workflow marketplace
- [ ] "Task Compatible" certification standard

## Design Philosophy

- **Generic core, specific edges** — the engine is universal; domain knowledge lives in workflows and integrations
- **Files are forever** — markdown outlasts any app or service
- **Open by default** — schema, protocols, and standards are open
- **Self-hosted first** — your data on your hardware
- **Multiple views, one truth** — every tool sees the same data
- **Incremental adoption** — start with Obsidian, add Nextcloud when you need teams, add the server when you need APIs

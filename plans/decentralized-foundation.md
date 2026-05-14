# Decentralized, Knowledge-First, Offline-First Foundation

This document pins down the architecture before more code lands. It
supersedes every earlier scoping doc in `plans/`. Subsequent work
follows the implementation phases at the bottom.

## 1. Vision (in one paragraph)

We're building a **self-hosted, decentralized, offline-first
collaborative knowledge platform**. The core is an
Obsidian/Logseq-compatible Knowledge layer — pages, blocks,
frontmatter, wiki-links, backlinks. Every other "feature" people
expect (tasks, projects, people, clients, calendar, fitness logs,
recipes, audio sessions, …) is **a `kind:` frontmatter convention
+ a Bases-style custom view over Knowledge pages**, not its own
data type. Servers are **per-org**; a typical user runs their own
server hosting their personal life + side projects + a music
studio + a software company, and connects as an *employee* to a
separate company's server, and as an *anonymous share-link
recipient* to a client's server. The client app is the
federation layer — it talks to N servers, each sovereign over
its data, and merges views client-side. Markdown round-trip
(import + export) is a gateway feature, not the source of truth;
the CRDT doc is canonical.

## 2. Actors and roles

Five distinct relationships any user can have with any server.
The capability layer (§5) is what makes these all expressible:

| Role | Example | Granted by | Scope |
|---|---|---|---|
| **Owner** | You on your personal server | Server bootstrap | Everything |
| **Admin** | You on your studio server | Owner | Org-wide, can manage members |
| **Member** | You as an employee | Admin invitation | Org-scoped; role-limited (see architect-auth) |
| **Collaborator** | A client working on one project on your server | Project admin | One project only; role limited within it |
| **Anonymous via share link** | A client downloading their files; an external reviewer | Share-link token | Single resource, single scope, time-limited |

A user has **N identities** — one per server they connect to.
Identities are not federated. The client app is the only place
data ever crosses orgs.

## 3. Architecture overview

```
                          ┌─────────────────────────────┐
                          │       Client (Dioxus)       │
                          │   Multi-server federation   │
                          └──┬─────────┬─────────┬──────┘
                  per-server │         │         │
                   identity  │         │         │ per-server
                  + tokens   │         │         │ vox sessions
                             ▼         ▼         ▼
                ┌────────────────┐  ┌────────┐  ┌──────────────┐
                │ personal.       │  │ studio │  │ acme-corp    │
                │ cody.dev        │  │ .fts.dev│  │ .com         │
                │ (owner)         │  │ (admin)│  │ (employee)   │
                └────────────────┘  └────────┘  └──────────────┘
                Each server is sovereign over its own org's data.
```

Each server (one per org):

```
┌──────────────────────────────────────────────────────────────┐
│                     task-server (per org)                    │
│                                                              │
│  ┌─ architect-auth ────────┐    ┌─ Capabilities ─────────┐   │
│  │  Users, sessions,       │ →  │  Decode token from WS  │   │
│  │  orgs, roles, teams,    │    │  Resolve to user OR    │   │
│  │  invitations, OAuth     │    │  share-link scope      │   │
│  │  → vox services         │    │  Attach to req context │   │
│  └─────────────────────────┘    └────────────────────────┘   │
│                                              │               │
│                                              ▼               │
│  ┌─ vox /vox endpoint ──────────────────────────────────┐   │
│  │   subscribe(doc_id, since, output: Tx<UpdateBytes>) │   │
│  │   apply_update(doc_id, bytes)                       │   │
│  │   per-entity Repo dispatchers (read paths)          │   │
│  │   AttachmentService (upload/download presigned URL) │   │
│  │   ShareService (create/list/revoke share links)     │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌─ Doc registry (HashMap<DocId, Arc<CrdtDoc>>) ────────┐   │
│  │   org-wide vault doc      (members, schemas,         │   │
│  │                            workflows, people, etc.)  │   │
│  │   project/<uuid> doc × N  (one per project)          │   │
│  │   page/<uuid> doc × N     (one per shared Knowledge  │   │
│  │                            page that's heavily       │   │
│  │                            edited — defer to Phase 3)│   │
│  └─────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌─ Persistence ──────────────────────────────────────────┐ │
│  │   SeaORM/SQLite per server (the org's data)           │ │
│  │   Object store for attachments (filesystem v0, S3 v1) │ │
│  └────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```

## 4. Knowledge as the data substrate

**Everything stored on a server is Knowledge.** No per-feature
data types. The Knowledge proto already on main defines:

- `Vault` — a workspace boundary
- `Folder` — first-class for empty-folder persistence
- `Page` — one markdown/canvas/base file with frontmatter
- `Block` — one paragraph/heading/list-item with stable id
- `KnowledgeTag` — global tag registry
- `Base` — Obsidian `.base` filtered/sorted view

The `refs.rs` types already model the relations:

- `LinkRef` — `[[Page]]` / `[[Page#heading]]` / `[[Page#^block]]`
- `EmbedRef` — `![[Page]]`
- `TagRef` — `#nested/tag`
- `EntityRef` — `[[entity://kind/uuid]]` (typed)
- `BlockRef` — `((block-id))`

### Custom workflows via `kind:` frontmatter

A Person is a Page with `kind: person`. A Project is a Page with
`kind: project`. A Workout is a journal Page with `kind: workout`.
A Recording Session is a Page with `kind: recording_session`. A
Workflow Definition (org admin-authored) is a Page with `kind:
workflow_template`.

Pages reference each other via wiki links:

```yaml
---
kind: project
client: "[[Acme Corporation]]"
lead: "[[Cody]]"
members: ["[[Alice]]", "[[Bob]]"]
status: in-progress
budget: 25000
acl:
  read: ["@org-members"]
  write: ["[[Cody]]", "[[Alice]]"]
  share_links: [4f3a..., 7b2c...]
---
# Q4 Album Mix

Tracking the mix sessions for [[Acme Corporation]]'s Q4 album.
- [[Recording Session 2026-11-12]]
- [[Recording Session 2026-11-19]]
```

When the user clicks `[[Acme Corporation]]`, the client navigates
to that page. The Acme page sees backlinks from every project
mentioning it — automatically.

### Two vault tiers per server

Server holds two Knowledge vaults:

- **`vault://org`** — global to the server. Members, schemas,
  glossary, workflow templates, org-wide policies. Every member
  reads; admins write.
- **`vault://project/<uuid>`** — one per project. Project members
  read+write per ACL. Pages here can reference org-vault pages
  freely.

A "person" lives in the org vault. A "project task" lives in the
project vault. A wiki-link from a project page to a person page
crosses vault boundaries (resolved at query time, both vaults are
on the same server).

### Custom views

A "Tasks" view is a Bases query: pages with `kind: task`,
filtered by status. A "Clients directory" is `kind: client`
sorted by name. A "This week" view is journal pages with date in
the last 7 days. Each is a small Rust component that consumes a
Bases query result and renders it (list / kanban / calendar /
gallery / map).

**Zero per-feature data crates.** Adding a new entity type is
adding a Knowledge page with a `kind:` frontmatter convention.

## 5. Capability tokens

### Goals

- Anonymous share links: token-in-URL grants scoped access with
  no account.
- Authenticated sessions: still bind to architect-auth users +
  org/role.
- Per-resource scope: a token grants access to **one resource
  (typically a project doc) with one scope** (read / read-write /
  read-attachments-only).
- Time-bound and revocable.
- No need for the recipient to register.

### Shape

```rust
// Wire format: signed by the server's secret key.
struct CapabilityToken {
    /// Token id; the server maintains a revocation table.
    id: Uuid,
    /// The doc this token gates access to.
    doc_id: DocId,
    /// What operations are allowed.
    scope: TokenScope,
    /// Issuer (server) — important when a client roams between
    /// servers and tokens accidentally get sent to the wrong one.
    issuer: ServerId,
    /// Optional user binding. If `Some`, this token only works
    /// for that auth user (a "magic link" to a private dashboard).
    /// If `None`, anonymous — anyone with the bytes is authorized.
    subject: Option<UserId>,
    issued_at: i64,
    expires_at: Option<i64>,
}

enum TokenScope {
    /// Read entities + subscribe to doc updates.
    Read,
    /// Read + apply_update (full collab).
    ReadWrite,
    /// Read only the attachments collection; no doc subscribe.
    /// Used for "client download your files" links.
    AttachmentsOnly,
    /// Custom: per-entity-type allow/deny lists.
    Custom { ops: Vec<String> },
}
```

### Encoding

**Use vox/Facet-encoded bytes + Ed25519 signature**, not JWT.

- We already have facet binary encoding everywhere.
- JWT's base64-JSON overhead buys nothing for non-browser
  scenarios.
- A token is a short opaque string (the facet bytes,
  base64url-encoded for URL safety) appended to the share URL.

`https://studio.example.com/share/<base64url-token>`

### Transport

Token rides as `?cap=<base64url>` on the vox WS handshake URL.
Server middleware parses + verifies + attaches a
`Capability { user, doc, scope }` to the request context. Every
vox dispatcher checks the cap before serving.

For architect-auth-issued sessions, the same query parameter
carries a session token; capability middleware tries token
formats in order until one verifies.

### Revocation

Server keeps a `RevokedTokens` table (token_id, revoked_at).
Capability middleware short-circuits if the token id is in the
table. Project admins call `ShareService::revoke(token_id)` to
invalidate a link.

## 6. ACL resolution

Each project doc carries ACL frontmatter on its root page
(`README.md` or equivalent inside the project vault). The
resolver runs on every authenticated call:

```rust
fn authorize(req: &Request, doc: &CrdtDoc) -> AuthDecision {
    match req.capability {
        Capability::ShareToken { scope, doc_id, .. }
            if doc_id == doc.id => scope.allows(req.method),

        Capability::AuthUser { user_id, .. } => {
            let acl = doc.read_root_acl();  // the YAML frontmatter
            // Resolve wiki-links to auth user ids via the
            // server's wiki-link index. Some [[Names]] won't
            // have an `auth_user_id` — they're just contacts,
            // not accounts. Those entries don't grant access.
            for entry in acl.write {
                if let Some(uid) = resolve_wikilink_to_user(&entry) {
                    if uid == user_id { return AuthDecision::Allow(WriteScope) }
                }
            }
            // Fallback: org-level role.
            if user_id.is_org_member() && acl.read_anyone_in_org {
                AuthDecision::Allow(ReadScope)
            } else {
                AuthDecision::Deny
            }
        }

        Capability::None => AuthDecision::Deny,
    }
}
```

### Wiki-link → user resolution

When a Person page in the org vault has frontmatter
`auth_user_id: <uuid>`, that page is **bound** to an
architect-auth user. ACL entries like `"[[Cody]]"` resolve to
that user via a server-maintained index
`page_basename → (page_id, auth_user_id)`.

A Person page without `auth_user_id` is just a contact — they
appear in directories, can be wiki-linked, can be tagged in
project members, but **don't grant any access** because no
account exists to grant access TO.

This is the "promote a contact to a collaborator" flow:

1. Admin creates `[[Jane Doe]]` page (kind: person, email: jane@…).
2. Admin invites Jane via architect-auth: `CreateInvitation`.
3. Jane accepts, gets an `auth_user_id`.
4. Admin (or server, automatically) writes `auth_user_id: <jane's uuid>` to her page's frontmatter.
5. Now `"[[Jane Doe]]"` in any project ACL grants Jane access.

## 7. Doc-id transport

### Naming

Doc IDs are **server-local**. The server already implies the org.
Format: `<resource-type>/<uuid>` or named like `vault/org` for the
single org vault.

| Doc id | Contents |
|---|---|
| `vault/org` | Org-wide Knowledge vault: people, schemas, workflows, glossary |
| `project/<uuid>` | One project — its tasks, milestones, files, attachments, ACL |
| `user/<uuid>` | Per-user private state (inbox, saved views) — *Phase 4* |

### WorkspaceSync trait, revised

```rust
#[vox::service]
trait WorkspaceSync {
    /// Subscribe to one doc. `since` is the peer's Loro
    /// VersionVector — server sends only the delta. First message
    /// is either a `Snapshot` (if `since == empty`) or an
    /// incremental update batch.
    async fn subscribe(
        &self,
        doc_id: DocId,
        since: VersionVectorBytes,
        output: Tx<UpdateBytes>,
    ) -> Result<(), SyncError>;

    /// Push local commits to a specific doc.
    async fn apply_update(
        &self,
        doc_id: DocId,
        bytes: UpdateBytes,
    ) -> Result<(), SyncError>;

    /// List doc IDs the caller can see (filtered by capability).
    async fn list_docs(&self) -> Result<Vec<DocSummary>, SyncError>;
}
```

`DocId` is the typed string `(kind, uuid)` pair.

Server's `HashMap<DocId, Arc<CrdtDoc>>` is the registry. LRU
eviction of cold docs to keep RAM bounded. Persistence loads on
first subscribe.

### Per-entity sync (still desirable, deferred)

Inside one project doc, a peer might only care about `task` blocks
and not `recording_session` blocks. Per-entity-kind sync streams
are an optimization layered on top of doc-id subscribe. Defer to
Phase 5.

## 8. Client federation

The client owns a `ServerRegistry`:

```rust
struct ServerRegistry {
    servers: Vec<ServerEntry>,
}

struct ServerEntry {
    url: ServerUrl,                // wss://studio.example.com/vox
    name: String,                  // "FastTrackStudio"
    identity: Option<AuthIdentity>,// signed-in user, if any
    capabilities: Vec<Capability>, // share tokens this client holds
    session: Option<LiveSession>,  // live vox session if connected
}
```

On startup, the client tries to reconnect to every server in the
registry. Each gets its own `LiveSession` (the pattern we
already shipped). UI views fan queries out to every connected
server, render the union.

**Anonymous mode.** The client can run with zero
`ServerEntry::identity`s — only `capabilities` (share-link tokens).
The window opens at `https://server.example.com/share/<token>`
and the client extracts the token, registers the URL, and opens
a session with that capability.

### Cross-server wiki links (deferred)

A future extension: `[[work:Cody]]` resolves to "server tagged
`work` in my registry, page named `Cody`". Resolution is purely
client-side; servers never know about each other. Phase 6+.

## 9. Attachments

Large media (audio masters, video deliverables, RAW photos, DAW
project files) doesn't belong in Loro. Object store, referenced
from Knowledge pages.

### Service

```rust
#[vox::service]
trait AttachmentService {
    /// Returns a presigned URL the client uploads the bytes to.
    /// The capability check has already verified the caller can
    /// write to this project.
    async fn initiate_upload(
        &self,
        project_id: Uuid,
        filename: String,
        content_hash: String,
        size_bytes: u64,
    ) -> Result<UploadTicket, AttachmentError>;

    /// Returns a short-lived presigned URL for download. Capability
    /// checks the caller's access to the project.
    async fn get_download_url(
        &self,
        attachment_id: Uuid,
    ) -> Result<DownloadUrl, AttachmentError>;
}
```

### Backend tiers

| v0 | Local filesystem under `${TASK_DATA_DIR}/attachments/<project_id>/<hash>`. Server-signed URLs valid for 5 min, served by axum at `/files/...`. |
| v1 | S3-compat (MinIO, Backblaze B2, AWS). Same RPC surface; server hands out presigned S3 URLs. |
| v2 | Content-addressed deduplication (same hash across projects = one blob). |

### Loro entity reference

A Block with `kind: attachment` carries `attachment_id: <uuid>` +
`filename`, `content_hash`, `size_bytes`, `mime_type` in frontmatter.
The actual bytes are NOT in Loro. The block is just a pointer.

### "Send files to a client without an account"

1. Project admin creates a share link, scope =
   `AttachmentsOnly`, expires in 30 days.
2. Client opens the URL.
3. Anonymous mode: the client subscribes to the project's
   attachments listing, downloads what they need.
4. Project admin can revoke the link at any time.

## 10. Markdown export/import

**Not real-time.** Two CLI/UI commands:

- `task export --server <url> --project <id> --format obsidian
  --out <dir>` writes a full Obsidian-compatible vault to disk.
  `.md` files for pages, `.obsidian/` config, attachments in
  `_attachments/`.
- `task import --server <url> --path <dir>` reads markdown +
  frontmatter, creates Knowledge entities, pushes them via
  `apply_update`.

Round-trip stability is the bar: `export → import → export`
must produce byte-identical output for unchanged content.

No filesystem watcher, no real-time conflict resolution between
disk and Loro, no `inotify`. If you want to edit in Obsidian
between sync sessions, you export, edit, import. The CRDT
fixes any merge weirdness on import.

## 11. Cross-cutting design decisions

Quick decisions on the small stuff so we don't relitigate later.

| Question | Decision | Why |
|---|---|---|
| Doc-id naming | `<kind>/<uuid>`, server-local | Server URL already implies org |
| Capability format | Facet bytes + Ed25519 sig | Reuses our tooling; JWT is browser-centric overhead |
| Token in transport | `?cap=<base64url>` on WS URL | Standard; capability middleware reads it |
| ACL location | Project-doc root page frontmatter | Versioned + collaborative + mergeable |
| Person ↔ auth user link | `auth_user_id` frontmatter on Person page | Optional binding; Person pages exist without accounts |
| Anonymous peer id in Loro | `share-link-<token-id>` | Stable, but identifiable in history |
| Wiki link resolution | Server-side index `basename → page_id` | O(1) lookup, rebuilt on commit |
| Cross-vault refs in one server | Allowed; resolved at query time | Both vaults on the same server, same identity check |
| Cross-server refs (`[[work:Cody]]`) | Client-side resolution only | Server never knows about other servers |
| LoroDoc per project vs per page | Project for now; per-page if needed | Per-project is the natural sharing unit; pages are mostly small |
| Schema versioning | Facet `#[facet(default)]` on new fields | We're already on the vox schema-evolution path |
| ACL conflict resolution | Loro's default (last-write per peer) | Admins should coordinate; tombstones if needed later |
| Member impersonation | Defer to architect-auth's `ImpersonateUser` | Already implemented |

## 12. What we keep from existing work

**Everything we've built on `thin-vertical-slice` is reusable:**

- CRDT-over-vox transport (`WorkspaceSync`, `Tx<UpdateBytes>`)
- `entity_crdt!` macro
- Server-side broadcast + subscribe/apply_update split
- Stress + browser test infrastructure
- Knowledge proto + crdt copied in (one entity migration in flight)

**What needs to change:**

- `WorkspaceSync` gains a `doc_id` parameter (Phase 1).
- Server's `AppState` holds `HashMap<DocId, Arc<CrdtDoc>>` instead
  of a single workspace doc.
- A capability middleware lives in front of every vox dispatcher.
- The project/task `*RepoLoro`s we built migrate **into** the
  Knowledge model — Project becomes a `kind: project` page, Task
  becomes a `kind: task` block. The standalone `project-proto` /
  `project-crdt` crates eventually become legacy.

## 13. Implementation phases

Each phase is independently testable + commitable. No phase
should depend on a future phase.

### Phase 1: Doc-id transport (foundation)

- Change `WorkspaceSync::{subscribe, apply_update}` to take
  `doc_id: DocId`.
- Server's `HashMap<DocId, Arc<CrdtDoc>>` with LRU eviction.
- Persistence schema: add `doc_id` column to snapshot + update
  tables.
- Client `LiveSession::open(server_url, doc_id)` opens a session
  scoped to one doc.
- **Test**: two peers subscribe to two different doc ids; edits
  to doc A don't appear in doc B.

### Phase 2: architect-auth integration

- Add `architect-auth = { path = "../architect-auth/crates/architect-auth" }`
  to workspace deps.
- Mount auth vox services (`CreateEmailPasswordUser`,
  `SignInEmailPassword`, `CurrentSession`, etc.) on the existing
  `/vox` route.
- Server has its own SQLite for auth state (separate from CRDT
  persistence).
- Client `ServerRegistry` stores per-server session tokens.
- **Test**: create user, sign in, get a session token, use it in
  subsequent vox calls. (Capability layer not yet enforcing.)

### Phase 3: Capability middleware

- Define `CapabilityToken` + Ed25519 signing.
- `ServerMiddleware` parses `?cap=<token>` from the WS URL,
  attaches `Capability` to request context.
- Every dispatcher's `subscribe` and `apply_update` checks
  capability against the requested `doc_id`.
- **Test**: anonymous client with a valid share token can
  subscribe to the doc it's scoped to, can't subscribe to other
  docs.

### Phase 4: Project ACL + share-link service

- ACL frontmatter convention on project-doc root page.
- Server-side resolver: wiki-link → auth user id, via a maintained
  basename index over the org vault.
- `ShareService::{create, list, revoke}` vox methods.
- **Test**: project admin creates a share link, anonymous client
  redeems it, gets read-only access. Admin revokes, client's
  next subscribe fails.

### Phase 5: Knowledge as platform

- Migrate the in-progress Knowledge entities to `entity_crdt!`
  (pick up the paused work).
- Wire Knowledge `*Repo` dispatchers on `/vox`.
- Two-tier vault model: `vault/org` + `vault/project/<uuid>`.
- Backlink index maintained server-side.
- Frontmatter index maintained server-side.
- **Test**: create a Person page with wiki-link from a project
  member entry, ACL resolves it to grant access.

### Phase 6: Custom views

- `BasesQuery` parser + executor (port from main; ~1300 lines on
  knowledge-proto).
- A small library of view components: `KindList`, `KindKanban`,
  `KindCalendar`, `KindGallery`. Each is generic over "page set
  with these frontmatter shape expectations."
- **Test**: define a Bases query for `kind: task`, render as
  kanban grouped by `status`. Update a task's status frontmatter
  via the kanban drag-drop; observe the change in another tab.

### Phase 7: Attachments

- `AttachmentService` with `initiate_upload` + `get_download_url`.
- v0: local filesystem backend with axum-served signed URLs.
- A `kind: attachment` block convention with content_hash +
  filename + mime_type.
- **Test**: upload a file from one tab, download from another tab.
  Verify share-link with `AttachmentsOnly` scope can't subscribe
  to the project doc.

### Phase 8: Client federation UI

- `ServerRegistry` with add/remove server flow.
- Per-server identity in the sidebar ("signed in as
  cody@personal-server").
- Unified views: a "Tasks" view fans out queries to every
  connected server.
- **Test**: connect to two local servers, see tasks from both in
  one view.

### Phase 9: Markdown export/import

- `task export --project <id> --format obsidian --out <dir>`.
- `task import --path <dir>`.
- Round-trip stability test: export → import → export → assert
  bytes equal.

### Phase 10: Per-entity-kind sync (optimization)

- Within a doc, split the broadcast by entity kind.
- Subscribers specify the kinds they care about.
- **Test**: a client subscribed only to `kind: task` doesn't
  receive bytes when `kind: recording_session` blocks change.

## 14. Open questions

Things I want your input on before Phase 1 starts:

1. **Object store v0 backend.** Filesystem under
   `${TASK_DATA_DIR}/attachments/` is the simplest, but the
   per-server NixOS module would need to mount this directory.
   Alternative: SQLite BLOB for v0 with no separate storage. Slower
   but zero ops burden. Which?

2. **Server ID.** Each server needs a stable ID for tokens to
   carry. Options: UUID generated at install time + persisted; or
   derived from the public key. Public-key-derived is nicer
   because clients can verify a token was issued by the server
   they think it was.

3. **Architect-auth's storage backend.** It uses SeaORM. Same
   database as the CRDT persistence, or a separate file?
   Recommendation: separate file (`${DATA_DIR}/auth.db` vs
   `${DATA_DIR}/crdt.db`) so each can be backed up / restored
   independently.

4. **ACL conflict resolution.** Loro's default merges work for
   adds. If two admins concurrently *revoke* someone's access,
   that's also fine (idempotent). The hard case: admin A grants
   `[[Bob]]` write access; concurrently admin B revokes Bob.
   Last-write-wins per peer means one of them silently loses.
   Acceptable for v0, but worth flagging.

5. **Schema for `acl:` frontmatter.** I sketched it above. Does
   that shape work, or do you want explicit per-resource scopes
   (e.g. "Alice can read attachments but not tasks")?

6. **Anonymous user discoverability.** When an anonymous-share-link
   user makes an edit, do other users see "share-link-abc123"
   attribution, or a friendlier "Anonymous guest"? Server can
   substitute on display while keeping the stable id in Loro.

7. **Cross-server wiki link precedence.** If `[[Cody]]` resolves
   on multiple connected servers, which wins? Recommendation:
   first explicit hit (`[[personal:Cody]]`) wins; bare names
   resolve to the currently-focused server.

## 15. What this is NOT

To prevent scope creep:

- **NOT** a real-time filesystem sync (Obsidian Sync). Markdown
  round-trip is a button, not a daemon.
- **NOT** a full Notion/Tana clone. We're not building a
  database-as-pages UI surface at first. Pages with frontmatter
  + Bases is the simplest version of that idea.
- **NOT** end-to-end encrypted. Servers see the CRDT bytes. E2EE
  is a future direction but adds significant complexity.
- **NOT** P2P. There's always a server. Two clients sync via the
  server they share, never directly.
- **NOT** an attempt to replace Git for source code. The
  `kind: project` page might contain git URLs and commit refs,
  but the source repository itself stays in Git.

## 16. Decision

If this matches your mental model, implementation starts at
Phase 1. Each phase becomes its own short `plans/<phase>.md`
doc + a feature branch + a PR. The capability layer is the
biggest risk surface; we'll write end-to-end auth tests before
exposing any of it to the network.

If anything in this document is off — especially the actor
model (§2), the ACL approach (§6), or the phase ordering (§13)
— call it out before code lands.

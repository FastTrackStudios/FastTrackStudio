# Task — domain glossary

Vocabulary only. No implementation detail; decisions live in ADRs and
on their tickets.

- **Vault** — the small, fully-replicated layer: markdown notes and
  structured overlays (tasks, projects, events). Lives in full on every
  device a user is logged into. Offline-first, multiplayer.
- **Files** *(working name — final term unsettled)* — the large binary
  layer: audio/video/project media. Lives primarily on servers;
  reaches devices only by selective sync or NAS-transparent access.
  Distinct from the Vault: the Vault indexes it, never contains it.
- **Selective sync** — pulling a chosen project's Files onto a device
  for local work, with ignore rules (e.g. `.rpp-bak`). The opposite of
  the Vault's everything-everywhere replication.
- **NAS-transparent access** — using server-resident Files in place
  over the network (NFS today) as if local, without syncing them.
- **File version chain** — the automatic, per-file history of every
  saved state of a single file. Safety net, not user ceremony: nobody
  stages or commits.
- **Named Version** — a user-facing, deliberately labeled version of a
  deliverable ("v3 for client"). Curated on top of automatic chains.
- **Project Version** — a whole-project iteration. Restarting a project
  creates a new Project Version of the same project, replacing the
  "Project Title old" / "Project Title NEW" folder idiom.
- **Divergent versions** — when two machines save the same file
  concurrently, both saves survive as sibling versions to be merged or
  chosen later. There is no locking and no lost data.
- **File Root** — a folder tree with its own identity: a first-class
  vault entity (own note) that projects *reference*, never own. Roots
  never overlap on disk — one tree, one root, versioned once. A root's
  live tree sits wholly on one Storage Location; its version-store
  blobs may be placed across locations. Identified by a stable id in
  its entity plus a marker file in the tree; the (location, path)
  binding is mutable. Roots may live anywhere, including inside a
  vault folder — vault replication excludes root subtrees. Policies
  (versioning, retention, placement) live on the root itself.
- **Root slice** — a reference to (root, subpath): how subprojects,
  share links, and note-embedded widgets point at part of a root
  without creating a nested root.
- **Drive** — the raw, NextCloud-style browsing surface over Storage
  Locations: loose files outside any root. Projects are a convenient
  view over Files, not a cage; a per-user Home root covers personal
  files that still deserve versioning.
- **Storage Location** — a named place Files can live: a server volume,
  an S3 bucket, an external drive. Deployment-scoped: the operator
  registers locations; orgs reach them only through Storage grants.
  Each location declares its capability classes — hosting *live trees*
  (POSIX/NFS) and/or holding *blobs* (get/put) — and is spoken for by
  exactly one Storage agent. Task decides *placement* (which location
  holds what); physical tiering/redundancy below a location (SSD cache,
  RAID, ZFS) belongs to the substrate.
- **Storage grant** — an org's admission onto a Storage Location: a
  capability subset, a byte quota (counted logically — the bytes the
  org's roots reference, dedup savings belong to the operator), and a
  path prefix that is the org's own subtree on a shared volume.
- **Storage agent** — the process that speaks for a location: in-process
  in task-server for its own volumes, the desktop app's headless agent
  for a plugged-in drive, or a standalone agent on a NAS/storage box.
  One protocol, three hostings; agents announce their volumes, the
  operator approves. Agents, not the server, carry blob transfers
  between locations — the coordinator is never the data path.
- **Pointer stub** — a small placeholder file standing in for
  non-resident content inside a live tree (a dehydrated file). The
  agent hydrates on demand: explicitly, by root policy patterns, or on
  access through Task-mediated surfaces. Raw NFS reads a stub as a
  stub — no fault-in without FUSE.
- **Removable location** — a location on an external drive:
  replica-first (a tracked replica of server-primary roots for
  portable/offline work; offline edits reconcile as divergent
  versions), hosting a live tree only when specifically declared.
  Expected-offline is a health state, not an error.
- **Relocation** — the deliberate move of a root's live tree between
  locations: checkpoint, copy, verify, flip the (location, path)
  binding inside a declared unavailability window; the source is
  demoted to a read-only copy. Never automatic.
- **Session checkpoint** — the guarantee that matters: everything is
  versioned by the end of a working session. High-frequency writes
  during a session (a recording pass creating files every few minutes)
  need not each become a version.
- **Migration source** — a legacy store (e.g. `nextcloud-data`) that
  content is imported *from*; never written back to. NextCloud runs
  alongside indefinitely; it is not decommissioned by this effort.

# Media roots at scale: versioning without duplication, binding without names

**Status:** designed 2026-08-14, not started

Two blockers found while putting ~5 TB of real production media under
Task on a NAS with 1.1 TB free.

## 1. The first checkpoint copies everything

A media root's checkpoint streams file content into the CAS chunk store
(`content.rs`: media roots "write-then-compare, where the chunk store
dedups"). The first capture of a root therefore writes a **second copy**
of every byte. For one 268 GB `.braw` that is 268 GB; for the tree here
it is several terabytes that do not exist.

That is why registration currently has to be paired with either "never
checkpoint" or "ignore all media" — both of which give up version
control on exactly the files people most want to recover.

### The fix: reflink the content in

`/mnt/storage` is XFS with `reflink=1`, **verified** (`cp --reflink=always`
of a 200 MB file consumed no space). A reflink clone shares extents and
only duplicates blocks when one side diverges.

So the first checkpoint can cost metadata only, while keeping history
honest:

- live file unchanged → one set of blocks, two references
- live file edited → XFS copies only the changed extents; the
  checkpoint keeps the originals, so old versions remain real
- live file deleted → the store's reference keeps the blocks alive

Shape: in the CAS write path, a file above a threshold (~64–256 MB) is
stored **whole, via `FICLONE`**, rather than FastCDC-chunked. Small files
keep chunking.

The trade is losing chunk-level dedup between versions of a large file.
For media that is nearly worthless — a re-encode changes every byte —
while cheap snapshots of *unchanged* files are the dominant case by
orders of magnitude.

Requirement: source and store on one filesystem. Satisfied — a root's
version store lives in `.fts-files/` inside the root itself.

Fallback when `FICLONE` fails (different filesystem, non-reflink FS, an
older kernel): fall back to the existing streaming write, and say so in
the span. Silent full copies are how a disk fills at 3am.

## 2. Roots bind to projects by NAME

`org_tree.rs:130` matches `root.name == project_folder_name`, plus an
`Album — <name>` special case. Rename either side and the media silently
disappears from the project — no error, just an empty `Media/`.

Tonight produced three separate demonstrations:

- a folder whose name carries an invisible **U+F022** (private-use
  character from a Mac font) — an exact-name copy failed on it
- **three spellings** of one project ("El Artista Eres Tu", "El Artisa",
  "El Ariste De Tu")
- "Jaramillo" vs "Jaramillio", inconsistent within one project's own files

### The fix: bind by id

The project note carries the link, because the vault is the
human-editable source of truth and survives outside the app:

```yaml
media_roots:
  - 3f9c1e88-...
```

`projects_area` resolves by id, falling back to name-matching for
anything not yet linked — so nothing breaks mid-migration.

Two capabilities fall out that name-matching cannot express, and both
are already needed:

- **several roots on one project** — the Yokasta job is four separate
  piles (sessions, a Blackmagic reel, Canon A/B roll, a choir shoot)
- **one root on several projects** — the Village choir footage belongs
  to Goodness of God *and* Yokasta, which is why it is currently parked
  in an inbox rather than filed

## Sequencing

1. **Bind by id** — small, unblocks correct registration, no data risk.
2. **Reflink storage** — the bigger change; until it lands, register
   roots without checkpointing, or with media excluded via the per-root
   ignore set.
3. Only then turn checkpointing on across the tree.

## What is registrable today

Nothing yet, for a third reason: Storage Locations are admitted from
volumes an agent **announces**, and the in-server agent announces only
`primary` under the data root (the PVC). A NAS mount is invisible to the
registry no matter how it is granted. `TASK_STORAGE_VOLUMES`
(`key=/abs/path`, comma-separated) fixes that by announcing extra
volumes at boot; missing paths are skipped with a warning rather than
failing the boot.

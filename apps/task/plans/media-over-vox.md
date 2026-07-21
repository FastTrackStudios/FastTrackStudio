# Media over vox — retire the HTTP side-channels

## Why

Song stems (and soon part bundles, sample previews, video) currently
reach clients through HTTP paths bolted on next to vox: the signed-URL
`/blobs/download` route (now with Range/206), and the interim `/media`
ServeDir. Every one of those is a workaround for "the browser's `<audio>`
element wants a URL". The architecture we actually want is the architect
one: **all data — including media bytes — travels the per-org vox lane**,
same origin, same auth, one protocol.

## What exists now (this PR)

- `media-proto` — `MediaService`: `stat(hash) -> MediaInfo`,
  `read(hash, start, len, Tx<MediaChunk>)`. Content-addressed, same hash
  namespace as attachments; ranged reads; server chunks at 256 KiB.
- `task-server` mounts it per org next to `AttachmentService`
  (`MediaServiceImpl` is a read-side view over the attachment blob
  store + catalog).
- `crates/task/ui::vox_clients::media_client(slug)`.
- e2e: upload through the attachment flow → stream back over vox
  (`tests/media_stream_e2e.rs`).
- `task media stat|get|verify-song` — the binary↔binary E2E surface:
  `verify-song` streams every stem on a song note over the real vox
  wire, sha256s the bytes against the frontmatter `content_hash`, and
  reports throughput. Audio-streaming smoke tests need no browser.

## Migration steps (follow-ups)

1. **SongView playback over vox** — replace the per-stem
   `HTMLAudioElement src=<signed url>` graph with MediaSource Extensions:
   one `MediaSource` per stem fed by `media_client.read(...)` chunks
   (`SourceBuffer.appendBuffer`), seek = abort + new ranged `read`.
   Requires the transcode target to be MSE-friendly (`audio/webm;
   codecs=opus` — Chrome/Firefox; ogg-opus is NOT reliably MSE-supported,
   so `task song ingest` should emit webm/opus once this lands).
   Keep the signed-URL path as fallback until verified on real audio.
2. **Setlist player** — same switch (it still uses `/media` today).
3. **Retire `/media`** — once songs are ingested as attachments and both
   players stream over vox, drop `TASK_SERVER_MEDIA_DIR` + the chart
   `serverPaths` `/media` entry.
4. **Uploads over vox** (optional, later) — `write(Tx<MediaChunk>)`-style
   ingest so `task song ingest` needs no HTTP PUT either; the signed-URL
   upload flow stays for third-party/browser drag-drop compatibility.
5. **Backpressure + prefetch tuning** — vox lane throughput vs 20-36
   concurrent stem streams; likely one `read` per stem with a small
   look-ahead window driven by the transport clock, not N full-file
   streams (this also replaces the browser's 6-connection HTTP cap
   concern from issue #30 item 6 — multiplexed over ONE WebSocket).

## Non-goals

- Server-side mixing (issue #30 item 6) is orthogonal: when it lands it
  becomes ONE `read`-style stream of the mixed bus over the same
  MediaService shape.

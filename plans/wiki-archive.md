# Wiki archive — URL front door for the raw→ingest pipeline

`task wiki archive <url|file>` routes a URL through a
content-type router to an extractor, stamps the extracted
markdown with provenance frontmatter (`source_url`,
`canonical_url`, `content_type`, `archived_at`, `extractor`,
`media`, `duration`), and feeds it to the UNCHANGED
raw→ingest pipeline (`import_raw_source` sha-dedup + ingest
queue). Crate: `features/wiki/wiki-archive`.

Tracked issues: phase 1 `32e7b28f` (router, articles,
YouTube `^t<sec>` transcripts, Readwise/Karakeep/Pocket/
Netscape importers, SourceViewer), phase 2 (PDFs with page
anchors, podcasts/whisper), phase 3 (social, extractor
health).

## Conventions

- Archived source filenames: `<title-slug>-<canon8>.md` where
  `canon8` = first 8 hex of sha256(canonical URL). Dedup is a
  filename scan — no frontmatter parsing, no extra RPC.
- Timestamped media: transcript coalesced into ~45 s blocks,
  each anchored `^t<seconds>` (legal under the existing
  obsidian block-anchor grammar — `[[Sources/x#^t870]]` deep
  links work with zero parser changes). Curator notes go under
  `## Notes` as `- [mm:ss] … ^t<sec>-noteN`.

## Phase-1 follow-ups

- Binary originals → `Wiki/media/archive/<sha-prefix>/`
  (content-hash-addressed). Needs a media-write RPC on the
  wiki surface; today only `raw/sources/` lands via RPC, so
  videos record `media:` as the canonical watch URL instead.
- `--force` re-archive imports a fresh copy (suffixed file)
  rather than overwriting — RawLayer has no overwrite verb.
- yt-dlp runs as a CLI-side subprocess with retries; the
  server-side background-job version (retryable, never
  save-blocking) lands when archiving moves behind an RPC verb.
- bgutil POT-provider sidecar for YouTube bot-checks: not
  wired; "update yt-dlp" is the standing fix.
- SourceViewer v1 (`/wiki/source/:name`) is read-only with
  seek-on-anchor-click (IFrame postMessage). The "note at
  current time" button (getCurrentTime → insert
  `- [mm:ss] … ^t<sec>-noteN` under `## Notes` on the
  generated source page) is the next slice — it needs a
  write path to wiki pages from the web app.
- Imported bare bookmarks land as `content_type: bookmark`
  without fetching; canonical dedup then blocks a later full
  `task wiki archive <url>` of the same page unless --force.
  A `task wiki archive upgrade <source>` verb (re-extract in
  place) would resolve this cleanly.

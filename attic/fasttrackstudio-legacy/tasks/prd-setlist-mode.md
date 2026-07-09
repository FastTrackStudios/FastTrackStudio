# PRD: Project Mode ↔ Setlist Mode

## Overview
FastTrackStudio's session system manages setlists — ordered sequences of songs, each in a separate REAPER project tab. "Setlist Mode" adds a concatenated setlist project tab within the same REAPER instance that combines all songs into a single timeline. The two views (individual song tabs and the setlist tab) stay in sync bidirectionally — seeking in one mirrors to the other via an offset map. The setlist project can also be saved/exported as a standalone `.rpp` file.

Core challenge: **bidirectional position mapping** via an offset map, **track structure merging** (shared Click/Guide tracks, per-song folder hierarchies), and **incremental sync** (applying changes from song projects into the setlist project without full regeneration).

## Goals
- Enable a single REAPER instance to hold both individual song project tabs and a combined setlist project tab
- Provide bidirectional position sync between song tabs and the setlist tab via an offset map
- Generate a combined setlist project with proper track structure: merged Click/Guide tracks, per-song `{SONG NAME}/` folders under TRACKS
- Support incremental change application from song projects to the setlist project
- Allow full manual regeneration as a fallback
- Support saving the setlist project to disk or keeping it in-memory
- Handle count-in time as part of each song's allocated time in the setlist
- Concatenate tempo maps and time signatures per-song at correct offsets
- Add full-song regions at boundaries for easy drag-to-reorder

## Quality Gates

These commands must pass for every user story:
- `cargo check -p session-proto` — Type checking for offset map and structure types
- `cargo check -p dawfile-reaper` — Type checking for RPP generation
- `cargo test -p session-proto` — Unit tests for offset map and track structure
- `cargo test -p dawfile-reaper` — Tests for RPP concatenation

For integration stories, also include:
- `cargo check -p session` — Orchestration crate compiles

## Track Structure

### Individual Song Project
```
Click/Guide/
├── Click
├── Loop          (shaker track — created but empty for now)
├── Count
└── Guide
TRACKS/
├── (song's own tracks)
└── Reference/
    ├── Mix
    └── Stem Split/
        └── (stem tracks)
```

### Setlist Project (combined)
```
Click/Guide/
├── Click         (items from ALL songs merged, offset by global_start)
├── Loop          (items from ALL songs merged)
├── Count         (items from ALL songs merged)
└── Guide         (items from ALL songs merged)
TRACKS/
├── {Song 1 Name}/
│   ├── (song 1 tracks)
│   └── Reference/
│       ├── Mix
│       └── Stem Split/
│           └── (stem tracks)
├── {Song 2 Name}/
│   ├── (song 2 tracks)
│   └── Reference/
│       ├── Mix
│       └── Stem Split/
│           └── (stem tracks)
└── ...
```

Songs with no content tracks still produce `{SONG NAME}/Reference/Stem Split/` skeleton.

## User Stories

### US-001: SetlistOffsetMap types and construction
**Description:** As a developer, I want pure data types for the setlist offset map so that I can map positions between song-local and setlist-global coordinate spaces.

**Acceptance Criteria:**
- [ ] `SongOffset` struct with fields: `index`, `song_id`, `project_guid`, `global_start_seconds`, `global_start_qn`, `duration_seconds`, `duration_qn`, `count_in_seconds`, `start_tempo`, `start_time_sig`
- [ ] `SetlistOffsetMap` struct with `songs: Vec<SongOffset>`, `total_seconds`, `total_qn`
- [ ] `SetlistOffsetMap::from_setlist(&Setlist) -> SetlistOffsetMap` builds the map from existing `Setlist` songs using `song.start_seconds`, `song.end_seconds`, `song.tempo`, `song.count_in_seconds`
- [ ] Cumulative offsets computed correctly — each song's `global_start_seconds` = sum of all preceding songs' durations (including count-ins)
- [ ] Module at `session-proto/src/offset_map.rs`, re-exported from `session-proto/src/lib.rs`

### US-002: Bidirectional position conversion methods
**Description:** As a developer, I want methods to convert positions between song-local and setlist-global time so that seeking in one view can be mirrored in the other.

**Acceptance Criteria:**
- [ ] `project_to_setlist(song_index: usize, local_seconds: f64) -> Option<f64>` returns global seconds
- [ ] `setlist_to_project(global_seconds: f64) -> Option<(usize, f64)>` returns `(song_index, local_seconds)` via binary search
- [ ] QN variants: `project_to_setlist_qn(song_index, local_qn) -> Option<f64>` and `setlist_to_project_qn(global_qn) -> Option<(usize, f64)>`
- [ ] Edge cases handled: position before first song, after last song, exactly on song boundaries
- [ ] Roundtrip property: `setlist_to_project(project_to_setlist(idx, pos)) == (idx, pos)` for valid inputs
- [ ] Unit tests covering: basic conversion, boundary positions, multi-song roundtrip, out-of-range inputs

### US-003: Setlist track structure types
**Description:** As a developer, I want Rust types that define the canonical track folder structure for both individual song projects and the combined setlist project, so that RPP generation can build the correct hierarchy.

**Acceptance Criteria:**
- [ ] `SetlistTrackStructure` type representing the full setlist project track tree
- [ ] Structure encodes the hierarchy documented in "Track Structure" section above
- [ ] `SongTrackMapping` type that maps a song's local tracks to their setlist-project counterparts (for sync identity)
- [ ] Songs with no actual tracks still produce `{SONG NAME}/Reference/Stem Split/` structure
- [ ] Track identity: `TrackIdentity` enum with GUID-preferred, name-path fallback for matching during incremental sync
- [ ] Types are pure data (no DAW dependency), in `session-proto/src/track_structure.rs`

### US-004: Track concatenation for RPP generation
**Description:** As a developer, I want to generate a combined RPP where Click/Guide items from all songs are merged onto shared tracks, and each song's content tracks appear under `{SONG NAME}/` folders.

**Acceptance Criteria:**
- [ ] Click/Guide tracks: items from each song placed on shared Click, Loop, Count, Guide tracks at correct time offsets (`global_start_seconds`)
- [ ] Content tracks: each song's tracks cloned under `TRACKS/{SONG NAME}/` folder with item positions offset by `global_start_seconds`
- [ ] Reference tracks: each song's Reference/Mix and Stem Split tracks placed under `{SONG NAME}/Reference/`
- [ ] New GUIDs generated for cloned tracks to avoid collision
- [ ] Module at `dawfile-reaper/src/setlist_rpp.rs`

### US-005: Tempo envelope concatenation
**Description:** As a developer, I want the setlist project's tempo envelope to be the concatenation of each song's tempo map at the correct offsets, so that playback timing is correct across the full timeline.

**Acceptance Criteria:**
- [ ] Each song's tempo envelope points offset by `global_start_seconds` / `global_start_qn`
- [ ] Boundary tempo points inserted at song transitions
- [ ] Time signature changes from each song propagated at correct positions
- [ ] Songs with different tempos/time signatures handled correctly
- [ ] Uses existing `TempoTimeEnvelope` type from dawfile-reaper

### US-006: Region and marker generation
**Description:** As a developer, I want the setlist project to have full-song regions at each song boundary for easy visual identification and drag-to-reorder, plus offset internal markers.

**Acceptance Criteria:**
- [ ] A region spanning each song's full duration added at `global_start_seconds` to `global_start_seconds + duration`
- [ ] Region named with song name for identification
- [ ] Each song's internal markers (sections, comments) offset by `global_start_seconds`
- [ ] Region/marker IDs re-numbered to avoid collisions across songs

### US-007: Incremental change application
**Description:** As a developer, I want to apply changes from a song project to the setlist project incrementally (without full regeneration) so that edits in individual song tabs propagate efficiently.

**Acceptance Criteria:**
- [ ] Detect which song project changed (via `project_guid` matching)
- [ ] Match tracks between song project and setlist project using `TrackIdentity` (GUID preferred, name-path fallback)
- [ ] Apply structural changes: track add/remove reflected in setlist project's `{SONG NAME}/` folder
- [ ] Click/Guide items for the changed song updated on shared tracks (remove old, insert new at correct offset)
- [ ] If song duration changes, recalculate offset map and shift all subsequent songs' positions
- [ ] Full regeneration available as manual fallback

### US-008: Setlist project persistence
**Description:** As a developer, I want the setlist project to be generatable in-memory and optionally saveable to disk as an `.rpp` file.

**Acceptance Criteria:**
- [ ] `generate_setlist_rpp(song_rpps, offset_map) -> ReaperProject` produces in-memory project
- [ ] `save_setlist_rpp(project, path) -> Result<()>` writes to disk
- [ ] Generated RPP can be reopened independently in REAPER
- [ ] Orchestration module at `session/src/setlist_mode.rs`

### US-009: Bidirectional position sync in session
**Description:** As a developer, I want the session layer to handle bidirectional position sync between song tabs and the setlist tab within the same REAPER instance.

**Acceptance Criteria:**
- [ ] When user seeks in a song tab, the setlist tab's cursor moves to the corresponding global position
- [ ] When user seeks in the setlist tab, the correct song tab is activated and its cursor moves to the corresponding local position
- [ ] Sync uses `SetlistOffsetMap` for all position translation
- [ ] Transport state (play/pause) synced between views
- [ ] No infinite feedback loops (seek in A → mirror to B → does NOT re-mirror back to A)

### US-010: SetlistService RPC method
**Description:** As a developer, I want a `generate_setlist_rpp` method on the `SetlistService` so that the UI and CLI can trigger setlist project generation.

**Acceptance Criteria:**
- [ ] `generate_setlist_rpp` added to `SetlistService` trait in `session-proto/src/services.rs`
- [ ] Method accepts output path (optional — `None` for in-memory only)
- [ ] Method returns the `SetlistOffsetMap` for use by the caller
- [ ] Error cases handled: missing song projects, parse failures

## Functional Requirements
- FR-1: The offset map must compute cumulative start times including count-in durations for each song
- FR-2: Position conversion must use binary search for O(log n) setlist-to-project lookups
- FR-3: Track merging must preserve Click/Guide as shared tracks and create per-song folders for content tracks
- FR-4: Songs with no content tracks must still produce the `{SONG NAME}/Reference/Stem Split/` folder structure
- FR-5: Generated RPP must be valid and parseable by REAPER
- FR-6: Incremental sync must identify tracks by GUID first, falling back to name/path matching
- FR-7: Duration changes in a song must trigger offset recalculation for all subsequent songs
- FR-8: All position mapping types and methods must be pure (no DAW dependency) and live in `session-proto`
- FR-9: RPP generation logic must live in `dawfile-reaper` and use existing `RChunk`/`write_rpp` infrastructure
- FR-10: Bidirectional sync must include loop-prevention to avoid infinite seek cascades

## Non-Goals
- Cross-instance sync (both views are in the same REAPER instance)
- Real-time audio streaming between project tabs
- Merging FX chains across songs (FX stay per-track as cloned)
- Loop track content generation (track is created but empty for now)
- Drag-to-reorder UI implementation (regions enable it, but the UX is out of scope)

## Technical Considerations
- **Existing types to reuse:** `Song.{start_seconds, end_seconds, tempo, time_signature, count_in_seconds}`, `Setlist`, `TempoTimeEnvelope`, `RChunk`, `write_rpp()`
- **Crate facade pattern:** All new session-proto types are re-exported through the `session` facade
- **Facet constraint:** Service method params must implement Facet; max 4 params per method — use request structs if needed
- **Track structure:** Individual song projects have the same hierarchy minus the `{SONG NAME}` level — tracks and Reference go directly under TRACKS

## Success Metrics
- Offset map roundtrip conversion is lossless for all valid positions
- Generated RPP opens in REAPER with correct track structure, tempo, and item positions
- Incremental sync correctly propagates single-song edits without full regeneration
- Bidirectional seek works without feedback loops

## Open Questions
- What gap (if any) should exist between songs in the setlist timeline beyond count-in time?
- Should the Loop (shaker) track be pre-populated with silence items or left completely empty?
- How should conflicting track names across songs be handled (e.g., two songs both have a track called "Guitar")?
